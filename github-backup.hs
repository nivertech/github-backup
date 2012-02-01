{- github-backup
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import System.Environment
import Control.Exception (bracket, try, SomeException)
import Text.Show.Pretty
import Control.Monad.State.Strict
import qualified Github.Data.Readable as Github
import qualified Github.Repos as Github
import qualified Github.Repos.Forks as Github
import qualified Github.PullRequests as Github
import qualified Github.Repos.Watching as Github
import qualified Github.Issues as Github
import qualified Github.Issues.Comments
import qualified Github.Issues.Milestones

import Common
import Utility.State
import qualified Git
import qualified Git.Construct
import qualified Git.Config
import qualified Git.Types
import qualified Git.Command
import qualified Git.Ref
import qualified Git.Branch

-- A github user and repo.
data GithubUserRepo = GithubUserRepo String String
	deriving (Eq, Show, Read, Ord)

toGithubUserRepo :: Github.Repo -> GithubUserRepo
toGithubUserRepo r = GithubUserRepo 
	(Github.githubUserLogin $ Github.repoOwner r)
	(Github.repoName r)

repoUrl :: GithubUserRepo -> String
repoUrl (GithubUserRepo user remote) =
	"git://github.com/" ++ user ++ "/" ++ remote ++ ".git"

repoWikiUrl :: GithubUserRepo -> String
repoWikiUrl (GithubUserRepo user remote) =
	"git://github.com/" ++ user ++ "/" ++ remote ++ ".wiki.git"

-- A name for a github api call.
type ApiName = String

-- A request to make of github. It may have an extra parameter.
data Request = RequestSimple ApiName GithubUserRepo
	| RequestNum ApiName GithubUserRepo Int
	deriving (Eq, Show, Read, Ord)

requestRepo :: Request -> GithubUserRepo
requestRepo (RequestSimple _ repo) = repo
requestRepo (RequestNum _ repo _) = repo

requestName :: Request -> String
requestName (RequestSimple name _) = name
requestName (RequestNum name _ _) = name

data BackupState = BackupState
	{ failedRequests :: S.Set Request
	, retriedRequests :: S.Set Request
	, gitRepo :: Git.Repo
	}

{- Our monad. -}
newtype Backup a = Backup { runBackup :: StateT BackupState IO a }
	deriving (
		Monad,
		MonadState BackupState,
		MonadIO,
		Functor,
		Applicative
	)

inRepo :: (Git.Repo -> IO a) -> Backup a
inRepo a = liftIO . a =<< getState gitRepo

failedRequest :: Request -> Github.Error-> Backup ()
failedRequest req e = unless (ignorable e) $ do
	set <- getState failedRequests
	changeState $ \s -> s { failedRequests = S.insert req set }
	where
		ignorable (Github.JsonError m) =
			"disabled for this repo" `isInfixOf` m
		ignorable _ = False

runRequest :: Request -> Backup ()
runRequest req = do
	-- avoid re-running requests that were already retried
	retried <- getState retriedRequests
	unless (S.member req retried) $
		(lookupApi req) req

type Storer = Request -> Backup ()
data ApiListItem = ApiListItem ApiName Storer Bool
apiList :: [ApiListItem]
apiList = 
	[ ApiListItem "userrepo" userrepoStore True
	, ApiListItem "watchers" watchersStore True
	, ApiListItem "pullrequests" pullrequestsStore True
	, ApiListItem "pullrequest" pullrequestStore False
	, ApiListItem "milestones" milestonesStore True
	, ApiListItem "issues" issuesStore True
	, ApiListItem "issuecomments" issuecommentsStore False
	-- comes last because it recurses on to the forks
	, ApiListItem "forks" forksStore True
	]

{- Map of Github api calls we can make to store their data. -}
api :: M.Map ApiName Storer
api = M.fromList $ map (\(ApiListItem n s _) -> (n, s)) apiList

{- List of toplevel api calls that are followed to get all data. -}
toplevelApi :: [ApiName]
toplevelApi = map (\(ApiListItem n _ _) -> n) $
	filter (\(ApiListItem _ _ toplevel) -> toplevel) apiList

lookupApi :: Request -> Storer
lookupApi req = fromMaybe bad $ M.lookup name api
	where
		name = requestName req
		bad = error $ "internal error: bad api call: " ++ name

userrepoStore :: Storer
userrepoStore = simpleHelper Github.userRepo $ \req r -> do
	when (Github.repoHasWiki r == Just True) $
		updateWiki $ toGithubUserRepo r
	store "repo" req r

watchersStore :: Storer
watchersStore = simpleHelper Github.watchersFor $ storeSorted "watchers"

pullrequestsStore :: Storer
pullrequestsStore = simpleHelper Github.pullRequestsFor $
	forValues $ \req r -> do
		let repo = requestRepo req
		let n = Github.pullRequestNumber r
		runRequest $ RequestNum "pullrequest" repo n

pullrequestStore :: Storer
pullrequestStore = numHelper Github.pullRequest $ \n ->
	store ("pullrequest" </> show n)

milestonesStore :: Storer
milestonesStore = simpleHelper Github.Issues.Milestones.milestones $
	forValues $ \req m -> do
		let n = Github.milestoneNumber m
		store ("milestone" </> show n) req m

issuesStore :: Storer
issuesStore = withHelper Github.issuesForRepo [] $ forValues $ \req i -> do
	let repo = requestRepo req
	let n = Github.issueNumber i
	store ("issue" </> show n) req i
	runRequest (RequestNum "issuecomments" repo n)

issuecommentsStore :: Storer
issuecommentsStore = numHelper Github.Issues.Comments.comments $ \n ->
	forValues $ \req c -> do
		let i = Github.issueCommentId c
		store ("issue" </> show n ++ "_comment" </> show i) req c

forksStore :: Storer
forksStore = simpleHelper Github.forksFor $ \req fs -> do
	storeSorted "forks" req fs
	mapM_ (traverse . toGithubUserRepo) fs
	where
		traverse fork = whenM (addFork fork) $
			gatherMetaData fork

forValues :: (Request -> v -> Backup ()) -> Request -> [v] -> Backup ()
forValues handle req vs = forM_ vs (handle req)

type ApiCall v = String -> String -> IO (Either Github.Error v)
type ApiWith v b = String -> String -> b -> IO (Either Github.Error v)
type ApiNum v = ApiWith v Int
type Handler v = Request -> v -> Backup ()
type Helper = Request -> Backup ()

simpleHelper :: ApiCall v -> Handler v -> Helper
simpleHelper call handle req@(RequestSimple _ (GithubUserRepo user repo)) =
	either (failedRequest req) (handle req) =<< liftIO (call user repo)
simpleHelper _ _ r = badRequest r

withHelper :: ApiWith v b -> b -> Handler v -> Helper
withHelper call b handle req@(RequestSimple _ (GithubUserRepo user repo)) =
	either (failedRequest req) (handle req) =<< liftIO (call user repo b)
withHelper _ _ _ r = badRequest r

numHelper :: ApiNum v -> (Int -> Handler v) -> Helper
numHelper call handle req@(RequestNum _ (GithubUserRepo user repo) num) =
	either (failedRequest req) (handle num req) =<< liftIO (call user repo num)
numHelper _ _ r = badRequest r

badRequest :: Request -> a
badRequest r = error $ "internal error: bad request type " ++ show r

store :: Show a => FilePath -> Request -> a -> Backup ()
store filebase req val = do
	file <- location (requestRepo req) <$> workDir
	liftIO $ do
		createDirectoryIfMissing True (parentDir file)
		writeFile file (ppShow val)
	where
		location (GithubUserRepo user repo) workdir =
			workdir </> user ++ "_" ++ repo </> filebase

workDir :: Backup FilePath
workDir = (</>)
		<$> (Git.gitDir <$> getState gitRepo)
		<*> pure "github-backup.tmp"

storeSorted :: Ord a => Show a => FilePath -> Request -> [a] -> Backup ()
storeSorted file req val = store file req (sort val)

gitHubRepos :: Backup [Git.Repo]
gitHubRepos = fst . unzip . gitHubPairs <$> getState gitRepo

gitHubRemotes :: Backup [GithubUserRepo]
gitHubRemotes = snd . unzip . gitHubPairs <$> getState gitRepo

gitHubPairs :: Git.Repo -> [(Git.Repo, GithubUserRepo)]
gitHubPairs = filter (not . wiki ) . mapMaybe check . Git.Types.remotes
	where
		check r@Git.Repo { Git.Types.location = Git.Types.Url u } =
			headMaybe $ mapMaybe (checkurl r $ show u) gitHubUrlPrefixes
		check _ = Nothing
		checkurl r u prefix
			| prefix `isPrefixOf` u && length bits == 2 =
				Just (r,
					GithubUserRepo (bits !! 0)
						(dropdotgit $ bits !! 1))
			| otherwise = Nothing
			where
				rest = drop (length prefix) u
				bits = split "/" rest
		dropdotgit s
			| ".git" `isSuffixOf` s = take (length s - length ".git") s
			| otherwise = s
		wiki (_, GithubUserRepo _ u) = ".wiki" `isSuffixOf` u

{- All known prefixes for urls to github repos. -}
gitHubUrlPrefixes :: [String]
gitHubUrlPrefixes = 
	[ "git@github.com:"
	, "git://github.com/"
	, "https://github.com/"
	, "http://github.com/"
	, "ssh://git@github.com/~/"
	]

onGithubBranch :: Git.Repo -> IO () -> IO ()
onGithubBranch r a = bracket prep cleanup (const a)
	where
		prep = do
			oldbranch <- Git.Branch.current r
			when (oldbranch == Just branchref) $
				error $ "it's not currently safe to run github-backup while the " ++
					branchname ++ " branch is checked out!"
			exists <- Git.Ref.matching branchref r
			if null exists
				then checkout [Param "--orphan", Param branchname]
				else checkout [Param branchname]
			return oldbranch
		cleanup Nothing = return ()
		cleanup (Just oldbranch)
			| name == branchname = return ()
			| otherwise = checkout [Param "--force", Param name]
			where
				name = show $ Git.Ref.base oldbranch
		checkout params = Git.Command.run "checkout" (Param "-q" : params) r
		branchname = "github"
		branchref = Git.Ref $ "refs/heads/" ++ branchname

{- Commits all files in the workDir into git, and deletes it. -}
commitWorkDir :: Backup ()
commitWorkDir = do
	dir <- workDir
	r <- getState gitRepo
	let git_false_worktree ps = boolSystem "git" $
		[ Param ("--work-tree=" ++ dir)
		, Param ("--git-dir=" ++ Git.gitDir r)
		] ++ ps
	liftIO $ whenM (doesDirectoryExist dir) $ onGithubBranch r $ do
		_ <- git_false_worktree [ Param "add", Param "." ]
		_ <- git_false_worktree [ Param "commit",
			 Param "-a", Param "-m", Param "github-backup"]
		removeDirectoryRecursive dir

updateWiki :: GithubUserRepo -> Backup ()
updateWiki fork = do
	remotes <- Git.remotes <$> getState gitRepo
	if any (\r -> Git.remoteName r == Just remote) remotes
		then do
			_ <- fetchwiki
			return ()
		else do
			-- github often does not really have a wiki,
			-- don't bloat config if there is none
			unlessM (addRemote remote $ repoWikiUrl fork) $
				removeRemote remote
			return ()
	where
		fetchwiki = inRepo $ Git.Command.runBool "fetch" [Param remote]
		remote = remoteFor fork
		remoteFor (GithubUserRepo user repo) =
			"github_" ++ user ++ "_" ++ repo ++ ".wiki"

addFork :: GithubUserRepo -> Backup Bool
addFork fork = do
	remotes <- gitHubRemotes
	if fork `elem` remotes
		then return False
		else do
			liftIO $ putStrLn $ "New fork: " ++ repoUrl fork
			_ <- addRemote (remoteFor fork) (repoUrl fork)
			return True
	where
		remoteFor (GithubUserRepo user repo) =
			"github_" ++ user ++ "_" ++ repo

{- Adds a remote, also fetching from it. -}
addRemote :: String -> String -> Backup Bool
addRemote remotename remoteurl =
	inRepo $ Git.Command.runBool "remote"
		[ Param "add"
		, Param "-f"
		, Param remotename
		, Param remoteurl
		]

removeRemote :: String -> Backup ()
removeRemote remotename = do
	_ <- inRepo $ Git.Command.runBool "remote"
		[ Param "rm"
		, Param remotename
		]
	return ()

{- Fetches from the github remote. Done by github-backup, just because
 - it would be weird for a backup to not fetch all available data.
 - Even though its real focus is on metadata not stored in git. -}
fetchRepo :: Git.Repo -> Backup Bool
fetchRepo repo = inRepo $
	Git.Command.runBool "fetch"
		[Param $ fromJust $ Git.Types.remoteName repo]

{- Gathers metadata for the repo. Retuns a list of files written
 - and a list that may contain requests that need to be retried later. -}
gatherMetaData :: GithubUserRepo -> Backup ()
gatherMetaData repo = do
	liftIO $ putStrLn $ "Gathering metadata for " ++ repoUrl repo ++ " ..."
	mapM_ call toplevelApi
	where
		call name = runRequest $ RequestSimple name repo

storeRetry :: [Request] -> Git.Repo -> IO ()
storeRetry [] r = do
	_ <- try $ removeFile (retryFile r) :: IO (Either SomeException ()) 
	return ()
storeRetry retryrequests r = writeFile (retryFile r) (show retryrequests)

loadRetry :: Git.Repo -> IO [Request]
loadRetry r = maybe [] (fromMaybe [] . readish)
	<$> catchMaybeIO (readFileStrict (retryFile r))

retryFile :: Git.Repo -> FilePath
retryFile r = Git.gitDir r </> "github-backup.todo"

retry :: Backup (S.Set Request)
retry = do
	todo <- inRepo loadRetry
	unless (null todo) $ do
		liftIO $ putStrLn $
			"Retrying " ++ show (length todo) ++
			" requests that failed last time..."
		mapM_ runRequest todo
	retriedfailed <- getState failedRequests
	changeState $ \s -> s
		{ failedRequests = S.empty
		, retriedRequests = S.fromList todo
		}
	return retriedfailed

summarizeRequests :: [Request] -> [String]
summarizeRequests = go M.empty
	where
		go m [] = map format $ sort $ map swap $ M.toList m
		go m (r:rs) = go (M.insertWith (+) (requestName r) (1 :: Integer) m) rs
		format (num, name) = show num ++ "\t" ++ name
		swap (a, b) = (b, a)

{- Save all backup data. Files that were written to the workDir are committed.
 - Requests that failed are saved for next time. Requests that were retried
 - this time and failed are ordered last, to ensure that we don't get stuck
 - retrying the same requests and not making progress when run again.
 -}
save :: S.Set Request -> Backup ()
save retriedfailed = do
	commitWorkDir
	failed <- getState failedRequests
	let toretry = S.toList failed ++ S.toList retriedfailed
	inRepo $ storeRetry toretry
	unless (null toretry) $
		error $ unlines $
			["Backup may be incomplete; " ++ 
				show (length toretry) ++ " requests failed:"
			] ++ map ("  " ++) (summarizeRequests toretry) ++ 
			[ "Run again later."
			]

newState :: Git.Repo -> BackupState
newState = BackupState S.empty S.empty

backupRepo :: Git.Repo -> IO ()
backupRepo repo = evalStateT (runBackup go) . newState =<< Git.Config.read repo
	where
		go = do
			retriedfailed <- retry
			remotes <- gitHubPairs <$> getState gitRepo
			when (null remotes) $
				error "no github remotes found"
			forM_ remotes $ \(r, remote) -> do
				_ <- fetchRepo r
				gatherMetaData remote
			save retriedfailed

backupName :: String -> IO ()
backupName name = do
	userrepos <- either (const []) id <$>
		Github.userRepos name Github.All
	orgrepos <- either (const []) id <$>
		Github.organizationRepos name
	let repos = userrepos ++ orgrepos
	when (null repos) $
		error $ "No GitHub repositories found for " ++ name
	status <- forM repos $ \repo -> do
		let dir = Github.repoName repo
		unlessM (doesDirectoryExist dir) $ do
			putStrLn $ "New repository: " ++ dir
			ok <- boolSystem "git"
				[ Param "clone"
				, Param (Github.repoGitUrl repo)
				, Param dir
				]
			unless ok $ error "clone failed"
		try (backupRepo =<< Git.Construct.fromPath dir)
			:: IO (Either SomeException ())
	unless (null $ lefts status) $
		error "Failed to successfully back everything up. Run again later."

usage :: String
usage = "usage: github-backup [username|organization]"

main :: IO ()
main = getArgs >>= go
	where
		go [] = backupRepo =<< Git.Construct.fromCwd
		go (name:[]) = backupName name
		go _= error usage

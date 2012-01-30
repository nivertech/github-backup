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
data RequestBase = RequestBase ApiName GithubUserRepo
	deriving (Eq, Show, Read, Ord)
data Request = RequestSimple RequestBase
	| RequestNum RequestBase Int
	deriving (Eq, Show, Read, Ord)

requestRepo :: Request -> GithubUserRepo
requestRepo (RequestSimple (RequestBase _ repo)) = repo
requestRepo (RequestNum (RequestBase _ repo) _) = repo

data BackupState = BackupState
	{ failedRequests :: S.Set Request
	, retriedRequests :: S.Set Request
	, backupRepo :: Git.Repo
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
inRepo a = liftIO . a =<< getState backupRepo

failedRequest :: Request -> Github.Error-> Backup ()
failedRequest req e = unless (ignorable e) $ do
	set <- getState failedRequests
	changeState $ \s -> s { failedRequests = S.insert req set }
	where
		ignorable (Github.JsonError m) =
			"disabled for this repo" `isInfixOf` m
		ignorable _ = False

runRequest :: Request -> Backup ()
runRequest req@(RequestSimple base) = runRequest' base req
runRequest req@(RequestNum base _) = runRequest' base req
runRequest' :: RequestBase -> Request -> Backup ()
runRequest' base req = do
	-- avoid re-running requests that were already retried
	retried <- getState retriedRequests
	unless (S.member req retried) $
		(lookupApi base) req

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
	, ApiListItem "moreforks" moreforksStore False
	]

{- Map of Github api calls we can make to store their data. -}
api :: M.Map ApiName Storer
api = M.fromList $ map (\(ApiListItem n s _) -> (n, s)) apiList

{- List of toplevel api calls that are followed to get all data. -}
toplevelApi :: [ApiName]
toplevelApi = map (\(ApiListItem n _ _) -> n) $
	filter (\(ApiListItem _ _ toplevel) -> toplevel) apiList

lookupApi :: RequestBase -> Storer
lookupApi (RequestBase name _) = fromMaybe bad $ M.lookup name api
	where
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
		runRequest $ RequestNum (RequestBase "pullrequest" repo) n

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
	runRequest (RequestNum (RequestBase "issuecomments" repo) n)

issuecommentsStore :: Storer
issuecommentsStore = numHelper Github.Issues.Comments.comments $ \n ->
	forValues $ \req c -> do
		let i = Github.issueCommentId c
		store ("issue" </> show n ++ "_comment" </> show i) req c

forksStore :: Storer
forksStore = simpleHelper Github.forksFor $ handleForks 1

moreforksStore :: Storer
moreforksStore = numHelper Github.forksForPage handleForks

handleForks :: Int -> Request -> [Github.Issues.Milestones.Repo] -> Backup ()
handleForks _ _ [] = return () -- reached an empty page
handleForks page req fs = do
	storeAppend "forks" req fs
	let repo = requestRepo req
	mapM_ (traverse . toGithubUserRepo) fs
	runRequest (RequestNum (RequestBase "moreforks" repo) (page+1))
	where
		traverse fork = whenM (addFork fork) $
			gatherMetaData fork

forValues :: (Request -> v -> Backup ()) -> Request -> [v] -> Backup ()
forValues handle req vs = forM_ vs (handle req)

type ApiCall v = String -> String -> IO (Either Github.Error v)
type ApiWith v b = String -> String -> b -> IO (Either Github.Error v)
type ApiNum v = ApiWith v Int
type Handler v = Request -> v -> Backup ()

simpleHelper :: ApiCall v -> Handler v -> Storer
simpleHelper call handle req@(RequestSimple (RequestBase _ (GithubUserRepo user repo))) =
	either (failedRequest req) (handle req) =<< liftIO (call user repo)
simpleHelper _ _ r = badRequest r

withHelper :: ApiWith v b -> b -> Handler v -> Storer
withHelper call b handle req@(RequestSimple (RequestBase _ (GithubUserRepo user repo))) =
	either (failedRequest req) (handle req) =<< liftIO (call user repo b)
withHelper _ _ _ r = badRequest r

numHelper :: ApiNum v -> (Int -> Handler v) -> Storer
numHelper call handle req@(RequestNum (RequestBase _ (GithubUserRepo user repo)) num) =
	either (failedRequest req) (handle num req) =<< liftIO (call user repo num)
numHelper _ _ r = badRequest r

badRequest :: Request -> a
badRequest r = error $ "internal error: bad request type " ++ show r

store :: Show a => FilePath -> Request -> a -> Backup ()
store filebase req val = do
	file <- storedFile filebase (requestRepo req)
	liftIO $ do
		createDirectoryIfMissing True (parentDir file)
		writeFile file (ppShow val)

workDir :: Backup FilePath
workDir = (</>)
		<$> (Git.gitDir <$> getState backupRepo)
		<*> pure "github-backup.tmp"

storeSorted :: Ord a => Show a => FilePath -> Request -> [a] -> Backup ()
storeSorted filebase req val = store filebase req (sort val)

storeAppend :: Read a => Ord a => Show a => FilePath -> Request -> [a] -> Backup ()
storeAppend filebase req val = do
	old <- readold =<< storedFile filebase (requestRepo req)
	storeSorted filebase req $ S.toList $ S.union
		(S.fromList val)
		(S.fromList $ fromMaybe [] $ readish old)
	where
		readold f = liftIO $ catchDefaultIO (readFileStrict f) []

storedFile :: FilePath -> GithubUserRepo -> Backup FilePath
storedFile file (GithubUserRepo user repo) = do
	top <- workDir
	return $ top </> user ++ "_" ++ repo </> file

gitHubRepos :: Backup [Git.Repo]
gitHubRepos = fst . unzip . gitHubPairs <$> getState backupRepo

gitHubRemotes :: Backup [GithubUserRepo]
gitHubRemotes = snd . unzip . gitHubPairs <$> getState backupRepo

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
	r <- getState backupRepo
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
	remotes <- Git.remotes <$> getState backupRepo
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
		call name = runRequest $
			RequestSimple $ RequestBase name repo

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
		error $ "Backup may be incomplete; " ++
			show (length toretry) ++
			" requests failed. Run again later."

newState :: Git.Repo -> BackupState
newState = BackupState S.empty S.empty

backup :: Git.Repo -> IO ()
backup repo = evalStateT (runBackup go) . newState =<< Git.Config.read repo
	where
		go = do
			retriedfailed <- retry
			remotes <- gitHubPairs <$> getState backupRepo
			when (null remotes) $
				error "no github remotes found"
			forM_ remotes $ \(r, remote) -> do
				_ <- fetchRepo r
				gatherMetaData remote
			save retriedfailed

backupUser :: String -> IO ()
backupUser username = do
	repos <- either (error . show) id <$>
		Github.userRepos username Github.All
	when (null repos) $
		error $ "No GitHub repositories found for user " ++ username
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
		try (backup =<< Git.Construct.fromPath dir)
			:: IO (Either SomeException ())
	unless (null $ lefts status) $
		error "Failed to successfully back everything up. Run again later."

usage :: String
usage = "usage: github-backup [username]"

main :: IO ()
main = getArgs >>= go
	where
		go [] = backup =<< Git.Construct.fromCwd
		go (username:[]) = backupUser username
		go _= error usage

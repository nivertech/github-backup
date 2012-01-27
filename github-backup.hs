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
import System.IO.Error (try)
import Control.Exception (bracket)
import System.Posix.Directory (changeWorkingDirectory)
import Text.Show.Pretty
import Control.Monad.State
import qualified Github.Data.Readable as Github
import qualified Github.Repos as Github
import qualified Github.Repos.Forks as Github
import qualified Github.PullRequests as Github
import qualified Github.Repos.Watching as Github
import qualified Github.Issues as Github
import qualified Github.Issues.Comments
import qualified Github.Issues.Milestones

import Common
import qualified Git
import qualified Git.Construct
import qualified Git.Config
import qualified Git.Types
import qualified Git.Command
import qualified Git.Ref
import qualified Git.Branch
import qualified Git.Queue

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

type BackupMap = M.Map Request (Either Github.Error [FilePath])

data BackupState = BackupState
	{ backupDone :: BackupMap
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
inRepo a = liftIO . a =<< gets backupRepo

changeDone :: (BackupMap -> BackupMap) -> Backup ()
changeDone a = do
	done <- gets backupDone
	modify $ \s -> s { backupDone = a done }

backupFiles :: [(Request, Either Github.Error [FilePath])] -> [FilePath]
backupFiles = concat . rights . map snd

backupFails :: [(Request, Either Github.Error [FilePath])] -> [Request]
backupFails = map fst . filter failed
	where
		failed (_, Left _) = True
		failed _ = False

failedRequest :: Request -> Github.Error -> Backup ()
failedRequest req e = changeDone $ M.insert req $ Left e

{- A given request in only run once. This is to avoid retries and other
 - loops. -}
runRequest :: Request -> Backup ()
runRequest req@(RequestSimple base) = runRequest' base req
runRequest req@(RequestNum base _) = runRequest' base req
runRequest' :: RequestBase -> Request -> Backup ()
runRequest' base req = do
	done <- gets backupDone
	case M.lookup req done of
		Nothing -> (lookupApi base) req
		_ -> return ()

{- List of Github api calls we can make to store their data. -}
type Storer = Request -> Backup ()
api :: M.Map ApiName Storer
api = M.fromList
	[ ("userrepo", userrepoStore)
	, ("forks", forksStore)
	, ("moreforks", moreforksStore)
	, ("watchers", watchersStore)
	, ("pullrequests", pullrequestsStore)
	, ("pullrequest", pullrequestStore)
	, ("milestones", milestonesStore)
	, ("issues", issuesStore)
	, ("issuecomments", issuecommentsStore)
	]

{- The toplevel api calls that are followed to get all data. -}
toplevelApi :: [ApiName]
toplevelApi =
	[ "userrepo"
	, "watchers"
	, "pullrequests"
	, "milestones"
	, "issues"
	, "forks" -- comes last because it recurses on to the forks
	]

lookupApi :: RequestBase -> Storer
lookupApi (RequestBase name _) = fromMaybe bad $ M.lookup name api
	where
		bad = error $ "internal error: bad api call: " ++ name

userrepoStore :: Storer
userrepoStore = simpleHelper Github.userRepo $ store "repo"

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
	runRequest (RequestNum (RequestBase "moreforks" repo) (page+1))
	mapM_ (traverse . toGithubUserRepo) fs
	where
		traverse fork = do
			addFork fork
			gatherMetaData fork

forValues :: (Request -> v -> Backup ()) -> Request -> [v] -> Backup ()
forValues handle req vs = forM_ vs (handle req)

type ApiCall v = String -> String -> IO (Either Github.Error v)
type ApiWith v b = String -> String -> b -> IO (Either Github.Error v)
type ApiNum v = ApiWith v Int
type Handler v = Request -> v -> Backup ()

simpleHelper :: ApiCall v -> Handler v -> Storer
simpleHelper call handle req@(RequestSimple (RequestBase _ (GithubUserRepo user repo))) =
	go =<< liftIO (call user repo)
	where
		go (Left e) = failedRequest req e
		go (Right v) = handle req v
simpleHelper _ _ r = badRequest r

withHelper :: ApiWith v b -> b -> Handler v -> Storer
withHelper call b handle req@(RequestSimple (RequestBase _ (GithubUserRepo user repo))) =
	go =<< liftIO (call user repo b)
	where
		go (Left e) = failedRequest req e
		go (Right v) = handle req v
withHelper _ _ _ r = badRequest r

numHelper :: ApiNum v -> (Int -> Handler v) -> Storer
numHelper call handle req@(RequestNum (RequestBase _ (GithubUserRepo user repo)) num) =
	go =<< liftIO (call user repo num)
	where
		go (Left e) = failedRequest req e
		go (Right v) = handle num req v
numHelper _ _ r = badRequest r

badRequest :: Request -> a
badRequest r = error $ "internal error: bad request type " ++ show r

store :: Show a => FilePath -> Request -> a -> Backup ()
store file req val = do
	let f = storedFile file $ requestRepo req
	liftIO $ do
		createDirectoryIfMissing True (parentDir f)
		writeFile f (ppShow val)
	changeDone $ M.insertWith comb req (Right [f])
	where
		comb (Right a) (Right b) = Right $ a ++ b
		comb (Right a) _ = Right a
		comb _ (Right b) = Right b
		comb (Left e) _ = Left e

storeSorted :: Ord a => Show a => FilePath -> Request -> [a] -> Backup ()
storeSorted file req val = store file req (sort val)

storeAppend :: Read a => Ord a => Show a => FilePath -> Request -> [a] -> Backup ()
storeAppend file req val = do
	old <- fromMaybe [] <$> readold
	storeSorted file req $ S.toList $ S.union
		(S.fromList val)
		(S.fromList $ fromMaybe [] $ readish old)
	where
		readold = liftIO $ catchMaybeIO $
			readFileStrict $ storedFile file $ requestRepo req

storedFile :: FilePath -> GithubUserRepo -> FilePath
storedFile file (GithubUserRepo user repo) = user ++ "_" ++ repo </> file

gitHubRepos :: Backup [Git.Repo]
gitHubRepos = fst . gitHubPairs <$> gets backupRepo

gitHubRemotes :: Backup [GithubUserRepo]
gitHubRemotes = snd . gitHubPairs <$> gets backupRepo

gitHubPairs :: Git.Repo -> ([Git.Repo], [GithubUserRepo])
gitHubPairs = unzip . mapMaybe check . Git.Types.remotes
	where
		check r@Git.Repo { Git.Types.location = Git.Types.Url u } =
			headMaybe $ mapMaybe (checkurl r $ show u) gitHubUrlPrefixes
		check _ = Nothing
		checkurl r u prefix
			| prefix `isPrefixOf` u && length bits == 2 =
				Just $ (r,
					GithubUserRepo (bits !! 0)
						(dropdotgit $ bits !! 1))
			| otherwise = Nothing
			where
				rest = drop (length prefix) u
				bits = split "/" rest
		dropdotgit s
			| ".git" `isSuffixOf` s = take (length s - length ".git") s
			| otherwise = s

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
			exists <- Git.Ref.matching (Git.Ref $ "refs/heads/" ++ branchname) r
			if null exists
				then checkout [Param "--orphan", Param branchname]
				else checkout [Param branchname]
			return oldbranch
		cleanup Nothing = return ()
		cleanup (Just oldbranch)
			| name == branchname = return ()
			| otherwise = checkout [Param name]
			where
				name = show $ Git.Ref.base oldbranch
		checkout params = Git.Command.run "checkout" (Param "-q" : params) r
		branchname = "github"

commitFiles :: [FilePath] -> Git.Repo -> IO ()
commitFiles [] _ = return ()
commitFiles files r = do
	mass "add" [Param "-f"]
	_ <- catchMaybeIO $ mass "commit" [Param "-m", Param "github-backup"]
	return ()
	where
		mass subcommand params = do
			let q = Git.Queue.add Git.Queue.new subcommand params files
			_ <- Git.Queue.flush q r
			return ()

addFork :: GithubUserRepo -> Backup ()
addFork fork = do
	remotes <- gitHubRemotes
	unless (fork `elem` remotes) $ do
		liftIO $ putStrLn $ "New fork: " ++ repoUrl fork
		_ <- inRepo $ Git.Command.runBool "remote"
			[ Param "add"
			, Param $ remoteFor fork
			, Param $ repoUrl fork
			]
		-- re-read git config to get the added remote
		r <- inRepo Git.Config.read
		modify $ \s -> s { backupRepo = r }
	where
		remoteFor (GithubUserRepo user repo) =
			"github_" ++ user ++ "_" ++ repo

{- Fetches from the github remotes. Done by githb-backup, just because
 - it would be weird for a backup to not fetch all available data.
 - Even though its real focus is on metadata not stored in git. -}
fetchRepos :: Backup ()
fetchRepos = do
	repos <- gitHubRepos
	forM_ repos $ \repo -> inRepo $
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
	_ <- try $ removeFile (retryFile r)
	return ()
storeRetry retryrequests r = writeFile (retryFile r) (show retryrequests)

loadRetry :: Git.Repo -> IO [Request]
loadRetry r = do
	c <- catchMaybeIO (readFileStrict (retryFile r))
	case c of
		Nothing -> return []
		Just s -> case readish s of
			Nothing -> return []
			Just v -> return v

retryFile :: Git.Repo -> FilePath
retryFile r = Git.gitDir r </> "github-backup.todo"

retry :: Backup BackupMap
retry = do
	todo <- inRepo $ loadRetry
	unless (null todo) $ do
		liftIO $ putStrLn $
			"Retrying " ++ show (length todo) ++
			" requests that failed last time..."
		mapM_ runRequest todo
	gets backupDone

backup :: Backup ()
backup = do
	remotes <- gitHubRemotes
	when (null remotes) $ do
		error "no github remotes found"
	retried <- retry
	mapM_ gatherMetaData remotes
	fetchRepos
	save retried

{- Save all backup data. Files that were written are committed.
 - Requests that failed are saved for next time. Requests that were retried
 - this time and failed are ordered last, to ensure that we don't get stuck
 - retrying the same requests and not making progress when run again.
 -}
save :: BackupMap -> Backup ()
save retried = do
	done <- gets backupDone
	let ordered = M.toList (done `M.difference` retried) ++ M.toList retried
	inRepo $ commitFiles $ backupFiles ordered
	let fails = backupFails ordered
	inRepo $ storeRetry fails
	unless (null fails) $ do
		error $ "Backup may be incomplete; " ++
			show (length fails) ++
			" requests failed. Run again later."

usage :: String
usage = "usage: github-backup [directory]"

getLocalRepo :: IO Git.Repo
getLocalRepo = getArgs >>= make >>= Git.Config.read
	where
		make [] = Git.Construct.fromCwd
		make (d:[]) = Git.Construct.fromPath d
		make _ = error usage

main :: IO ()
main = do
	r <- getLocalRepo
	changeWorkingDirectory $ Git.repoLocation r
	let backupstate = BackupState M.empty r
	onGithubBranch r $ evalStateT (runBackup backup) backupstate

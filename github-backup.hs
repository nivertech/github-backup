{- github-backup
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Data.Map as M
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

{- Now a little monad, to remember which Requests have been run
 - already, so we can avoid doing the same thing twice. -}
type BackupMap = M.Map Request (Either Github.Error [FilePath])
newtype Backup a = Backup { runBackup :: StateT BackupMap IO a }
	deriving (
		Monad,
		MonadState BackupMap,
		MonadIO,
		Functor,
		Applicative
	)

backupFiles :: [(Request, Either Github.Error [FilePath])] -> [FilePath]
backupFiles = concat . rights . map snd

backupFails :: [(Request, Either Github.Error [FilePath])] -> [Request]
backupFails = map fst . filter failed
	where
		failed (_, Left _) = True
		failed _ = False

failedRequest :: Request -> Github.Error -> Backup ()
failedRequest req e = modify $ M.insert req $ Left e

runRequest :: Request -> Backup ()
runRequest req@(RequestSimple base) = runRequest' base req
runRequest req@(RequestNum base _) = runRequest' base req
runRequest' :: RequestBase -> Request -> Backup ()
runRequest' base req = do
	done <- get
	case M.lookup req done of
		Nothing -> (lookupApi base) req
		_ -> return ()

{- List of Github api calls we can make to store their data. -}
type Storer = Request -> Backup ()
api :: M.Map ApiName Storer
api = M.fromList
	[ ("userrepo", userrepoStore)
	, ("forks", forksStore)
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
	, "forks"
	, "watchers"
	, "pullrequests"
	, "milestones"
	, "issues"
	]

lookupApi :: RequestBase -> Storer
lookupApi (RequestBase name _) = fromMaybe bad $ M.lookup name api
	where
		bad = error $ "internal error: bad api call: " ++ name

userrepoStore :: Storer
userrepoStore = simpleHelper Github.userRepo $ store "repo"

forksStore :: Storer
forksStore = simpleHelper Github.forksFor $ storeSorted "forks"

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

forValues :: (Request -> v -> Backup ()) -> Request -> [v] -> Backup ()
forValues handle req vs = forM_ vs (handle req)

simpleHelper :: (String -> String -> IO (Either Github.Error v))
	-> (Request -> v -> Backup ())
	-> Request
	-> Backup ()
simpleHelper query handle req@(RequestSimple (RequestBase _ (GithubUserRepo user repo))) =
	go =<< liftIO (query user repo)
	where
		go (Left e) = failedRequest req e
		go (Right v) = handle req v
simpleHelper _ _ r = badRequest r

withHelper :: (String -> String -> b -> IO (Either Github.Error v))
	-> b
	-> (Request -> v -> Backup ())
	-> Request
	-> Backup ()
withHelper query b handle req@(RequestSimple (RequestBase _ (GithubUserRepo user repo))) =
	go =<< liftIO (query user repo b)
	where
		go (Left e) = failedRequest req e
		go (Right v) = handle req v
withHelper _ _ _ r = badRequest r

numHelper :: (String -> String -> Int -> IO (Either Github.Error v))
	-> (Int -> Request -> v -> Backup ())
	-> Request
	-> Backup ()
numHelper query handle req@(RequestNum (RequestBase _ (GithubUserRepo user repo)) num) =
	go =<< liftIO (query user repo num)
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
		putStrLn $ "\t" ++ f
	modify $ M.insertWith comb req (Right [f])
	where
		comb (Right a) (Right b) = Right $ a ++ b
		comb (Right a) _ = Right a
		comb _ (Right b) = Right b
		comb (Left e) _ = Left e

storeSorted :: Ord a => Show a => FilePath -> Request -> [a] -> Backup ()
storeSorted file req val = store file req (sort val)

storedFile :: FilePath -> GithubUserRepo -> FilePath
storedFile file (GithubUserRepo user repo) = user ++ "_" ++ repo </> file

{- Finds already configured remotes that use github. -}
gitHubRemotes :: Git.Repo -> ([Git.Repo], [GithubUserRepo])
gitHubRemotes r
	| null rs = error "no github remotes found"
	| otherwise = unzip rs
	where
		rs = gitHubUserRepos r

gitHubUserRepos :: Git.Repo -> [(Git.Repo, GithubUserRepo)]
gitHubUserRepos = mapMaybe check . Git.Types.remotes
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

commitFiles :: Git.Repo -> [FilePath] -> IO ()
commitFiles _ [] = return ()
commitFiles r files = do
	mass "add" [Param "-f"]
	_ <- catchMaybeIO $ mass "commit" [Param "-m", Param "github-backup"]
	return ()
	where
		mass subcommand params = do
			let q = Git.Queue.add Git.Queue.new subcommand params files
			_ <- Git.Queue.flush q r
			return ()

{- Fetches from a git remote. github-backup does this for all github
 - remotes, just because it would be weird for a backup to not fetch
 - all available data. Even though its real focus is on metadata not stored
 - in git. -}
fetch :: [Git.Repo] -> Git.Repo -> IO ()
fetch repos r = forM_ repos $ \repo ->
	Git.Command.runBool "fetch" [Param $ fromJust $ Git.Types.remoteName repo] r

{- Gathers metadata for the repo. Retuns a list of files written
 - and a list that may contain requests that need to be retried later. -}
gatherMetaData :: GithubUserRepo -> Backup ()
gatherMetaData repo = do
	liftIO $ putStrLn $ "Gathering metadata for " ++ repoUrl repo ++ " ..."
	mapM_ (call repo) toplevelApi

call :: GithubUserRepo -> ApiName -> Backup ()
call repo name = runRequest $ RequestSimple $ RequestBase name repo

{- Find forks of the repos. Then go on to find forks of the forks, etc.
 -
 - Each time a new fork is found, it's added as a git remote, and
 - fetched from.
 -
 - Note that this code guards against fork cycles, although that Should
 - Never Happen. -}
findForks :: Git.Repo -> Backup ()
findForks r = do
	liftIO $ putStrLn "Finding forks..."
	let remotes = snd $ gitHubRemotes r
	findForks' r remotes remotes
findForks' :: Git.Repo -> [GithubUserRepo] -> [GithubUserRepo] -> Backup ()
findForks' _ _ [] = return ()
findForks' r done rs = do
	mapM_ query rs
	new <- findnew
	findForks' r (done++rs) new
	where
		query repo = call repo "forks"
		findnew = do
			files <- filter ("/forks" `isSuffixOf`) . 
				backupFiles . M.toList <$> get
			forks <- concat . catMaybes <$> mapM readfork files
			let new = excludedone $ map toGithubUserRepo $ forks
			mapM_ addfork new
			return new
		excludedone l = l \\ done
		readfork :: FilePath -> Backup (Maybe [Github.Repo])
		readfork file = liftIO $ readish <$> readFile file
		addfork fork = liftIO $ do
			putStrLn $ "New fork: " ++ repoUrl fork
			Git.Command.runBool "remote"
				[ Param "add"
				, Param "-f"
				, Param $ remoteFor fork
				, Param $ repoUrl fork
				] r
		remoteFor (GithubUserRepo user repo) =
			"github_" ++ user ++ "_" ++ repo

storeRetry :: Git.Repo -> [Request] -> IO ()
storeRetry r [] = do
	_ <- try $ removeFile (retryFile r)
	return ()
storeRetry r retryrequests = writeFile (retryFile r) (show retryrequests)

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

retry :: Git.Repo -> Backup BackupMap
retry r = do
	todo <- liftIO $ loadRetry r
	unless (null todo) $ do
		liftIO $ putStrLn $
			"Retrying " ++ show (length todo) ++
			" requests that failed last time..."
		mapM_ runRequest todo
	get

{- A backup starts by retrying any requests that failed last time.
 - This way, if API limits or other problems are stopping the backup 
 - part way through, incremental progress is made.
 -
 - The backup process first finds forks on github. Then for each fork,
 - it looks up all the metadata.
 -}
backup :: Git.Repo -> Backup ()
backup r = do
	retried <- retry r

	findForks r
	r' <- liftIO $ Git.Config.read r

	let (repos, remotes) = gitHubRemotes r'
	liftIO $ fetch repos r'
	forM_ remotes gatherMetaData

	save r' retried

{- Save all backup data. Files that were written are committed.
 - Requests that failed are saved for next time. Requests that were retried
 - this time and failed are ordered last, to ensure that we don't get stuck
 - retrying the same requests and not making progress when run again.
 -}
save :: Git.Repo -> BackupMap -> Backup ()
save r retried = do
	done <- get
	let ordered = M.toList (done `M.difference` retried) ++ M.toList retried
	liftIO $ commitFiles r $ backupFiles ordered
	let fails = backupFails ordered
	liftIO $ storeRetry r fails
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
	onGithubBranch r $ evalStateT (runBackup $ backup r) M.empty

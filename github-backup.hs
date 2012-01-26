{- github-backup
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Main where

import System.Environment
import Control.Exception (bracket)
import System.Posix.Directory (changeWorkingDirectory)
import Text.Show.Pretty
import qualified Github.Data.Definitions as Github
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

usage :: String
usage = "usage: github-backup [directory]"

getLocalRepo :: [String] -> IO Git.Repo
getLocalRepo [] = Git.Construct.fromCwd
getLocalRepo (d:[]) = Git.Construct.fromPath d
getLocalRepo _ = error usage

-- A github user and repo.
data GithubUserRepo = GithubUserRepo String String

instance Show (GithubUserRepo) where
	show (GithubUserRepo user remote) = "git://github.com/" ++ user ++ "/" ++ remote ++ ".git"

toGithubUserRepo :: Github.Repo -> GithubUserRepo
toGithubUserRepo r = GithubUserRepo 
	(Github.githubUserLogin $ Github.repoOwner r)
	(Github.repoName r)

remoteFor :: Github.Repo -> String
remoteFor r = "github_" ++
	(Github.githubUserLogin $ Github.repoOwner r) ++ "_" ++
	(Github.repoName r)

showRepo :: Github.Repo -> String
showRepo = show . toGithubUserRepo

runRepoWith :: (String -> String -> b -> IO (Either Github.Error v)) -> b -> Github.Repo -> IO (Maybe v)
runRepoWith a b = run (\user repo -> a user repo b) . toGithubUserRepo

runRepo :: (String -> String -> IO (Either Github.Error v)) -> Github.Repo -> IO (Maybe v)
runRepo a = run a . toGithubUserRepo

run :: (String -> String -> IO (Either Github.Error v)) -> GithubUserRepo -> IO (Maybe v)
run a r@(GithubUserRepo user repo) = handle =<< a user repo
	where
		handle (Right v) = return $ Just v
		handle (Left e) = do
			hPutStrLn stderr $ "problem accessing API for " ++ show r ++ ": " ++ show e
			return Nothing

{- Finds already configured remotes that use github.
 - Fetches from them because it would be surprising if a github backup
 - didn't do so, and this is the only place the remote names are available. -}
gitHubRemotes :: Git.Repo -> IO [Github.Repo]
gitHubRemotes r = do
	let (repos, remotes) = unzip  $ gitHubUserRepos r
	when (null remotes) $ error "no github remotes found"
	forM_ repos $ \repo ->
		Git.Command.runBool "fetch" [Param $ fromJust $ Git.Types.remoteName repo] r
	catMaybes <$> mapM (run Github.userRepo) remotes

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
	]

{- Recursively look for forks, and forks of forks, of the repos.
 - Note that this guards against cycles in the fork tree, although
 - presumably that should never happen. -}
findForks :: [Github.Repo] -> IO [Github.Repo]
findForks = findForks' []
findForks' :: [Github.Repo] -> [Github.Repo] -> IO [Github.Repo]
findForks' _ [] = return []
findForks' done rs = do
	forks <- filter new . concat . catMaybes <$>
		mapM (runRepo Github.forksFor) rs
	forks' <- findForks' (done++rs) forks
	return $ nub $ forks ++ forks'
	where
		new r = not $ r `elem` rs || r `elem` done

{- Adds new forks, fetching from them. -}
addForks :: Git.Repo -> [Github.Repo] -> IO ()
addForks _ [] = putStrLn "no new forks"
addForks r forks = do 
	putStrLn $ "new forks: " ++ unwords (map showRepo forks)
	forM_ forks $ \fork -> do
		Git.Command.runBool "remote"
			[ Param "add"
			, Param "-f"
			, Param $ remoteFor fork
			, Param $ Github.repoGitUrl fork
			] r

onGithubBranch :: Git.Repo -> IO () -> IO ()
onGithubBranch r a = bracket prep cleanup (const a)
	where
		prep = do
			oldbranch <- Git.Branch.current r
			exists <- Git.Ref.matching (Git.Ref $ "refs/heads/" ++ branchname) r
			if null exists
				then Git.Command.run "checkout"
					[Param "--orphan", Param branchname] r
				else Git.Command.run "checkout"
					[Param branchname] r
			return oldbranch
		cleanup Nothing = return ()
		cleanup (Just oldbranch)
			| name == branchname = return ()
			| otherwise = Git.Command.run "checkout" [Param name] r
			where
				name = show $ Git.Ref.base oldbranch
		branchname = "github"

commitFiles :: Git.Repo -> [FilePath] -> IO ()
commitFiles _ [] = return ()
commitFiles r files = do
	mass "add" [Param "-f"]
	mass "commit" [Param "-m", Param "github-backup"]
	where
		mass subcommand params = do
			let q = Git.Queue.add Git.Queue.new subcommand params files
			_ <- Git.Queue.flush q r
			return ()

saveMetaData :: Show a => Github.Repo -> String -> a -> IO FilePath
saveMetaData repo file val = do
	let file' = remoteFor repo </> file
	createDirectoryIfMissing True (parentDir file')
	writeFile file' (ppShow val)
	return file'

gatherMetaData :: Github.Repo -> IO [FilePath]
gatherMetaData repo = do
	putStrLn $ "gathering metadata for " ++ showRepo repo
	concat <$> mapM (\a -> a repo)
		[ pullRequests
		, watchers
		, issues
		, milestones
		]

pullRequests :: Github.Repo -> IO [FilePath]
pullRequests repo = runRepo Github.pullRequestsFor repo >>= collect
	where
		collect Nothing = return []
		collect (Just rs) = forM rs $ \r -> do
			let n = Github.pullRequestNumber r
			details <- runRepoWith Github.pullRequest n repo
			saveMetaData repo ("pullrequest" </> show n) details

watchers :: Github.Repo -> IO [FilePath]
watchers repo = runRepo Github.watchersFor repo >>= collect
	where
		collect Nothing = return []
		collect (Just ws) = do
			f <- saveMetaData repo "watchers" $ sort ws
			return [f]

issues :: Github.Repo -> IO [FilePath]
issues repo = runRepoWith Github.issuesForRepo [] repo >>= collect
	where
		collect Nothing = return []
		collect (Just is) = concat <$> forM is get
		get i = do
			let n = Github.issueNumber i
			f <- saveMetaData repo ("issue" </> show n) i
			fs <- issueComments n repo
			return $ f:fs

issueComments :: Int -> Github.Repo -> IO [FilePath]
issueComments n repo = runRepoWith Github.Issues.Comments.comments n repo >>= collect
	where
		collect Nothing = return []
		collect (Just cs) = forM cs $ \c -> do
			let i = Github.issueCommentId c
			saveMetaData repo ("issue" </> show n ++ "_comment" </> show i) c

milestones :: Github.Repo -> IO [FilePath]
milestones repo = runRepo Github.Issues.Milestones.milestones repo >>= collect
	where
		collect Nothing = return []
		collect (Just ms) = forM ms $ \m -> do
			let n = Github.milestoneNumber m
			saveMetaData repo ("milestone" </> show n) m

main :: IO ()
main = do
	r <- Git.Config.read =<< getLocalRepo =<< getArgs
	changeWorkingDirectory $ Git.repoLocation r
	remotes <- gitHubRemotes r
	forks <- findForks remotes
	addForks r forks
	onGithubBranch r $
		concat <$> forM (forks ++ remotes) gatherMetaData
			>>= commitFiles r

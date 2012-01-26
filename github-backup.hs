module Main where

import System.Environment
import qualified Github.Data as Github
import Github.Repos
import Github.Repos.Forks

import Common
import qualified Git
import qualified Git.Construct
import qualified Git.Config
import qualified Git.Types
import qualified Git.Command

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

showRepo :: Github.Repo -> String
showRepo = show . toGithubUserRepo

runRepo :: (String -> String -> IO (Either Error v)) -> Github.Repo -> IO (Maybe v)
runRepo a = run a . toGithubUserRepo

run :: (String -> String -> IO (Either Error v)) -> GithubUserRepo -> IO (Maybe v)
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
	catMaybes <$> mapM (run userRepo) remotes

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
		mapM (runRepo forksFor) rs
	forks' <- findForks' (done++rs) forks
	return $ nub $ forks ++ forks'
	where
		new r = not $ r `elem` rs || r `elem` done

{- Adds new forks, fetching from them. -}
addForks :: Git.Repo -> [Github.Repo] -> IO ()
addForks _ [] = return ()
addForks r forks = do 
	putStrLn $ "new forks: " ++ unwords (map showRepo forks)
	forM_ forks $ \fork -> do
		Git.Command.runBool "remote"
			[ Param "add"
			, Param "-f"
			, Param $ remoteFor fork
			, Param $ Github.repoGitUrl fork
			] r

remoteFor :: Github.Repo -> String
remoteFor r = "github_" ++
	(Github.githubUserLogin $ Github.repoOwner r) ++ "_" ++
	(Github.repoName r)

main :: IO ()
main = do
	r <- Git.Config.read =<< getLocalRepo =<< getArgs
	remotes <- gitHubRemotes r
	forks <- findForks remotes
	addForks r forks

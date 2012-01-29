github-backup is a simple tool you run in a git repository you cloned from
GitHub. It backs up everything GitHub publishes about the repository, including
other forks, issues, comments, milestones, pull requests, and watchers.

## Installation

    git clone git://github.com/joeyh/github-backup
    cd github-backup
    make
    ./github-backup

Or use cabal:

    cabal install github-backup --bindir=$HOME/bin

## Why backup GitHub repositories

There are a couple of reasons to want to back this stuff up:

* In case something happens to GitHub. More generally because
  keeping your data in the cloud *and* relying on the cloud to
  back it up is foolish.

* So you can keep working on your repository while on a plane, or
  on a remote beach or mountaintop. Just like Linus intended.

## What to expect

Each time you run github-backup, it will find any new forks of your project
on GitHub. It will add remotes to your repository for the forks, using names
like `github_linus_divemonitor`. It will fetch from every fork.

It downloads metadata from each fork. This is stored
into a branch named "github". Each fork gets a directory in there,
like `linus_divemonitor`. Inside the directory there will be some
files, like `linus_divemonitor/watchers`. There may be further
directories, like for comments: `linus_divemonitor/comments/1`.

You can follow the commits to the github branch to see what information
changed on GitHub over time.

The format of the files in the github branch is currently Haskell
serialized data types. This is plain text, and readable, if you squint.

## Limitations

github-backup is repository-focused. It does not try to back up other
information from GitHub. In particular, social network stuff, like
users who are following you, is not backed up. And it only backs up
the repository you run it in, not any other repositories you might have
on GitHub. Although it might learn to do that later..

github-backup will find and backup forks of a repository, and all forks
of those forks, etc. However, it cannot go *up* the fork tree. So if
your GitHub repositoriy is a fork of something else, the something else
won't be backed up. There is an easy solution though. Just add the
parent as a git remote. Then github-backup will find it, and back it up.

Currently, only 30 of each thing will be returned. This is a bug in 
the haskell github library I'm using. Hope to get it fixed soon.

Currently, the GitHub API does not seem to provide a way to access notes
added to commits and notes added to lines of code. So those notes won't get
backed up. The GitHub folks have been told about this limitation of their API.

The labels that can be added to issues and milestones are not backed up.
They could be, but don't seem important enough for the extra work involved.
Yell if you need them.

github-backup re-downloads all issues, comments, and so on
each time it's run. This may be slow if your repo has a lot of them,
or even if it just has a lot of forks.

Bear in mind that this uses the GitHub API; don't run it every 5 minutes.
GitHub [rate limits](http://developer.github.com/v3/#rate-limiting) the
API to 5000 requests per hour. However, github-backup *does* do an
incremental backup, picking up where it left off, so will complete the
backup eventually even if it's rate limited.

## Author

github-backup was written by Joey Hess <joey@kitenet.net>

It is made possible thanks to:

* Mike Burns's [haskell github library](http://hackage.haskell.org/package/github)
* GitHub, for providing an API exposing this data. 

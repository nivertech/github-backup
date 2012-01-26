github-backup is a simple tool you run in a git repository you cloned from
Github. It backs up everything Github knows about the repository, including
other forks, issues, comments, milestones, pull requests, and watchers.

## Installation

  git clone git://github.com/joeyh/github-backup
  cd github-backup
  make
  ./github-backup

Or use cabal:

  cabal install github-backup --bindir=$HOME/bin

## Why backup Github

There are a couple of reasons to want to back this stuff up:

* In case something happens to Github. More generally because
  keeping your data in the cloud *and* relying on the cloud to
  back it up is foolish.

* So you can keep working on your repository while on a plane, or
  on a remote beach or mountiaintop. Just like Linus intended.

## What to expect

Each time you run github-backup, it will find any new forks of your project
on github. It will add remotes to your repository for the forks, using names
like `github_linus_divemonitor`. It will fetch from every fork.

Then the next pass will download metadata from each fork. This is stored
into a branch named "github". Each fork gets a directory in there,
like `linus_divemonitor`. Inside the directory there will be some
files, like `linus_divemonitor/watchers`. There may be further
directories, like for comments: `linus_divemonitor/comments/1`.

You can follow the commits to the github branch to see what information
changed on github over time.

The format of the files in the github branch is currently Haskell
serialized data types. This is plain text, and readable, if you squint.

## Limitations

github-backup is repository-focused. It does not try to back up other
information from Github. In particular, social network stuff, like
users who are following you, is not backed up.

github-backup will find and backup all forks of a repository, and all forks
of those forks, etc. However, it cannot go *up* the fork tree. So if
your Github repositoriy is a fork of something else, the something else
won't be backed up. There is an easy solution though. Just add the
parent as a git remote. Then github-backup will find it, and back it up.

Currently, github-backup re-downloads all issues, comments, and so on
each time it's run. This may be slow if your repo has a lot of them,
or even if it just has a lot of forks. 

Bear in mind that this uses the Github API; don't run it every 5 minutes.
Github [rate limits](http://developer.github.com/v3/#rate-limiting) the
API to 5000 requests per hour; this could prevent github-backup
from finishing if it goes over.

## Author

github-backup was written by Joey Hess <joey@kitenet.net>

It is made possible thanks to:

* Mike Burns's [haskell github library](http://hackage.haskell.org/package/github)
* Github, for providing an API exposing this data. 

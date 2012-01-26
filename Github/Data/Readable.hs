{-# LANGUAGE StandaloneDeriving #-}

-- | This module re-exports the @Github.Data.Definitions@ module, adding
-- instances of @Read@ to it.

module Github.Data.Readable (module Github.Data.Definitions) where

import Github.Data.Definitions

deriving instance Read GithubDate
deriving instance Read GithubUser
deriving instance Read Repo

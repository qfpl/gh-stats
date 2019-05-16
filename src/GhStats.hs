module GhStats where

import GitHub.Data.Repos (Repo)
import GitHub.Endpoints.Repos (organizationRepos)
import GitHub.Internal.Prelude (Vector)

data Stats =
  Stats {
    repos :: Vector Repo
  }

getStats ::
  IO (Either Error Stats)

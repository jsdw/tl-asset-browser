module Store exposing --where
  ( store
  )

import Debug
import Task exposing (Task, andThen)
import List
import String

--
-- UNDER CONSTRUCTION
--
-- Turn into "dumb" store that takes in API responses in its update func
-- and updates its cache to handle these. The store will expose functions
-- that are partially applied to the cache(s), each of which will return
--
-- (result, Cmd)
--
-- Any update func that wants to draw from the store can subscribe to API
-- change events in the simple case
--


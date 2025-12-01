module Solutions where

import Data.Map (Map)
import Data.Map qualified as Map
import Day.Day01 qualified


import DayVersion
import Utils ((=:))

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ "1" =: Day.Day01.run
    ]

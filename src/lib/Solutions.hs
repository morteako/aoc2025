module Solutions where

import Data.Map (Map)
import Data.Map qualified as Map
import Day.Day01 qualified
import Day.Day02 qualified
import Day.Day03 qualified

import DayVersion
import Utils ((=:))

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ "1" =: Day.Day01.run
    , "2" =: Day.Day02.run
    , "3" =: Day.Day03.run
    ]

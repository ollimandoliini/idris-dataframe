import System.File
import Data.Vect

import Data.DataFrame
import Data.DataFrame.Row
import Data.DataFrame.ReadDataFrame
import Data.DataFrame.CSV


MyDf n = DataFrame n [("foo", Nat), ("bar", Bool), ("baz", Nat)]


getMyDf : IO (Maybe (n ** MyDf n))
getMyDf = fromCsvFileWithHeader "../data.csv"


main : IO ()
main = do
  mDf <- getMyDf
  case mDf of
    Just (n ** df) =>
      let col1 = getColumn "foo" df
          col2 = getColumn "baz" df
          foo = zipWith (+) col1 col2
          newColumn = "bizzz" ::: foo
          newDf = addColumn newColumn df
      in printLn newDf
    Nothing => printLn "Could not read"


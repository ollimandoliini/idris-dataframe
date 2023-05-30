import Data.Vect
import Data.List.Elem

import Data.DataFrame.ReadDataFrame
import Data.DataFrame
import Data.String
import Data.List1
import System.File


listToVect : List a -> (n ** Vect n a)
listToVect [] = (_ ** [])
listToVect (x :: xs) =
  let (_ ** vs ) = listToVect xs
  in (_ ** x :: vs)

headerAndRows : String -> Either String (String, (n ** Vect n String))
headerAndRows content = case lines content of
                          [] => Left "Empty file"
                          (header :: rows) => Right (header, listToVect rows)


split' : (Char -> Bool) -> String -> List String
split' c s =
  let (head ::: tail) = split c s
  in (head :: tail)


csvToMatrix : (m : Nat) -> String -> Maybe (n ** Vect m (Vect n String))
csvToMatrix m fileBody =
  let (n ** lines') = (listToVect . lines) fileBody
      mVects = traverse (toVect m  . (split' (== ','))) lines'
  in (\vects => (n ** vects)) . transpose <$> mVects


readVect : ReadDataFrameValue a => Vect n String -> Maybe (Vect n a)
readVect = traverse read


matrixToDataFrame : {xs : _} -> ReadDataFrame xs => Vect m (Vect n String) -> Maybe (DataFrame n xs)
matrixToDataFrame {xs=((label, type) :: xs')} (y :: ys) = do
  col <- readVect y
  cols <- matrixToDataFrame ys
  pure ((label ::: col) :: cols)
matrixToDataFrame {xs=[]} [] = Just Nil
matrixToDataFrame {xs=[]} (y :: ys) = Nothing
matrixToDataFrame {xs=(x ::xs)} [] = Nothing


parseDataFrame : {xs : _ } -> ReadDataFrame xs => String -> Maybe (n ** DataFrame n xs)
parseDataFrame csv = do
  (n ** matrix) <- csvToMatrix (length xs) csv
  dataFrame <- matrixToDataFrame matrix
  pure (n ** dataFrame)


checkHeader : {xs : _} -> ReadDataFrame xs => String -> Maybe (DataFrame 0 xs)
checkHeader headerRow = checkHeader' $ split' (== ',') headerRow
  where
    checkHeader' : {xs : _} -> ReadDataFrame xs => List String -> Maybe (DataFrame 0 xs)
    checkHeader' {xs=((label, x) :: xs)} (y :: ys) =
      if label == y
      then do
        rest <- checkHeader' ys
        pure ((label ::: []) :: rest)
      else
        Nothing
    checkHeader' {xs=[]} [] = Just []
    checkHeader' {xs=(x :: xs)} [] = Nothing
    checkHeader' {xs=[]} (y :: ys) = Just []


fromCsvFileWithHeader : {xs : _} -> ReadDataFrame xs => String -> IO (Maybe (n ** DataFrame n xs))
fromCsvFileWithHeader filePath = do
  eFileContent <- readFile filePath
  case eFileContent of
    Right fileContent =>
      case lines fileContent of
        (header :: body) => pure $ do
            _ <- checkHeader {xs} header
            parseDataFrame (unlines body)
        [] => pure Nothing
    Left _ => pure Nothing


fromCsvFile : {xs : _} -> ReadDataFrame xs => String -> IO (Maybe (n ** DataFrame n xs))
fromCsvFile filePath = do
  eFileContent <- readFile filePath
  printLn eFileContent
  pure $ case eFileContent of
    Right s => do
      parseDataFrame s
    Left _ => Nothing

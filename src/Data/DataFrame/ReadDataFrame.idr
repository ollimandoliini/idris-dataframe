import Data.String


interface ReadDataFrameValue a where
  read : String -> Maybe a

implementation ReadDataFrameValue String where
  read = Just

implementation ReadDataFrameValue Nat where
  read = parsePositive

implementation ReadDataFrameValue Bool where
  read "true" = Just True
  read "false" = Just False
  read _ = Nothing


ReadAll : List Type -> Type
ReadAll [] = ()
ReadAll (x::xs) = (ReadDataFrameValue x, ReadAll xs)

ReadDataFrame : List (String, Type) -> Type
ReadDataFrame xs = ReadAll (map Builtin.snd xs)

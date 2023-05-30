import Data.Vect
import Data.String
import public Data.List.Elem
import Data.DataFrame.Row


data Column : (n : Nat) -> String -> (a : Type) -> Type where
  (:::) : (l : String) -> (v : Vect n a) -> Column n l a


namespace DataFrame
    public export
    data DataFrame : (n : Nat) -> List (String, Type) -> Type where
        Nil : DataFrame n []
        (::) : Column n l a -> DataFrame n ts -> DataFrame n ((l, a) :: ts)


ShowAll : List Type -> Type
ShowAll [] = ()
ShowAll (x::xs) = (Show x, ShowAll xs)

colToCells : Show a => Column n label a -> Vect n String
colToCells (label ::: column) = map show column

dfToStringMatrix : {xs : _ } -> ShowAll (map Builtin.snd xs) => DataFrame n xs -> Vect (length xs) (Vect n String) 
dfToStringMatrix [] = []
dfToStringMatrix (x :: xs') = colToCells x :: dfToStringMatrix xs'

showRow : Vect m String -> String
showRow [] = ""
showRow (x :: xs) = x ++ go xs ""
  where
    go : Vect n String -> String -> String
    go [] acc = acc
    go (x :: xs) acc = go xs (acc ++ " | " ++ x)

vecToList : Vect n String -> List String
vecToList [] = []
vecToList (x :: xs) = x :: vecToList xs


implementation {n : _} -> {xs : _} -> ShowAll (map Builtin.snd xs) => Show (DataFrame n xs) where
  show xs = header ++ "\n" ++ (replicate (length header) '-') ++ "\n" ++ rows
    where
      mkHeader : DataFrame n xs' -> String
      mkHeader [] = ""
      mkHeader ((label ::: _) :: cols) = label ++ (go cols "")
        where
          go : DataFrame n ys -> String -> String
          go ((label ::: type) :: cols) acc = go cols (acc ++ " | " ++ label)
          go [] acc = acc

      header : String
      header = mkHeader xs
      
      rows : String
      rows = unlines . vecToList . map showRow . transpose $ dfToStringMatrix xs


getColumnType : (colName : lty) -> (cols : List (lty,Type)) -> Elem colName (map Builtin.fst cols) -> Type
getColumnType _ ((_, x) :: xs) Here = x
getColumnType colName ((lty, x) :: xs) (There rest) = getColumnType colName xs rest


getColumn : (colName : String) ->
            (df : DataFrame n cols) ->
            {auto el : Elem colName (map Builtin.fst cols)} ->
            Vect n (getColumnType colName cols el)
getColumn colName ((colName ::: vect) :: cols) {el=Here} = vect
getColumn colName ((_ ::: vect) :: cols) {el=There el} = getColumn colName cols


addColumn : Column n colName a -> DataFrame n xs -> DataFrame n ((colName, a) :: xs)
addColumn col df = col :: df


addRow : Row xs -> DataFrame n xs -> DataFrame (n + 1) xs
addRow [] [] = []
addRow ((_, val) :: fields) ((colName ::: vec) :: cols) = (colName ::: (vec ++ [val])) :: addRow fields cols



namespace Row
    public export
    data Row : List (String, Type) -> Type where
        Nil : Row []
        (::) : (String, a) -> Row xs -> Row ((label, a) :: xs)

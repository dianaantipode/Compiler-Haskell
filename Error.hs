module Error where

data ErrorOr a =
    Result a
  | ParseError String
  | TypeError String
  deriving Show

promote :: ErrorOr a -> ErrorOr b
promote (ParseError s     ) = ParseError s
promote (TypeError s      ) = TypeError s

instance Eq a => Eq (ErrorOr a) where
  Result x == Result y = x==y
  _ == _ = False

liftErr :: (a -> b) -> (ErrorOr a -> ErrorOr b)
liftErr f (Result x) = Result (f x)
liftErr f (ParseError s) = ParseError s
liftErr f (TypeError s) = TypeError s

joinErr :: ErrorOr (ErrorOr a) -> ErrorOr a
joinErr (Result (Result x)) = Result x
joinErr (Result (ParseError s)) = ParseError s
joinErr (Result (TypeError s)) = TypeError s
joinErr (ParseError s   ) = ParseError s
joinErr (TypeError s    ) = TypeError s


--eof

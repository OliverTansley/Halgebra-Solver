
module Algebra (Expr,eval) where

{- Algebraic Expression Datatype -}

    data Expr a = 
        X 
        | Value a 
        | Plus (Expr a) (Expr a) 
        | Sub  (Expr a) (Expr a)
        | Div  (Expr a) (Expr a)
        | Mult (Expr a) (Expr a)

{- Show Instance for Expression type -}

    instance Show a => Show (Expr a) where
        show X = "X"
        show (Value a) = show a
        show (Plus expr1 expr2) =  show expr1 ++ "+" ++ show expr2
        show (Sub expr1 expr2) = show expr1 ++ "-" ++ show expr2 
        show (Mult expr1 expr2) = show expr1 ++ "*" ++ show expr2
        show (Div expr1 expr2) = "(" ++ show expr1 ++ "/" ++ show expr2 ++ ")"

{- displayEq shows the representation of the equation and its value in full -}

    displayEq :: Show a => Expr a -> Int -> [Char]
    displayEq expr ans = (show expr) ++ "=" ++ show ans

{- eval: Determines the value of X within the algebraic expression -}

    eval :: Fractional a => Expr a -> a -> Expr a

    -- Base case (X = number)
    eval X i = (Value i)

    -- Addition
    eval (Plus (Value a) (Value b)) _ = Value (a+b)
    eval (Plus (Value a) expr) ans = Value (a+b) where (Value b) = eval expr ans
    eval (Plus expr (Value a)) ans = Value (b+a) where (Value b) = eval expr ans
    eval (Plus X (expr)) ans = Value (ans - a) where (Value a) = eval expr ans
    eval (Plus (expr) X) ans = Value (a + ans) where (Value a) = eval expr ans
    eval (Plus (expr1) (expr2)) ans = Value (ans1 + ans2) where 
                                                            (Value ans1) = eval expr1 ans
                                                            (Value ans2) = eval expr2 ans

    -- Subtraction
    eval (Sub (Value a) (Value b)) _ = (Value (a-b))
    eval (Sub (Value a) expr) ans = Value (a-b) where (Value b) = eval expr ans
    eval (Sub expr (Value a)) ans = Value (b-a) where (Value b) = eval expr ans
    eval (Sub X (expr)) ans = Value (ans + a) where (Value a) = eval expr ans
    eval (Sub (expr) X) ans = Value (a - ans) where (Value a) = eval expr ans
    eval (Sub (expr1) (expr2)) ans = Value (ans1 - ans2) where 
                                                            (Value ans1) = eval expr1 ans 
                                                            (Value ans2) = eval expr2 ans

    -- Division
    eval (Div (Value a) (Value b)) _ = (Value (a/b))
    eval (Div (Value a) expr) ans = Value (a/b) where (Value b) = eval expr ans
    eval (Div expr (Value a)) ans = Value (b/a) where (Value b) = eval expr ans
    eval (Div X (expr)) ans = Value (ans * a) where (Value a) = eval expr ans
    eval (Div (expr) X) ans = Value (a / ans) where (Value a) = eval expr ans
    eval (Div (expr1) (expr2)) ans = Value (ans1 / ans2) where 
                                                            (Value ans1) = eval expr1 ans 
                                                            (Value ans2) = eval expr2 ans

    -- Multiplication
    eval (Mult (Value a) (Value b)) _ = (Value (a*b))
    eval (Mult (Value a) expr) ans = Value (a*b) where (Value b) = eval expr ans
    eval (Mult expr (Value a)) ans = Value (b*a) where (Value b) = eval expr ans
    eval (Mult X (expr)) ans = Value (ans / a) where (Value a) = eval expr ans
    eval (Mult (expr) X) ans = Value (a * ans) where (Value a) = eval expr ans
    eval (Mult (expr1) (expr2)) ans = Value (ans1 * ans2) where 
                                                            (Value ans1) = eval expr1 ans 
                                                            (Value ans2) = eval expr2 ans
import Data.Char (isSpace) -- for rm'ing whitespace
import Data.Char (isDigit) --  for checking if something is a Number

-- helper func impl of split with delimiters
splitKeepDelims :: String -> String -> [String]
splitKeepDelims delims str = split' str []
  where
    split' [] acc = if null acc then [] else [reverse acc]
    split' (c:cs) acc
      | c `elem` delims = 
          if null acc 
          then [c] : split' cs []
          else reverse acc : [c] : split' cs []
      | otherwise = split' cs (c:acc)

-- simple helpers function to remove whitespace
removeWhitespace :: String -> String
removeWhitespace = filter (not . isSpace)

-- token enum for the diffrent types of tokens that can be read
data TokenType = Plus | Minus | Multi | Divide | Number | OpenParen | CloseParen | Error

instance Enum TokenType where
    toEnum 0 = Plus
    toEnum 1 = Minus
    toEnum 2 = Multi
    toEnum 3 = Divide
    toEnum 4 = Number
    toEnum 5 = OpenParen
    toEnum 6 = CloseParen
    toEnum 7 = Error
    toEnum _ = Error

    fromEnum Plus = 0 
    fromEnum Minus = 1
    fromEnum Multi = 2
    fromEnum Divide = 3
    fromEnum Number = 4
    fromEnum OpenParen = 5
    fromEnum CloseParen = 6
    fromEnum Error = 7


-- match a string to its operator
tokenOf :: String -> TokenType
tokenOf "+" = Plus
tokenOf "-" = Minus
tokenOf "*" = Multi
tokenOf "/" = Divide
tokenOf "(" = OpenParen
tokenOf ")" = CloseParen
tokenOf x
    | all isDigit x = Number
    | otherwise     = Error

-- passes in xs as String and adds to the list of TokenType as a ast
lexer :: [String] -> [TokenType]
lexer [] = []
lexer (x:xs) = tokenOf x : lexer xs


-- filters out all of the non numbers
isNumberOp :: String -> Bool 
isNumberOp "" = False 
isNumberOp str = all isDigit str

filterNumbers :: [String] -> [String]
filterNumbers strs = filter isNumberOp strs

-- parser
data Expr
    = Num Integer
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving (Show)

eval :: Expr -> Integer
eval (Num n) = n
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a `div` eval b

-- parse a list of tokens into an Expr and then evaluate it
parse :: [TokenType] -> [String] -> Integer
parse tokens numbers = eval expr
    where
        -- main parser, starts parsing expression with lowest precedence
        (expr, _, _) = parseExpr tokens numbers 0
        
        -- parseExpr handles + and -
        parseExpr :: [TokenType] -> [String] -> Int -> (Expr, [TokenType], Int)
        parseExpr ts nums nIdx =
            let (termExpr, ts', nIdx') = parseTerm ts nums nIdx
            in parseExpr' termExpr ts' nums nIdx'
        
        parseExpr' :: Expr -> [TokenType] -> [String] -> Int -> (Expr, [TokenType], Int)
        parseExpr' acc [] _ nIdx = (acc, [], nIdx)
        parseExpr' acc (t:ts) nums nIdx =
            case t of
                Plus  -> let (rhs, ts', nIdx') = parseTerm ts nums nIdx
                         in parseExpr' (Add acc rhs) ts' nums nIdx'
                Minus -> let (rhs, ts', nIdx') = parseTerm ts nums nIdx
                         in parseExpr' (Sub acc rhs) ts' nums nIdx'
                _     -> (acc, t:ts, nIdx) -- return remaining tokens
        
        -- parseTerm handles * and /
        parseTerm :: [TokenType] -> [String] -> Int -> (Expr, [TokenType], Int)
        parseTerm ts nums nIdx =
            let (factorExpr, ts', nIdx') = parseFactor ts nums nIdx
            in parseTerm' factorExpr ts' nums nIdx'
        
        parseTerm' :: Expr -> [TokenType] -> [String] -> Int -> (Expr, [TokenType], Int)
        parseTerm' acc [] _ nIdx = (acc, [], nIdx)
        parseTerm' acc (t:ts) nums nIdx =
            case t of
                Multi  -> let (rhs, ts', nIdx') = parseFactor ts nums nIdx
                          in parseTerm' (Mul acc rhs) ts' nums nIdx'
                Divide -> let (rhs, ts', nIdx') = parseFactor ts nums nIdx
                          in parseTerm' (Div acc rhs) ts' nums nIdx'
                _      -> (acc, t:ts, nIdx) -- return remaining tokens
        
        -- parseFactor handles numbers and parentheses
        parseFactor :: [TokenType] -> [String] -> Int -> (Expr, [TokenType], Int)
        parseFactor [] _ nIdx = (Num 0, [], nIdx)
        parseFactor (t:ts) nums nIdx =
            case t of
                Number -> (Num (read (nums !! nIdx)), ts, nIdx + 1)
                OpenParen ->
                    let (exprInside, ts', nIdx') = parseExpr ts nums nIdx
                    in case ts' of
                           (CloseParen:rest) -> (exprInside, rest, nIdx')
                           _                 -> (exprInside, ts', nIdx') -- ignore missing close
                _ -> (Num 0, ts, nIdx) -- fallback for errors

main :: IO ()
main = do
    contents <- readFile "src/equ.txt"
    let pattern = filter (not . null) $ splitKeepDelims "+-*/()" (removeWhitespace contents)
    -- lex inot tokens and get all of the numbers
    -- I will got through and keep track of how many numbers I have seen so that when I see a new one it is that element of the number list
    let numberList = filterNumbers pattern
    let tokenList = lexer pattern
    print ( parse tokenList numberList )

module Dbn where


import System.Environment
import Text.Read (readMaybe)
import Debug.Trace (trace)
import Data.Function ((&))


compile :: IO ()
compile = do
  (file:_) <- getArgs

  inputString <- readFile file

  let outputString = generator . transformer . parser . lexer $ inputString

  putStrLn outputString


--------------------------------------------------------------------------------------------------
-- Lexer

data Token
  = TokenWord String
  | TokenNumber Int
  deriving (Eq, Show)


lexer :: String -> [Token]
lexer =
  map tokenize . words


tokenize :: String -> Token
tokenize word =
  case (readMaybe word) of
    Just i -> TokenNumber i
    _      -> TokenWord word


--------------------------------------------------------------------------------------------------
-- Parser


data AST
  = Drawing [Syntax]
  deriving (Eq, Show)


data Syntax
  = NumberLiteral Int
  | CallExpression String [Syntax]
  deriving (Eq, Show)


parser :: [Token] -> AST
parser =
  snd . foldl go (Nothing, Drawing [])
  where
    go :: (Maybe Syntax, AST) -> Token -> (Maybe Syntax, AST)

    go (Nothing, ast) (TokenWord w) =
      (Just (CallExpression w []), ast)

    go (Just (CallExpression w@"Paper" []), Drawing xs) (TokenNumber i) =
      (Nothing, Drawing ((CallExpression w [NumberLiteral i]):xs))
      
    go (Just (CallExpression w@"Pen" []), Drawing xs) (TokenNumber i) =
      (Nothing, Drawing ((CallExpression w [NumberLiteral i]):xs))

    go (Just (CallExpression w@"Line" []), ast) (TokenNumber i) =
      (Just (CallExpression w ((NumberLiteral i):[])), ast)

    go (Just (CallExpression w@"Line" (x1:[])), ast) (TokenNumber i) =
      (Just (CallExpression w ((NumberLiteral i):x1:[])), ast)

    go (Just (CallExpression w@"Line" (x2:x1:[])), ast) (TokenNumber i) =
      (Just (CallExpression w ((NumberLiteral i):x2:x1:[])), ast)

    go (Just (CallExpression w@"Line" (x3:x2:x1:[])), Drawing xs) (TokenNumber i) =
      (Nothing, Drawing (CallExpression w ((NumberLiteral i):x3:x2:x1:[]):xs))

    go _ _ =
      error "Syntax Error!"


--------------------------------------------------------------------------------------------------
-- Transformer


type Colour =
  Int


data Item =
  Line { colour :: Colour
       , x1     :: Int
       , y1     :: Int
       , x2     :: Int
       , y2     :: Int
       }
  deriving (Eq, Show)


data Canvas =
  Canvas { paper :: Colour, items :: [Item] }
  deriving (Eq, Show)


transformer :: AST -> Canvas
transformer (Drawing xs) =
  foldl go (0, Canvas 100 []) xs & snd
  where
    go :: (Colour, Canvas) -> Syntax -> (Colour, Canvas)

    go (c, canvas) (CallExpression "Paper" [NumberLiteral i]) =
      (c, canvas { paper = i })

    go (c, canvas) (CallExpression "Pen" [NumberLiteral i]) =
      (i, canvas)

    go (c, canvas@(Canvas { items = is })) (CallExpression "Line" ((NumberLiteral x4):(NumberLiteral x3):(NumberLiteral x2):(NumberLiteral x1):[])) =
      (c, canvas { items = is ++ [Line c x1 x2 x3 x4] })

    go x y | trace ("x: " ++ show x ++ ", y: " ++ show y) False = undefined


--------------------------------------------------------------------------------------------------
-- Generator


generator :: Canvas -> String
generator (Canvas { paper = p, items = xs}) =
  "<svg width=\"100\" height=\"100\" viewBox=\"0 0 100 100\" version=\"1.1\" \
  \xmlns=\"http://www.w3.org/2000/svg\">"

  ++ background

  ++ tags

  ++ "</svg>"


  where
    colour 0   = "rgb(255%, 255%, 255%)"
    colour 100 = "rgb(0%, 0%, 0%)"

    background =
      "<rect x=\"0\" y=\"0\" width=\"100\" height=\"100\" fill=\""
      ++ colour p
      ++ "\"></rect>"

    tags = map go xs & unwords

    go :: Item -> String
    go (Line c x1 y1 x2 y2) =
      "<line x1=\"" ++ show x1 ++ "\" y1=\"" ++ show y1
      ++ "\" x2=\"" ++ show x2 ++ "\" y2=\"" ++ show y2
      ++ "\" style=\"stroke:rgb(0,0,0)\"></line>"

module TurboParser where

import Control.Applicative
import Data.Char

import ParserLib
import TurboDef

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Stmt)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans

mainParser :: Parser Stmt
mainParser = whitespaces *> stmt <* eof

literal :: Parser Double
literal = pure (\int frac -> read (int ++ frac))
          <*> some (satisfy isDigit)
          <*> (pure (:) <*> char '.' <*> some (satisfy isDigit)
               <|> return "")
          <* whitespaces

varname = identifier ["pendown", "penup", "turn", "forward", "for", "to"]

stmt = assignment
       <|> pen
       <|> turn
       <|> forward
       <|> forLoop
       <|> compound

assignment = pure (:=) <*> varname <* terminal "=" <*> expr

pen = keyword "pendown" *> pure PenDown
      <|> keyword "penup" *> pure PenUp

turn = keyword "turn" *> fmap Turn expr

forward = keyword "forward" *> fmap Forward expr

forLoop =
    keyword "for"
    >> varname
    >>= \v -> terminal "="
    >> expr
    >>= \e1 -> keyword "to"
    >> expr
    >>= \e2 -> stmts
    >>= \s -> return (For v e1 e2 s)

stmts = terminal "{" *> many (stmt <* terminal ";") <* terminal "}"

compound = fmap Seq stmts 

expr = additive

additive = chainl1 multiplicative (operator "+" *> pure (:+)
                                   <|> operator "-" *> pure (:-))

multiplicative = chainl1 prefixed (operator "*" *> pure (:*)
                                   <|> operator "/" *> pure (:/))

prefixed =
    operator "-" *> fmap Neg prefixed
    <|> atom

atom = fmap RLit literal
       <|> fmap RVar varname
       <|> terminal "(" *> expr <* terminal ")"

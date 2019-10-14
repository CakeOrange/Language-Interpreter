module TurboParser where

import Control.Applicative

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
    where
        stmt = assign <|> pencmd <|> turn <|> forward <|> forloop <|> seq

        assign = do
            v <- special
            operator "="
            e <- expr
            pure (v := e)
        pencmd = true <|> false
        true = do
            keyword "pendown"
            pure PenDown
        false = do
            keyword "penup"
            pure PenUp
        turn = do
            keyword "turn"
            e <- expr
            pure (Turn e)
        forward = do
            keyword "forward"
            e <- expr
            pure (Forward e)
        forloop = do
            keyword "for"
            name <- special
            operator "="
            from <- expr
            keyword "to"
            to <- expr
            seqs <- seq
            pure (For name from to (getLst seqs))
        seq = do
            char '{' *> whitespaces
            stmts <- many stmtsemicolon
            char '}' *> whitespaces
            pure (Seq stmts)
        stmtsemicolon = do
            s <- stmt
            char ';' *> whitespaces
            pure (s)
            
        expr = adds

        literal = do
            -- fmap RLit integer
            value1 <- whitespaces *> natural
            char '.' *> whitespaces
            value2 <- natural <* whitespaces 
            pure (RLit (concateDouble value1 value2))
            <|> (natural >>= \n -> fmap RLit (return (realToFrac n)))

        var = fmap RVar special

        atoms = literal
            <|> var
            <|> between (char '(' *> whitespaces) 
                        (char ')' *> whitespaces) 
                        expr
            <|> neg
                        
        adds = chainl1 muls ((char '+' *> whitespaces *> pure (:+)) 
            <|> (char '-' *> whitespaces *> pure (:-)))

        muls = chainl1 atoms ((char '*' *> whitespaces *> pure (:*))
            <|> (char '/' *> whitespaces *> pure (:/))) 

        neg = do
            operator "-"
            e <- atoms
            pure (Neg e)
        

        special = identifier ["pendown","penup","turn","forward","for","to"]

-- numDigits :: Integer -> Integer
-- numDigits 0 = 1
-- numDigits n = toInteger (round (logBase 10 (fromIntegral n)) + 1)

-- createDouble :: Integer -> Integer -> Double
-- createDouble a b = (int + dec)
--         where
--             int = fromIntegral a 
--             digs = numDigits b
--             dec = (fromIntegral b) / (fromIntegral (10 ^ digs)) 

concateDouble :: Integer -> Integer -> Double
concateDouble a b = read full :: Double
        where
            int = show a
            dec = show b
            full = int ++ "." ++ dec

--double :: Parser Double
--double = fmap read ((satisfy isDigit) ++ (satisfy isDot) ++ many (satisfy isDigit)) <* whitespaces

-- isDot :: Char -> Bool
-- isDot x
--     | x == '.' = True
--     | otherwise = False

getLst :: Stmt -> [Stmt]
getLst (Seq lst) = lst
        
    
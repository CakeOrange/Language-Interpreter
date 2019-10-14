-- How to use: runghc testTurboParser.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import ParserLib
import TurboDef
import TurboParser (mainParser)

parse :: String -> Maybe Stmt
parse = runParser mainParser

tests =
    [ testHandout
    , testJunkAfter
    , testExpr
    ]
-- more test cases when marking

testHandout =
    "handout" ~: parse inp
    ~?= Just ((Turn (Neg (RLit 90))))
  where
    inp = "\nturn   - 90 \n"

testJunkAfter = "junk after" ~: parse "pendown ( " ~?= Nothing

testExpr = "complicated expr" ~: parse inp ~?= Just answer
  where
    inp = "v = 15 + 225 / (cx1 - cx2) * - -dy2 - 30"
    answer = "v" := (RLit 15 :+ (RLit 225 :/ (RVar "cx1" :- RVar "cx2"))
                                 :* Neg (Neg (RVar "dy2")))
                    :- RLit 30

main = do
    args <- getArgs
    case args of
      a:_ | Just n <- readMaybe a, 0 <= n, n < length tests ->
            do c@Counts{errors=e, failures=f} <- runTestTT (tests !! n)
               if e == 0 && f == 0
                   then return c
                   else exitFailure
          | otherwise -> error "No such test number."
      _ -> runTestTT (TestList tests)

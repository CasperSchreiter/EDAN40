module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show

program = (iter Statement.parse) >-> buildProgram
buildProgram = Program 

shw :: T -> String
shw (Program stmts) = foldr1 (++) $ map (++ "\n") $ map Statement.toString stmts

exec (Program p) = Statement.exec p Dictionary.empty

instance Parse T where
  parse = program
  toString = shw
             

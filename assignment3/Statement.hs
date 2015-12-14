module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip;" >-> buildSkip
buildSkip s = Skip

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin = Begin 

ifStmt = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

whileStmt = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

readStmt = accept "read" -# word #- require ";" >-> buildRead
buildRead = Read

writeStmt = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite = Write  

shw :: String -> Statement -> String
shw ind (Assignment v e) = ind ++  v ++ ":=" ++ (Expr.toString e) ++ ";"
shw ind Skip = ind ++ "skip;"
shw ind (Begin stmts) = ind ++ "begin\n" ++ (shw' (ind ++ "\t") stmts) ++ "\n" ++ ind ++ "end" 
shw ind (If e s1 s2) = ind ++ "if " ++ (Expr.toString e) ++ " then\n" ++ (shw (ind ++ "\t") s1) ++ "\n" ++ ind ++ "else\n" ++ (shw (ind ++ "\t") s2)
shw ind (While e s) = ind ++ "while " ++ (Expr.toString e) ++ " do\n" ++ (shw (ind ++ "\t") s)   
shw ind (Read s) = ind ++  "read " ++ s ++ ";"
shw ind (Write e) = ind ++ "write " ++ (Expr.toString e) ++ ";"

shw' :: String -> [Statement] -> String
shw' ind (stmt:[]) = (shw ind stmt)
shw' ind (stmt:stmts) = (shw ind stmt) ++ "\n" ++ (shw' ind stmts)

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] dict input = []
exec (Skip:stmts) dict input = exec stmts dict input
exec (Begin (stmts):stmtss) dict input = (exec (stmts++stmtss) dict input)
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While cond stmt: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (stmt:(While cond stmt:stmts)) dict input
    else exec stmts dict input 
exec (Assignment v e : stmts) dict input = exec stmts (Dictionary.insert (v, Expr.value e dict) dict) input
exec (Read e : stmts) dict input = exec stmts (Dictionary.insert (e, head input) dict) (tail input )
exec (Write e : stmts) dict input = (Expr.value e dict):(exec stmts dict input)

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifStmt ! whileStmt ! readStmt ! writeStmt
  toString = shw ""

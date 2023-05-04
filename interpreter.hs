import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

import Grammar.Abs
import Grammar.Par
import Grammar.Lex


type Var = String
type Loc = Int
type Env = Map.Map Var Loc
type Err = String
type Store = Map.Map Loc Int
type Type = String

type Interpreter a = (ReaderT Env (StateT Store (ExceptT Err Identity))) a

evalExpr :: Expr Int -> Interpreter Int 

evalExpr (EVar x) = do
    r <- ask -- r :: Env
    s <- get -- s :: State
    if member x r then do
    {
        l <- (r ! x);        -- (r ! x) :: Loc
        if member l s then 
            return (s ! l) -- Int
        else
            throwError ("Weird error - localisation not known in state!")
    }
    else
        throwError ("Unknown variable " ++ x ++ " at some place!")

evalExpr (EInt n) = do
    return n
    
evalExpr (EAdd e1 e2) = do
    v1 <- eval e1 -- v1 :: Int
    v2 <- eval e2
    return (v1 + v2)
    
evalProg :: Program Int -> Interpreter ()

evalProg insts = evalInsts insts
    
evalInsts :: [Inst Int] -> Interpreter ()

evalInsts [] = return ()
evalInsts (inst:tl) = do
{
    evalInst inst
    evalInsts tl
}
    
evalInst :: Inst Int -> Interpreter ()
    
evalInst (IAssign x e) = do 
    r <- ask -- r :: Env
    s <- get -- s :: State
    if member x r then do
        l <- (r ! x)        -- (r ! x) :: Loc
        if member l s then do
            n <- (evalExpr e)
            put (insert s e) -- Int
        else
            throwError ("Weird error - localisation not known in state!")
    else
        throwError ("Unknown variable " ++ x ++ " at some place!")

evalInst (IIf e i1 i2) = do
    n <- (evalExpr e)   
    if (n >= 0) then do
        evalInstr i1
    else
        evalInstr i2

evalInst (IInit t x e) = do
    n <- evalExpr e
    env_v <- ask
    (store, locations) <- get
    l <- (newlock locations)
    local (insert env_v x l)
    put (insert store l n) 


progTree = undefined -- trzeba przepuścić przez parser

runInterpreter :: (Interpreter a) -> Env -> Store -> Either Err a
runInterpreter v r = runIdentity (runExceptT (runStateT (runReaderT v r)))

interpret = runInterpreter (evalProg progTree) map.Empty

interpretFile :: FilePath -> IO ()
interpretFile file = readFile file >>= interpretText

interpretText :: String -> IO ()
interpretText input = interpret parsedTokens
  where
    tokens = myLexer input
    parsedTokens = pProgram tokens

path = "przyklady/przyklad1.txt"
main = interpretFile path






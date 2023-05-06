import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Class

import Grammar.Abs
import Grammar.Par
import Grammar.Lex


type Loc = Integer
type Env = Map.Map Ident Loc
type Err = String
type Store = Map.Map Loc Integer
type StoreLocations = (Store, Integer)
type Type = String

type Interpreter a = (ReaderT Env (ExceptT Err (StateT StoreLocations Identity))) a

evalExpr :: Expr -> Interpreter Integer 

evalExpr (EVar _ x) = do
    env <- ask
    (store, loc) <- get
    if Map.member x env then
        let l = (env Map.! x) in       -- (env ! x) :: Loc
        if Map.member l store then 
            return (store Map.! l) -- Int
        else
            throwError ("Weird error - localisation not known in state!")
    else
        throwError ("Unknown variable " ++ (show x) ++ " at some place!")

evalExpr (EInt _ n) = do
    return n
    
evalExpr (EAdd _ e1 e2) = do
    v1 <- evalExpr e1 -- v1 :: Int
    v2 <- evalExpr e2
    return (v1 + v2)
    
evalProg :: Program -> Interpreter ()

evalProg (Prog _ insts) = evalInsts insts
    
evalInsts :: [Inst] -> Interpreter ()

evalInsts [] = return ()
evalInsts (inst:tl) = do
    evalInst inst
    evalInsts tl
    
evalInst :: Inst -> Interpreter ()
    
evalInst (IAssign _ x e) = do 
    env <- ask
    (store, loc) <- get
    if Map.member x env then
        let l = (env Map.! x) in
        if Map.member l store then do
            n <- (evalExpr e)
            put ((Map.insert l n store), loc)
        else
            throwError ("Weird error - localisation not known in state!")
    else
        throwError ("Unknown variable " ++ (show x) ++ " at some place!")

evalInst (IIf _ e i1 i2) = do
    n <- (evalExpr e)   
    if (n >= 0) then do
        evalInst i1
    else
        evalInst i2

evalInst (IInit _ t x e) = do
    n <- (evalExpr e)
    env_v <- ask
    (store, loc) <- get
    local (const env_v) (Map.insert x loc env_v)
    put ((Map.insert loc n store), (loc + 1)) 


progTree = undefined -- trzeba przepuścić przez parser

runInterpreter :: (Interpreter a) -> Env -> Store -> Either Err a
runInterpreter monad env store = runIdentity (runExceptT (runStateT (runReaderT monad env) store))

interpret progTree = runInterpreter (evalProg progTree) Map.empty (Map.empty, 0)

interpretFile :: FilePath -> IO ()
interpretFile file = readFile file >>= interpretText

interpretText :: String -> IO ()
interpretText input = interpret parsedTokens
  where
    tokens = myLexer input
    parsedTokens = pProgram tokens

path = "przyklady/przyklad1.txt"
main = interpretFile path

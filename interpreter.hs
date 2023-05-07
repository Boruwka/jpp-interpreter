import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Class

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Grammar.Lex
import Grammar.Print
import Grammar.Abs
import Grammar.ErrM
import Grammar.Par


type Loc = Integer
type Env = Map.Map Ident Loc
type ErrMess = String
type Store = Map.Map Loc Integer
type StoreLocations = (Store, Integer)
type Type = String

type Interpreter a = (ReaderT Env (ExceptT ErrMess (StateT StoreLocations Identity))) a

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

evalProg (Prog _ insts) = do
    evalInsts insts
    return ()
    
evalInsts :: [Inst] -> Interpreter (Env -> Env)

evalInsts [] = return id
evalInsts (inst:tl) = do
    env_modifier <- evalInst inst
    local env_modifier (evalInsts tl)
    
evalInst :: Inst -> Interpreter (Env -> Env)
    
evalInst (IAssign _ x e) = do 
    env <- ask
    (store, loc) <- get
    if Map.member x env then
        let l = (env Map.! x) in
        if Map.member l store then do
            n <- (evalExpr e)
            put ((Map.insert l n store), loc)
            return id
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
    env <- ask
    (store, loc) <- get
    --local (const (Map.insert x loc env_v))
    put ((Map.insert loc n store), (loc + 1)) 
    return (\env -> (Map.insert x loc env))



runInterpreter :: (Interpreter a) -> Env -> StoreLocations -> (Either ErrMess a, StoreLocations)
runInterpreter monad env store = runIdentity (runStateT (runExceptT (runReaderT monad env)) store)

interpret :: Program' BNFC'Position -> (Either ErrMess (), StoreLocations)
interpret progTree = runInterpreter (evalProg progTree) (Map.empty) ((Map.empty), 0)

{-interpretFile :: FilePath -> IO ()
interpretFile file = readFile file >>= interpretText

interpretText :: String -> IO ()
interpretText input = print (show (interpret progTree))
  where
    tokens = myLexer input
    progTree = pProgram tokens-}
    
type PInfo = Maybe (Int, Int)

type ParseFun a = [Token] -> Err a

--myLLexer = myLexer

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = readFile f >>= run p

run :: ParseFun Program -> String -> IO ()
run p s = let ts = myLexer s in case p ts of
            Bad s   -> do
                        putStrLn "Interpreter failed to parse the program"
                        exitFailure
            Ok tree -> do
                        print (show (interpret tree))
                        exitSuccess


path = "przyklady/przyklad1.txt"
main = runFile pProgram path

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

-- expressions -----------

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
   
    
evalExpr (ETrue _ ) = return 1

evalExpr (EFalse _ ) = return 0

evalExpr (ENeg _ e) = do
    n <- (evalExpr e)
    return (-n)
    
evalExpr (ENot _ e) = do
    n <- (evalExpr e)
    return (bool_to_integer (not (n > 0)))
    
evalExpr (EMul _ e1 op e2) = 
    let fop = mulop_to_fun op in do
        evalWithOperator e1 e2 fop    
    
evalExpr (EAdd _ e1 op e2) = 
    let fop = addop_to_fun op in do
        evalWithOperator e1 e2 fop
    
evalExpr (EAnd _ e1 e2) = 
    evalWithOperator e1 e2 (fun_bool_to_integer (&&))
    
evalExpr (EOr _ e1 e2) = 
    evalWithOperator e1 e2 (fun_bool_to_integer (||))
    
evalExpr (ERel _ e1 op e2) =
    let fop = relop_to_fun op in do
        n <- evalWithOperator e1 e2 fop
        return n
        
evalWithOperator :: Expr -> Expr -> (Integer -> Integer -> Integer) -> Interpreter Integer
evalWithOperator e1 e2 op = do
    n1 <- evalExpr e1
    n2 <- evalExpr e2
    return (op n1 n2)
    
addop_to_fun :: AddOp -> (Integer -> Integer -> Integer) 

addop_to_fun (Plus _ ) = (+)
addop_to_fun (Minus _ ) = (-)

mulop_to_fun :: MulOp -> (Integer -> Integer -> Integer) 

mulop_to_fun (Times _ ) = (*)
mulop_to_fun (Div _ ) = div
mulop_to_fun (Mod _ ) = mod
    
relop_to_fun :: RelOp -> (Integer -> Integer -> Integer)
relop_to_fun (LTH _ ) n1 n2 =  bool_to_integer (n1 < n2)
relop_to_fun (LE _ ) n1 n2 = bool_to_integer (n1 <= n2)
relop_to_fun (GTH _ ) n1 n2 = bool_to_integer (n1 > n2)
relop_to_fun (GE _ ) n1 n2 = bool_to_integer (n1 >= n2)
relop_to_fun (EQU _ ) n1 n2 = bool_to_integer (n1 == n2)
relop_to_fun (NE _ ) n1 n2 = bool_to_integer (n1 /= n2)

bool_to_integer :: Bool -> Integer
bool_to_integer True = 1
bool_to_integer False = 0

integer_to_bool :: Integer -> Bool
integer_to_bool n = (n > 0)

fun_bool_to_integer :: (Bool -> Bool -> Bool) -> Integer -> Integer -> Integer
fun_bool_to_integer f n1 n2 = 
    bool_to_integer (f b1 b2) where
        b1 = integer_to_bool n1
        b2 = integer_to_bool n2
        
    
-- program and blocks
    
evalProg :: Program -> Interpreter ()

evalProg (Prog _ blocks) = evalBlocks blocks
    
evalBlocks :: [Block] -> Interpreter ()

evalBlocks [] = return ()
evalBlocks (b:tl) = do
    evalBlock b
    local id (evalBlocks tl)
    
evalBlock :: Block -> Interpreter ()

evalBlock (Bl _ insts) = do
    mod <- evalInsts insts
    return ()
    
-- instructions -----------------
    
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

evalInst (IIf _ e b1 b2) = do
    n <- (evalExpr e)   
    if (n > 0) then do
        evalBlock b1
        return id
    else do
        evalBlock b2
        return id

evalInst (IInit _ t x e) = do
    n <- (evalExpr e)
    env <- ask
    (store, loc) <- get
    --local (const (Map.insert x loc env_v))
    put ((Map.insert loc n store), (loc + 1)) 
    return (\env -> (Map.insert x loc env))
    
evalInst (IWhile a e b) = do
    n <- evalExpr e
    if (n > 0) then do
        evalBlock b
        evalInst (IWhile a e b)
    else do
        return id


-- running ----------------------

runInterpreter :: (Interpreter a) -> Env -> StoreLocations -> (Either ErrMess a, StoreLocations)
runInterpreter monad env store = runIdentity (runStateT (runExceptT (runReaderT monad env)) store)

interpret :: Program' BNFC'Position -> (Either ErrMess (), StoreLocations)
interpret progTree = runInterpreter (evalProg progTree) (Map.empty) ((Map.empty), 0)

    
type PInfo = Maybe (Int, Int)

type ParseFun a = [Token] -> Err a


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

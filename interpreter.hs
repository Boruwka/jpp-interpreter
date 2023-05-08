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

type Fun = (Block, [Arg])
type Loc = Integer
type EnvV = Map.Map Ident Loc
type EnvF = Map.Map Ident Fun
type Env = (EnvV, EnvF)
type ErrMess = String
type Store = Map.Map Loc Value
type StoreLocations = (Store, Integer)
data Value = ValInt Integer | ValBool Bool | ValStr String | VVoid

type Interpreter a = (ReaderT Env (ExceptT ErrMess (StateT StoreLocations Identity))) a

instance Show Value where
    show (ValInt n) = show n
    show (ValBool b) = show b
    show (ValStr s) = show s
    show (VVoid) = "()"

-- expressions -----------

evalExpr :: Expr -> Interpreter Value 

evalExpr (EVar _ x) = do
    (env_v, env_f) <- ask
    (store, loc) <- get
    if Map.member x env_v then
        let l = (env_v Map.! x) in
        if Map.member l store then 
            return (store Map.! l) 
        else
            throwError ("Weird error - localisation not known in state!")
    else
        throwError ("Unknown variable " ++ (show x) ++ " at some place!")

evalExpr (EInt _ n) = return (ValInt n)  
    
evalExpr (ETrue _ ) = return (ValBool True)

evalExpr (EFalse _ ) = return (ValBool False)

evalExpr (EString _ s) = return (ValStr s)

evalExpr (ENeg _ e) = do
    ev <- (evalExpr e)
    case ev of 
        (ValInt n) -> return (ValInt (negate n))
        _ -> throwError("Error, only integer expression can be negated!")
    
evalExpr (ENot _ e) = do
    ev <- (evalExpr e)
    case ev of 
        (ValBool b) -> return (ValBool (not b))
        _ -> throwError("Error, only boolean expression can be logically negated!")
    
evalExpr (EMul _ e1 op e2) = 
    let fop = mulop_to_fun op in
        evalWithIntOperator e1 e2 fop    
    
evalExpr (EAdd _ e1 op e2) = 
    let fop = addop_to_fun op in
        evalWithIntOperator e1 e2 fop
    
evalExpr (EAnd _ e1 e2) = 
    evalWithBoolOperator e1 e2 (&&)
    
evalExpr (EOr _ e1 e2) = 
    evalWithBoolOperator e1 e2 (||)
    
evalExpr (ERel _ e1 op e2) =
    let fop = relop_to_fun op in
        evalWithRelOperator e1 e2 fop
        
evalWithIntOperator :: Expr -> Expr -> (Integer -> Integer -> Integer) -> Interpreter Value
evalWithIntOperator e1 e2 op = do
    ev1 <- evalExpr e1
    ev2 <- evalExpr e2
    case ev1 of 
        (ValInt n1) -> case ev2 of
            (ValInt n2) -> return (ValInt (op n1 n2))
            _ -> throwError ("Error: both arguments of an arithmetic operation must be integers.")
        _ -> throwError ("Error: both arguments of an arithmetic operation must be integers.")
    
    
evalWithBoolOperator :: Expr -> Expr -> (Bool -> Bool -> Bool) -> Interpreter Value
evalWithBoolOperator e1 e2 op = do
    ev1 <- evalExpr e1
    ev2 <- evalExpr e2
    case ev1 of 
        (ValBool b1) -> case ev2 of
            (ValBool b2) -> return (ValBool (op b1 b2))
            _ -> throwError ("Error: both arguments of an logical operation must be booleans.")
        _ -> throwError ("Error: both arguments of an arithmetic operation must be boolean.")
    
evalWithRelOperator :: Expr -> Expr -> (Integer -> Integer -> Bool) -> Interpreter Value
evalWithRelOperator e1 e2 op = do
    ev1 <- evalExpr e1
    ev2 <- evalExpr e2
    case ev1 of 
        (ValInt n1) -> case ev2 of
            (ValInt n2) -> return (ValBool (op n1 n2))
            _ -> throwError ("Error: both arguments of a relational operation must be integers.")
        _ -> throwError ("Error: both arguments of a relational operation must be integers.")
    
addop_to_fun :: AddOp -> (Integer -> Integer -> Integer) 

addop_to_fun (Plus _ ) = (+)
addop_to_fun (Minus _ ) = (-)

mulop_to_fun :: MulOp -> (Integer -> Integer -> Integer) 

mulop_to_fun (Times _ ) = (*)
mulop_to_fun (Div _ ) = div
mulop_to_fun (Mod _ ) = mod
    
relop_to_fun :: RelOp -> (Integer -> Integer -> Bool)
relop_to_fun (LTH _ ) n1 n2 =  (n1 < n2)
relop_to_fun (LE _ ) n1 n2 = (n1 <= n2)
relop_to_fun (GTH _ ) n1 n2 = (n1 > n2)
relop_to_fun (GE _ ) n1 n2 = (n1 >= n2)
relop_to_fun (EQU _ ) n1 n2 = (n1 == n2)
relop_to_fun (NE _ ) n1 n2 = (n1 /= n2)

{-bool_to_integer :: Bool -> Integer
bool_to_integer True = 1
bool_to_integer False = 0

integer_to_bool :: Integer -> Bool
integer_to_bool n = (n > 0)

fun_bool_to_integer :: (Bool -> Bool -> Bool) -> Integer -> Integer -> Integer
fun_bool_to_integer f n1 n2 = 
    bool_to_integer (f b1 b2) where
        b1 = integer_to_bool n1
        b2 = integer_to_bool n2-}
        
    
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
    (env_v, env_f) <- ask
    (store, loc) <- get
    if Map.member x env_v then
        let l = (env_v Map.! x) in
        if Map.member l store then do
            n <- (evalExpr e)
            put ((Map.insert l n store), loc)
            return id
        else
            throwError ("Weird error - localisation not known in state!")
    else
        throwError ("Unknown variable " ++ (show x) ++ " at some place!")

evalInst (IIf _ e b1 b2) = do
    ev <- (evalExpr e)
    case ev of
        (ValBool True) -> do
            evalBlock b1
            return id
        (ValBool False) -> do
            evalBlock b2
            return id 
        _ -> throwError ("Error: if condition must be boolean.")   
            

evalInst (IInit _ t x e) = do
    ev <- (evalExpr e)
    case t of
        (TypeInt _ ) -> case ev of 
            (ValInt _) -> do
                (env_v, env_f) <- ask
                (store, loc) <- get
                put ((Map.insert loc ev store), (loc + 1)) 
                return (\(env_v, env_f) -> ((Map.insert x loc env_v), env_f))
            _ -> throwError ("Error. Type declared integer but value is not integer.")
        (TypeStr _ ) -> case ev of 
            (ValStr _) -> do
                (env_v, env_f) <- ask
                (store, loc) <- get
                put ((Map.insert loc ev store), (loc + 1)) 
                return (\(env_v, env_f) -> ((Map.insert x loc env_v), env_f))
            _ -> throwError ("Error. Type declared string but value is not string.")
            
        (TypeBool _ ) -> case ev of 
            (ValBool _) -> do
                (env_v, env_f) <- ask
                (store, loc) <- get
                put ((Map.insert loc ev store), (loc + 1)) 
                return (\(env_v, env_f) -> ((Map.insert x loc env_v), env_f))
            _ -> throwError ("Error. Type declared bool but value is not boolean.")
        _ -> throwError ("Error. Not known type.")
    
evalInst (IWhile a e b) = do
    ev <- evalExpr e
    case ev of 
        (ValBool True) -> do
            evalBlock b
            evalInst (IWhile a e b) 
        (ValBool False) -> do
            return id
        _ -> throwError("Error. While condition must be boolean.")


-- running ----------------------

runInterpreter :: (Interpreter a) -> Env -> StoreLocations -> (Either ErrMess a, StoreLocations)
runInterpreter monad env store = runIdentity (runStateT (runExceptT (runReaderT monad env)) store)

interpret :: Program' BNFC'Position -> (Either ErrMess (), StoreLocations)
interpret progTree = runInterpreter (evalProg progTree) (Map.empty, Map.empty) ((Map.empty), 0)

    
type PInfo = Maybe (Int, Int)

type ParseFun a = [Token] -> Err a


runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = readFile f >>= run p

show_state_or_error :: (Either ErrMess (), StoreLocations) -> String
show_state_or_error (Left errMess, _) = show errMess
show_state_or_error (Right (), s) = show s

run :: ParseFun Program -> String -> IO ()
run p s = let ts = myLexer s in case p ts of
            Bad s   -> do
                        putStrLn "Interpreter failed to parse the program"
                        putStrLn (show s)
                        print "blad parsowania"
                        exitFailure
            Ok tree -> do
                        print (show_state_or_error (interpret tree))
                        exitSuccess


--path = "przyklady/przyklad1.txt"
main = do
    [path] <- getArgs
    runFile pProgram path

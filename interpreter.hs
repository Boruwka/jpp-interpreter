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

type Interpreter a = (ReaderT Env (ExceptT ErrMess (StateT StoreLocations (IO)))) a

instance Show Value where
    show (ValInt n) = show n
    show (ValBool b) = show b
    show (ValStr s) = show s
    show (VVoid) = "()"

-- expressions -----------

evalExpr :: Expr -> Interpreter Value 

evalExpr (EVar pos x) = do
    (env_v, env_f) <- ask
    (store, loc) <- get
    if Map.member x env_v then
        let l = (env_v Map.! x) in
        if Map.member l store then 
            return (store Map.! l) 
        else
            throwError ("Weird error " ++ " at position " ++ show pos ++ " - localisation not known in state!")
    else
        throwError ("Unknown variable " ++ (show x) ++ " at position " ++ (show pos))

evalExpr (EInt _ n) = return (ValInt n)  
    
evalExpr (ETrue _ ) = return (ValBool True)

evalExpr (EFalse _ ) = return (ValBool False)

evalExpr (EString _ s) = return (ValStr s)

evalExpr (ECall pos f es) = do
    (env_v, env_f) <- ask
    if Map.member f env_f then
        let (body, args) = (env_f Map.! f) in do
            env_mod <- (assign_args es args env_v pos)
            let env_mod_with_clean = (env_mod . (\(env_v, env_f) -> (Map.empty, env_f))) in do
                local env_mod_with_clean (evalBlock body)
                return VVoid -- na razie funkcja nic nie zwraca nigdy
    else 
        throwError("Error. Function " ++ (show f) ++ " not declared" ++ " at position " ++ (show pos))
    

evalExpr (ENeg pos e) = do
    ev <- (evalExpr e)
    case ev of 
        (ValInt n) -> return (ValInt (negate n))
        _ -> throwError("Error, only integer expression can be negated!" ++ " at position " ++ (show pos))
    
evalExpr (ENot pos e) = do
    ev <- (evalExpr e)
    case ev of 
        (ValBool b) -> return (ValBool (not b))
        _ -> throwError("Error, only boolean expression can be logically negated!" ++ " at position " ++ (show pos))
    
evalExpr (EMul pos e1 op e2) = 
    let fop = mulop_to_fun op in do
        evalWithIntOperator e1 e2 fop (Just op) pos   
    
evalExpr (EAdd pos e1 op e2) = 
    let fop = addop_to_fun op in
        evalWithIntOperator e1 e2 fop Nothing pos
    
evalExpr (EAnd pos e1 e2) = 
    evalWithBoolOperator e1 e2 (&&) pos
    
evalExpr (EOr pos e1 e2) = 
    evalWithBoolOperator e1 e2 (||) pos
    
evalExpr (ERel pos e1 op e2) =
    let fop = relop_to_fun op in
        evalWithRelOperator e1 e2 fop pos
        
evalWithIntOperator :: Expr -> Expr -> (Integer -> Integer -> Integer) -> Maybe MulOp -> BNFC'Position -> Interpreter Value
evalWithIntOperator e1 e2 op raw_op pos = do
    ev1 <- evalExpr e1
    ev2 <- evalExpr e2
    detect_zero_division ev1 ev2 raw_op pos
    case ev1 of 
        (ValInt n1) -> case ev2 of
            (ValInt n2) -> return (ValInt (op n1 n2))
            _ -> throwError ("Error: both arguments of an arithmetic operation must be integers." ++ " at position " ++ (show pos))
        _ -> throwError ("Error: both arguments of an arithmetic operation must be integers." ++ " at position " ++ (show pos))
    
    
evalWithBoolOperator :: Expr -> Expr -> (Bool -> Bool -> Bool) -> BNFC'Position -> Interpreter Value
evalWithBoolOperator e1 e2 op pos = do
    ev1 <- evalExpr e1
    ev2 <- evalExpr e2
    case ev1 of 
        (ValBool b1) -> case ev2 of
            (ValBool b2) -> return (ValBool (op b1 b2))
            _ -> throwError ("Error: both arguments of an logical operation must be booleans." ++ " at position " ++ (show pos))
        _ -> throwError ("Error: both arguments of an arithmetic operation must be boolean." ++ " at position " ++ (show pos))
    
evalWithRelOperator :: Expr -> Expr -> (Integer -> Integer -> Bool) -> BNFC'Position -> Interpreter Value
evalWithRelOperator e1 e2 op pos = do
    ev1 <- evalExpr e1
    ev2 <- evalExpr e2
    case ev1 of 
        (ValInt n1) -> case ev2 of
            (ValInt n2) -> return (ValBool (op n1 n2))
            _ -> throwError ("Error: both arguments of a relational operation must be integers." ++ " at position " ++ (show pos))
        _ -> throwError ("Error: both arguments of a relational operation must be integers." ++ " at position " ++ (show pos))
    
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

detect_zero_division :: Value -> Value -> Maybe MulOp -> BNFC'Position -> Interpreter Value
detect_zero_division v1 v2 op pos = do
    case v2 of 
        (ValInt 0) -> case op of
            (Just (Div _)) -> throwError("Error. Division by 0 at position " ++ (show pos))
            (Just (Mod _)) -> throwError("Error. Taking modulo by 0 at position " ++ (show pos))
            _ -> return (VVoid)
        _ -> return (VVoid)
     
     
assign_args :: [Expr] -> [Arg] -> EnvV -> BNFC'Position -> Interpreter (Env -> Env)

assign_args [] [] _ _ = return id

assign_args [] _ _ pos = throwError ("Too few arguments given" ++ " at position " ++ (show pos))

assign_args _ [] _ pos = throwError ("Too many arguments given" ++ " at position " ++ (show pos))

assign_args (e:etl) ((ArgVal a t ident):atl) old_env pos = do
    env_modifier <- evalInst (IInit a t ident e) 
    env_mods <- (assign_args etl atl old_env pos) 
    return (env_mods . env_modifier)  
    
assign_args (e:etl) ((ArgRef a t ident):atl) old_env pos =
    let l = (old_env Map.! ident) in
        let env_modifier = (\(env_v, env_f) -> ((Map.insert ident l env_v), env_f)) in do
            env_mods <- (assign_args etl atl old_env pos) 
            return (env_mods . env_modifier) 
    
-- program and blocks
    
evalProg :: Program -> Interpreter ()

evalProg (Prog _ insts) = do
    mod <- evalInsts insts
    return ()
    
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
    env_mods <- local env_modifier (evalInsts tl)
    return (env_mods . env_modifier)
    
    
evalInst :: Inst -> Interpreter (Env -> Env)
    
evalInst (IAssign pos x e) = do 
    (env_v, env_f) <- ask
    (store, loc) <- get
    if Map.member x env_v then
        let l = (env_v Map.! x) in
        if Map.member l store then do
            n <- (evalExpr e)
            put ((Map.insert l n store), loc)
            return id
        else
            throwError ("Weird error - localisation not known in state!" ++ " at position " ++ (show pos))
    else
        throwError ("Unknown variable " ++ (show x)  ++ " at position " ++ (show pos))

evalInst (IIfElse pos e b1 b2) = do
    ev <- (evalExpr e)
    case ev of
        (ValBool True) -> do
            evalBlock b1
            return id
        (ValBool False) -> do
            evalBlock b2
            return id 
        _ -> throwError ("Error: if condition must be boolean." ++ " at position " ++ (show pos))  
        
evalInst (IIf pos e b) = do
    ev <- (evalExpr e)
    case ev of
        (ValBool True) -> do
            evalBlock b
            return id
        (ValBool False) -> do
            return id 
        _ -> throwError ("Error: if condition must be boolean." ++ " at position " ++ (show pos))   
            

evalInst (IInit pos t x e) = do
    ev <- (evalExpr e)
    check_type t ev pos
    (env_v, env_f) <- ask
    (store, loc) <- get
    put ((Map.insert loc ev store), (loc + 1)) 
    return (\(env_v, env_f) -> ((Map.insert x loc env_v), env_f))
    
    
evalInst (IWhile pos e b) = do
    ev <- evalExpr e
    case ev of 
        (ValBool True) -> do
            evalBlock b
            evalInst (IWhile pos e b) 
        (ValBool False) -> do
            return id
        _ -> throwError("Error. While condition must be boolean." ++ " at position " ++ (show pos))
        
evalInst (IFunDef _ name args body) = do
    return (\(env_v, env_f) -> (env_v, Map.insert name (body, args) env_f))
    
evalInst (IExpr _ e) = do
    evalExpr e
    return id
    
evalInst (IPrint pos e) = do
    ev <- evalExpr e
    liftIO $ print (show ev)
    return id

check_type :: Type -> Value -> BNFC'Position -> Interpreter ()
check_type t ev pos = do
    case t of
        (TypeInt _ ) -> case ev of 
            (ValInt _) -> return ()
            _ -> throwError ("Error. Type declared integer but value is not integer." ++ " at position " ++ (show pos))
        (TypeStr _ ) -> case ev of 
            (ValStr _) -> return ()
            _ -> throwError ("Error. Type declared string but value is not string." ++ " at position " ++ (show pos))
        (TypeBool _ ) -> case ev of 
            (ValBool _) -> return ()
            _ -> throwError ("Error. Type declared bool but value is not boolean." ++ " at position " ++ (show pos))
        (TypeVoid _ ) -> case ev of 
            (VVoid) -> return ()
            _ -> throwError ("Error. Type declared void but value is not void." ++ " at position " ++ (show pos))

-- running ----------------------


runInterpreter :: (Interpreter a) -> Env -> StoreLocations -> IO (Either ErrMess a, StoreLocations)
runInterpreter monad env store = (runStateT (runExceptT (runReaderT monad env)) store)

show_without_storeloc :: (Either ErrMess (), StoreLocations) -> String
show_without_storeloc (Left err, s) = show err
show_without_storeloc (Right (), s) = ""

interpret :: Program' BNFC'Position -> IO ()
interpret progTree = do
    result <- ((runInterpreter (evalProg progTree) (Map.empty, Map.empty) ((Map.empty), 0)))
    print (show_without_storeloc result)
    return ()

    
type PInfo = Maybe (Int, Int)

type ParseFun a = [Token] -> Err a


runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = readFile f >>= run p
                        
run :: ParseFun Program -> String -> IO ()
run p s = let ts = myLexer s in case p ts of
            Bad s   -> do
                        putStrLn "Interpreter failed to parse the program"
                        putStrLn (show s)
                        print "blad parsowania"
                        exitFailure
            Ok tree -> do
                        interpret tree
                        exitSuccess


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Run interpreter on program from stdin"
    , "  filepath        Run interpreter on program from filepath"
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run pProgram
    [f] -> runFile pProgram f
    _ -> putStrLn "Too many arguments" >> exitFailure

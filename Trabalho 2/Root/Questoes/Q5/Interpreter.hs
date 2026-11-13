module Root.Questoes.Q5.Interpreter where

import Root.Questoes.Q5.AbsLI
import Prelude hiding (lookup)

type RContext = [(String, Valor)]

type ErrorMessage = String

data Valor
  = ValorStr String
  | ValorInt Integer
  | ValorBool Bool

instance Eq Valor where
  (ValorInt i1) == (ValorInt i2) = i1 == i2
  (ValorStr s1) == (ValorStr s2) = s1 == s2
  (ValorBool b1) == (ValorBool b2) = b1 == b2

instance Show Valor where
  show (ValorInt vint) = show vint
  show (ValorStr vstr) = vstr
  show (ValorBool vb) = show vb

s :: Valor -> String
s (ValorStr str) = str

i :: Valor -> Integer
i (ValorInt vint) = vint

b :: Valor -> Bool
b (ValorBool vbool) = vbool

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm

execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
  SAss id exp -> 
    case eval context exp of
      Right v -> Right (update context (getStr id) v)
      Left msg -> Left msg

  SBlock [] -> Right context
  
  SBlock (stmt:stms) -> 
    case execute context stmt of
      Right newContext -> execute newContext (SBlock stms)
      Left msg -> Left msg

  SWhile exp stm ->
    case eval context exp of
      Right val ->
        if i val /= 0
          then case execute context stm of
                 Right newContext -> execute newContext (SWhile exp stm)
                 Left msg -> Left msg
          else Right context
      Left msg -> Left msg

  SdoWhile stm exp ->
    case execute context stm of
      Right newContext -> execute newContext (SWhile exp stm)
      Left msg -> Left msg

  STry tryStms catchStms finallyStms ->
    let
      execTry ctx [] = Right ctx
      execTry ctx (stmt:ss) =
        case execute ctx stmt of
          Right newCtx -> execTry newCtx ss
          Left _ -> Left ctx
    in case execTry context tryStms of
      Right tryCtx -> execute tryCtx (SBlock finallyStms)
      Left failCtx -> 
        case execute failCtx (SBlock catchStms) of
          Right catchCtx -> execute catchCtx (SBlock finallyStms)
          Left msg -> Left msg

eval :: RContext -> Exp -> Either ErrorMessage Valor
eval context x = case x of
  EAdd exp0 exp1 ->
    case eval context exp0 of
      Right v0 -> case eval context exp1 of
        Right v1 -> Right (ValorInt (i v0 + i v1))
        Left msg -> Left msg
      Left msg -> Left msg

  ESub exp0 exp1 ->
    case eval context exp0 of
      Right v0 -> case eval context exp1 of
        Right v1 -> Right (ValorInt (i v0 - i v1))
        Left msg -> Left msg
      Left msg -> Left msg

  EMul exp0 exp1 ->
    case eval context exp0 of
      Right v0 -> case eval context exp1 of
        Right v1 -> Right (ValorInt (i v0 * i v1))
        Left msg -> Left msg
      Left msg -> Left msg

  EDiv exp0 exp1 ->
    case eval context exp0 of
      Right v0 -> case eval context exp1 of
        Right v1 ->
          if i v1 == 0
            then Left "divisao por 0"
            else Right (ValorInt (i v0 `div` i v1))
        Left msg -> Left msg
      Left msg -> Left msg

  ECon exp0 exp1 ->
    case eval context exp0 of
      Right v0 -> case eval context exp1 of
        Right v1 -> Right (ValorStr (s v0 ++ s v1))
        Left msg -> Left msg
      Left msg -> Left msg

  EOr exp0 exp1 ->
    case eval context exp0 of
      Right v0 -> case eval context exp1 of
        Right v1 -> Right (ValorBool (b v0 || b v1))
        Left msg -> Left msg
      Left msg -> Left msg

  EAnd exp0 exp1 ->
    case eval context exp0 of
      Right v0 -> case eval context exp1 of
        Right v1 -> Right (ValorBool (b v0 && b v1))
        Left msg -> Left msg
      Left msg -> Left msg

  ENot exp0 ->
    case eval context exp0 of
      Right v0 -> Right (ValorBool (not (b v0)))
      Left msg -> Left msg

  EInt n -> Right (ValorInt n)
  
  EStr str -> Right (ValorStr str)
  
  ETrue -> Right (ValorBool True)
  
  EFalse -> Right (ValorBool False)
  
  EVar id -> Right (lookup context (getStr id))

getStr :: Ident -> String
getStr (Ident str) = str

lookup :: RContext -> String -> Valor
lookup ((chave, valor) : cs) str
  | chave == str = valor
  | otherwise = lookup cs str

update :: RContext -> String -> Valor -> RContext
update [] str valor = [(str, valor)]
update ((chave, valor_antigo) : cs) str novo_valor
  | chave == str = (chave, novo_valor) : cs
  | otherwise = (chave, valor_antigo) : update cs str novo_valor
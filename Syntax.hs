module Syntax(Bind,
              mkNonRec, mkRec,
              Expr,
              mkVar, mkLam, mkLet, mkApp, mkCase,
              Alt,
              mkAlt,
              AltCon,
              mkDefault, mkDataAlt,
              DataCon,
              mkDataCon,
              Type,
              mkAlgType, mkFunType,
              Ident,
              mkIdent) where

data Bind
  = Rec [(Ident, Expr)]
  | NonRec Ident Expr
    deriving (Eq, Ord, Show)

mkRec = Rec
mkNonRec = NonRec

data Expr
  = Lam Ident Expr
  | Let Bind Expr
  | App Expr Expr
  | Var Ident
  | Case Expr [Alt]
    deriving (Eq, Ord, Show)

mkVar = Var
mkLam = Lam
mkLet = Let
mkApp = App
mkCase = Case

data Alt
  = Alt AltCon Expr
    deriving (Eq, Ord, Show)

mkAlt = Alt

data AltCon
  = Default
  | DataAlt DataCon [Ident]
    deriving (Eq, Ord, Show)

mkDefault = Default
mkDataAlt = DataAlt

data DataCon
  = DataCon String [Type] Type
    deriving (Eq, Ord, Show)

mkDataCon = DataCon

data Type
  = AlgType String [DataCon]
  | FunType Type Type
    deriving (Eq, Ord)

mkAlgType = AlgType
mkFunType = FunType

instance Show Type where
  show (AlgType n dcs) = n
  show (FunType l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"

data Ident
  = Ident String Type
    deriving (Eq, Ord, Show)

mkIdent = Ident

module Main(main) where

import Control.Applicative
import CoreSyn
import Data.List as L
import DynFlags
import GHC
import GHC.Paths
import GhcMonad
import Name
import Outputable
import TyCon
import Type
import Var

import Syntax

filePath = "/Users/dillon/Haskell/Misc/GHCTest/TypeTest.hs"

main :: IO ()
main =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    }
      coreMod <- compileToCoreSimplified filePath
      liftIO $ putStrLn $ show $ translateBindingsToLHC coreMod

translateBindingsToLHC :: CoreModule -> [Syntax.Bind]
translateBindingsToLHC m =
  L.map coreBindingToLHC $ cm_binds m

coreBindingToLHC :: CoreSyn.Bind CoreBndr -> Syntax.Bind
coreBindingToLHC (NonRec b e) =
  mkNonRec (coreVarToIdent b) (coreExprToLHC e)
coreBindingToLHC (Rec bs) =
  mkRec $ L.map rBindToLHC bs

rBindToLHC :: (CoreBndr, CoreSyn.Expr CoreBndr) -> (Ident, Syntax.Expr)
rBindToLHC (b, e) =
  (coreVarToIdent b, coreExprToLHC e)

coreVarToIdent :: Var -> Ident
coreVarToIdent v = mkIdent (getOccString v) (coreTypeToLHC $ varType v)

coreExprToLHC :: CoreSyn.Expr CoreBndr -> Syntax.Expr
coreExprToLHC (Var id) =
  mkVar $ coreVarToIdent id
coreExprToLHC (App l r) =
  mkApp (coreExprToLHC l) (coreExprToLHC r) 
coreExprToLHC (Lam v e) =
  mkLam (coreVarToIdent v) (coreExprToLHC e)
coreExprToLHC (Let b e) =
  mkLet (coreBindingToLHC b) (coreExprToLHC e)
coreExprToLHC (Case e _ _ alts) =
  mkCase (coreExprToLHC e) (L.map coreAltToLHCAlt alts)
coreExprToLHC (Tick _ e) = coreExprToLHC e
coreExprToLHC (Cast _ _) = error "coreExprToLHC: cast"
coreExprToLHC (Type _) = error "coreExprToLHC: type"
coreExprToLHC (Coercion _) = error "coreExprToLHC: coercion"
coreExprToLHC (Lit _) = error "coreExprToLHC: literal"

coreAltToLHCAlt (DEFAULT, [], e) =
  mkAlt mkDefault (coreExprToLHC e)
coreAltToLHCAlt (DataAlt dc, vars, e) =
  mkAlt (mkDataAlt (coreDataConToLHC dc) $ L.map coreVarToIdent vars) (coreExprToLHC e)
coreAltToLHCAlt (dc, vars, e) =
  error "coreAltToLHCAlt: alt is not DEFAULT or DataAlt"
  
coreDataConToLHC dc =
  let (_, _, argTypes, retType) = dataConSig dc
      lhcArgTypes = L.map coreTypeToLHC argTypes
      lhcRetType = coreTypeToLHC retType
      id = getOccString dc in
  mkDataCon id lhcArgTypes lhcRetType

coreTypeToLHC :: GHC.Type -> Syntax.Type
coreTypeToLHC t =
  case isAlgType t of
   True -> coreAlgTypeToLHC t
   False -> case isFunTy t of
     True -> coreFunTypeToLHC t
     False -> error "coreTypeToLHC: not algebraic or function type"

coreFunTypeToLHC t =
  let (argTypes, resType) = splitFunTys t
      lhcArgTypes = L.map coreTypeToLHC argTypes
      lhcResType = coreTypeToLHC resType in
   L.foldr mkFunType lhcResType lhcArgTypes

coreAlgTypeToLHC t =
  case splitTyConApp_maybe t of
   Just (tc, []) -> coreTyConToAlgType tc
   Just (tc, _) -> error "No polymorphic types allowed"
   Nothing -> error "coreAlgTypeToLHC: No type constructor"

coreTyConToAlgType tc =
  case algTyConRhs tc of
   DataTyCon cons _ -> mkAlgType (getOccString tc) (L.map coreDataConToLHC cons)
   NewTyCon con _ _ _ -> mkAlgType (getOccString tc) [coreDataConToLHC con]

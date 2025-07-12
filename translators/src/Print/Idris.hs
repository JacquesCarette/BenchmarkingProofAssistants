{-# OPTIONS_GHC -fno-warn-orphans #-}
module Print.Idris
  ( printModule
  , render
  , runIdris
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Grammar
import Print.Generic

newtype Idris ann = Idris {get :: Doc ann}

instance Keywords (Doc ann) where
  import_ = "import"
  assign  = "="
  data_   = "data"
  recrd   = "record"
  univ    = "Type"
  arr     = "->"
  lcons   = comma
  vcons   = comma
  typesep = ":"
  natT    = "Nat"
  strT    = "String"
  vectT   = "Vect"

instance TypeAnn (Doc ann) where
  typeAnn trm typ = trm <+> typesep <+> typ
  teleCell Explicit trm typ = parens $ trm <+> typesep <+> typ
  teleCell Implicit trm typ = braces $ trm <+> typesep <+> typ

-- append an Import if needed
printWithImport :: Import -> Doc ann -> Doc ann
printWithImport (ImportLib VecMod) m = m <> import_ <+> "Data.Vect" <> hardline
-- There rest are builtin
printWithImport (ImportLib NatMod) m = m
printWithImport (ImportLib StringMod) m = m
printWithImport (ImportLib ListMod) m = m

printArgL :: Arg [ Name ] Tm -> Doc ann
printArgL (Arg [] t _) = printTm t
printArgL (Arg l@(_:_) t v) = teleCell v (hsep $ punctuate comma $ map pretty l) (printTm t)

printTm :: Tm -> Doc ann
printTm (Univ) = univ
printTm (Arr t1 t2) = printArgL t1 <+> arr <+> printTm t2
printTm (Pi lt t) = foldr (\a d -> printArgL a <+> arr <+> d) (printTm t) lt
printTm (PCon t []) = pretty t
printTm (PCon name types) = pretty name <+> hsep (map printTm types)
printTm (DCon t []) = pretty t
printTm (DCon name types) = pretty name <+> hsep (map printTm types)
printTm (Var var) = pretty var
printTm (Paren e) = parens $ printTm e
printTm (Binary op e1 e2) = printTm e1 <+> printOp2 op <+> printTm e2
printTm (Let ds expr) =
  "let" <+> align (vcat (map printLocalDefn ds) <+> "in") <> line <>
  printTm expr
printTm (App fun args) = printTm fun <+> (fillSep (map (group . printTm) args))
printTm (Unary o t) = parens $ printOp1 o <+> printTm t
printTm (Lit l) = printLit l
printTm (KCon NatT _) = natT
printTm (KCon StringT _) = strT
printTm (KCon VecT [s, t]) = vectT <+> printTm t <+> printTm s
printTm (KCon VecT _) = error "Vect takes exactly two arguments"

printLit :: Literal -> Doc ann
printLit (Nat n) = pretty n
printLit (Bool b) = pretty b
printLit (String str) = dquotes $ pretty str
printLit (Vec l) =  brackets $ fillSep $ punctuate vcons $ map printTm l
printLit (List l) = brackets $ fillSep $ punctuate lcons $ map printTm l

printOp1 :: Op1 -> Doc ann
printOp1 Suc = "S"

printOp2 :: Op2 -> Doc ann
printOp2 Plus = "+"

printFieldT :: FieldT -> Doc ann
printFieldT (FieldT fname ftype) = typeAnn (pretty fname) (printTm ftype)

printFieldDecl :: FieldDecl -> Doc ann
printFieldDecl (FieldDecl fields) = vsep $ map printFieldT fields

printDataConst :: Parameters -> DataCons -> Doc ann
printDataConst p (DataCons l) = vsep $ map (prettyCon p) l

prettyCon :: Parameters -> Constr -> Doc ann
prettyCon [] (Constr n t) = typeAnn (pretty n) (printTm t)
prettyCon p  (Constr n t) = typeAnn (pretty n)
                                    (encloseSep emptyDoc (space <> arr) (space <> arr <> space)
                                       (map (pretty.arg) p))
                            <+> printTm t

printCase :: Name -> Pat -> Doc ann
printCase var (Pat a e) = pretty var <+> (hsep $ map (pretty . arg) a) <+> assign <+> printTm e

printMatch :: Name -> Patterns -> Doc ann
printMatch nm (Patterns p) = vsep (map (printCase nm) p)

printLocalDefn :: LocalDefn -> Doc ann
printLocalDefn (LocDefFun var ty args expr) =
  typeSig <> tvar <+> assign <+> align (printTm expr)
    where
        typeSig = case ty of
            Just t -> typeAnn (pretty var) ((hsep $ punctuate (space <> arr) $ map (printTm . argty) args) <+> arr <+> printTm t) <> line
            Nothing -> mempty
        tvar = case args of
            [] -> pretty var
            (_:_) -> pretty var <+> (hsep $ map (pretty . arg) args)

printDef :: Definition -> Doc ann
printDef (DefTVar var t expr) =
  typeAnn (pretty var) (printTm t) <> hardline <>
  pretty var <+> assign <+> align (printTm expr) <> hardline

printDef (DefPatt var ty _ cons) = typeAnn (pretty var) (printTm ty) <> line <> printMatch var cons
printDef (DefPDataType name params cons ty) =
  data_ <+>
    prettyParams params <+> "where" <> hardline <> indent 1 (printDataConst params cons) <> hardline
    where
      prettyParams [] = typeAnn (pretty name) (printTm ty)
      prettyParams p = typeAnn (pretty name) (concatWith (\x y -> x <+> arr <+> y) $
                                map (\(Arg x y v) -> teleCell v (pretty x) (printTm y)) p)
                           <+> arr <+> printTm ty

printDef (DefRecType name params consName fields _) =
    recrd <+> pp_params <+> "where" <> hardline <>
    indent 4 (vsep $ "constructor" <+> pretty consName : printFieldDecl fields : []) <>
    hardline
    where
      ll = map (\(Arg n t v) -> teleCell v (pretty n) (printTm t)) params
      pp_params = if null params then pretty name else pretty name <+> hsep ll

printDef (DefRec name recType consName (FieldDef fields)) =
    typeAnn (pretty name) (printTm recType) <> hardline <>
    pretty name <+> assign <+> pretty consName <+> nest 4 (sep (map (printTm . fval) fields)) <>
    hardline

printDef (OpenName _) = emptyDoc
printDef (Separator '\n' n _) = blanklines n
printDef (Separator c n b) =
  let s = hcat $ replicate (fromIntegral n) (pretty c) in
  if b then hardline <> s <> hardline else s

printModule :: Module -> Idris ann
printModule (Module _ imports defs) =
    let
        mmain = "module Main" <> hardline
        headers = foldr printWithImport mmain imports
        -- Concatenate all definitions
        body = vcat $ map printDef defs

    in Idris $ headers <> hardline <> body <> hardline <>
               "main : IO()" <> hardline <>
               "main = putStrLn " <> dquote <> dquote

render :: Module -> T.Text
render = renderStrict . layoutPretty defaultLayoutOptions . get . printModule

runIdris :: Module -> IO()
runIdris m = T.writeFile ("out/" ++ (T.unpack $ mname m) ++ ".idr") $ render m

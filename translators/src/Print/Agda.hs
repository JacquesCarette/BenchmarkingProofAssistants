module Print.Agda
  ( printModule
  , render
  , runAgda
  ) where

import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)
import qualified Prettyprinter as P
import Prettyprinter.Render.Text (renderStrict)

import Grammar
import Print.Generic

newtype Agda ann = Agda {get :: P.Doc ann}

instance HasDoc Agda where
  doc = get
  build = Agda

-- needs to be declared here because generic instance overlaps too much
instance Semigroup (Agda ann) where
  a <> b = Agda $ (get a) <> (get b)

instance Monoid (Agda ann) where
  mempty = emptyDoc

instance IsString (Agda ann) where
  fromString s = Agda $ P.pretty s

instance Keywords (Agda ann) where
  import_ = "open" <+> "import"
  assign  = Agda "="
  recrd   = Agda "record"
  univ    = Agda "Set"
  data_   = Agda "data"
  arr     = Agda "->"
  lcons   = Agda "\x2237"
  vcons   = Agda "\x2237"
  typesep = Agda ":"
  natT    = Agda "Nat"
  strT    = Agda "String"
  vectT   = Agda "Vec"

instance TypeAnn (Agda ann) where
  typeAnn trm typ = trm <+> typesep <+> typ
  teleCell Explicit trm typ = parens $ trm <+> typesep <+> typ
  teleCell Implicit trm typ = braces $ trm <+> typesep <+> typ

printImport :: Import -> Agda ann
printImport (ImportLib NatMod) = import_ <+> "Agda.Builtin.Nat"
printImport (ImportLib VecMod) = import_ <+> "Data.Vec.Base"
printImport (ImportLib ListMod) = import_ <+> "Agda.Builtin.List"
printImport (ImportLib StringMod) = import_ <+> "Agda.Builtin.String"

printArgL :: Arg [ Name ] Tm -> Agda ann
printArgL (Arg [] t _) = printTm t
printArgL (Arg l@(_:_) t v) = teleCell v (hsep $ map pretty l)  (printTm t)

-- Print Terms
printTm :: Tm -> Agda ann
printTm (Univ) = univ
printTm (Pi lt t) = foldr (\a d -> printArgL a <+> arr <+> d) (printTm t) lt
printTm (Arr t1 t2) = printArgL t1 <+> arr <+> printTm t2
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
printTm (App fun args) = printTm fun <+> softline' <> (sep $ map printTm args)
printTm (Unary o t) = parens $ printOp1 o <+> printTm t
printTm (Lit l) = printLit l
printTm (KCon NatT _) = natT
printTm (KCon StringT _) = strT
printTm (KCon VecT l) = vectT <+> hsep (map printTm l)

printOp1 :: Op1 -> Agda ann
printOp1 Suc = "suc"

printOp2 :: Op2 -> Agda ann
printOp2 Plus = "+"

printFieldT :: FieldT -> Agda ann
printFieldT (FieldT fname ftype) = typeAnn (pretty fname) (printTm ftype)

printFieldDecl :: FieldDecl -> Agda ann
printFieldDecl (FieldDecl fields) = vsep $ map printFieldT fields

printConstr :: Constr -> Agda ann
printConstr (Constr nm ty) = typeAnn (pretty nm) (printTm ty)

printDataConst :: DataCons -> Agda aa
printDataConst (DataCons l) = vsep $ map printConstr l

printCase :: Name -> Pat -> Agda ann
printCase var (Pat a e) = pretty var <+> (hsep $ map (pretty . arg) a) <+> assign <+> printTm e

printMatch :: Name -> Patterns -> Agda ann
printMatch nm (Patterns p) = vsep (map (printCase nm) p)

printLit :: Literal -> Agda ann
printLit (Nat n) = pretty n
printLit (Bool b) = pretty b
printLit (String str) = dquotes $ pretty str
printLit (Vec l) = parens $ encloseSep emptyDoc (space <> vcons <+> lbracket <> rbracket)
  (space <> vcons <> space) (map printTm l)
printLit (List l) = parens $ encloseSep emptyDoc (space <> lcons <+> lbracket <> rbracket)
  (space <> lcons <> space) (map printTm l)

printLocalDefn :: LocalDefn -> Agda ann
printLocalDefn (LocDefFun var ty args expr) =
   typeSig <> tvar <+> assign <+> align (printTm expr)
    where
        typeSig = case ty of
            Just t -> typeAnn (pretty var) ((hsep $ punctuate (space <> arr) $ map (printTm . argty) args) <+> arr <+> printTm t) <> line
            Nothing -> mempty
        tvar = case args of
            [] -> pretty var
            (_:_) -> pretty var <+> (hsep $ map (pretty . arg) args)

-- Function to print variable definitions
printDef :: Definition -> Agda ann
printDef (DefTVar var t expr) =
  typeAnn (pretty var) (printTm t) <> hardline <>
  pretty var <+> assign <+> align (printTm expr) <> hardline

printDef (DefPatt var ty _ cons) = typeAnn (pretty var) (printTm ty) <> line <> printMatch var cons

-- Function to print datatype definitions
printDef (DefPDataType name params cons ty) =
  data_ <+> typeAnn (pParams params) (printTm ty) <+> "where" <> hardline <>
    indent 1 (printDataConst cons) <> hardline
    where
      pParams [] = pretty name
      pParams _  = pretty name <+> hsep (map (\(Arg x y v) -> teleCell v (pretty x) (printTm y)) params)

-- Function for records
printDef (DefRecType name params consName fields _) =
    recrd <+> typeAnn pp_params univ <+> "where" <> line <>
    indent 4 (vsep $ "constructor" <+> pretty consName : "field" : (indent 4 $ printFieldDecl fields) : []) <>
    hardline
    where
      ll = map (\(Arg n t v) -> teleCell v (pretty n) (printTm t)) params
      pp_params = if null params then pretty name else pretty name <+> hsep ll

printDef (DefRec name recTm consName (FieldDef fields)) =
    typeAnn (pretty name) (printTm recTm) <> hardline <>
    pretty name <+> assign <+> pretty consName <+> nest 4 (sep (map (printTm . fval) fields))

printDef (OpenName _) = mempty
printDef (Separator '\n' n _) = vcat $ replicate (fromIntegral n) emptyDoc
printDef (Separator c n b) =
  let s = hcat $ replicate (fromIntegral n) (pretty c) in
  if b then hardline <> s <> hardline else s


-- Print the Agda module
printModule :: Module -> Agda ann
printModule (Module name imports defs) =
    let
        headers = "module" <+> pretty name <+> "where" <> hardline <>
          vsep (map printImport imports)
        -- Concatenate all definitions
        body = vcat $ map printDef defs

    in headers <> hardline <> hardline <> body

render :: Module -> T.Text
render = renderStrict . P.layoutPretty P.defaultLayoutOptions . get . printModule

runAgda :: Module -> IO()
runAgda m = T.writeFile ("out/" ++ (T.unpack $ mname m) ++ ".agda") $ render m

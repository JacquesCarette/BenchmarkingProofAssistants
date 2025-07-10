module Print.Lean
  ( printModule
  , runLean
  , render
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Grammar
import Print.Generic

newtype Lean ann = Lean {get :: Doc ann}

class Keywords rep where
  import_ :: rep
  assign  :: rep
  arr     :: rep
  univ    :: rep
{-
  data_   :: rep
  rec     :: rep
-}
instance Keywords (Doc ann) where
  import_ = "import"
  assign  = ":="
  arr     = "->"
  univ    = "Type"
{-
  data_   = "data"
  rec     = "record"
-}

class TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: Visibility -> rep -> rep -> rep

instance TypeAnn (Doc ann) where
  typeAnn trm typ = trm <+> ":" <+> typ
  teleCell Explicit trm typ = parens $ trm <+> ":" <+> typ
  teleCell Implicit trm typ = braces $ trm <+> ":" <+> typ

-- append an Import if needed
printWithImport :: Import -> Doc ann -> Doc ann
printWithImport (ImportLib VecMod) m = m <> import_ <+> "Init.Data.Vector" <> hardline
-- There rest are builtin
printWithImport (ImportLib NatMod) m = m
printWithImport (ImportLib StringMod) m = m
printWithImport (ImportLib ListMod) m = m

-- Print types
printTm :: Tm -> Doc ann
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
printTm (Let [] expr) = printTm expr
printTm (Let (d:[]) expr) = "let" <+> printLocalDefn d <> hardline <> printTm expr
printTm (Let (d:ds) expr) =
  -- "let" <+> intercalate "\nlet " (map printLocalDefn (d:ds)) ++ "\n" ++ printTm expr
  vcat (map (\x -> "let" <+> printLocalDefn x) (d:ds)) <> line <>
  printTm expr
printTm (If cond thn els) = "if" <+> printTm cond <+> "then" <> hardline <>
  indent 4 (printTm thn) <> hardline <>
  "else" <+> printTm els
printTm (Where expr ds) = printTm expr <> hardline <>
  indent 4 ("where" <> hardline <> vsep (map printLocalDefn ds))
printTm (App fun args) = printTm fun <+> fillSep (map (group . printTm) args)
printTm (Unary o t) = parens $ printOp1 o <+> printTm t
printTm (Lit l) = printLit l
printTm (KCon NatT _) = "Nat"
printTm (KCon StringT _) = "String"
printTm (KCon VecT l) = "Vector" <+> hsep (map printTm l)

printReturnType :: Tm -> Doc ann
printReturnType (PCon t []) = pretty t
printReturnType (Arr _ t) = printReturnType t
printReturnType t@(KCon _ _) = printTm t
printReturnType _ = error "should not occur as a return type"

printArg :: Pretty a => Arg a Tm -> Doc ann
printArg a = parens $ typeAnn (pretty $ arg a) (printTm $ argty a)

printArgL :: Arg [ Name ] Tm -> Doc ann
printArgL (Arg [] t _) = printTm t
printArgL (Arg l@(_:_) t v) = teleCell v (hsep $ map pretty l) (printTm t) 

printLit :: Literal -> Doc ann
printLit (Nat n) = pretty n
printLit (Bool b) = pretty b
printLit (String str) = dquotes $ pretty str
printLit (Vec l) = "#" <> brackets (hsep $ punctuate comma (map printTm l))
printLit (List l) = brackets $ hsep $ punctuate comma (map printTm l)

printOp1 :: Op1 -> Doc ann
printOp1 Suc = "Nat.succ"  -- use `Nat.succ` explicitly

printOp2 :: Op2 -> Doc ann
printOp2 Plus = "+"

printFieldT :: FieldT -> Doc ann
printFieldT (FieldT fname ftype) = typeAnn (pretty fname) (printTm ftype)

printFieldDecl :: FieldDecl -> Doc ann
printFieldDecl (FieldDecl fields) = vsep $ map printFieldT fields

printConstr :: Constr -> Doc ann
printConstr (Constr nm ty) = pipe <+> typeAnn (pretty nm) (printTm ty)

printDataConst :: DataCons -> Doc aa
printDataConst (DataCons l) = vsep $ map printConstr l

printCase :: Pat -> Doc ann
printCase (Pat a e) = pipe <+> (hsep $ map (pretty . arg) a) <+> "=>" <+> printTm e

printMatch :: Patterns -> Doc ann
printMatch (Patterns p) = vsep (map printCase p)

printLocalDefn :: LocalDefn -> Doc ann
printLocalDefn (LocDefFun var Nothing args expr) =
  prettyArgs var printArg args <+> assign <+> printTm expr
printLocalDefn (LocDefFun var (Just t) args expr) =
  typeAnn (prettyArgs var printArg args) (printReturnType t) <+> assign <+> printTm expr

printDef :: [Definition] -> Definition -> Doc ann
printDef _ (DefTVar var t expr) = "def" <+> typeAnn (pretty var) (printTm t) <+>
  assign <+> printTm expr
printDef _ (DefPatt var ty _ cons) = "def" <+> typeAnn (pretty var) (printTm ty) <> hardline <> printMatch cons
printDef _ (DefPDataType name params constr t) =
  "inductive" <+>
      typeAnn (pParams params) (printTm t) <+> "where" <> hardline <> printDataConst constr
  where
    pParams [] = pretty name
    pParams _  = pretty name <+> hsep (map (\(Arg x y v) -> teleCell v (pretty x) (printTm y)) params)

-- records Def
printDef _ (DefRecType name params consName fields _) =
    "structure" <+> prettyParams <+> "where" <> hardline <>
    indent 4 (pretty consName <+> "::" <> hardline <> printFieldDecl fields) <> hardline
      where
        prettyParams = case params of
            [] -> pretty name
            _ -> pretty name <+> hsep (map (\(Arg n t v) -> teleCell v (pretty n) (printTm t)) params)

-- OpenLine: It takes a list of record definitions (recs) and uses it to build an open line.
-- Exclusive lean syntax needed for simplicity
printDef recs (DefRec name recType consName (FieldDef fields)) =
    openLine <>
    typeAnn (pretty name) (printTm recType) <+> assign <+> pretty consName <+>
    hsep (map (printTm . fval) fields)
  where
    recNamesList = [ rName | DefRecType rName _ _ _ _ <- recs ]
    openLine = if null recNamesList then emptyDoc else "open" <+> hsep (map pretty recNamesList) <> hardline
printDef _ (OpenName n) = "open" <+> pretty n
printDef _ (Separator '\n' n _) = blanklines n
printDef _ (Separator c n b) =
  let s = hcat $ replicate (fromIntegral n) (pretty c) in
  if b then hardline <> s <> hardline else s


printModule :: Module -> Lean ann
printModule (Module _ imports defs) =
    let
        headers = foldr printWithImport emptyDoc imports
        ctx = [ d | d@(DefRecType _ _ _ _ _) <- defs ]  -- extract record definitions from the module
        body = vcat (map (printDef ctx) defs)
    in Lean $ if null imports then body else headers <> hardline <> body

render :: Module -> T.Text
render = renderStrict . layoutPretty defaultLayoutOptions . get . printModule

runLean :: Module -> IO()
runLean m = T.writeFile ("out/" ++ (T.unpack $ mname m) ++ ".lean") $ render m

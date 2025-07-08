module Print.Rocq
  ( printModule
  , render
  , runRocq
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)

import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Grammar
import Print.Generic (prettyArgs)

newtype Rocq ann = Rocq {get :: Doc ann}

-- to be migrated
class Keywords rep where
  import_ :: rep
  assign  :: rep
  univ    :: rep
  rec     :: rep
  arr     :: rep

instance Keywords (Doc ann) where
  import_ = "Require" <+> "Import"
  assign  = ":="
  rec     = "Record"
  univ    = "Type"
  arr     = "->"


class TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: rep -> rep -> rep

instance TypeAnn (Doc ann) where
  typeAnn trm typ = trm <+> ":" <+> typ
  teleCell trm typ = parens $ trm <+> ":" <+> typ


-- FIXME: end '.' should not be hard-coded
printImport :: Import -> Doc ann
printImport (ImportLib VecMod) = import_ <+> "Coq.Vectors.Vector." <> hardline <>
  "Import VectorNotations." -- FIXME
printImport (ImportLib StringMod) = import_ <+> "Coq.Strings.String."
-- the rest are builtin
printImport (ImportLib NatMod) = emptyDoc
printImport (ImportLib ListMod) = emptyDoc

printTm :: Tm -> Doc ann
printTm (Univ) = univ
printTm (Pi lt t) = foldr (\a d -> printArgL a <+> arr <+> d) (printTm t) lt
printTm (Arr t1 t2) = printTm t1 <+> arr <+> printTm t2
printTm (PCon t []) = pretty $ if  "Cap_" `T.isPrefixOf` t || "Record" `T.isPrefixOf` t
                             then t else (T.toLower t) -- if starts with keyword Cap_ maintain, else lower case
printTm (PCon "Vec" args) = "Vect" <+> hsep (map printTm args)
printTm (PCon name types) = pretty (T.toLower name) <+> hsep (map printTm types)
printTm (DCon name types) = pretty name <+> hsep (map printTm types)
printTm (Index names ty) = "forall" <+> braces (typeAnn (pretty $ T.toLower (T.unwords names)) (printTm ty))
printTm (Var var) = pretty var
printTm (Paren e) = parens $ printTm e
printTm (Binary op e1 e2) = printTm e1 <+> printOp2 op <+> printTm e2
printTm (Let ds expr) =
  "let" <+> align (vcat (map printLocalDefn ds) <+> "in") <> line <>
  printTm expr
printTm (If cond thn els) = "if" <+> printTm cond <+> "then" <+> printTm thn <+> "else" <+> printTm els
printTm (Where expr ds) =
  printTm expr <> hardline <>
  indent 4 ("where " <+> vcat (map printLocalDefn ds))
printTm (App fun args) = printTm fun <+> (hsep $ map printTm args)
printTm (Unary o e) = parens $ printOp1 o <+> printTm e
printTm (Lit l) = printLit l

printReturnType :: Tm -> Doc ann
printReturnType (PCon t []) = pretty $ T.toLower t --required for nested functions
printReturnType (Arr _ t) = printReturnType t
printReturnType _ = error "should not occur as a return type"

printArg :: Pretty a => Arg a Tm -> Doc ann
printArg a = parens $ typeAnn (pretty $ arg a) (printTm $ argty a)

printArgL :: Arg [ Name ] Tm -> Doc ann
printArgL (Arg [] t) = printTm t
printArgL (Arg (x:xs) t) = teleCell (foldr (\ nm d -> pretty nm <+> d) (pretty x) xs)  (printTm t) 

-- this is partial on purpose
printTele :: Tm -> Doc ann
printTele (Pi lt t) = foldr (\a d -> printArgL a <+> d) (":" <+> printTm t) lt
printTele _ = error "expecting a Pi type, got something else"

printLit :: Literal -> Doc ann
printLit (Nat n) = pretty n
printLit (Bool b) = pretty b
printLit (String str) = dquotes $ pretty str
printLit (Vec l) = encloseSep lbracket rbracket (semi <> space) (map printTm l)
printLit (List l) = encloseSep lbracket rbracket (comma <> space) (map printTm l)

printOp1 :: Op1 -> Doc ann
printOp1 Suc = "S"

printOp2 :: Op2 -> Doc ann
printOp2 Plus = "+"

printFieldT :: FieldT -> Doc ann
printFieldT (FieldT fname ftype) = typeAnn (pretty fname) (printTm ftype) <> semi

printFieldDecl :: FieldDecl -> Doc ann
printFieldDecl (FieldDecl fields) = vsep $ map printFieldT fields

printLocalDefn :: LocalDefn -> Doc ann
printLocalDefn (LocDefFun var Nothing args expr) =
  prettyArgs var printArg args <+> assign <+> printTm expr
printLocalDefn (LocDefFun var (Just t) args expr) = typeAnn targs (printReturnType t) <+> assign <+>
  printTm expr
  where targs = prettyArgs var printArg args

printDef :: Definition -> Doc ann
printDef (DefTVar var t expr) =
  nest 4 ("Definition" <+> typeAnn (pretty var) (printTm t) <+> assign <> softline <>
  (printTm expr <> dot <> hardline))
printDef (DefPatt var ty m cons) = "Fixpoint" <+> pretty var <+> printTele ty
          <+> assign <> hardline <>
  "match" <+> pretty m <+> "with" <> hardline <>
  vsep (map (\(a, e) -> pipe <+> (hsep $ map (pretty . T.toLower . arg) a) <+> "=>" <+> printTm e) cons)
  <> softline' <+> "end" <> dot <> hardline
printDef (DefPDataType name params args ty) =
  "Inductive" <+> printParams params <+> assign <> hardline <>
  vsep (map (\(x, y) -> pipe <+> typeAnn (pretty $ T.toLower x) (printIndices y)) args) <> dot
  where
    printIndices :: Tm -> Doc ann
    printIndices (Arr (Index n t) ctype) = (printTm (Index n t)) <> comma <+> (printTm ctype)
    printIndices t = printTm t

    printParams [] = typeAnn (pretty $ T.toLower name) (printTm ty)
    printParams (_:_) =  (pretty $ T.toLower name) <+>
      typeAnn (hsep (map (\(x, y) -> teleCell (pretty $ T.toLower x) (printTm y)) params)) (printTm ty)

--Function for Records
printDef (DefRecType name params consName fields _) = 
    rec <+> typeAnn recName univ <+> assign <+> pretty consName <+>
    lbrace <> hardline <> indent 2 (printFieldDecl fields) <> hardline <> rbrace <> dot <> hardline
    where
        recName = case params of
            [] -> pretty name
            _ -> pretty name <+> hsep (map (\(Arg n t) -> teleCell (pretty n) (printTm t)) params)

printDef (DefRec name recType consName fields) =
  "Definition" <+> typeAnn (pretty name) (printTm recType) <+> assign <> hardline <>
  indent 2 constructorCall <> dot <> hardline
  where
    hasParams = case recType of
      (DCon _ tys) -> Just tys
      (PCon _ _)   -> Nothing
      _            -> error "invalid type for a record"

    printFieldDef (a , b) = pretty a <+> assign <+> printTm b <> semi

    constructorCall = case hasParams of
      Nothing -> pretty consName <+> hsep (map (printTm . snd) fields)
      Just _  -> pretty consName <+> braces (pipe <+> (hsep $ map printFieldDef fields) <+> pipe)

printDef (OpenName _) = emptyDoc
printDef (Separator c n b) =
  let s = pretty $ replicate (fromIntegral n) c in
  if b then hardline <> s <> hardline else s


printModule :: Module -> Rocq ann
printModule (Module name imports defs) =
    let headers =
          (if null imports then emptyDoc
                      else vsep (map printImport imports) <> hardline <> hardline) <>
            "Module" <+> pretty name <> dot <> hardline
        body = vcat (map printDef defs)
    in Rocq $ headers <> hardline <> body <> hardline <> "End" <+> pretty name <> dot

render :: Module -> T.Text
render = renderStrict . layoutPretty defaultLayoutOptions . get . printModule

runRocq :: Module -> IO()
runRocq m = T.writeFile (T.unpack $ "out/" `T.append` modname m `T.append` ".v") $ render m

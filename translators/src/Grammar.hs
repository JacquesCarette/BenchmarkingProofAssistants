module Grammar (Module (..), Import (..), Definition (..), Tm (..), Arg (..)
  , KnownMods (..), Op1 (..), Op2 (..), LocalDefn (..), Literal (..)
  , FieldDecl (..), FieldT (..), FieldV (..), FieldDef (..), KnownT (..)
  , DataCons (..), Constr (..), Parameters, Patterns (..), Pat (..)
  , Visibility (..)
  , Name
  , nat, con, num, bool, list, vec, vecT, string, stringT, suc, plus, app1, appnm
  , decfields, fieldty, fv, rec, datacons, dcons, match, case_
  , earg, mearg, aarg, iarg, miarg) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Numeric.Natural (Natural)

-- grammar

data Module = Module 
  { mname :: Name
  , mimports :: [Import]
  , mdefs :: [Definition]
  }

data KnownMods = NatMod | ListMod | VecMod | StringMod

newtype Import = ImportLib KnownMods

data Definition
  = DefPatt Name Tm Name Patterns
    -- ^ Function definition by pattern-matching.
    -- Function name; signature; (Rocq only: Name for Match); constructors
  | DefTVar Name Tm Tm
    -- ^ Define a (top-level) variable with a type annotation, and a definiens
  | DefPDataType Name Parameters DataCons Tm
    -- ^ Datatype name, parameters, constructors, overall type
  | DefRecType Name Parameters Name FieldDecl Tm
    -- ^ [Arg] for parameters (empty list if no params), Name is the type constructor
  | DefRec Name Tm Name FieldDef
    -- ^ Record name, record type, constructor, field definitions
  | OpenName Name
    -- ^ Just for Lean, to refer to user-defined datatypes directly
  | Separator Char Natural Bool
    -- ^ To allow a "separator line" in the produced code, of that character repeated n times. 
    -- It is on a line of its own if True, spit out as-is and in-place if false

data LocalDefn
  = LocDefFun Name (Maybe Tm) [Arg Name Tm] Tm

data Tm
  = PCon Name [Tm]        -- (parameterized) type constructor
  | DCon Name [Tm]        -- dependent type constructor (note that a dependent type is also parameterized)
  | KCon KnownT [Tm]      -- built-in (like Vec)
  | Arr (Arg [Name] Tm) Tm           -- (non-dependent) function type
  | Pi (NonEmpty (Arg [Name] Tm)) Tm -- Dependent function type
  | Univ                  -- a Universe, aka "Type" itself, called "Set" in Agda
  | Var Name
  | Binary Op2 Tm Tm      -- only for known, hard-coded binary operations
  | Unary Op1 Tm          -- only for known, hard-coded unary operations
  | Let [LocalDefn] Tm
  | If Tm Tm Tm
  | Where Tm [LocalDefn]
  | App Tm [Tm]
  | Paren Tm
  | Lit Literal
  -- | Record (Maybe Name) [FieldV]       -- a record value
  -- | Lam                  -- we don't as-yet use it?

data KnownT = NatT | VecT | StringT

data Visibility = Explicit | Implicit
data Arg a b = Arg { arg :: a, argty :: b , vis :: Visibility}

-- Separate FieldT and FieldV for printing purposes
-- A single Field type
data FieldT = FieldT { fname :: Name, fty :: Tm }
-- A single Field value
data FieldV = FieldV { flabel :: Name, fval :: Tm }

newtype FieldDecl = FieldDecl [FieldT]
newtype FieldDef  = FieldDef  [FieldV]       -- a record value

newtype DataCons = DataCons [Constr]
data Constr = Constr {cname :: Name, cty :: Tm}

newtype Patterns = Patterns [Pat]
data Pat = Pat {args :: [Arg Name Tm], argsty :: Tm}

data Literal
  = Nat Natural
  -- ^ Natural number literals.
  -- We will attempt to translate these as literals like @100@
  -- instead of @succ@ and @zero@ constructors.
  | Bool Bool
  -- ^ Boolean literals.
  | List [Tm]
  -- ^ List literals.
  -- We will attempt to translate these as literals like @[x, y, z]@
  -- as opposed to cons constructors.
  | Vec [Tm]
  -- ^ Vector literals.
  -- We will attempt to translate these as literals like @[x, y, z]@
  -- as opposed to @cons@ and @nil@ constructors.
  | String String
  -- ^ String literals.

data Op2 = Plus
data Op1 = Suc

-- aliases for readability purposes
type Name = Text
type Parameters = [Arg Name Tm]


--------------------------
-- useful short-hands for things that are used often

nat :: Tm
nat = KCon NatT []

con :: Name -> Tm
con n = PCon n []

num :: Natural -> Tm
num = Lit . Nat

bool :: Bool -> Tm
bool = Lit . Bool

list :: [ Tm ] -> Tm
list = Lit . List

vec :: [ Tm ] -> Tm
vec = Lit . Vec

vecT :: Tm -> Tm -> Tm
vecT t n = KCon VecT [t, n]

string :: String -> Tm
string = Lit . String

stringT :: Tm
stringT = KCon StringT []

suc :: Tm -> Tm
suc = Unary Suc

plus :: Tm -> Tm -> Tm
plus = Binary Plus

app1 :: Name -> Tm -> Tm
app1 a b = App (Var a) [b]

appnm :: Name -> [Tm] -> Tm
appnm a b = App (Var a) b

fieldty :: Name -> Tm -> FieldT
fieldty = FieldT

fv :: Name -> Tm -> FieldV
fv = FieldV

decfields :: [FieldT] -> FieldDecl
decfields = FieldDecl

rec :: [FieldV] -> FieldDef
rec = FieldDef

datacons :: [Constr] -> DataCons
datacons = DataCons

dcons :: Name -> Tm -> Constr
dcons = Constr

match :: [Pat] -> Patterns
match = Patterns

case_ :: [Arg Name Tm] -> Tm -> Pat
case_ = Pat

-- single-name explicit Arg
earg :: Name -> Tm -> Arg Name Tm
earg n t = Arg n t Explicit

-- single-name implicit Arg
iarg :: Name -> Tm -> Arg Name Tm
iarg n t = Arg n t Implicit

-- multi-name explicit Arg
mearg :: [Name] -> Tm -> Arg [Name] Tm
mearg n t = Arg n t Explicit

-- multi-name implicit Arg
miarg :: [Name] -> Tm -> Arg [Name] Tm
miarg n t = Arg n t Implicit

-- anonymous explicit Arg
aarg :: Tm -> Arg [Name] Tm
aarg t = Arg [] t Explicit

{-# Language OverloadedStrings #-}
module Tests (tests) where

import Data.IntMap.Strict as Map ( fromList, IntMap )
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Numeric.Natural (Natural)
import GHC.Natural (minusNatural)
import Text.Show () -- just the instances

import Grammar

-- re-usable generators for the tests below

-- the 'n == 0' case is often trivial
trivial :: Natural -> [a] -> [a]
trivial n c = if n == 0 then [] else c

-- there is likely a nicer way to say this?
-- (The code is 1-based, keep that for now)
iter :: Natural -> (Natural -> a) -> [a]
iter n f = map f [1..n]

-- various name and variable generators
nm :: Char -> Natural -> T.Text
nm c n = T.cons c $ T.show n

nms :: Char -> Natural -> [T.Text]
nms c m = iter m (nm c)

v :: Char -> Natural -> Tm
v c n = Var $ nm c n

vx :: Natural -> Tm
vx = v 'x'

vxs :: Natural -> Tm
vxs n | n == 0    = num 1
      | otherwise = vx n

mkName :: Show a => T.Text -> a -> T.Text
mkName t s = T.append t $ T.show s

sum2vars :: Natural -> Tm
sum2vars n | n == 0    = num 1
           | otherwise = plus (vx n) (vx n)

nary :: Tm -> Natural -> Tm
nary t n = foldr Arr t (replicate (fromIntegral n) (aarg t))

-- this is our list of expandable tests. each test should take a Natural as an
-- argument and return a program written in the internal grammar (see
-- Grammar.hs)
_tests :: [Natural -> Module] -- todo: add commented Haskell representation for each test
_tests =
    [ \n -> let --1
            lets p = foldr (\a b -> Let [LocDefFun (nm 'x' a) Nothing [] $ vxs (a-1)] b) (vxs p) [1..p]
        in Module "LetExample" [ImportLib NatMod] $ trivial n [DefTVar "n" nat $ lets n]

    , \n -> let --2
        lets p = foldr (\a b -> Let [LocDefFun (nm 'x' a) Nothing [] $ sum2vars $ a-1] b) (vx p) [1..p]
    in Module "LetAddExample" [ImportLib NatMod] $ trivial n [DefTVar "n" nat $ lets n]

    , -- 3 Description: Generate Nested Functions
    \n -> let --3
            decl = [ DefTVar "n" nat (Let (genFunc n) (genCall n)) ]
            -- Generate function definitions dynamically based on (1 to n)
            genFunc :: Natural -> [LocalDefn]
            genFunc p = foldr (\a b -> LocDefFun (nm 'f' a)
                                       (Just nat)
                                       (iter a (\i -> earg (nm 'x' i) nat))
                                       (foldl (\acc i -> plus acc (vx i)) (num 1) [1..a]) : b)
                              []
                              [1..p]

            -- Generate function call expressions
            genCall :: Natural -> Tm
            genCall p = foldr (\a b -> plus (appnm (nm 'f' a) (map (num . (+ 1)) [1..a])) b)
                              (app1 "f1" (num 2)) $ reverse [2..p]

        in Module "NestedFunction" [ImportLib NatMod] $ trivial n decl
    , \n -> let --4 A specified number of simple datatype declarations.
      genData m = foldl (\b a -> DefPDataType (nm 'X' a) [] 
                                 (datacons [dcons (nm 'Y' a) (con (nm 'X' a))]) Univ : b) [] [1..m]
        in Module "DataSimpleDeclarations" [ImportLib NatMod]  $ genData n
    , \n ->     --5 Variable declaration with an identifier of a specified length.
           Module "LongIdentifier" [ImportLib NatMod]  $ trivial n [DefTVar (T.pack $ replicate (fromIntegral n) 'x') nat $ num 0]

    -- 6 Description: A record declaration with N dependent fields
    ,\n -> let --6
        -- Generate field definitions dynamically
        genFields :: Natural -> FieldDecl
        genFields p = decfields $ fieldty "f1"  nat :
                      (foldr (\a b -> fieldty (nm 'f' a) (vecT nat (genSize (a-1))) : b) [] $ [2..p])

        -- Helper function to correctly reference `suc` or `S`
        genSize p = foldr (\_ b -> suc b) (Var "f1") [2..p]

        -- Generate example initialization dynamically
        genExample :: Natural -> FieldDef
        genExample k = rec $ map (\i -> fv (nm 'f' i) (vec $ replicate (fromIntegral i) (num 1))) [1..k]

        -- Define the record structure
        xDef = DefRecType "Cap_X" [] "Const" (genFields n) Univ

        -- Define the example initialization
        exampleInit = DefRec "example" (con "Cap_X") "Const" (genExample n)

        decl = [xDef, exampleInit]
        in Module "Fields_DependentRecordModule" [ImportLib NatMod, ImportLib VecMod] $ trivial n decl

    , --7 Description: Generate a very long chain (N) of dependent record definitions
    \n -> let
        -- Generate Record Definitions
        genRecords :: Natural -> [Definition]
        genRecords p = foldr (\ (i,c) b -> DefRecType (mkName "Dummy" i) [] (mkName "Const" i)
          (decfields [fieldty (nm 'f' i) c]) Univ : b)
          [] $ ((1, nat) : map (\i -> (i, con (mkName "Dummy" (i-1)))) [2..p])

        -- Generate Example Init
        genExample :: Natural -> Tm
        genExample p = foldr (\a b -> Paren $ (app1 (mkName "Const" a) b)) (num 10) $ reverse [1..p]

        exampleInit = DefRec "example" (con $ mkName "Dummy" n) (mkName "Const" n) $
          rec [fv "example" (genExample $ minusNatural n 1)] -- HACK
        decl = (genRecords n ++ [exampleInit])

        in Module "ChainDef_DependentRecordModule" [ImportLib NatMod] $ trivial n decl

    , -- 8 Description: Generate record with N parameters
    \n -> let
        decl = [recDef, exampleInit]
        -- Helper to build the sum exp 1 + 2 + ... + n
        buildSum m = foldr (\a b -> plus b (num a)) (num 1) $ reverse [2..m]

        -- Create param as a list of args: f1 : Nat, f2 : Nat, …, fn : Nat
        params = iter n (\i -> earg (nm 'f' i) nat)

        -- Define the record X with param, a constructor "Const",
        -- two fields "sums" and "values", and overall type Set.
        recDef = DefRecType "X" params "Const" (decfields [fieldty "sums" nat]) (con "Set")

        -- Build the record type application as a string: "X 1 2 ... n"
        recTypeInstance = DCon "X" $ iter n num

        -- Define the record instance "example" with computed field values:
        exampleInit = DefRec "example" recTypeInstance "Const" $ rec [fv "sums" (Paren $ buildSum n)]
      in Module "Parameters_DependentRecordModule" [ImportLib NatMod] $ trivial n decl
    , \n -> -- 9
        -- Generate a file with n newlines where n = user input
        Module "NewlineFile" [] $ [Separator '\n' n False]

    , \n -> let -- 10 Description: A record declaration with N independent fields
        -- Generate field definitions dynamically
        genFields p = decfields $ foldr (\a b -> fieldty (nm 'f' a) nat : b) [] [1..p]
        -- Define the record structure
        xDef = DefRecType "Cap_X" [] "Const" (genFields n) Univ
        -- Generate example initialization dynamically
        genExample p = rec $ foldr (\a b -> fv (nm 'f' a) (num 1) : b) [] [1..p]
        -- Define the example initialization
        exampleInit = DefRec "example" (con "Cap_X") "Const" (genExample n)
    in Module "Fields_NonDependentRecordModule" [ImportLib NatMod] $ trivial n [xDef,exampleInit]

    , \n -> let -- 11 Description: Generate a very long chain (N) of independent record definitions
        exampleInit = DefRec "example" (con $ mkName "Dummy" n) (mkName "Const" n) 
          $ rec [fv "f1" (num 1)]
        -- Generate Record Definitions
        genRecords :: Natural -> [Definition]
        genRecords p = foldl (\b a -> DefRecType (mkName "Dummy" a) [] (mkName "Const" a) 
                             (decfields [fieldty (nm 'f' a) nat]) Univ : b)
                             [exampleInit] $ reverse [1..p]
    in Module "ChainDefFields_NonDependentRecordModule" [ImportLib NatMod] $ trivial n (genRecords n)

    , \n -> --12 Description: create a simple datatype with N constructors accepting no parameters
        Module "Constructors_Datatypes" [] $ trivial n 
            [DefPDataType "D" [] (datacons $ iter n (\ i -> (dcons (nm 'C' i) (con "D")))) Univ]

    , \n ->  --13 Description: creates a datatype with a single constructor accepting N parameters
        let
            decl = [DefPDataType "D" (iter n (\i -> earg (nm 'p' i) Univ)) 
                        (datacons [dcons "C" (PCon "D" (iter n (con . nm 'p')))]) Univ]
        in Module "Parameters_Datatypes" [] $ trivial n decl

    , --14 Description: defines N variables, and uses both the first and last one in a declaration, N>=2
     \n ->
    let
        -- result = x1 + xn
        resultDef = DefTVar "result" nat $ plus (Var "x1") (vx n)

        decl = foldl (\b a -> DefTVar (nm 'x' a) nat (num a) : b) [resultDef] $ reverse [1..n]
    in Module "FirstLast_VariableModule" [ImportLib NatMod] $ trivial n decl
    , -- 15 Description: defines lots of dependent variables (10 at each level of dependency) and then use the most nested ones in a declaration
    \n -> let
    varsPerLevel = 10  -- Number of variables per level

    -- Generate variable names format x$level$ L $index$
    varName :: Natural -> Natural -> T.Text
    varName level idx = nm 'x' level `T.append` nm 'L' idx

    -- Define expressions for each variable
    genTm :: Natural -> Natural -> Tm
    genTm idx level = if level == 0 then num idx else plus (Var $ varName level idx) (num idx)

    --  sum of all xN_1 .. xN_10 + 100
    resultDef = DefTVar "result" nat $ foldl (\acc idx -> plus acc (Var $ varName n idx)) (num 100) [1..varsPerLevel]

    -- Generate DefTVar for each level
    genLevelDefs :: Natural -> [Definition]
    genLevelDefs level =
        foldl (\b a -> iter varsPerLevel (\idx -> DefTVar (varName a idx) nat (genTm idx (a-1))) ++ b) [resultDef]
        $ reverse [1..level]

    in Module "DeepDependency_VariableModule" [ImportLib NatMod] $ trivial n (genLevelDefs n)

    , \n -> let -- 16 Description: Simple datatype declaration with a specified number of indices, defined implicitly.
        decl = [DefPDataType "D" [] 
                   (datacons [dcons "C1" (Arr (miarg (nms 'x' n) nat) (con ("D " `T.append` T.unwords (nms 'x' n))))])
                   (Arr (aarg $ nary nat (n-1)) Univ)]
       in Module "DataImplicitIndices" [ImportLib NatMod] $ trivial n decl

    , \n -> let -- 17 Description: A file consisting of a single long line (length specified by the user).
        decl = [DefTVar "A" stringT $ string $ replicate (fromIntegral n) 'x']
        in Module "SingleLongLine" [ImportLib StringMod]  $ trivial n decl

    , \n ->  --18 Description: A single datatype where 'n' represents the number of 'Type' parameters, all needed for 'n' constructors
        let
            decl =  [DefPDataType "D" (iter n (\i -> earg (nm 'p' i) Univ))
                                 (datacons $ iter n (\ i -> dcons (nm 'C' i) (PCon "D" (iter n (\j -> con (nm 'p' j) )))) ) Univ]
        in Module "ConstructorsParameters_Datatypes" [] $ trivial n decl

    , \n -> let -- 19  Description: A single datatype where 'n' represents the number of indices, all needed for 'n' constructors
        decl = [DefPDataType "D" []
           (datacons (iter n (\ i -> dcons (nm 'C' i) (Arr (miarg (nms 'x' i) nat)
                                          (PCon "D" $ iter n (\j -> if j <= i then con (nm 'X' j) else con "0"))
                                      ))))
           (Arr (aarg $ nary nat (n-1)) Univ)]
        in Module "IndicesConstructors_Datatypes" [ImportLib NatMod] $ trivial n decl
    , \n -> let -- 20  Description: A single datatype where 'n' represents the number of 'Type' parameters as well as the number of indices
        decl = [DefPDataType "D" (iter n (\i -> earg (nm 'p' i) Univ))
          (datacons [dcons "C" (Arr (miarg (nms 'X' n) nat) (PCon "D" ((iter n (con . nm 'p')) ++ 
                     iter n (con . nm 'X'))))])
          (Arr (aarg $ nary nat (n-1)) Univ)]
        in Module "IndicesParameters_Datatypes" [ImportLib NatMod] $ trivial n decl
    ,  \n -> --21 Description: A function pattern matching on 'n' constructors of a datatype
        let
        decl = [DefPDataType "D" [] (datacons $ iter n (\ i -> dcons (nm 'C' i) (con "D"))) Univ, --create datatype
          OpenName "D",
          DefPatt "F" (Pi (NE.singleton (mearg ["C"] (con "D"))) nat) "C" 
              (match $ iter n (\i -> (case_ [earg (nm 'C' i) (con "D")] (num i)))),
          DefTVar "N" nat (genCall n)]
        genCall p = foldr (\a b -> plus (app1 "F" (con (nm 'C' a))) b)
                                        (app1 "F" (con "C1"))
          (reverse [2..p])
    in
       Module "Pattern_Matching_Datatypes" [ImportLib NatMod] $ trivial n decl
     ]


-- this is the list of expandable tests formatted as an IntMap so each test can be accessed by index
-- to access the expandable test at index i: tests ! i
tests :: IntMap (Natural -> Module)
tests = Map.fromList $ zip [1..(length _tests)] _tests

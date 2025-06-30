import Data.Text (Text)
import Data.Text qualified as T

import Numeric.Natural

import Panbench.Bwd (Bwd)
import Panbench.Bwd qualified as Bwd
import Panbench

-- [TODO: Reed M, 28/06/2025] Experimenting with a DSL a bit.
name :: Text -> Natural -> Text
name x n = x <> T.pack (show n)

var :: Text -> Natural -> Tm
var x n = Var (name x n)

names :: Text -> [Natural] -> [Text]
names x ns = name x <$> ns

vars :: Text -> [Natural] -> [Tm]
vars x ns = Var <$> names x ns

lets_ :: [(Text, Tm)] -> (Bwd Tm -> Tm) -> Tm
lets_ = Bwd.scoped (\(x, e) -> Let [LocDefFun x Nothing [] e]) (\(x, _) -> Var x)

main :: IO ()
main = panbenchMain "LetAddExample" \n ->
  Module "LetAddExample" [ImportLib NatMod] $
  [ DefTVar "n" (Just nat) $
    lets_ (zip (names "x" [1..n]) (num 1:[plus (var "x" i) (var "x" i) | i <- [1..n-1] ])) \es ->
      Bwd.last es
  ]

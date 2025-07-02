import Data.Text (Text)
import Data.Text qualified as T

import Numeric.Natural

import Panbench.Data.List.Bwd (Bwd)
import Panbench.Data.List.Bwd qualified as Bwd
import Panbench

-- [TODO: Reed M, 28/06/2025] Experimenting with a DSL a bit.
name :: Text -> Natural -> Text
name x n = x <> T.pack (show n)

names :: Text -> [Natural] -> [Text]
names x ns = name x <$> ns

vars :: Text -> [Natural] -> [Tm]
vars x ns = Var <$> names x ns

lets_ :: [(Text, Tm)] -> (Bwd Tm -> Tm) -> Tm
lets_ = Bwd.scoped (\(x, e) -> Let [LocDefFun x Nothing [] e]) (\(x, _) -> Var x)

main :: IO ()
main = panbenchMain "LetExample" [ImportLib NatMod] \n ->
  [ DefTVar "n" nat $
    lets_ (zip (names "x" [1..n]) (num 1 : vars "x" [1..n-1])) \es ->
      Bwd.last es
  ]

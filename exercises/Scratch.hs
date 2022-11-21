import Data.Maybe      
import Data.List

type Ingredient = String
type Amount     = Int
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage = if canBake then maxCakes else 0
    where canBake = all isJust $ fmap (\(i,_) -> lookup i storage) recipe
          maxCakes = minimum $ zipWith div (map snd relStor) (map snd $ sortOn fst recipe)
          relStor = sortOn fst $ filter (\(s,_) -> s `elem` map fst recipe) storage

r1 :: Recipe
r1 = [("flour",500), ("sugar",200), ("eggs",1)]
s1 :: Storage
s1 = [("flour",1200), ("sugar",1200), ("eggs",5), ("milk",200)]
r2 = [("apples",3), ("flour",300), ("sugar",150), ("milk",100), ("oil",100)]
s2 = [("sugar",500), ("flour",2000), ("milk",2000)]
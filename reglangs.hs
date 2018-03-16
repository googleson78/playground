import Data.List
import Control.Applicative
import Data.Maybe

type Var = Int
type Const = Char
type Lang = [String]

data Poly = Null | Empty | Mult Char Var | Sum Poly Poly 
    deriving (Eq, Show)

reg :: Poly -> [Lang] -> Lang
reg Null _ = []
reg Empty _ = [[]]
reg (Mult x y) ls = map (x:) (ls !! y)
reg (Sum x y) ls = reg x ls `union` reg y ls

data RegEx  = EmptySet 
            | EmptyString 
            | Sing Char 
            | Union RegEx RegEx
            | Concat RegEx RegEx
            | Kleene RegEx 
            deriving (Eq)

instance Show RegEx where
    show EmptySet       =   "Ø"
    show EmptyString    =   "ε"
    show (Sing a)       =   [a]
    show (Union r1 r2)  =   "(" ++ reg1 ++ "|" ++ reg2 ++ ")"
                                where reg1 = show r1
                                      reg2 = show r2
    show (Concat r1 r2) =   reg1 ++ reg2
                                where reg1 = show r1
                                      reg2 = show r2
    show (Kleene r)     =   "(" ++ reg ++ ")*"
                                where reg = show r

instance Monoid RegEx where
    mempty = EmptyString
    mappend a b = Concat a b

eps :: String
eps = []

-- 
(!!!) :: [a] -> Int -> Maybe a
(!!!) [] _     = Nothing
(!!!) (x:_) 0  = Just x
(!!!) (_:xs) n = xs !!! (pred n)

diags :: [(Int, Int)]
diags = concatMap genPairs [0..]
    where genPairs n = zip [n, n - 1..0] [0..n]

zip2Tuple :: (a -> b, c -> d) -> (a, c) -> (b, d)
zip2Tuple (f, g) (x, y) = (f x, g y)


-- forgive me lord, for I have sinned (?)
--
-- TODO: ADD EXPLANATION AND PROOF SKETCH
-- basically relies on diagonalization to create an iso between (N) and (N x N)
concat' :: Lang -> Lang -> Lang
concat' xs ys = ap'ed >>= maybeToList
    where indexed     = map (zip2Tuple ((xs!!!), (ys!!!))) diags
          exhausted   = takeWhile (/= (Nothing, Nothing)) indexed
          ap'ed       = map (uncurry (liftA2 (++))) exhausted

regLang :: RegEx -> Lang
regLang EmptySet       =    []
regLang EmptyString    =    [eps]
regLang (Sing a)       =    [[a]]

regLang (Union r1 r2)  =    lang1 `union` lang2
                                where lang1 = regLang r1
                                      lang2 = regLang r2

regLang (Concat r1 r2) =    lang1 `concat'` lang2
                                where lang1 = regLang r1
                                      lang2 = regLang r2

regLang (Kleene r)     =    concat $ iterate (lang `concat'`) [eps]
                                where lang = regLang r

recognizes :: String -> RegEx -> Bool
recognizes w = elem w . lengthLessThanW . regLang
    where wl              = length w
          lengthLessThanW = takeWhile ((<=wl) . length)


a = Sing 'a'
b = Sing 'b'
c = Sing 'c'
k = Sing 'k'
ab = Union a b
bc = Union b c
abbc = Concat ab bc
abbca = Union abbc a
abbcak = Concat abbca k
aORb = Union a b
aORc = Union a c
cKleene = Kleene c
bKleene = Kleene b
bKconcatcK = Concat bKleene cKleene
reg1 = Concat cKleene aORb
reg2 = Concat aORb aORc
reg3 = Concat reg1 reg2
reg4 = Union reg3 reg1
reg5 = (Kleene bKconcatcK)
reg6 = (Concat reg5 a)

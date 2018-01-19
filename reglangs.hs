import Data.List

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

concat' :: Lang -> Lang -> Lang
concat' l1 l2 = [a ++ b | a <- l1, b <- l2]

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

-- THIS DOES NOT WORK WELL
regLang (Kleene r)     =    concat $ iterate (lang `concat'`) [eps]
                                where lang = regLang r

-- needs fixing for stuff like b*c*
-- recognizes :: RegEx -> String -> Bool
-- recognizes rx w = elem w $ filter (((length w) ==) . length) (regLang rx)

a = Sing 'a'
b = Sing 'b'
c = Sing 'c'
aORb = Union a b
aORc = Union a c
cKleene = Kleene c
bKleene = Kleene b
bKconcatcK = Concat bKleene cKleene
reg1 = Concat cKleene aORb
reg2 = Concat aORb aORc
reg3 = Concat reg1 reg2
reg4 = Union reg3 reg1

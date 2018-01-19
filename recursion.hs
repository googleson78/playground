data PRFun = Zero | Succ | Proj Int | Comp PRFun [PRFun] | Rec PRFun PRFun deriving (Eq)

interp :: PRFun -> ([Int] -> Int)
interp (Zero) = const 0
interp (Succ) = succ . head
interp (Proj n) = (!! n)
interp (Comp f gs) = (\xs -> let interp_gs = map interp gs
                                 applied_gs = map ($ xs) interp_gs
                             in  interp f $ applied_gs)
interp h@(Rec f g) = (\(x:xs) -> if x == 0 
                                   then interp f $ xs 
                                   else interp g $ (x - 1):xs ++ [interp h $ (x - 1):xs])

const' :: PRFun
const' = (Proj 0)

plus :: PRFun
plus = (Rec f g)
       where f = const'
             g = (Comp (Succ) [(Proj 2)])

mult :: PRFun
mult = (Rec f g)
       where f = (Zero)
             g = (Comp plus [(Proj 1), (Proj 2)]) 

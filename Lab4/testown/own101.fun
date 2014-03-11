twice f x = f (f x) ;
fst x y = x ;

main = fst (twice (\x -> x)) 6 7 ; -- result 7


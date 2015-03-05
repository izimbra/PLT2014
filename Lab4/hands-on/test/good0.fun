-- Church numerals
-- n is represented as \ f -> \ x -> f^n x

zero = \ f -> \ x -> x ;
one  = \ f -> \ x -> f x ;
two  = \ f -> \ x -> f (f x) ;

three f x = f (f (f x)) ;

plus n m = \ f -> \ x -> n f (m f x) ;

times n m = \ f -> \ x -> n (m f) x ;

riddle n m = \ f -> \ x -> n m f x ;

add = \ x -> \ y -> x + y ;

main = times two (plus three two) (\ x -> 1 + x) 0 ;


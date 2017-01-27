import Html exposing (text)
import List exposing (append, head, foldr, tail, map2, repeat, drop, length, isEmpty)
import Maybe 

euclid: Int -> Int -> List (Int, Int)
euclid m k = 
  if k == 0 then [(m, 0)]
    else 
      [(m, k)] ++ (euclid k (m % k))

bjorkland: Int -> Int -> List Bool 
bjorkland ones zeros = 
  foldr append [] (bl_impl (repeat ones [True]) (repeat zeros [False]))


bl_impl: List (List Bool) -> List (List Bool) -> List (List Bool)
bl_impl l1 l2 = 
--  let _ = Debug.log "blah" [l1, l2] in
  if (isEmpty l2) then
      l1
    else
      let (paired,remainder) = mahmap2x List.append l1 l2 in
        bl_impl paired remainder

mahmap2x: (a -> a -> a) -> List a -> List a -> (List a, List a)
mahmap2x f l r = 
  case (head l, head r) of 
    (Nothing, Nothing ) -> ([],[])
    (Just lh, Nothing) -> ([],l)
    (Nothing, Just rh) -> ([],r) 
    (Just lh, Just rh) -> (let pr = mahmap2x f (etail l) (etail r) in
                             ( (f lh rh) :: (fst pr), (snd pr)))

mahmap2: (a -> a -> a) -> List a -> List a -> List a
mahmap2 f l r = 
  case (head l, head r) of 
    (Nothing, Nothing ) -> []
    (Just lh, Nothing) -> lh :: (mahmap2 f (etail l) r)
    (Nothing, Just rh) -> rh :: (mahmap2 f l (etail r))
    (Just lh, Just rh) -> (f lh rh) :: ( mahmap2 f (etail l) (etail r))

etail: List a -> List a
etail l = 
 case tail l of 
  Just t -> t
  Nothing -> []

main = text "Hello World"

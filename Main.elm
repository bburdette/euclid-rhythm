-- import Html exposing (text)
import List exposing (append, head, foldr, tail, map2, repeat, drop, length, isEmpty)
import Maybe 
-- import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html exposing (Html, Attribute, text, div, input, span, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Tuple exposing (first, second)
import String


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
  -- strictly speaking should be is 0 or 1 elts.
  -- this produces equivalent rhythms but official bjorkland appends the last 
  -- single elt of l2 to the end of l1 instead of after the first elt of l1.
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
                             ( (f lh rh) :: (first pr), (second pr)))

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


-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

type alias Model = 
  { ones: Int
  , zeros: Int
  }


main =
  Html.beginnerProgram { model = Model 1 1, view = view, update = update }


-- UPDATE

type Msg = OneUp
         | OneDown
         | ZeroUp
         | ZeroDown

update: Msg -> Model -> Model 
update m model = 
  case m of 
    OneUp -> { model | ones = model.ones + 1 }
    OneDown -> { model | ones = model.ones - 1 }
    ZeroUp -> { model | zeros = model.zeros + 1 }
    ZeroDown -> { model | zeros = model.zeros - 1 }


-- VIEW

view model =
  div []
    [ span [] [ text "ones"
              , button [onClick OneUp] [text "moar"] 
              , text (toString model.ones)
              , button [onClick OneDown] [text "less"] 
             -- , input [ placeholder "Text to reverse", onInput Ones, myStyle ] []
              ]
    , span [] [ text "zeros"
              , button [onClick ZeroUp] [text "moar"] 
              , text (toString model.zeros)
              , button [onClick ZeroDown] [text "less"] 
             -- , input [ placeholder "Text to reverse", onInput Ones, myStyle ] []
              ]
    , div [ myStyle ] [ text (toString (bjorkland model.ones model.zeros))]
    ]

myStyle =
  style
    [ ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

notMyStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]


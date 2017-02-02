-- import Html exposing (text)
import List exposing (append, head, foldr, tail, map2, repeat, drop, length, isEmpty)
import Array exposing (Array, fromList)
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
  -- strictly speaking should be 'is 0 or 1 elts', not 'is 0'.
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
  , pattern: Array Bool
  , pos: Int
  , correctSoFar: Bool
  , correctCount: Int
  , incorrectCount: Int
  }

main =
  Html.beginnerProgram { model = Model 1 1 Array.empty 0 True 0 0, view = view, update = update }


-- UPDATE

type Msg = OneUp
         | OneDown
         | ZeroUp
         | ZeroDown
         | XClick
         | DotClick

update: Msg -> Model -> Model 
update m model = 
  case m of 
    OneUp -> updatePattern model (model.ones + 1) model.zeros 
    OneDown -> updatePattern model (model.ones - 1) model.zeros 
    ZeroUp -> updatePattern model model.ones (model.zeros + 1)
    ZeroDown -> updatePattern model model.ones (model.zeros - 1)
    XClick -> clickUpdate model True
    DotClick -> clickUpdate model False 

updatePattern: Model -> Int -> Int -> Model
updatePattern m o z =
  let ones = if o > 0 then o else 1
      zeros = if z > 0 then z else 1 in
  { m | ones = ones
      , zeros = zeros
      , pattern = fromList (bjorkland ones zeros)
      , pos = 0
      , correctSoFar = True
      , correctCount = 0
      , incorrectCount = 0 }
    

clickUpdate: Model -> Bool -> Model
clickUpdate m b = 
  let curposval = Array.get m.pos m.pattern 
      good = (Just b) == curposval in
    { m | 
      correctCount = if good then (m.correctCount + 1) else m.correctCount
      , incorrectCount = if good then m.incorrectCount else (m.incorrectCount + 1)
      , pos = (m.pos + 1) % (Array.length m.pattern)
      }

-- VIEW

showpattern: List Bool -> String
showpattern lb = 
  String.concat (List.map (\b -> if b then "x" else ".") lb)


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
    , div [ myStyle ] [pattwbold (showpattern (bjorkland model.ones model.zeros)) model.pos]
    , div [] [ button [onClick XClick] [text "x"]
             , button [onClick DotClick] [text "."]]
    , div [] [ text "correct iterations: "
             , text (toString model.correctCount)]
    , div [] [ text "incorrect iterations: "
             , text (toString model.incorrectCount)]
    ]

-- pattwbold: String -> Int -> 
pattwbold s i = div []
  [ span [] [text (String.left i s)]
  , span [style [("font-weight", "bold")] ] 
         [ text (String.slice i (i+1) s) ]
  , text (String.dropLeft (i+1) s) ]

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


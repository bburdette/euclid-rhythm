module Main exposing (Model, Msg(..), bjorkland, bl_impl, clickUpdate, descriptionDict, descriptions, etail, euclid, mahmap2x, main, myStyle, noneorone, notMyStyle, pattwbold, showpattern, subscriptions, update, updatePattern, view)

import Array exposing (Array, fromList)
import Browser
import Dict
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import List exposing (append, drop, foldr, head, isEmpty, length, map2, repeat, tail)
import Maybe
import String
import Tuple exposing (first, second)


euclid : Int -> Int -> List ( Int, Int )
euclid m k =
    if k == 0 then
        [ ( m, 0 ) ]

    else
        [ ( m, k ) ] ++ euclid k (remainderBy m k)


bjorkland : Int -> Int -> List Bool
bjorkland ones zeros =
    foldr append [] (bl_impl (repeat ones [ True ]) (repeat zeros [ False ]))


bl_impl : List (List Bool) -> List (List Bool) -> List (List Bool)
bl_impl l1 l2 =
    case l2 of
        [] ->
            l1

        [ a ] ->
            List.append l1 l2

        _ ->
            let
                ( paired, remainder ) =
                    mahmap2x List.append l1 l2
            in
            bl_impl paired remainder


mahmap2x : (a -> a -> a) -> List a -> List a -> ( List a, List a )
mahmap2x f l r =
    case ( head l, head r ) of
        ( Nothing, Nothing ) ->
            ( [], [] )

        ( Just lh, Nothing ) ->
            ( [], l )

        ( Nothing, Just rh ) ->
            ( [], r )

        ( Just lh, Just rh ) ->
            let
                pr =
                    mahmap2x f (etail l) (etail r)
            in
            ( f lh rh :: first pr, second pr )


etail : List a -> List a
etail l =
    case tail l of
        Just t ->
            t

        Nothing ->
            []


noneorone : List a -> Bool
noneorone l =
    case l of
        [] ->
            True

        [ a ] ->
            True

        _ ->
            False



----------------------------------------------------------------------------------


type alias Model =
    { ones : Int
    , zeros : Int
    , pattern : Array Bool
    , pos : Int
    , correctSoFar : Bool
    , correctCount : Int
    , incorrectCount : Int
    }


main =
    Browser.document
        { init =
            \() ->
                ( Model 1 1 (fromList (bjorkland 1 1)) 0 True 0 0
                , Cmd.none
                )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Keyboard.downs Key
    Sub.none



-- UPDATE


type Msg
    = OneUp
    | OneDown
    | ZeroUp
    | ZeroDown
    | XClick
    | DotClick



-- | Key Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd msg )
update m model =
    let
        newm =
            case m of
                OneUp ->
                    updatePattern model (model.ones + 1) model.zeros

                OneDown ->
                    updatePattern model (model.ones - 1) model.zeros

                ZeroUp ->
                    updatePattern model model.ones (model.zeros + 1)

                ZeroDown ->
                    updatePattern model model.ones (model.zeros - 1)

                XClick ->
                    clickUpdate model True

                DotClick ->
                    clickUpdate model False

        {- Key c ->
           case c of
               88 ->
                   clickUpdate model True

               190 ->
                   clickUpdate model False

               _ ->
                   model
        -}
    in
    ( newm, Cmd.none )


updatePattern : Model -> Int -> Int -> Model
updatePattern m o z =
    let
        ones =
            if o > 0 then
                o

            else
                1

        zeros =
            if z > 0 then
                z

            else
                1
    in
    { m
        | ones = ones
        , zeros = zeros
        , pattern = fromList (bjorkland ones zeros)
        , pos = 0
        , correctSoFar = True
        , correctCount = 0
        , incorrectCount = 0
    }


clickUpdate : Model -> Bool -> Model
clickUpdate m b =
    let
        curposval =
            Array.get m.pos m.pattern

        good =
            Just b == curposval
    in
    { m
        | correctCount =
            if good then
                m.correctCount + 1

            else
                m.correctCount
        , incorrectCount =
            if good then
                m.incorrectCount

            else
                m.incorrectCount + 1
        , pos = remainderBy (m.pos + 1) <| Array.length m.pattern
    }



-- VIEW


showpattern : List Bool -> String
showpattern lb =
    String.concat
        (List.map
            (\b ->
                if b then
                    "x"

                else
                    "."
            )
            lb
        )


view : Model -> Browser.Document Msg
view model =
    { title = "euclid rhythms"
    , body =
        [ div [ style "text-align" "center" ]
            [ span []
                [ text "ones"
                , button [ onClick OneUp ] [ text "more" ]
                , text (String.fromInt model.ones)
                , button [ onClick OneDown ] [ text "less" ]
                ]
            , span []
                [ text "zeros"
                , button [ onClick ZeroUp ] [ text "more" ]
                , text (String.fromInt model.zeros)
                , button [ onClick ZeroDown ] [ text "less" ]
                ]
            , div myStyle [ pattwbold (showpattern (bjorkland model.ones model.zeros)) model.pos ]
            , div []
                [ button [ onClick XClick ] [ text "x" ]
                , button [ onClick DotClick ] [ text "." ]
                ]
            , div []
                [ text "correct: "
                , text (String.fromInt model.correctCount)
                ]
            , div []
                [ text "incorrect: "
                , text (String.fromInt model.incorrectCount)
                ]
            , div [] [ text "try X and . keys" ]
            , span []
                [ text (Maybe.withDefault "" (Dict.get ( model.ones, model.ones + model.zeros ) descriptionDict)) ]
            ]
        ]
    }


pattwbold : String -> Int -> Html Msg
pattwbold s i =
    div []
        [ span [] [ text (String.left i s) ]
        , span [ style "font-weight" "bold" ]
            [ text (String.slice i (i + 1) s) ]
        , text (String.dropLeft (i + 1) s)
        ]


myStyle =
    [ style "height" "40px"
    , style "padding" "10px 0"
    , style "font-size" "2em"
    , style "text-align" "center"
    ]


notMyStyle =
    [ style "width" "100%"
    , style "height" "40px"
    , style "padding" "10px 0"
    , style "font-size" "2em"
    , style "text-align" "center"
    ]



---------------------------------------------------------------------------
-- rhythm descriptions from the BANFF paper.


descriptions : List ( ( Int, Int ), String )
descriptions =
    [ ( ( 4, 12 ), "This is the (12/8)-time Fandango clapping pattern in the Flamenco music of southern Spain, where 'x' denotes a loud clap and '.' a soft clap [10]." )
    , ( ( 2, 3 ), "This is a common Afro-Cuban drum pattern. For example, it is the conga rhythm of the (6/8)-time Swing Tumbao. It is also common in Latin American music, as for example in the Cueca." )
    , ( ( 2, 5 ), "This is a thirteenth century Persian rhythm called Khafif-e-ramal. It is also the metric pattern of the second movement of Tchaikovsky's Symphony No. 6. When it is started on the second onset ([x . . x .]) it is the metric pattern of Dave Brubeck's Take Five as well as Mars from The Planets by Gustav Holst." )
    , ( ( 3, 4 ), "This is the archetypal pattern of the Cumbia from Colombia, as well as a Calypso rhythm from Trinidad. It is also a thirteenth century Persian rhythm called Khalif-e-saghil, as well as the trochoid choreic rhythmic pattern of ancient Greece." )
    , ( ( 3, 5 ), "when started on the second onset, is another thirteenth century Persian rhythm by the name of Khafif-e-ramal, as well as a Rumanian folk-dance rhythm." )
    , ( ( 3, 7 ), "This is a Ruchenitza rhythm used in a Bulgarian folk-dance. It is also the metric pattern of Pink Floyd's Money." )
    , ( ( 3, 8 ), "This is the Cuban tresillo pattern discussed in the preceding." )
    , ( ( 4, 7 ), "This is another Ruchenitza Bulgarian folk-dance rhythm." )
    , ( ( 4, 9 ), "This is the Aksak rhythm of Turkey. It is also the metric pattern used by Dave Brubeck in his piece Rondo a la Turk." )
    , ( ( 4, 11 ), "This is the metric pattern used by Frank Zappa in his piece titled Outside Now." )
    , ( ( 5, 6 ), "yields the York-Samai pattern, a popular Arab rhythm, when started on the second onset." )
    , ( ( 5, 7 ), "This is the Nawakhat pattern, another popular Arab rhythm." )
    , ( ( 5, 8 ), "This is the Cuban cinquillo pattern discussed in the preceding. When it is started on the second onset it is also the Spanish Tango and a thirteenth century Persian rhythm, the Al-saghilal-sani." )
    , ( ( 5, 9 ), "This is a popular Arab rhythm called Agsag-Samai. When started on the second onset, it is a drum pattern used by the Venda in South Africa, as well as a Rumanian folk-dance rhythm." )
    , ( ( 5, 11 ), "This is the metric pattern used by Moussorgsky in Pictures at an Exhibition." )
    , ( ( 5, 12 ), "This is the Venda clapping pattern of a South African children's song." )
    , ( ( 5, 16 ), "This is the Bossa-Nova rhythm necklace of Brazil. The actual Bossa-Nova rhythm usually starts on the third onset as follows: [x . . x . . x . . . x . . x . .]. However, there are other starting places as well, as for example [x . . x . . x . . x . . . x . .]." )
    , ( ( 7, 8 ), "This is a typical rhythm played on the Bendir (frame drum), and used in the accompaniment of songs of the Tuareg people of Libya." )
    , ( ( 7, 12 ), "This is a commonWest African bell pattern. For example, it is used in the Mpre rhythm of the Ashanti people of Ghana." )
    , ( ( 7, 16 ), "This is a Samba rhythm necklace from Brazil. The actual Samba rhythm is [x . x . . x . x . x . . x . x .] obtained by starting E(7,16) on the last onset. When E(7,16) is started on the fifth onset it is a clapping pattern from Ghana." )
    , ( ( 9, 16 ), "This is a rhythm necklace used in the Central African Republic.  When it is started on the fourth onset it is a rhythm played in West and Central Africa,, as well as a cow-bell pattern in the Brazilian samba. When it is started on the penultimate onset it is the bell pattern of the Ngbaka-Maibo rhythms of the Central African Republic." )
    , ( ( 11, 24 ), "This is a rhythm necklace of the Aka Pygmies of Central Africa. It is usually started on the seventh onset." )
    , ( ( 13, 24 ), "This is another rhythm necklace of the Aka Pygmies of the upper Sangha. It is usually started on the fourth onset." )
    ]


descriptionDict =
    Dict.fromList descriptions

module Main exposing (..)

import Html exposing (Html, text, div, h1, ul, li, span)
import Html.Attributes exposing (src, id, class)


---- MODEL ----


type Node
    = Node String (List Node)
    | Leaf String


initialTree : Node
initialTree =
    Node "My Tree "
        [ Leaf "hello"
        , Leaf "wat"
        , Node "child folder "
            [ Node "child folder "
                [ Leaf "hello"
                , Leaf "wat"
                ]
            ]
        , Leaf "hello"
        , Leaf "wat"
        , Node "child folder "
            [ Leaf "hello"
            , Leaf "wat"
            ]
        ]


type alias Model =
    { tree : Node }


init : ( Model, Cmd Msg )
init =
    ( { tree = initialTree }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { tree } =
    ul [] <| [ treeToView tree ]


treeToView : Node -> Html Msg
treeToView node =
    case node of
        Node name children ->
            li [ class "item" ]
                [ div [ class "bold" ]
                    [ text name
                    , span [] [ text "[+]" ]
                    ]
                , ul [] <| (List.map treeToView children) ++ [ li [ class "add" ] [ text "+" ] ]
                ]

        Leaf name ->
            li [ class "item" ]
                [ div []
                    [ text name ]
                ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

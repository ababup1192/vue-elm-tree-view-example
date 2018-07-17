module Main exposing (..)

import Html exposing (Html, div, h1, li, span, text, ul)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (onClick, onDoubleClick)



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


type alias Msg =
    Node


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( { model | tree = msg }
    , Cmd.none
    )



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
                , ul [] <|
                    List.indexedMap
                        (\index node ->
                            treeToView node
                                |> Html.map
                                    (\newNode ->
                                        Node name
                                            (children
                                                |> List.indexedMap
                                                    (\i oldNode ->
                                                        if i == index then
                                                            newNode

                                                        else
                                                            oldNode
                                                    )
                                            )
                                    )
                        )
                        children
                        ++ [ li
                                [ class "add"
                                , onClick (Node name (children ++ [ Leaf "new stuff" ]))
                                ]
                                [ text "+" ]
                           ]
                ]

        Leaf name ->
            li
                [ class "item"
                , onDoubleClick (Node name [ Leaf "new stuff" ])
                ]
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

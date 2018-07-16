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
    { operation : Operation
    , at : List Int
    }


type Operation
    = Add
    | ToFolder
    | Toggle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( { model
        | tree =
            updateNode msg.operation msg.at model.tree
      }
    , Cmd.none
    )


updateNode : Operation -> List Int -> Node -> Node
updateNode operation address node =
    case ( operation, address, node ) of
        ( Add, [], Node name children ) ->
            Node name
                (children
                    ++ [ Leaf "new stuff" ]
                )

        ( ToFolder, [], Leaf name ) ->
            Node name [ Leaf "new stuff" ]

        ( _, i :: rest, Node name children ) ->
            Node name
                (children
                    |> List.indexedMap
                        (\index child ->
                            if index == i then
                                updateNode operation rest child

                            else
                                child
                        )
                )

        _ ->
            node



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
                        (\index child ->
                            treeToView child
                                |> Html.map
                                    (\msg ->
                                        { msg
                                            | at = index :: msg.at
                                        }
                                    )
                        )
                        children
                        ++ [ li
                                [ class "add"
                                , onClick
                                    { operation = Add
                                    , at = []
                                    }
                                ]
                                [ text "+" ]
                           ]
                ]

        Leaf name ->
            li
                [ class "item"
                , onDoubleClick
                    { operation = ToFolder
                    , at = []
                    }
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

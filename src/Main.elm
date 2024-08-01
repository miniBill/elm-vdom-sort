module Main exposing (main)

import Browser
import Dagre.Attributes
import Dict
import Graph exposing (Edge, Graph, NodeId)
import Html exposing (Html)
import Html.Events
import IntDict
import Render
import Render.StandardDrawers
import Render.StandardDrawers.Attributes
import Render.StandardDrawers.Types
import Set exposing (Set)


type alias NodeLabel =
    ( NodeId, List Int )


swapGraph : Int -> Graph (List Int) ( Int, Int )
swapGraph size =
    let
        go :
            Set NodeLabel
            -> List (List NodeLabel)
            -> List (List (Edge ( Int, Int )))
            -> Graph (List Int) ( Int, Int )
        go visited queue edges =
            case queue of
                [] ->
                    Graph.fromNodesAndEdges
                        (visited
                            |> Set.toList
                            |> List.map
                                (\( id, list ) ->
                                    { id = id
                                    , label = list
                                    }
                                )
                        )
                        (List.concat edges)
                        |> cleanup

                [] :: tail ->
                    go visited tail edges

                (head :: tail) :: rest ->
                    if Set.member head visited then
                        go visited (tail :: rest) edges

                    else
                        let
                            ( newQueue, newEdges ) =
                                generateSwaps size head
                                    |> List.unzip
                        in
                        go
                            (Set.insert head visited)
                            (newQueue :: tail :: rest)
                            (newEdges :: edges)

        root : NodeLabel
        root =
            withId size (List.range 0 (size - 1))
    in
    go Set.empty [ [ root ] ] []


cleanup : Graph (List Int) ( Int, Int ) -> Graph (List Int) ( Int, Int )
cleanup graph =
    let
        newEdges : List (Edge ( Int, Int ))
        newEdges =
            graph
                |> Graph.reverseEdges
                |> Graph.bfs
                    (\path _ acc ->
                        case path of
                            [] ->
                                acc

                            [ _ ] ->
                                acc

                            current :: prev :: _ ->
                                { from = current.node.id
                                , to = prev.node.id
                                , label =
                                    IntDict.get current.node.id prev.outgoing
                                        |> Maybe.withDefault ( -1, -1 )
                                }
                                    :: acc
                    )
                    []
    in
    Graph.fromNodesAndEdges (Graph.nodes graph) newEdges


withId : Int -> List Int -> NodeLabel
withId size list =
    ( toId size list, list )


toId : Int -> List Int -> NodeId
toId size list =
    List.foldl (\e acc -> acc * size + e) 0 list


generateSwaps : Int -> NodeLabel -> List ( NodeLabel, Edge ( Int, Int ) )
generateSwaps size ( fromId, fromList ) =
    List.range 0 (size - 1)
        |> List.concatMap
            (\from ->
                List.range 0 (size - 1)
                    |> List.filterMap
                        (\to ->
                            if from == to then
                                Nothing

                            else
                                let
                                    result : List Int
                                    result =
                                        applySwap from to fromList

                                    resultId : NodeId
                                    resultId =
                                        toId size result
                                in
                                ( ( resultId, result )
                                , { from = fromId
                                  , to = resultId
                                  , label = ( from, to )
                                  }
                                )
                                    |> Just
                        )
            )


applySwap : Int -> Int -> List Int -> List Int
applySwap from to list =
    let
        dict =
            list
                |> List.indexedMap (\i v -> ( toFloat i, v ))
                |> Dict.fromList
    in
    case Dict.get (toFloat from) dict of
        Nothing ->
            list

        Just v ->
            dict
                |> Dict.remove (toFloat from)
                |> Dict.insert (toFloat to - 0.5) v
                |> Dict.values


type alias Model =
    Int


type alias Msg =
    Int


type alias Flags =
    {}


main : Program Flags Model Model
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Model
subscriptions _ =
    Sub.none


update : Model -> Model -> ( Model, Cmd Msg )
update msg _ =
    ( msg, Cmd.none )


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( 4, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text "Size: "
        , if model > 0 then
            Html.button [ Html.Events.onClick (max 0 (model - 1)) ] [ Html.text "-" ]

          else
            Html.text ""
        , Html.text (" " ++ String.fromInt model ++ " ")
        , Html.button [ Html.Events.onClick (model + 1) ] [ Html.text "+" ]
        , Render.draw
            [ Dagre.Attributes.rankDir Dagre.Attributes.LR ]
            [ Render.style "width: 100%"
            , Render.style "max-width: 100vw"
            , Render.style "max-height: 100vh"
            , Render.style "font-family: monospace"
            , Render.nodeDrawer
                (Render.StandardDrawers.svgDrawNode
                    [ Render.StandardDrawers.Attributes.label
                        (\{ label } -> String.join " " (List.map String.fromInt label))
                    , Render.StandardDrawers.Attributes.shape
                        (\_ ->
                            Render.StandardDrawers.Types.RoundedBox 1
                        )
                    , Render.StandardDrawers.Attributes.fontSize 8
                    ]
                )
            , Render.edgeDrawer
                (Render.StandardDrawers.svgDrawEdge
                    [ Render.StandardDrawers.Attributes.label
                        (\{ label } ->
                            let
                                ( from, to ) =
                                    label
                            in
                            String.fromInt from ++ " -> " ++ String.fromInt to
                        )
                    , Render.StandardDrawers.Attributes.arrowHead Render.StandardDrawers.Types.Triangle
                    , Render.StandardDrawers.Attributes.fontSize 8
                    , Render.StandardDrawers.Attributes.strokeWidth (\_ -> 1.5)
                    ]
                )
            ]
            (swapGraph model)
        ]

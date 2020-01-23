module Main exposing (main)

{-| Part of a composition used for the background of my Elm Europe talk.
-}

import Color exposing (Color)
import Dataset exposing (Student, students)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeId)
import IntDict
import List exposing (range)
import Scale exposing (SequentialScale)
import Scale.Color
import TypedSvg exposing (circle, g, line, polygon, svg, text_, title)
import TypedSvg.Attributes exposing (class, fill, points, stroke, textAnchor, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, r, strokeWidth, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..))


w : Float
w =
    1200


h : Float
h =
    640


colorScale : SequentialScale Color
colorScale =
    Scale.sequential Scale.Color.viridisInterpolator ( 200, 700 )


type alias Entity =
    Force.Entity NodeId { value : Student }



--init : Graph Entity ()
--init =
--    let
--        graph =
--            Graph.mapContexts
--                (\({ node, incoming, outgoing } as ctx) ->
--                    { incoming = incoming
--                    , outgoing = outgoing
--                    , node =
--                        { label =
--                            Force.entity node.id
--                                (CustomNode
--                                    (IntDict.size incoming + IntDict.size outgoing)
--                                    node.label
--                                )
--                        , id = node.id
--                        }
--                    }
--                )
--                miserablesGraph
--            --Graph.fromNodesAndEdges <| List.map .alias students <| []
--
--
--        links =
--            graph
--                |> Graph.edges
--                |> List.map
--                    (\{ from, to } ->
--                        { source = from
--                        , target = to
--                        , distance = 30
--                        , strength = Nothing
--                        }
--                    )
--
--        forces =
--            [ Force.customLinks 1 links
--            , Force.manyBodyStrength -30 <| List.map .id <| Graph.nodes graph
--            , Force.center (w / 2) (h / 2)
--            ]
--    in
--    Graph.nodes graph
--        |> List.map .label
--        |> Force.computeSimulation (Force.simulation forces)
--        |> updateGraphWithList graph


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }



--linkElement : Graph Entity () -> Edge () -> Svg msg
--linkElement graph edge =
--    let
--        retrieveEntity =
--            Maybe.withDefault (Force.entity 0 (CustomNode 0 "")) << Maybe.map (.node >> .label)
--
--        source =
--            retrieveEntity <| Graph.get edge.from graph
--
--        target =
--            retrieveEntity <| Graph.get edge.to graph
--    in
--    line
--        [ strokeWidth 1
--        , stroke <| Scale.convert colorScale source.x
--        , x1 source.x
--        , y1 source.y
--        , x2 target.x
--        , y2 target.y
--        ]
--        []


hexagon ( x, y ) size attrs =
    let
        angle =
            2 * pi / 6

        p =
            range 0 6
                |> List.map toFloat
                |> List.map (\a -> ( x + cos (a * angle) * size, y + sin (a * angle) * size ))
                |> points
    in
    polygon
        (p :: attrs)


nodeSize size node =
    hexagon ( node.x, node.y )
        size
        [ fill <| Fill <| Scale.convert colorScale node.x
        ]
        [ title [] [ text node.value.name ] ]


type alias Position a =
    { a | x : Float, y : Float }


makeEntities : List Student -> List Entity
makeEntities students =
    let
        nodes =
            List.indexedMap Force.entity students

        forces =
            [ Force.manyBodyStrength -20 <| List.map .id nodes
            , Force.center (w / 2) (h / 2)
            ]
    in
    Force.computeSimulation (Force.simulation forces) nodes


nodeElement : Entity -> Svg msg
nodeElement node =
    let
        initials =
            String.split " " node.value.alias |> List.map (String.left 1) |> String.concat
    in
    g []
        [ circle
            [ r 12
            , cx node.x
            , cy node.y
            , fill FillNone
            , stroke <| Scale.convert colorScale 0.5
            ]
            []
        , text_
            [ x node.x
            , y node.y
            , fontSize 10
            , fill <| Fill <| Scale.convert colorScale 0.6
            , textAnchor AnchorMiddle
            ]
            [ text initials ]
        ]


view model =
    svg [ viewBox 0 0 w h ]
        [ -- g [ class [ "links" ] ] <| List.map (linkElement model) <| Graph.edges model
          --, g [ class [ "nodes" ] ] <| List.map nodeElement <| Graph.nodes model
          g [ class [ "nodes" ] ] <| List.map nodeElement <| makeEntities students
        ]


main =
    view ()

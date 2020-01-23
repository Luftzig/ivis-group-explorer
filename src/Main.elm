module Main exposing (main)

{-| Part of a composition used for the background of my Elm Europe talk.
-}

import Browser exposing (sandbox)
import Browser.Events
import Color exposing (Color)
import Dataset exposing (Student, emptyStudent, students)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html)
import IntDict
import List exposing (range)
import List.Extra
import Scale exposing (SequentialScale)
import Scale.Color
import Set
import Time
import TypedSvg exposing (circle, g, line, polygon, rect, svg, text_, title)
import TypedSvg.Attributes exposing (class, fill, fillOpacity, points, stroke, textAnchor, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, dx, dy, fontSize, height, r, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events exposing (onMouseEnter, onMouseLeave)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Opacity)


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


type alias Model =
    { students : Graph Entity ()
    , simulation : Force.State NodeId
    , hover : Maybe NodeId
    }


type Msg
    = Tick Time.Posix
    | ClickNode NodeId
    | HoverNode NodeId
    | StopHoverNode


nodeElement : Maybe NodeId -> Entity -> Svg Msg
nodeElement hovered node =
    let
        initials =
            String.split " " node.value.alias |> List.map (String.left 1) |> String.concat

        hover =
            case hovered of
                Just id ->
                    if id == node.id then
                        [ rect
                            [ x <| node.x - 60
                            , y <| node.y - 30
                            , fill <| Fill <| Color.white
                            , stroke <| Scale.convert colorScale 0.6
                            , width 120
                            , height 20
                            ]
                            []
                        , text_
                            [ x <| node.x
                            , y <| node.y - 15
                            , fontSize 10
                            , fill <| Fill <| Scale.convert colorScale 0.6
                            , textAnchor AnchorMiddle
                            ]
                            [ text <| String.join ", " <| Set.toList node.value.tags ]
                        ]

                    else
                        []

                Nothing ->
                    []
    in
    g
        [ onMouseEnter <| HoverNode node.id
        , onMouseLeave StopHoverNode
        ]
        ([ circle
            [ r 12
            , cx node.x
            , cy node.y
            , fill <| Fill <| Scale.convert colorScale 200
            , stroke <| Scale.convert colorScale 600
            ]
            []
         , text_
            [ x node.x
            , y node.y
            , fontSize 10
            , fill <| Fill <| Scale.convert colorScale 650
            , textAnchor AnchorMiddle
            ]
            [ text initials ]
         ]
            ++ hover
        )


view : Model -> Html Msg
view model =
    svg [ viewBox 0 0 w h ]
        [ g [ class [ "links" ] ] <|
            List.map (linkElement model.students) <|
                Graph.edges model.students
        , g
            [ class [ "nodes" ] ]
          <|
            List.map (nodeElement model.hover) <|
                List.map .label <|
                    Graph.nodes model.students
        ]


linkElement : Graph Entity () -> Edge () -> Svg Msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 emptyStudent) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 emptyStudent) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
    line
        [ strokeWidth 1
        , stroke (Color.rgb255 170 170 170)
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                ( newState, newStudents ) =
                    Force.tick model.simulation <| List.map .label <| Graph.nodes model.students
            in
            ( { model | simulation = newState, students = updateGraphWithList model.students newStudents }, Cmd.none )

        ClickNode id ->
            ( model, Cmd.none )

        HoverNode id ->
            ( { model | hover = Just id }, Cmd.none )

        StopHoverNode ->
            ( { model | hover = Nothing }, Cmd.none )


hasCommonInterest : ( Entity, Entity ) -> Bool
hasCommonInterest ( e1, e2 ) =
    Set.intersect e1.value.tags e2.value.tags |> Set.isEmpty |> not


links : List Entity -> Force.Force NodeId
links entities =
    let
        byInterests =
            List.Extra.select entities |> List.concatMap linkByInterest |> Force.customLinks 1
    in
    byInterests


linkByInterest : ( Entity, List Entity ) -> List { source : NodeId, target : NodeId, distance : Float, strength : Maybe Float }
linkByInterest ( e, es ) =
    let
        makeLink e2 =
            let
                intersection =
                    Set.intersect e.value.tags e2.value.tags
            in
            if Set.isEmpty intersection then
                Nothing

            else
                Just
                    { source = e.id
                    , target = e2.id
                    , distance = 75 / (toFloat <| Set.size intersection)
                    , strength = Nothing
                    }
    in
    List.filterMap makeLink es


init : () -> ( Model, Cmd Msg )
init _ =
    let
        nodes : List (Node Entity)
        nodes =
            List.indexedMap (\i s -> Node i (Force.entity i s)) students

        graph =
            Graph.fromNodesAndEdges nodes
                (nodes
                    |> List.map .id
                    |> List.Extra.uniquePairs
                    |> List.map (\( n1, n2 ) -> { from = n1, to = n2, label = () })
                )

        toLink { from, to } =
            ( from, to )

        forces =
            [ Force.manyBodyStrength -120 <| List.map .id nodes
            , Force.center (w / 2) (h / 2)
            , Force.links <| List.map toLink <| Graph.edges graph
            ]
    in
    ( { students = graph, simulation = Force.simulation forces, hover = Nothing }, Cmd.none )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch [ Browser.Events.onAnimationFrame Tick ]
        }

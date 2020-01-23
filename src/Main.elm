module Main exposing (main)

{-| Part of a composition used for the background of my Elm Europe talk.
-}

import Browser exposing (sandbox)
import Browser.Events
import Color exposing (Color)
import Dataset exposing (Student, emptyStudent, students)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html, button, div)
import Html.Events
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
    , activeRules : Rules
    }


type alias Rules =
    { tags : Bool }


type Msg
    = Tick Time.Posix
    | ClickNode NodeId
    | HoverNode NodeId
    | ChangeRules Rules
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
    let
        activeRules =
            model.activeRules
    in
    div []
        [ button
            [ Html.Events.onClick <| ChangeRules { activeRules | tags = not activeRules.tags }
            ]
            [ Html.text
                ("Tags "
                    ++ (if activeRules.tags then
                            "Active"

                        else
                            "Disabled"
                       )
                )
            ]
        , svg [ viewBox 0 0 w h ]
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

        ChangeRules rules ->
            if model.activeRules == rules then
                ( model, Cmd.none )

            else
                ( { model
                    | simulation =
                        Force.simulation
                            (baseForces model.students ++ ruleForce model.students rules)
                    , activeRules = rules
                  }
                , Cmd.none
                )


ruleForce graph { tags } =
    if tags then
        [ Graph.edges graph |> List.filterMap (toTagsLink graph) |> Force.customLinks 1 ]

    else
        []


commonTags graph edge =
    let
        source_ =
            Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target_ =
            Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
    case ( source_, target_ ) of
        ( Nothing, _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing

        ( Just source, Just target ) ->
            Just
                { count = Set.intersect source.value.tags target.value.tags |> Set.size
                , source = source
                , target = target
                }


toTagsLink :
    Graph Entity ()
    -> Edge ()
    -> Maybe { source : NodeId, target : NodeId, distance : Float, strength : Maybe Float }
toTagsLink graph edge =
    case commonTags graph edge of
        Nothing ->
            Nothing

        Just { count, source, target } ->
            if count == 0 then
                Nothing

            else
                Just { source = source.id, target = target.id, distance = 75, strength = Just (0.3 * toFloat count) }


baseForces graph =
    let
        toBaseLink { from, to } =
            { source = from, target = to, distance = 200, strength = Just 0.01 }
    in
    [ Force.manyBodyStrength -30 <| List.map .id <| Graph.nodes graph
    , Force.center (w / 2) (h / 2)
    , Force.customLinks 1 <| List.map toBaseLink <| Graph.edges graph
    ]


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
    in
    ( { students = graph
      , simulation = Force.simulation <| baseForces graph
      , activeRules = { tags = False }
      , hover = Nothing
      }
    , Cmd.none
    )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch [ Browser.Events.onAnimationFrame Tick ]
        }

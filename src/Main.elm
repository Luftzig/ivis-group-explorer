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
import List exposing (range, repeat)
import List.Extra
import Scale exposing (SequentialScale)
import Scale.Color
import Set
import Time
import TypedSvg exposing (circle, g, line, polygon, rect, svg, text_, title)
import TypedSvg.Attributes exposing (class, fill, fillOpacity, points, stroke, textAnchor, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, dx, dy, fontSize, height, r, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events exposing (onClick, onMouseEnter, onMouseLeave)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Opacity)


w : Float
w =
    1200


h : Float
h =
    640


baseDistance =
    250


closeDistance =
    100


colorScale : SequentialScale Color
colorScale =
    Scale.sequential Scale.Color.viridisInterpolator ( 0, 100 )


type alias Entity =
    Force.Entity NodeId { value : Student }


type alias Model =
    { students : Graph Entity ()
    , simulation : Force.State NodeId
    , hover : Maybe NodeId
    , activeRules : Rules
    }


type SkillRule
    = Inactive
    | Complementary
    | Similar


type alias Rules =
    { tags : Bool
    , visualization : SkillRule
    }


type Msg
    = Tick Time.Posix
    | ClickNode NodeId
    | HoverNode NodeId
    | ChangeRules Rules
    | StopHoverNode


hoverElement : NodeContext Entity () -> Svg Msg
hoverElement { node } =
    g []
        [ rect
            [ x <| node.label.x - 60
            , y <| node.label.y - 30
            , fill <| Fill <| Color.white
            , stroke <| Scale.convert colorScale 60
            , width 120
            , height 20
            ]
            []
        , text_
            [ x <| node.label.x
            , y <| node.label.y - 15
            , fontSize 10
            , fill <| Fill <| Scale.convert colorScale 60
            , textAnchor AnchorMiddle
            ]
            [ text <| String.join ", " <| Set.toList node.label.value.tags ]
        ]


nodeElement : Maybe NodeId -> Entity -> Svg Msg
nodeElement hovered node =
    let
        initials =
            String.split " " node.value.alias |> List.map (String.left 1) |> String.concat
    in
    g
        [ onMouseEnter <| HoverNode node.id
        , onMouseLeave StopHoverNode
        , onClick <| ClickNode node.id
        ]
        [ circle
            [ r 12
            , cx node.x
            , cy node.y
            , fill <| Fill <| Scale.convert colorScale 0
            , stroke <| Scale.convert colorScale 90
            ]
            []
        , text_
            [ x node.x
            , y node.y
            , fontSize 10
            , fill <| Fill <| Scale.convert colorScale 95
            , textAnchor AnchorMiddle
            ]
            [ text initials ]
        ]


tagsButton rules =
    button
        [ Html.Events.onClick <| ChangeRules { rules | tags = not rules.tags }
        ]
        [ Html.text
            ("Tags "
                ++ (if rules.tags then
                        "Active"

                    else
                        "Disabled"
                   )
            )
        ]


visualizationButton rules =
    let
        nextMode rule =
            case rule of
                Inactive ->
                    Complementary

                Complementary ->
                    Similar

                Similar ->
                    Inactive

        label rule =
            case rule of
                Inactive ->
                    "Inactive"

                Complementary ->
                    "Complementary"

                Similar ->
                    "Similar"
    in
    button
        [ Html.Events.onClick <| ChangeRules { rules | visualization = nextMode rules.visualization } ]
        [ Html.text ("Visualization Skill: " ++ label rules.visualization)
        ]


view : Model -> Html Msg
view model =
    let
        activeRules =
            model.activeRules
    in
    div []
        [ div []
            [ tagsButton activeRules
            , visualizationButton activeRules
            ]
        , svg [ viewBox 0 0 w h ]
            [ g [ class [ "links" ] ] <|
                List.map (linkElement model.hover model.activeRules model.students) <|
                    Graph.edges model.students
            , g
                [ class [ "nodes" ] ]
              <|
                List.map (nodeElement model.hover) <|
                    List.map .label <|
                        Graph.nodes model.students
            , Maybe.withDefault (g [] []) <|
                Maybe.map hoverElement <|
                    Maybe.andThen (\id -> Graph.get id model.students) <|
                        model.hover
            ]
        ]


linkElement : Maybe NodeId -> Rules -> Graph Entity () -> Edge () -> Svg Msg
linkElement hovered { tags, visualization } graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 emptyStudent) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 emptyStudent) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph

        isHovered =
            case hovered of
                Just id ->
                    id == source.id || id == target.id

                Nothing ->
                    False

        hasInCommon =
            if tags then
                commonTags graph edge

            else
                Nothing

        highlightCommon =
            case hasInCommon of
                Nothing ->
                    0

                Just { count } ->
                    count
    in
    if tags || visualization /= Inactive then
    line
        [ strokeWidth <|
            if tags then
                toFloat highlightCommon * 1

            else
                0.1
        , stroke
            (if isHovered && highlightCommon > 0 then
                Scale.convert colorScale 60

             else
                Color.rgb255 170 170 170
            )
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []
    else g [] []


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


centerNode : Maybe (NodeContext Entity ()) -> Maybe (NodeContext Entity ())
centerNode mContext =
    case mContext of
        Just context ->
            let
                node =
                    context.node

                label =
                    node.label

                newLabel =
                    { label | x = w / 2, y = h / 2 }
            in
            Just { context | node = { node | label = newLabel } }

        Nothing ->
            Nothing


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
            ( { model
                | students = Graph.update id centerNode model.students
                , simulation = Force.reheat model.simulation
              }
            , Cmd.none
            )

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


baseForce graph =
    let
        toBaseLink { from, to } =
            { source = from, target = to, distance = baseDistance, strength = Nothing }
    in
    Force.customLinks 1 <| List.map toBaseLink <| Graph.edges graph


ruleForce graph { tags, visualization } =
    List.concat
        [ if not tags && visualization == Inactive then
            [ baseForce graph ]

          else
            []
        , if tags then
            [ Graph.edges graph |> List.concatMap (toTagsLink graph) |> Force.customLinks 1 ]

          else
            []
        , case visualization of
            Inactive ->
                []

            Similar ->
                [ Graph.edges graph
                    |> List.concatMap (similarSkillLink .visualizationSkill graph)
                    |> Force.customLinks 1
                ]

            Complementary ->
                [ Graph.edges graph
                    |> List.concatMap (complementarySkillLink .visualizationSkill graph)
                    |> Force.customLinks 1
                ]
        ]


commonTags graph edge =
    case getEdgeEntities graph edge of
        Nothing ->
            Nothing

        Just { source, target } ->
            Just
                { count = Set.intersect source.value.tags target.value.tags |> Set.size
                , source = source
                , target = target
                }


getEdgeEntities : Graph Entity () -> Edge () -> Maybe { source : Entity, target : Entity }
getEdgeEntities graph edge =
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
            Just { source = source, target = target }


type alias CustomLink =
    { source : NodeId, target : NodeId, distance : Float, strength : Maybe Float }


closeLink source target = { source = source.id, target = target.id, distance = closeDistance, strength = Nothing}

baseLink source target = { source = source.id, target = target.id, distance = baseDistance, strength = Nothing}

similarSkillLink : (Student -> Int) -> Graph Entity () -> Edge () -> List CustomLink
similarSkillLink getSkill graph edge =
    case getEdgeEntities graph edge of
        Nothing ->
            []

        Just { source, target } ->
            let
                sourceSkill =
                    getSkill source.value

                targetSkill =
                    getSkill target.value

                distance =
                    abs (sourceSkill - targetSkill)
            in
            if distance <= 3 && sourceSkill >= 0 && targetSkill >= 0 then
                repeat distance <| closeLink source target

            else
                [baseLink source target]


complementarySkillLink : (Student -> Int) -> Graph Entity () -> Edge () -> List CustomLink
complementarySkillLink getSkill graph edge =
    case getEdgeEntities graph edge of
        Nothing ->
            []

        Just { source, target } ->
            let
                sourceSkill =
                    getSkill source.value

                targetSkill =
                    getSkill target.value

                avg =
                    toFloat (sourceSkill + targetSkill) / 2

                distance =
                    abs (avg - 5)
            in
            if distance <= 3 && sourceSkill >= 0 && targetSkill >= 0 then
                repeat (3 - round distance) <| closeLink source target

            else
                [baseLink source target]


toTagsLink :
    Graph Entity ()
    -> Edge ()
    -> List CustomLink
toTagsLink graph edge =
    case commonTags graph edge of
        Nothing ->
            []

        Just { count, source, target } ->
            if count > 0 then
              repeat count <| closeLink source target
            else
              [baseLink source target]


baseForces graph =
    [ Force.manyBodyStrength -30 <| List.map .id <| Graph.nodes graph
    , Force.center (w / 2) (h / 2)
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
      , simulation = Force.simulation <| (baseForce graph :: baseForces graph)
      , activeRules = { tags = False, visualization = Inactive }
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

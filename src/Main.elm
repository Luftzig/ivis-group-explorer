module Main exposing (main)

{-| Part of a composition used for the background of my Elm Europe talk.
-}

import Browser exposing (sandbox)
import Browser.Events
import Color exposing (Color)
import Dataset exposing (Student, students)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeId)
import IntDict
import List exposing (range)
import Scale exposing (SequentialScale)
import Scale.Color
import Time
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


type alias Model =
    { students : List Entity
    , simulation : Force.State NodeId
    }


type Msg
    = Tick Time.Posix
    | ClickNode NodeId


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
          g [ class [ "nodes" ] ] <| List.map nodeElement <| .students model
        ]


update msg model =
    case msg of
        Tick time ->
            let
                ( newState, newStudents ) =
                    Force.tick model.simulation model.students
            in
            ( { model | simulation = newState, students = newStudents }, Cmd.none )

        ClickNode id ->
            ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        nodes =
            List.indexedMap Force.entity students

        forces =
            [ Force.manyBodyStrength -20 <| List.map .id nodes
            , Force.center (w / 2) (h / 2)
            ]
    in
    ( { students = nodes, simulation = Force.simulation forces }, Cmd.none )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch [ Browser.Events.onAnimationFrame Tick ]
        }

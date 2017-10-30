module Main exposing (..)

--import Debug exposing (log)

import AnimationFrame
import Dict exposing (..)
import Html exposing (Html)
import Json.Decode exposing (Decoder, decodeString, field, int, list)
import List exposing (..)
import Navigation exposing (Location, programWithFlags)
import Svg exposing (Svg, line, rect, svg)
import Svg.Attributes exposing (..)
import Task
import Time exposing (Time)
import WebSocket exposing (listen, send)
import Window


type alias Flags =
    { dynamicWsPort : Bool, delay : Time }


main : Program Flags Model Msg
main =
    programWithFlags
        NewLocation
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias CellsDict =
    Dict ( Int, Int ) Bool


type alias Url =
    { host : String, portNum : String }


type alias Grid =
    { width : Int, height : Int, cellSize : Int }


type alias Timing =
    { waitUntil : Time, sentAt : Time, delay : Time }


type alias Model =
    { grid : Grid, url : Url, cells : CellsDict, timing : Timing }


initModel : Flags -> Location -> Model
initModel { dynamicWsPort, delay } location =
    { grid = { width = 0, height = 0, cellSize = 1 }
    , url =
        { host = location.hostname
        , portNum =
            if dynamicWsPort then
                location.port_
            else
                "8080"
        }
    , cells = empty
    , timing = { waitUntil = 0, sentAt = 0, delay = delay }
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    ( initModel flags location, Task.perform SetScreenSize Window.size )


type alias AliveCells =
    List (List Int)


cellsDecoder : Decoder AliveCells
cellsDecoder =
    field "alive" (list (list int))


toCellsDict : AliveCells -> CellsDict
toCellsDict =
    List.map
        (\x ->
            case x of
                [ x, y ] ->
                    ( ( x, y ), True )

                _ ->
                    ( ( 0, 0 ), False )
        )
        >> fromList



-- UPDATE


type Msg
    = SetScreenSize Window.Size
    | NewMessage String
    | NewLocation Location
    | NewFrame Time
    | CurrentTime Time


wsAddress : Url -> String
wsAddress { host, portNum } =
    "ws://" ++ host ++ ":" ++ portNum ++ "/websocket"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { grid, url, cells, timing } =
            model

        wsAddr =
            wsAddress url
    in
    case msg of
        SetScreenSize { width, height } ->
            let
                c =
                    Basics.max 10 (width // 90)

                w =
                    width - width % c - c

                h =
                    height - height % c - c

                newGrid =
                    { width = w, height = h, cellSize = c }

                cw =
                    toString (w // c)

                hw =
                    toString (h // c)

                msg =
                    "{ \"start\" : { \"width\" : " ++ cw ++ ", \"height\" : " ++ hw ++ " }}"
            in
            ( { model | grid = newGrid }, send wsAddr msg )

        NewMessage cellsJson ->
            let
                result =
                    cellsJson
                        |> decodeString cellsDecoder
                        |> Result.map toCellsDict

                newCells =
                    case result of
                        Ok c ->
                            c

                        Err _ ->
                            empty
            in
            ( { model | cells = newCells }, Task.perform CurrentTime Time.now )

        NewFrame time ->
            if time < timing.waitUntil then
                ( model, Cmd.none )
            else
                ( { model | timing = { timing | sentAt = time, waitUntil = time + 1000 } }, send wsAddr "{\"next\" : 1}" )

        CurrentTime time ->
            let
                    latency =
                         time - timing.sentAt
                    wait =
                      Basics.max 0 (timing.delay - latency)
                in
            ( { model | timing = { timing |  waitUntil = time + wait } }, Cmd.none )

        NewLocation _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { url } =
    Sub.batch
        [ listen (wsAddress url) NewMessage
        , Window.resizes SetScreenSize
        , AnimationFrame.times NewFrame
        ]



-- VIEW


range : Int -> Int -> Int -> List Int
range from to step =
    if from < to then
        from :: range (from + step) to step
    else
        []


renderGrid : Int -> Int -> Int -> List (Svg msg)
renderGrid width height cellSize =
    let
        xs =
            range 0 (width + 1) cellSize

        ys =
            range 0 (height + 1) cellSize

        ws =
            toString width

        hs =
            toString height

        lineStyle =
            "stroke:#999999;stroke-width:1"

        xls =
            List.map (\x -> line [ x1 (toString x), y1 "0", x2 (toString x), y2 hs, style lineStyle ] []) xs

        yls =
            List.map (\y -> line [ x1 "0", y1 (toString y), x2 ws, y2 (toString y), style lineStyle ] []) ys
    in
    xls ++ yls


renderCells : Int -> CellsDict -> List (Svg msg)
renderCells size =
    toList >> List.map (\( ( x, y ), _ ) -> rect [ Svg.Attributes.x (toString (x * size + 1)), Svg.Attributes.y (toString (y * size + 1)), Svg.Attributes.width (toString (size - 1)), Svg.Attributes.height (toString (size - 1)), style "fill:#007799" ] [])


view : Model -> Html Msg
view { grid, cells } =
    let
        { width, height, cellSize } =
            grid

        b =
            rect [ x "0", y "0", Svg.Attributes.width (toString width), Svg.Attributes.height (toString height), style "fill:#e6e6e6" ] []

        g =
            renderGrid width height cellSize

        c =
            renderCells cellSize cells
    in
    svg
        [ Svg.Attributes.width (toString width), Svg.Attributes.height (toString height) ]
        (b :: (c ++ g))

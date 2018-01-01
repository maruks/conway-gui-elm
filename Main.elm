module Main exposing (..)

import AnimationFrame
--import Debug exposing (log)
import Dict exposing (..)
import Html exposing (Html)
import Json.Decode exposing (Decoder, andThen, decodeString, fail, field, int, list, null)
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


type alias Point =
    ( Int, Int )


type alias Color =
    Int


type alias CellsDict =
    Dict Point Color


type alias Url =
    { host : String, portNum : String }


type alias Grid =
    { width : Int, height : Int, cellSize : Int }


type alias Timing =
    { waitUntil : Time, sentAt : Time, delay : Time }


type alias ColorsDict =
    Dict Int String


type alias Model =
    { grid : Grid, url : Url, cells : CellsDict, timing : Timing, colors : ColorsDict }


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
    , colors = Dict.singleton 0 "#e6e6e6"
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    ( initModel flags location, Task.perform SetScreenSize Window.size )


type alias Cell =
    { color : Int, point : List Int }


type alias CellsList =
    List Cell


type alias ColorMsg =
    { color : String, code : Int }


type alias ColorList =
    List ColorMsg


type Message
    = CellsMessage CellsList
    | ColorsMessage ColorList
    | ErrorCode Int


messageDecoder : Decoder Message
messageDecoder =
    field "type" Json.Decode.string
        |> andThen payloadDecoder


cellDecoder : Decoder Cell
cellDecoder =
    Json.Decode.map2 Cell
        (field "color" int)
        (field "point" (list int))


colorDecoder : Decoder ColorMsg
colorDecoder =
    Json.Decode.map2 ColorMsg
        (field "color" Json.Decode.string)
        (field "code" int)


payloadDecoder : String -> Decoder Message
payloadDecoder msgType =
    case msgType of
        "cells" ->
            field "cells" (Json.Decode.oneOf [ null [], list cellDecoder ]) |> Json.Decode.map CellsMessage

        "colors" ->
            field "colors" (list colorDecoder) |> Json.Decode.map ColorsMessage

        "error" ->
            field "code" int |> Json.Decode.map ErrorCode

        _ ->
            fail "unknown type"


toCellsDict : CellsList -> CellsDict
toCellsDict =
    List.map
        (\c ->
            case c.point of
                [ x, y ] ->
                    ( ( x, y ), c.color )

                _ ->
                    ( ( 0, 0 ), c.color )
        )
        >> fromList


toColorsDict : ColorList -> ColorsDict
toColorsDict =
    List.map
        (\c ->
            ( c.code, c.color )
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
                    toString <| w // c

                hw =
                    toString <| h // c

                msg =
                    "{ \"start\" : { \"width\" : " ++ cw ++ ", \"height\" : " ++ hw ++ " }}"
            in
            ( { model | grid = newGrid }, send wsAddr msg )

        NewMessage message ->
            let
                result =
                    decodeString messageDecoder message
            in
            case result of
                Ok (CellsMessage cells) ->
                    let
                        newCells =
                            toCellsDict cells

                        task =
                            if Dict.isEmpty newCells then
                                Task.perform SetScreenSize Window.size
                            else
                                Task.perform CurrentTime Time.now
                    in
                    ( { model | cells = newCells }, task )

                Ok (ColorsMessage colors) ->
                    ( { model | colors = toColorsDict colors }, Cmd.none )

                Ok (ErrorCode _) ->
                    ( model, Task.perform SetScreenSize Window.size )

                Err _ ->
                    ( { model | cells = empty }, Cmd.none )

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
            ( { model | timing = { timing | waitUntil = time + wait } }, Cmd.none )

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
            "stroke:#999999; stroke-opacity:0.5; stroke-width: 1"

        xls =
            List.map (\x -> line [ x1 <| toString x, y1 "0", x2 <| toString x, y2 hs, style lineStyle ] []) xs

        yls =
            List.map (\y -> line [ x1 "0", y1 <| toString y, x2 ws, y2 <| toString y, style lineStyle ] []) ys
    in
    xls ++ yls


getOr : Dict comparable v -> comparable -> v -> v
getOr dict key default =
    case get key dict of
        Nothing ->
            default

        Just p ->
            p


renderCells : Int -> ColorsDict -> CellsDict -> List (Svg msg)
renderCells size colors =
    toList >> List.map (\( ( x, y ), colorCode ) -> rect [ Svg.Attributes.x <| toString <| x * size + 1, Svg.Attributes.y <| toString <| y * size + 1, Svg.Attributes.width <| toString size, Svg.Attributes.height <| toString size, "fill:" ++ getOr colors colorCode "#0099cc" |> style ] [])


view : Model -> Html Msg
view { grid, cells, colors } =
    let
        { width, height, cellSize } =
            grid

        b =
            rect [ x "0", y "0", Svg.Attributes.width <| toString width, Svg.Attributes.height <| toString height, "fill:" ++ getOr colors 0 "#e6e6e6" |> style ] []

        g =
            renderGrid width height cellSize

        c =
            renderCells cellSize colors cells
    in
    svg
        [ Svg.Attributes.width <| toString width, Svg.Attributes.height <| toString height ]
        (b :: (c ++ g))

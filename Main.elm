module Main exposing (..)

--import Debug exposing (log)

import AnimationFrame
import Dict exposing (..)
import Html exposing (Html)
import Json.Decode exposing (Decoder, andThen, decodeString, fail, field, int, list, null)
import List exposing (..)
import Array exposing (Array)
import Navigation exposing (Location, programWithFlags)
import Svg exposing (Svg, line, rect, svg)
import Svg.Attributes exposing (..)
import Task
import Time exposing (Time)
import WebSocket exposing (listen, send)
import Window


type alias Flags =
    { dynamicWsPort : Bool, delay : Time, bufferSize : Int }


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

type alias CellsDictBuffer =
    { buffer : Array CellsDict, size : Int, currentIndex : Int, showIndex : Int }


type alias Url =
    { host : String, portNum : String, pathName : String }


type alias GridSize =
    { width : Int, height : Int, cellSize : Int }


type alias Timing =
    { waitUntil : Time, delay : Time }


type alias ColorsDict =
    Dict Int String

type alias Model =
    { grid : GridSize, url : Url, cells : CellsDictBuffer, timing : Timing, colors : ColorsDict }

initBuffer : Int -> CellsDictBuffer
initBuffer size = { buffer = Array.repeat size empty, size = size, currentIndex = 0, showIndex = 0 }

isBufferFull : CellsDictBuffer -> Bool
isBufferFull {size, currentIndex, showIndex } = showIndex == rem (currentIndex + 1) size

addCellsToBuffer : CellsDictBuffer -> CellsDict -> CellsDictBuffer
addCellsToBuffer cellsBuffer cells = let { buffer, size , currentIndex , showIndex } =
                                             cellsBuffer

                                         nextCurrentIdx =
                                             rem (currentIndex + 1) size
                                in
                                    if (isBufferFull cellsBuffer) then
                                        cellsBuffer
                                    else
                                        {cellsBuffer | buffer = Array.set nextCurrentIdx cells buffer, currentIndex = nextCurrentIdx }

nextBufferFrame : CellsDictBuffer -> CellsDictBuffer
nextBufferFrame ({ buffer, size , currentIndex , showIndex } as b) =
    let
        nextBuffer = if (currentIndex == showIndex) then
                         b
                     else
                         { b | showIndex = rem (showIndex + 1) size }
    in
        nextBuffer

visibleBufferFrame : CellsDictBuffer -> CellsDict
visibleBufferFrame { buffer, showIndex } =
    case Array.get showIndex buffer of
        Just cells -> cells
        Nothing -> empty

initModel : Flags -> Location -> Model
initModel { dynamicWsPort, delay, bufferSize } location =
    { grid = { width = 0, height = 0, cellSize = 1 }
    , url =
        { host = location.hostname
        , pathName = location.pathname
        , portNum =
            if dynamicWsPort then
                location.port_
            else
                "8080"
        }
    , cells = initBuffer bufferSize
    , timing = { waitUntil = 0, delay = delay }
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
    field "tag" Json.Decode.string
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

wsAddress : Url -> String
wsAddress { host, portNum, pathName } =
    let
        xs =
            String.split "/" pathName

        path =
            (String.split "/" pathName |> List.take (List.length xs - 1)) ++ [ "websocket" ] |> String.join "/"
    in
    "ws://" ++ host ++ ":" ++ portNum
        ++ (if String.startsWith "/" path then
                path
            else
                "/" ++ path
           )

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
                     (Basics.max width height) // 80 |> Basics.max 10

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
            ( { model | grid = newGrid, cells = initBuffer cells.size }, send wsAddr msg )

        NewMessage message ->
            let
                result =
                    decodeString messageDecoder message
            in
            case result of
                Ok (CellsMessage cellsMsg) ->
                    let
                        newCells =
                            toCellsDict cellsMsg

                        newBuffer = addCellsToBuffer cells newCells

                        task =
                            if Dict.isEmpty newCells then
                                Task.perform SetScreenSize Window.size
                            else
                                Cmd.none

                    in
                    ( { model | cells = newBuffer }, task )

                Ok (ColorsMessage colors) ->
                    ( { model | colors = toColorsDict colors }, Cmd.none )

                Ok (ErrorCode _) ->
                    ( model, Task.perform SetScreenSize Window.size )

                Err _ ->
                    ( { model | cells = initBuffer cells.size }, Cmd.none )

        NewFrame time ->
            if time < timing.waitUntil then
                ( model, if isBufferFull cells then
                                Cmd.none
                            else
                                send wsAddr "{\"next\" : 1}" )
            else
                ( { model | timing = { timing | waitUntil = time + timing.delay } , cells = nextBufferFrame cells }, Cmd.none )

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
            renderCells cellSize colors (visibleBufferFrame cells)
    in
    svg
        [ Svg.Attributes.width <| toString width, Svg.Attributes.height <| toString height ]
        (b :: (c ++ g))

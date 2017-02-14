-- Conway's game of life in Elm by Robert Woodhead (trebor@animeigo.com)
--
-- I used this as a learning tool to get more comfortable with Elm. As such, it
-- is pedantically over-commented, but that may be helpful to other newbies.


port module Main exposing (..)

import Html exposing (Html, div, text, br, button, input, fieldset, p, i, span, output, textarea, ul, li, a, table, tbody, tr, td)
import Html.Attributes as Attr exposing (class, style, type_, checked, cols, rows, name, title, attribute, href, target)
import Html.Events as Events exposing (onClick, onInput)
import Svg exposing (Svg, rect)
import Svg.Attributes as SAttr exposing (x, y, fill, viewBox, id, cursor)
import Time exposing (every, second)
import Json.Decode
import Mouse
import VirtualDom
import Dialog
import List.Extra exposing (transpose)
import Regex exposing (regex, find, Match, HowMany)
import Char
import Random


--import Debug exposing (log)
-----------------------------------------------------
--
-- Types and Constants
--
----------------------------------------------------


type Cell
    = Alive
    | Dead


type alias Board =
    List (List Cell)


type alias BoardCount =
    List (List Int)



-- Size of board in internal coordinate system (scaled when SVG is displayed)
-- Currently, boards always have equal width and height.


boardDim : Int
boardDim =
    4096



-- Minimum cell size in internal coordinate system.


minCellSize : Int
minCellSize =
    8



-- Size range of a board in cells (both horizontal and vertical)
-- These sizes should be divisible by 2 for smooth zooming.


minBoardSize : Int
minBoardSize =
    32


maxBoardSize : Int
maxBoardSize =
    boardDim // minCellSize



-----------------------------------------------------
--
-- Model and Msgs
--
----------------------------------------------------


type Mode
    = Playing
    | Import
    | Export
    | About
    | Patterns


type InputMode
    = UsingMouse
    | UsingTouch


type Direction
    = Up
    | Down
    | Left
    | Right
    | Clockwise


type Msg
    = Tick Time.Time
    | ToggleRun
    | MouseDown Mouse.Position
    | MouseMove Mouse.Position
    | MouseUp Mouse.Position
    | BoardLocation ObjectLocation
    | SetWrap Bool
    | Speed String
    | BoardSize String
    | SetMode Mode
    | SetInputMode InputMode
    | ImportBoard String
    | ProcessImport
    | SetPattern Int
    | Shift Direction
    | ClearBoard
    | MakeSoup
    | Trim


type alias Model =
    { board : Board
    , headers : List String
    , paintRow : Int
    , paintCol : Int
    , paintColor : Cell
    , painting : Bool
    , boardLocation : ObjectLocation
    , wrap : Bool
    , running : Bool
    , speed : Int
    , mode : Mode
    , inputMode : InputMode
    , importedBoard : String
    , seed : Random.Seed
    }



-----------------------------------------------------
--
-- App Initialization
--
----------------------------------------------------
-- We get some initialization information passed to us when we start up,
-- so we can set the inputMode correctly.


type alias Flags =
    { hasTouch : Bool
    , randomSeed : Float
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        -- Use the first builtin board
        pattern =
            List.head builtinBoards

        ( headers, board ) =
            loadBoard
                (case pattern of
                    Just str ->
                        str

                    _ ->
                        ""
                )

        cellsize =
            max minCellSize (boardDim // List.length board)

        -- Set the appropriate input mode for the device
        inputMode =
            if flags.hasTouch then
                UsingTouch
            else
                UsingMouse
    in
        ( { board = board
          , headers = headers
          , paintRow = 0
          , paintCol = 0
          , paintColor = Dead
          , painting = False
          , boardLocation = ObjectLocation 0 0 0 0
          , wrap = False
          , running = False
          , speed = 0
          , mode = Playing
          , inputMode = inputMode
          , importedBoard = ""
          , seed = Random.initialSeed (round flags.randomSeed)
          }
        , initialLocation ()
        )



-- Always listen for incoming messages about board dimension changes,
-- and optionally get ticks if we are free-running the board.
-- In addition, if we are using a mouse, we need to subscribe to
-- some mouse events


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ boardLocation BoardLocation
        , if model.running && model.speed > 0 then
            every (second / toFloat model.speed) Tick
          else
            Sub.none
        , if model.inputMode == UsingMouse && model.mode == Playing then
            if model.painting then
                Sub.batch
                    [ Mouse.ups MouseUp
                    , Mouse.moves MouseMove
                    ]
            else
                Mouse.downs MouseDown
          else
            Sub.none
        ]



-- Data structure returned by the boardLocation port.


type alias ObjectLocation =
    { top : Int, left : Int, width : Int, height : Int }



-- Outbound port for querying the board dimensions.


port initialLocation : () -> Cmd msg



-- Inbound port for receving updates on the dimensions.


port boardLocation : (ObjectLocation -> msg) -> Sub msg



-----------------------------------------------------
--
-- Update
--
----------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Move to next generation (explicit command or subscription tick)
        ToggleRun ->
            let
                running =
                    (not model.running) && (model.speed > 0)

                result =
                    generation
                        { model
                            | running = running
                            , painting = False
                        }
            in
                ( result, Cmd.none )

        Tick time ->
            ( generation model, Cmd.none )

        -- Move between the playing, export and import modes
        SetMode mode ->
            ( { model
                | mode = mode
                , running = False
                , painting = False
              }
            , Cmd.none
            )

        -- Change the input mode (tablet or mouse)
        SetInputMode inputMode ->
            ( { model
                | inputMode = inputMode
                , running = False
                , painting = False
              }
            , Cmd.none
            )

        -- Begin editing the gameboard
        MouseDown position ->
            let
                -- Convert from window coordinates to cell coordinates, taking into
                -- account board position and size. We also need to check if we
                -- don't have a popup dialog on top of us; that is done by
                -- transformCoordinates
                ( row, col, valid ) =
                    transformCoordinates model position

                result =
                    if valid then
                        let
                            -- Toggle a cell and remember what color we set. Turn
                            -- on painting if we not on a touch device; this will
                            -- cause us to subscribe to MouseMove and MouseUp
                            -- events.
                            paintColor =
                                if (getCell model.board row col) == Alive then
                                    Dead
                                else
                                    Alive

                            newboard =
                                setCell model.board row col paintColor

                            painting =
                                model.inputMode == UsingMouse
                        in
                            { model
                                | board = newboard
                                , paintRow = row
                                , paintCol = col
                                , paintColor = paintColor
                                , painting = painting
                            }
                    else
                        model
            in
                ( result, Cmd.none )

        -- Follow the mouse around the gameboard, setting or resetting cells.
        -- We will only get this message between a MouseDown and a MouseUp.
        MouseMove position ->
            let
                ( row, col, valid ) =
                    transformCoordinates model position

                result =
                    if valid then
                        let
                            newboard =
                                setCell model.board row col model.paintColor
                        in
                            { model
                                | board = newboard
                            }
                    else
                        model
            in
                ( result, Cmd.none )

        -- Terminate painting
        MouseUp position ->
            let
                result =
                    { model | painting = False }
            in
                ( result, Cmd.none )

        -- Shift or rotate the board in one of the four directions.
        Shift direction ->
            let
                deadrow =
                    List.repeat (List.length model.board) Dead

                shifted =
                    case direction of
                        Up ->
                            if model.wrap then
                                rotateList 1 model.board
                            else
                                shiftList deadrow 1 model.board

                        Down ->
                            if model.wrap then
                                rotateList -1 model.board
                            else
                                shiftList deadrow -1 model.board

                        Left ->
                            if model.wrap then
                                List.map (rotateList 1) model.board
                            else
                                List.map (shiftList Dead 1) model.board

                        Right ->
                            if model.wrap then
                                List.map (rotateList -1) model.board
                            else
                                List.map (shiftList Dead -1) model.board

                        Clockwise ->
                            List.Extra.transpose model.board
                                |> List.map List.reverse

                result =
                    { model
                        | board = shifted
                        , painting = False
                    }
            in
                ( result, Cmd.none )

        -- Clear the board but maintain size
        ClearBoard ->
            let
                board =
                    List.map (List.map (\c -> Dead)) model.board

                result =
                    { model | board = board, painting = False }
            in
                ( result, Cmd.none )

        MakeSoup ->
            let
                ( seed, board ) =
                    makeSoup model.seed model.board

                result =
                    { model
                        | board = board
                        , seed = seed
                        , painting = False
                    }
            in
                ( result, Cmd.none )

        -- Do we wrap around the edges of the board?
        SetWrap wrap ->
            let
                result =
                    { model
                        | wrap = wrap
                        , painting = False
                    }
            in
                ( result, Cmd.none )

        -- Speed of free run
        Speed v ->
            let
                speed =
                    String.toInt v
                        |> Result.withDefault 0

                result =
                    { model
                        | speed = speed
                        , painting = False
                    }
            in
                ( result, Cmd.none )

        -- Zoom the board
        BoardSize v ->
            let
                -- We always change size by 2 cells so the zooming is even
                halfsize =
                    String.toInt v
                        |> Result.withDefault (maxBoardSize // 2)

                boardsize =
                    max minBoardSize (2 * halfsize)

                curdim =
                    List.length model.board

                board =
                    resizeBoard boardsize model.board

                result =
                    { model
                        | board = board
                        , painting = False
                    }
            in
                ( result, Cmd.none )

        -- Trim the board to the minimum bounding rectangle plus some slop
        Trim ->
            let
                cells =
                    trimBoard model.board

                w =
                    List.head cells
                        |> Maybe.withDefault []
                        |> List.length

                h =
                    List.length cells

                boardsize =
                    (max minBoardSize (8 + max w h))

                board =
                    resizeBoard boardsize cells

                result =
                    { model
                        | board = board
                        , painting = False
                    }
            in
                ( result, Cmd.none )

        -- Update the location of the board (incoming message from port)
        BoardLocation location ->
            let
                result =
                    { model
                        | boardLocation = location
                        , painting = False
                    }
            in
                ( result, Cmd.none )

        -- As changes are made in the import textarea, we get ImportBoard
        -- messages which we use to update the importedBoard string.
        ImportBoard pattern ->
            let
                result =
                    { model | importedBoard = pattern }
            in
                ( result, Cmd.none )

        -- Then when the user clicks the Import button, this message
        -- fires and we actually process the board.
        ProcessImport ->
            let
                ( headers, board ) =
                    loadBoard model.importedBoard

                result =
                    { model
                        | board = board
                        , headers = headers
                        , mode = Playing
                        , importedBoard = ""
                        , painting = False
                    }
            in
                ( result, Cmd.none )

        -- Almost identical to ImportBoard, but we grab a built-in board.
        SetPattern index ->
            let
                pattern =
                    builtinBoards
                        |> List.drop index
                        |> List.head
                        |> Maybe.withDefault ""

                ( headers, board ) =
                    loadBoard pattern

                result =
                    { model
                        | board = board
                        , headers = headers
                        , mode = Playing
                        , importedBoard = ""
                        , painting = False
                    }
            in
                ( result, Cmd.none )



-- Transform from mouse position coordinates to Board row,col coordinates, and
-- check if they are valid. Return a (row,col,valid) tuple.


transformCoordinates : Model -> Mouse.Position -> ( Int, Int, Bool )
transformCoordinates model position =
    let
        lmb =
            List.length model.board

        bdf =
            toFloat boardDim

        cs =
            bdf / toFloat lmb

        sx =
            bdf / toFloat model.boardLocation.width

        sy =
            bdf / toFloat model.boardLocation.height

        x =
            sx * toFloat (position.x - model.boardLocation.left)

        y =
            sy * toFloat (position.y - model.boardLocation.top)

        col =
            floor (x / cs)

        row =
            floor (y / cs)

        valid =
            col >= 0 && col < lmb && row >= 0 && row < lmb && model.mode == Playing
    in
        ( row, col, valid )



-----------------------------------------------------
--
-- Life Functions
--
----------------------------------------------------
--
-- Step a generation


type alias VerticalCountShifter =
    Int -> List (List Int) -> List (List Int)


type alias HorizontalCountShifter =
    Int -> List Int -> List Int


generation : Model -> Model
generation model =
    let
        mbs =
            List.length model.board

        -- If we are not wrapping the board, then we may need to expand it.
        --
        -- Note: one nice thing about let blocks is that you can define local
        -- functions inside of them. I have a personal convention of always
        -- ending them with the _ character as a cue about where to look.
        startBoard =
            if (not model.wrap) && (mbs < maxBoardSize) && hasEdgeCell_ model.board then
                resizeBoard (mbs + 2) model.board
            else
                model.board

        -- Curry the vertical and horizontal count shifter functions. This
        -- lets us use a single function to count all the neighboring cells
        ( vshift, hshift ) =
            if model.wrap then
                ( rotateList
                , rotateList
                )
            else
                ( shiftList (List.repeat (List.length startBoard) 0)
                , shiftList 0
                )

        -- Count the neighboring cells, passing in the shifting functions
        neighbors =
            countCells vshift hshift startBoard

        -- Evolve the next generation
        newBoard =
            evolveBoard startBoard neighbors

        result =
            { model | board = newBoard }

        -- Does the board have a live cell on its perimeter?
        hasEdgeCell_ : Board -> Bool
        hasEdgeCell_ board =
            let
                bsm1 =
                    mbs - 1

                top =
                    case List.head board of
                        Just row ->
                            List.member Alive row

                        Nothing ->
                            False

                bottom =
                    case List.head (List.drop bsm1 board) of
                        Just row ->
                            List.member Alive row

                        Nothing ->
                            False

                sides =
                    List.foldl rowEdgeCell_ False board

                -- Does a row have a live cell on its edges? Note that this is
                -- a local function of a local function!
                rowEdgeCell_ : List Cell -> Bool -> Bool
                rowEdgeCell_ row carry =
                    let
                        left =
                            case List.head row of
                                Just cell ->
                                    cell == Alive

                                Nothing ->
                                    False

                        right =
                            case List.head (List.drop bsm1 row) of
                                Just cell ->
                                    cell == Alive

                                Nothing ->
                                    False
                    in
                        carry || left || right
            in
                top || bottom || sides
    in
        result



-- Set each element in the board to the count of live cells
-- in itself and its eight neighbors, with optional wrapping
-- at the edges. Note that this changes the survival
-- condition because the count includes the potentially
-- surviving cell.


countCells : VerticalCountShifter -> HorizontalCountShifter -> Board -> BoardCount
countCells vshift hshift board =
    let
        -- Convert Cells to integer counts
        boardcount =
            List.map toRowCount_ board

        -- Create left and right shifted or rotated boards
        rl =
            List.map (hshift 1) boardcount

        rr =
            List.map (hshift -1) boardcount

        -- Sum the board with the shifted versions
        rowNeighbors =
            List.map3 rowSum_ boardcount rl rr

        -- Create up and down shifted counts
        ru =
            vshift 1 rowNeighbors

        rd =
            vshift -1 rowNeighbors

        -- Sum everything (the final result)
        result =
            List.map3 rowSum_ rowNeighbors rd ru

        -- Cell to Integer conversion
        toRowCount_ : List Cell -> List Int
        toRowCount_ row =
            List.map
                (\cell ->
                    if cell == Alive then
                        1
                    else
                        0
                )
                row

        -- Add up 3 lists
        rowSum_ : List Int -> List Int -> List Int -> List Int
        rowSum_ row adj1 adj2 =
            List.map3 cellSum_ row adj1 adj2

        -- Add 3 numbers
        cellSum_ : Int -> Int -> Int -> Int
        cellSum_ cell adj1 adj2 =
            cell + adj1 + adj2
    in
        result



-- Cell Getter and Setter


getCell : Board -> Int -> Int -> Cell
getCell board row col =
    board
        |> List.drop row
        |> List.head
        |> Maybe.withDefault []
        |> List.drop col
        |> List.head
        |> Maybe.withDefault Dead


setCell : Board -> Int -> Int -> Cell -> Board
setCell board row col newcell =
    let
        setRow_ rownum therow =
            if rownum /= row then
                therow
            else
                List.indexedMap setColumn_ therow

        setColumn_ colnum cell =
            if colnum /= col then
                cell
            else
                newcell
    in
        List.indexedMap setRow_ board



-- These rotate and shift functions have generic type signatures.
-- This is because we also use them when the user shifts/rotates the
-- board, so they need to handle List Cell and List Int.


rotateList : Int -> List a -> List a
rotateList rot list =
    let
        r =
            rot % (List.length list)
    in
        (List.drop r list) ++ (List.take r list)


shiftList : a -> Int -> List a -> List a
shiftList blank rot list =
    if rot >= 0 then
        (List.drop rot list) ++ (List.repeat rot blank)
    else
        (List.repeat -rot blank) ++ (List.take ((List.length list) + rot) list)



-- Evolve a board. A dead cell becomes alive if it has 3 and only 3
-- neighboring live cells; a live cell survives if it has 2 or 3
-- neighboring live cells.


evolveBoard : Board -> BoardCount -> Board
evolveBoard board neighbors =
    let
        -- Neighbors counts include the current cell, so the "survive"
        -- states are not quite what you expect.
        evolveRow_ : List Cell -> List Int -> List Cell
        evolveRow_ row neighbors =
            List.map2 evolveCell_ row neighbors

        evolveCell_ : Cell -> Int -> Cell
        evolveCell_ cell neighbors =
            if neighbors == 3 || ((cell == Alive) && (neighbors == 4)) then
                Alive
            else
                Dead
    in
        List.map2 evolveRow_ board neighbors



-- Make a soup board of 11.111% live cells. This is about the right ratio to
-- maximize generation of interesting patterns according to this note:
-- https://sourceforge.net/p/golly/discussion/467856/thread/2ea0e461/
--
-- To ensure a different pattern each time, we pass in the current model.seed
-- value and return the updated value. This is how Random generators maintain
-- their state between invocations.


makeSoup : Random.Seed -> Board -> ( Random.Seed, Board )
makeSoup seed board =
    let
        -- Random generator that produces an Alive cell 1/9th of the time.
        soupCell =
            Random.map
                (\i ->
                    if i == 0 then
                        Alive
                    else
                        Dead
                )
                (Random.int 0 8)

        -- size of the board
        dim =
            List.length board

        -- Random generator that produces a row of soup cells
        soupRow =
            Random.list dim soupCell

        -- Random generator that produces a board of soup cells
        soupBoard =
            Random.list dim soupRow

        -- Create a soup board and a new seed
        ( newboard, newseed ) =
            Random.step soupBoard seed
    in
        ( newseed, newboard )



-- Load a board. First we try RLE format, and if that doesn't work, we
-- try Cell format.


loadBoard : String -> ( List String, Board )
loadBoard pattern =
    let
        ( h1, c1 ) =
            decodeRLE pattern

        ( headers, cells ) =
            if c1 == [] then
                decodeCELL pattern
            else
                ( h1, c1 )

        w =
            List.head cells
                |> Maybe.withDefault []
                |> List.length

        h =
            List.length cells

        boardsize =
            (max minBoardSize (8 + max w h))

        board =
            resizeBoard boardsize cells
    in
        ( headers, board )



-- Decode a board from the LifeWiki .cell format into a list of the headers
-- and a Board. See: http://www.conwaylife.com/wiki/Plaintext


decodeCELL : String -> ( List String, Board )
decodeCELL template =
    let
        -- Kill leading and trailing whitespace, then split into headers and actual cells.
        ( comments, boardcells ) =
            template
                |> String.trim
                |> String.lines
                |> List.partition (String.startsWith "!")

        -- Get rid of the ! characters on the header lines.
        headers =
            List.map (String.dropLeft 1) comments

        -- Get maximum width.
        mw =
            List.map String.length boardcells
                |> List.maximum
                |> Maybe.withDefault 0

        -- Pad short rows to the max width.
        padshort =
            List.map (String.padRight mw '.') boardcells

        -- The basic board, trimmed to minimum size.
        board =
            List.map rowToCell_ padshort

        -- Local helper functions.
        rowToCell_ : String -> List Cell
        rowToCell_ row =
            List.map strToCell_ (String.toList row)

        strToCell_ : Char -> Cell
        strToCell_ char =
            if (char == '.') || (char == ' ') then
                Dead
            else
                Alive
    in
        ( headers, board )



-- Decode a board from the LifeWiki .RLE format into a list of the headers
-- and a Board. See: http://www.conwaylife.com/wiki/RLE


decodeRLE : String -> ( List String, Board )
decodeRLE template =
    let
        -- Kill blank lines, leading and trailing whitespace, then split into headers and pattern.
        ( comments, pattern ) =
            template
                |> String.lines
                |> List.filter (\s -> s /= "")
                |> List.map String.trim
                |> List.partition (String.startsWith "#")

        -- Remap RLE headers to .CELL-style headers.
        headers =
            comments
                |> List.map fixRLEHeaders
                |> List.filter (\s -> s /= "")

        -- First line of pattern must be size header. I'm going crazy with pattern-matching case
        -- statements here as a learning exercise.
        board =
            case pattern of
                header :: body ->
                    let
                        -- Extract the size of the pattern from the header.
                        xy =
                            header
                                |> Regex.split (Regex.All) (Regex.regex "[\\D]+")
                                |> List.drop 1
                                |> List.map String.toInt
                                |> List.map (Result.withDefault 0)
                                |> flip List.append [ 0, 0 ]
                                |> List.take 2
                    in
                        case xy of
                            -- Failure: we got an invalid # of columns.
                            0 :: rest ->
                                []

                            -- Failure: an invalid # of rows.
                            _ :: 0 :: rest ->
                                []

                            -- Success, we have at least 2 values, both non-zero. We might have
                            -- extra items if the size line contains a rule parameter.
                            x :: y :: rest ->
                                let
                                    -- Grab the rle code, decode the lines into .cell format, then
                                    -- use decodeCell to do the rest of the work.
                                    ( _, converted ) =
                                        body
                                            |> String.concat
                                            |> String.split "!"
                                            |> List.head
                                            |> Maybe.withDefault ""
                                            |> String.toList
                                            |> rleToCell "" 0
                                            |> decodeCELL
                                in
                                    converted

                            -- Failure: anything else.
                            _ ->
                                []

                _ ->
                    []
    in
        ( headers, board )



-- Convert RLE to CELL format. This is basically a fancy foldl with two accumulators,
-- one for the output string, and one for the current multiplier. If the input RLE
-- is bad the results are undefined.


rleToCell : String -> Int -> List Char -> String
rleToCell acc mult rle =
    case rle of
        cur :: rest ->
            case cur of
                -- Emit mult blank cells (minimum of 1), reset mult to 0, continue.
                'b' ->
                    rleToCell (acc ++ String.repeat (max mult 1) ".") 0 rest

                -- Emit mult live cells (minimum of 1), reset mult to 1, continue.
                'o' ->
                    rleToCell (acc ++ String.repeat (max mult 1) "O") 0 rest

                -- Emit mult endoflines (minimum of 1), reset mult to 0, continue.
                '$' ->
                    rleToCell (acc ++ String.repeat (max mult 1) "\n") 0 rest

                -- If we have a digit, shift it into the mult, continue.
                -- If it isn't a digit, just continue.
                _ ->
                    if Char.isDigit cur then
                        let
                            digit =
                                cur
                                    |> String.fromChar
                                    |> String.toInt
                                    |> Result.withDefault 0
                        in
                            rleToCell acc (mult * 10 + digit) rest
                    else
                        rleToCell acc mult rest

        -- If we reach the end, then we are done.
        [] ->
            acc



-- Remap RLE-style headers to CELL-style.


fixRLEHeaders : String -> String
fixRLEHeaders hdr =
    let
        remap_ : String -> String -> String -> String
        remap_ prefix repl line =
            if String.startsWith prefix (String.toUpper line) then
                line
                    |> String.dropLeft (String.length prefix)
                    |> String.trim
                    |> (++) repl
            else
                line

        kill_ : String -> String -> String
        kill_ prefix line =
            if String.startsWith prefix (String.toUpper line) then
                ""
            else
                line
    in
        hdr
            |> remap_ "#C" "!"
            |> remap_ "#N" "!Name: "
            |> remap_ "#O" "!Author: "
            |> kill_ "#"



-- Resize a board to a desired (square) dimension.


resizeBoard : Int -> Board -> Board
resizeBoard tosize board =
    let
        h =
            List.length board

        w =
            case List.head board of
                Just row ->
                    List.length row

                Nothing ->
                    0

        -- Figure out the magnitude of the width change
        wd =
            abs (tosize - w)

        -- Split into left and right halves, but try to balance where the remainder goes,
        -- if we happen to be changing by an odd number of cells (which we shouldn't!)
        ( l, r ) =
            if rem tosize 2 == 0 then
                ( wd // 2, wd - (wd // 2) )
            else
                ( wd - (wd // 2), wd // 2 )

        -- Adjust width of rows
        rewidthed =
            if tosize > w then
                List.map padRow_ board
            else if tosize < w then
                List.map trimRow_ board
            else
                board

        -- Now do the same for the height change
        ld =
            abs (tosize - h)

        ( t, b ) =
            if rem tosize 2 == 0 then
                ( ld // 2, ld - (ld // 2) )
            else
                ( ld - (ld // 2), ld // 2 )

        deadrow =
            (List.repeat tosize Dead)

        result =
            if tosize > h then
                List.concat
                    [ List.repeat t deadrow
                    , rewidthed
                    , List.repeat b deadrow
                    ]
            else if tosize < h then
                rewidthed
                    |> List.drop t
                    |> List.take tosize
            else
                rewidthed

        -- Note that our local functions have access to values
        -- defined in the let block, so we don't need to pass them!
        padRow_ : List Cell -> List Cell
        padRow_ row =
            List.concat
                [ List.repeat l Dead
                , row
                , List.repeat r Dead
                ]

        trimRow_ : List Cell -> List Cell
        trimRow_ row =
            row
                |> List.drop l
                |> List.take tosize
    in
        result



-- Trim a board to its minimum bounding rectangle


trimBoard : Board -> Board
trimBoard board =
    let
        -- Take the board, and:
        --    Remove any Dead rows at its start.
        --    Flip the rows, repeat, then flip back.
        --    Remove columns if all entries are Dead.
        --    Flip the columns inside each row, repeat
        --    and then flip back.
        result =
            board
                |> trimRows_
                |> List.reverse
                |> trimRows_
                |> List.reverse
                |> trimCols_
                |> List.map List.reverse
                |> trimCols_
                |> List.map List.reverse

        -- Remove empty rows until non-empty row found.
        trimRows_ : Board -> Board
        trimRows_ board =
            case board of
                -- Gotta love the :: pattern match!
                row :: rest ->
                    if List.member Alive row then
                        board
                    else
                        trimRows_ rest

                [] ->
                    []

        -- Remove empty columns until non-empty column found
        -- A little trickier since we have to look inside the
        -- row lists. Also need to be aware of the gotcha of
        -- a completely empty list!
        trimCols_ : Board -> Board
        trimCols_ board =
            let
                fc =
                    firstColumn_ board
            in
                if List.isEmpty fc || List.member Alive (firstColumn_ board) then
                    board
                else
                    List.map (List.drop 1) board
                        |> trimCols_

        -- Make a list of the initial cells in each row of a Board.
        firstColumn_ : Board -> List Cell
        firstColumn_ board =
            List.map
                (\r -> Maybe.withDefault Dead (List.head r))
                board
    in
        result



-- Convert board to .cell text representation.


boardToStr : Board -> String
boardToStr board =
    let
        rowToStr_ : List Cell -> String
        rowToStr_ row =
            List.map
                (\c ->
                    if c == Alive then
                        'O'
                    else
                        '.'
                )
                row
                |> String.fromList
                |> dotTrim_

        dotTrim_ : String -> String
        dotTrim_ s =
            if String.endsWith "." s then
                dotTrim_ (String.dropRight 1 s)
            else
                s
    in
        List.map rowToStr_ board
            |> String.join "\n"



-----------------------------------------------------
--
-- View -- uses Bootstrap.css with some custom css tweaks
-- in app.css for things like styling the sliders and
-- tweaking buttons and toolbars.
--
----------------------------------------------------
--
-- view helpers


slider : (String -> msg) -> String -> Int -> Int -> Int -> Html msg
slider msg cname minv maxv value =
    input
        [ type_ "range"
        , name cname
        , title cname
        , attribute "min-width" "100px"
        , attribute "max-width" "100px"
        , Attr.min (toString minv)
        , Attr.max (toString maxv)
        , Attr.value (toString value)
        , onInput msg
        ]
        []


link : String -> String -> Html Msg
link url label =
    if String.startsWith "mailto:" url then
        a [ href url, target "_blank" ] [ text label ]
    else
        a [ href url ] [ text label ]



-- Select is not currently used in the app, but I'm keeping it handy just in case...


select : (String -> msg) -> List ( String, String, Bool ) -> Html msg
select msg items =
    Html.select
        [ class "form-control"
        , onInput msg
        ]
        (List.map
            selectOption
            items
        )


selectOption : ( String, String, Bool ) -> Html msg
selectOption ( v, l, s ) =
    Html.option [ Attr.value v, Attr.selected s ] [ text l ]



-- Helper function: everyone's favorite idiom for use in style lists.
-- instead of ("bork","derf") you can write "bork" => "derf"


(=>) : a -> b -> ( a, b )
(=>) =
    (,)



-- Click handler that gives us the global mouse position.


offsetPosition : Json.Decode.Decoder Mouse.Position
offsetPosition =
    Json.Decode.map2 Mouse.Position (Json.Decode.field "pageX" Json.Decode.int) (Json.Decode.field "pageY" Json.Decode.int)


mouseEvent : String -> (Mouse.Position -> msg) -> VirtualDom.Property msg
mouseEvent event messager =
    let
        options =
            { preventDefault = True, stopPropagation = True }
    in
        VirtualDom.onWithOptions event options (Json.Decode.map messager offsetPosition)


mouseClick : (Mouse.Position -> msg) -> VirtualDom.Property msg
mouseClick =
    mouseEvent "click"



-- SVG board.


boardView : Model -> Html Msg
boardView model =
    let
        lb =
            List.length model.board

        cellsize =
            boardDim // lb

        boardWidth =
            cellsize * lb

        bw =
            toString boardWidth

        bd =
            toString boardDim

        ctype =
            if (model.boardLocation.height // lb) < 16 then
                "crosshair"
            else
                "cell"

        attrs =
            [ cursor ctype
            , viewBox ("0 0 " ++ bw ++ " " ++ bw)
            , id "board"
            , style
                [ "background-color" => "lightgrey"
                ]
            ]
    in
        div
            [ class "row" ]
            [ div
                [ class "col-xs-12" ]
                [ Svg.svg
                    (if model.inputMode == UsingTouch then
                        [ mouseClick MouseDown ] ++ attrs
                     else
                        attrs
                    )
                    (boardSvg cellsize model.board)
                ]
            ]


boardSvg : Int -> Board -> List (Svg Msg)
boardSvg csize board =
    List.indexedMap (rowSvg csize) board
        |> List.concat


rowSvg : Int -> Int -> List Cell -> List (Svg Msg)
rowSvg csize row cells =
    List.indexedMap (cellSvg csize row) cells
        |> List.filterMap identity


cellSvg : Int -> Int -> Int -> Cell -> Maybe (Svg Msg)
cellSvg csize ypos xpos cell =
    if cell == Alive then
        Just
            (rect
                [ SAttr.width (toString (csize - 4))
                , SAttr.height (toString (csize - 4))
                , x (toString (xpos * csize))
                , y (toString (ypos * csize))
                , fill "black"
                ]
                []
            )
    else
        Nothing



-- Controls.


controlsView : Model -> Html Msg
controlsView model =
    div
        [ class "row" ]
        [ runningControls model
        , editingControls model
        , miscControls model
        ]


runningControls : Model -> Html Msg
runningControls model =
    div
        [ class "col-xs-3" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col-xs-12 text-center center-block btn-toolbar btn-toolbar-centered" ]
                [ div
                    [ class "btn-group"
                    , style [ "margin-right" => "5px" ]
                    ]
                    [ button
                        [ class
                            (if model.running then
                                "btn btn-danger"
                             else
                                "btn btn-primary"
                            )
                        , onClick ToggleRun
                        ]
                        [ text
                            (if model.running then
                                "Stop"
                             else if model.speed == 0 then
                                "Step"
                             else
                                "Run"
                            )
                        ]
                    , if model.wrap then
                        button
                            [ class "btn btn-default"
                            , title "Edges of the board wrap around"
                            , onClick (SetWrap False)
                            ]
                            [ span [ class "glyphicon glyphicon-transfer" ] []
                            ]
                      else
                        button
                            [ class "btn btn-default"
                            , title "Edges of the board expand as needed"
                            , onClick (SetWrap True)
                            ]
                            [ span [ class "glyphicon glyphicon-fullscreen" ] []
                            ]
                    , if model.inputMode == UsingMouse then
                        button
                            [ class "btn btn-default"
                            , title "Using a mouse; click, hold and move to change the board"
                            , onClick (SetInputMode UsingTouch)
                            ]
                            [ span [ class "glyphicon glyphicon-pencil" ] []
                            ]
                      else
                        button
                            [ class "btn btn-default"
                            , title "On a tablet; click to toggle cells"
                            , onClick (SetInputMode UsingMouse)
                            ]
                            [ span [ class "glyphicon glyphicon-hand-up" ] []
                            ]
                    ]
                ]
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-xs-12 text-center center-block" ]
                [ br [] []
                , slider Speed "Slide to change generation speed (step, slower <-> faster)" 0 20 model.speed
                ]
            ]
        ]


editingControls : Model -> Html Msg
editingControls model =
    div [ class "col-xs-5" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col-xs-12 text-center center-block btn-toolbar btn-toolbar-centered" ]
                [ div
                    [ class "btn-group"
                    , style [ "margin-right" => "5px" ]
                    ]
                    [ button
                        [ class "btn btn-default", onClick (Shift Up), title "Shift Up" ]
                        [ span [ class "glyphicon glyphicon-arrow-up" ] [] ]
                    , button
                        [ class "btn btn-default", onClick (Shift Down), title "Shift Down" ]
                        [ span [ class "glyphicon glyphicon-arrow-down" ] [] ]
                    , button
                        [ class "btn btn-default", onClick (Shift Left), title "Shift Left" ]
                        [ span [ class "glyphicon glyphicon-arrow-left" ] [] ]
                    , button
                        [ class "btn btn-default", onClick (Shift Right), title "Shift Right" ]
                        [ span [ class "glyphicon glyphicon-arrow-right" ] [] ]
                    , button
                        [ class "btn btn-default", onClick (Shift Clockwise), title "Rotate Clockwise" ]
                        [ span [ class "glyphicon glyphicon-repeat" ] [] ]
                    , button
                        [ class "btn btn-default", onClick Trim, title "Trim to fit" ]
                        [ span [ class "glyphicon glyphicon-resize-small" ] [] ]
                    ]
                , div
                    [ class "btn-group"
                    , style [ "margin-right" => "5px" ]
                    ]
                    [ button
                        [ class "btn btn-default dropdown-toggle"
                        , attribute "data-toggle" "dropdown"
                        , title "Misc. Options"
                        ]
                        [ span [ class "glyphicon glyphicon-option-vertical" ] []
                        , span [ class "caret" ] []
                        ]
                    , ul [ class "dropdown-menu" ]
                        [ li []
                            [ a [ href "#", onClick ClearBoard ] [ text "Clear Board" ]
                            ]
                        , li []
                            [ a [ href "#", onClick MakeSoup ] [ text "Make Soup" ]
                            ]
                        ]
                    ]
                ]
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-xs-12 text-center center-block greyslider" ]
                [ br [] []
                , slider BoardSize
                    "Slide to zoom board (in <-> out)"
                    (minBoardSize // 2)
                    (maxBoardSize // 2)
                    ((List.length model.board) // 2)
                ]
            ]
        ]


miscControls : Model -> Html Msg
miscControls model =
    div [ class "col-xs-4" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col-xs-12 text-center center-block btn-toolbar btn-toolbar-centered" ]
                [ div
                    [ class "btn-group"
                    , style [ "margin-right" => "5px" ]
                    ]
                    [ button
                        [ class "btn btn-default", onClick (SetMode Import), title "Import from .CELL or .RLE notation…" ]
                        [ span [ class "glyphicon glyphicon-import" ] [] ]
                    , button
                        [ class "btn btn-default", onClick (SetMode Export), title "Export in .CELL notation…" ]
                        [ span [ class "glyphicon glyphicon-export" ] [] ]
                    , button
                        [ class "btn btn-default", onClick (SetMode Patterns), title "List of interesting patterns…" ]
                        [ span [ class "glyphicon glyphicon-list" ] [] ]
                    , button
                        [ class "btn btn-info", onClick (SetMode About), title "About this app…" ]
                        [ span [ class "glyphicon glyphicon-question-sign" ] [] ]
                    ]
                ]
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-xs-12 text-center center-block" ]
                [ Html.h4 []
                    [ link "https://en.wikipedia.org/wiki/Conway's_Game_of_Life" "Conway's Game of Life"
                    , text " in "
                    , link "http://elm-lang.org/" "Elm"
                    ]
                ]
            ]
        ]



-- Popup dialogs.


dialogsView : Model -> Html Msg
dialogsView model =
    Dialog.view
        (case model.mode of
            Import ->
                importExport
                    (Just ImportBoard)
                    (Just
                        (button
                            [ class "btn btn-primary", onClick ProcessImport, title "Import the board" ]
                            [ text "Import" ]
                        )
                    )
                    False
                    "Enter .CELL or .RLE pattern below, then click Import:"
                    ""

            Export ->
                model.board
                    |> trimBoard
                    |> boardToStr
                    |> (++) exportHeader
                    |> importExport
                        Nothing
                        Nothing
                        True
                        "Copy .CELL pattern below to export:"

            About ->
                aboutUs

            Patterns ->
                patternsList

            Playing ->
                Nothing
        )



-- Import or Export .cell pattern.


importExport : Maybe (String -> Msg) -> Maybe (Html Msg) -> Bool -> String -> String -> Maybe (Dialog.Config Msg)
importExport msg footer readonly caption contents =
    let
        textAttributes =
            [ style
                [ "display" => "block"
                , "margin-left" => "auto"
                , "margin-right" => "auto"
                , "font-family" => "courier,monospace"
                ]
            , Attr.readonly readonly
            , class ""
            , cols 80
            , rows 20
            ]
    in
        Just
            { closeMessage = Just (SetMode Playing)
            , containerClass = Just "elm-dialog"
            , header = Just (span [ class "h2" ] [ text caption ])
            , body =
                Just
                    (textarea
                        (case msg of
                            Just message ->
                                [ onInput message ] ++ textAttributes

                            Nothing ->
                                textAttributes
                        )
                        [ text contents ]
                    )
            , footer = footer
            }



-- App help.


aboutUs : Maybe (Dialog.Config Msg)
aboutUs =
    Just
        { closeMessage = Just (SetMode Playing)
        , containerClass = Just "elm-dialog"
        , header = Just (span [ class "h2" ] [ link "https://en.wikipedia.org/wiki/Conway's_Game_of_Life" "Conway's Game of Life" ])
        , body =
            Just
                (div
                    []
                    [ div [ class "h4" ] [ text "The Rules of Life:" ]
                    , ul []
                        [ li [] [ text "Any live cell with fewer than two live neighbours dies, as if caused by underpopulation." ]
                        , li [] [ text "Any live cell with two or three live neighbours lives on to the next generation." ]
                        , li [] [ text "Any live cell with more than three live neighbours dies, as if by overpopulation." ]
                        , li [] [ text "Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction." ]
                        ]
                    , div [ class "h4" ] [ text "Useful Tips:" ]
                    , ul []
                        [ li [] [ text "The white slider changes how fast new generations are computed; if it is all the way to the left, then it single-steps." ]
                        , li []
                            [ text "You can choose whether the edges of the board wrap around ( "
                            , span [ class "glyphicon glyphicon-transfer" ] []
                            , text " ) or the board can expand to fit the pattern ( "
                            , span [ class "glyphicon glyphicon-fullscreen" ] []
                            , text " ). Just click the button to toggle."
                            ]
                        , li []
                            [ text "You may use either Tablet ( "
                            , span [ class "glyphicon glyphicon-hand-up" ] []
                            , text " ) or Mouse ( "
                            , span [ class "glyphicon glyphicon-pencil" ] []
                            , text " ) input mode. In Mouse-mode, you can hold the mouse button down and move the mouse to "
                            , text "set or clear multiple cells. In Tablet-mode, each tap or click toggles a single cell."
                            ]
                        , li []
                            [ text "The grey slider changes the size of the board. "
                            , text "The buttons above it shift and rotate the board. "
                            , text "You can also trim ( "
                            , span [ class "glyphicon glyphicon-resize-small" ] []
                            , text " ) the board to fit the current pattern. "
                            , text "Finally, the Options button ( "
                            , span [ class "glyphicon glyphicon-option-vertical" ] []
                            , text " ) lets you clear the board and make random soup."
                            ]
                        , li []
                            [ text "You can import ( "
                            , span [ class "glyphicon glyphicon-import" ] []
                            , text " ) and export ( "
                            , span [ class "glyphicon glyphicon-export" ] []
                            , text " ) patterns in .CELL or .RLE format. A good source for patterns is the "
                            , link "http://www.conwaylife.com/wiki/Main_Page" "Life Wiki"
                            , text ". You can also choose some interesting patterns by clicking on "
                            , span [ class "glyphicon glyphicon-list" ] []
                            , text "."
                            ]
                        , li []
                            [ text "Click on "
                            , span [ class "badge" ] [ text "Explore History" ]
                            , text " to open Elm's time-travelling debugger. You can rewind the application to any prior state!"
                            ]
                        ]
                    , br [] []
                    , text "Implemented in "
                    , link "http://elm-lang.org" "Elm"
                    , text " by Robert Woodhead ("
                    , link "mailto:trebor@animeigo.com?Subject=Life-in-Elm" "trebor@animeigo.com"
                    , text ")"
                    ]
                )
        , footer = Nothing
        }



-- Built-in pattern selection.


patternsList : Maybe (Dialog.Config Msg)
patternsList =
    Just
        { closeMessage = Just (SetMode Playing)
        , containerClass = Just "elm-dialog"
        , header = Just (span [ class "h2" ] [ text "Interesting Life Patterns" ])
        , body =
            Just
                (div [ class "pre-scrollable" ]
                    [ table [ class "table table-striped" ]
                        [ tbody []
                            (List.indexedMap patternRow builtinBoards)
                        ]
                    ]
                )
        , footer = Nothing
        }


patternRow : Int -> String -> Html Msg
patternRow index pattern =
    let
        ( name, author, description, url ) =
            patternInfo pattern
    in
        tr []
            [ td
                []
                [ div
                    [ style [ "cursor" => "pointer" ] ]
                    [ Html.b [] [ text name ]
                    , if author == "" then
                        text ""
                      else
                        span []
                            [ text " by "
                            , i [] [ text author ]
                            ]
                    , text ". "
                    , text description
                    , if url == "" then
                        text ""
                      else
                        span []
                            [ text " ("
                            , link ("http" ++ url) "More Info…"
                            , text ")"
                            ]
                    ]
                ]
            , td
                []
                [ button
                    [ class "btn btn-primary", onClick (SetPattern index), title ("Load " ++ name) ]
                    [ text "Load" ]
                ]
            ]



-- Extract header information from a pattern.


patternInfo : String -> ( String, String, String, String )
patternInfo pattern =
    let
        -- Hack to fix url lines without the http://
        fixed =
            pattern
                |> String.split "!www."
                |> String.join "!http://www."

        name =
            patternGet "!Name:" fixed

        author =
            patternGet "!Author:" fixed

        url =
            patternGet "!http" fixed

        description =
            List.filter patternDescription (String.lines fixed)
                |> List.map (String.dropLeft 1)
                |> String.join " "
    in
        ( name, author, description, url )


patternDescription : String -> Bool
patternDescription line =
    String.startsWith "!" line
        && not (String.startsWith "!Name:" line)
        && not (String.startsWith "!Author:" line)
        && not (String.startsWith "!http" line)


patternGet : String -> String -> String
patternGet prefix pattern =
    pattern
        |> String.lines
        |> List.filter (String.startsWith prefix)
        |> List.map (String.dropLeft (String.length prefix))
        |> String.join " "



-- Main view.


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ boardView model
        , controlsView model
        , dialogsView model
        ]



-- Sample boards.


builtinBoards : List String
builtinBoards =
    [ """
!Name: Gosper glider gun
!Author: Bill Gosper
!The first known gun and the first known finite pattern with unbounded growth.
!Generates “gliders”, patterns that replicate themselves in slightly shifted
!positions.
!www.conwaylife.com/wiki/index.php?title=Gosper_glider_gun
........................O
......................O.O
............OO......OO............OO
...........O...O....OO............OO
OO........O.....O...OO
OO........O...O.OO....O.O
..........O.....O.......O
...........O...O
............OO
"""
    , """
!Name: Puffer 1
!Author: Bill Gosper
!An orthogonal, period-128 puffer and the first puffer to be discovered.
!www.conwaylife.com/wiki/index.php?title=Puffer_1
.OOO......O.....O......OOO
O..O.....OOO...OOO.....O..O
...O....OO.O...O.OO....O
...O...................O
...O..O.............O..O
...O..OO...........OO..O
..O...OO...........OO...O
"""
    , """
!Name: Various Oscillators
!These patterns repeat after various numbers of generations.
!www.conwaylife.com/wiki/Category:Oscillators
...............O
..............O.O
..............OO.O
..OO..........OO.OO...OO...........OO
..OO..........OO.O....OO...........OO
..............O.O
...............O






................OO.................OOO...OOO
...................................O..O.O..O
.O...OO....O..O...O................O.......O
OO...O.O...O..O....O
OO....OO...O....O.O.O...............O.....O
O................O.O.O...............OO.OO
..................O....O
...................O...O

....................OO


......O.....O
......O.....O
......OO...OO

..OOO..OO.OO..OOO
....O.O.O.O.O.O....................OO.....OO
......OO...OO......................OO.....OO

......OO...OO
....O.O.O.O.O.O
..OOO..OO.OO..OOO

......OO...OO
......O.....O
......O.....O
"""
    , """
!Name: Rectifier
!Author: Adam P. Goucher
!A 180-degree stable reflector with a recovery time of 106 generations.
!www.conwaylife.com/wiki/index.php?title=Rectifier
.O.O.......O
..OO......O.O
..O.......O.O
...........O











....................OO
....................OO

..OO
.O.O
.O
OO
...............................OO
..............................O..O..OO
..............................O.O....O
...........OO..................O.....O.OO
..........O.O.....................OO.O.O
..........O.......................O..O..O
.........OO....................O....O..OO
...............................OOOOO

.................................OO.O
.................................O.OO
"""
    , """
!Name: Herschel
!A heptomino shaped like the lowercase letter h, which occurs at generation 20 of the B-heptomino.
!An example of a “Methuselah” pattern that generates a large object before it finally stabilizes,
!typically after emitting one or more gliders.
!www.conwaylife.com/wiki/index.php?title=Herschel
O
OOO
O.O
..O
"""
    , """
!Name: Acorn
!Author: Charles Corderman
!A methuselah that stabilizes after 5206 generations.
!www.conwaylife.com/wiki/index.php?title=Acorn
.O
...O
OO..OOO
"""
    , """
!Name: Noah's ark
!Author: Charles Corderman
!A diagonal puffer made up of two switch engines that was found in 1971.
!A puffer is a pattern that moves itself but leaves debris behind.
!www.conwaylife.com/wiki/Noah%27s_ark
..........O.O
.........O
..........O..O
............OOO





.O
O.O

O..O
..OO
...O
"""
    ]


exportHeader : String
exportHeader =
    """!Name: [Fill In]
!Author: [Fill In]
!
!Computed by Elm-Life
"""

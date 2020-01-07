module Main exposing (main)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  {
    inputList: List String, 
    intCode : String
  }


init : Model
init =
  Model [] ""


-- UPDATE
type Msg 
    = UpdateList String
    | UpdateIntcode String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateList input ->
            { model | inputList = String.split "\n" input}
       
        UpdateIntcode input ->
            { model | intCode = input}



-- VIEW
stringsToInts : List String -> List Int
stringsToInts ints =
    List.map (\a -> Maybe.withDefault 0 (a |> String.trim |> String.toInt)) ints

-- Fuel required to launch a given module is based on its mass. Specifically, to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2.
calculateFuel : Int -> Int
calculateFuel mass =
    floor (toFloat mass / 3) - 2
    
calculateAll : List String -> Int
calculateAll inputs =
    List.sum (List.map calculateFuel (stringsToInts inputs))

calculateAllPart2 : List String -> Int
calculateAllPart2 inputs =
    let
        recurse : Int -> Int -> Int
        recurse mass sum =
            let
                needed = calculateFuel mass
            in
            if needed > 0 then
                recurse needed sum + needed
            else
                sum
    in
    List.sum (List.map (\mass -> recurse mass 0) (stringsToInts inputs))

{-
1,9,10,3,
2,3,11,0,
99,
30,40,50
-}

type MaybeArgs 
    = Args { arg1: Int, arg2: Int, outputPos: Int}
    | MissingArg
type alias IntCode = Array Int
type alias Instruction = {
        opcode: Int,
        arg1: Int,
        arg2: Int,
        arg3: Int
    }

-- rawCode = "1,1,1,4,99,5,6,0,99"
-- String.split "," rawCode 
-- |> List.map String.trim 
-- |> List.filterMap String.toInt


-- chunk : Int -> List a -> List (List a)
-- chunk size list =
--     case List.take size list of
--        [] -> []
--        sublist -> sublist :: chunk size (List.drop size list)
evaluateIntCode : String -> Result String IntCode
evaluateIntCode code =
    {-- evaluate the code and return the final state
    each evaluation should yield a Result of Err or Ok new program,
    which should be fed into the next evaluation. so it's recursive
    For each set of 4 values:
        Read the first opcode
        if 1 - perform addition, using the next 3 values as positions
           return a new copy of the program with modified values
        if 2 - perform multiplication, using the next 3 values as positions
        if 99 - return the final state of the code
    --}
    let
        initialProgram = String.split "," code
                        |> stringsToInts
                        |> Array.fromList

        loop : Int -> IntCode -> Result String IntCode
        loop offset currentProgram  =
            let
                opcode = get offset currentProgram
                opArgs = extractArgs offset currentProgram
            in

            case opcode of
                Just 1 -> performOperation (+) currentProgram opArgs
                            |> Result.andThen (loop (offset + 4))
                Just 2 -> performOperation (*) currentProgram opArgs
                            |> Result.andThen (loop (offset + 4))
                Just 99 -> Ok currentProgram
                _ -> Err ("Unrecognized opcode" ++ Debug.toString opcode)
    in
    loop 0 initialProgram
        

extractArgs : Int -> IntCode -> MaybeArgs
extractArgs offset program =
    let
        arg1 = get (offset + 1) program
                |> Maybe.andThen (\pos -> get pos program)
        arg2 = get (offset + 2) program 
                |> Maybe.andThen (\pos -> get pos program)
        arg3 = get (offset + 3) program
    in

    case [arg1, arg2, arg3] of
       [Just v1, Just v2, Just v3] -> Args { arg1 = v1, arg2 = v2, outputPos = v3}
    
       _ -> MissingArg

performOperation : (Int -> Int -> Int) -> IntCode -> MaybeArgs -> Result String IntCode
performOperation op program positions =
    case positions of
        Args { arg1, arg2, outputPos } -> Ok (set outputPos (op arg1 arg2) program)

        MissingArg -> Err "One of the arguments is missing"

formatIntCode : Result String IntCode -> String
formatIntCode result =
    case result of
        Ok code -> 
            String.join "," (List.map String.fromInt (Array.toList code))
        Err msg -> msg
    
findNounAndVerbFor : String -> Int -> String
findNounAndVerbFor code targetOutput =
    "Blerp"
    {--
        looping pattern is: try val1 = 0 and val2 = 0..99, then val1 = 1 and val2 = 0.99, and so on until val1 > 99
        evaluateIntCode with the initial memory modified thusly:
        position 1 and 2 set to 0..99 and 0, walking pos 1
        Halt if pos 0 of the output is equal to the targetoutput
        return 100 * pos1 + pos2
    --}
    -- let
    --     initialProgram = String.split "," code
    --                     |> stringsToInts
    --                     |> Array.fromList
    --     loop : Int -> Int -> Int
    --     loop value1 value2 =
    --         if value2 < 99 then 
    --             loop value1 (value2 + 1)
    --         else
    --             let
    --                 modifiedMemory = String.concat [
    --                     (String.left 1 initialProgram), 
    --                     (String.fromInt value1), 
    --                     (String.fromInt value2), 
    --                     (String.dropLeft 3 initialProgram)]
    --             in 
    --             case evaluateIntCode modifiedMemory of
    --             Ok output ->
    --                     if String.fromInt (Array.get 0 output) == targetOutput then
    --                         String.fromInt (100 * value1 + value2)
    --                     else
    --                         loop 
    --             Err msg -> msg
    -- in
    -- loop 0 0



view : Model -> Html Msg
view model =
    div [class "wrapper", style "padding" "20px"][
        div [class "day-1"][
            textarea [placeholder "Multiple Input", onInput UpdateList, rows 10][]
            , div [class "part-1", style "padding" "10px 0px 10px 0px"][
                div [][
                    div [][text "Part 1 - Calculate total fuel based on mass"]
                    , div [][text (String.fromInt (calculateAll model.inputList))]
                ] 
            ]
            ,div [class "part-2", style "padding" "10px 0px 10px 0px"][
                div [][
                    div [][text "Part 2 - Calculate total fuel needed, taking fuel into account!"]
                    , div [][text (String.fromInt (calculateAllPart2 model.inputList))]
                ]
            ]
        ]
        , hr [][]
        , div [class "day-2"][
            textarea [placeholder "Intcode input", onInput UpdateIntcode, rows 10][]
            , div [class "part-1", style "padding" "10px 0px 10px 0px"][
                div[][text model.intCode]
                , div [][text (Debug.toString (evaluateIntCode model.intCode))]
                , div [][text (formatIntCode (evaluateIntCode model.intCode))]
            ]
            , div [class "part-2", style "padding" "10px 0px 10px 0px"][
                div [][text (findNounAndVerbFor model.intCode 19690720)]
            ]
        ]
    ]
    

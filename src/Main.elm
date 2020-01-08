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
    inputList: List String, -- Day 1
    intCode : String -- Day 2
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
stringsToInts strings =
    strings
    |> List.filterMap (\s -> String.trim s |> String.toInt)


-- Fuel required to launch a given module is based on its mass. Specifically, to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2.
calculateFuel : Int -> Int
calculateFuel mass =
    floor (toFloat mass / 3) - 2
    
calculateAll : List String -> Int
calculateAll inputs =
    stringsToInts inputs
    |> List.map calculateFuel
    |> List.sum

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
    stringsToInts inputs
    |> List.map (\mass -> recurse mass 0)
    |> List.sum

-- IntCode Computer

type MaybeArgs 
    = Args { arg1: Int, arg2: Int, outputPos: Int}
    | MissingArg
type alias IntCode = Array Int
type alias Instruction = {
        opcode: Int,
        param1: Int,
        param2: Int,
        param3: Int
    }

-- Used for Day 1 Part 1
runTwoInputWrapper : Int -> Int -> String -> String
runTwoInputWrapper a b raw =
    case runTwoInputComputer a b (convertToIntCode raw) of
       Ok result -> result |> String.fromInt
       Err msg -> msg

-- Utility to parse an input string into an IntCode
convertToIntCode : String -> IntCode
convertToIntCode raw =
    String.split "," raw
    |> List.map String.trim
    |> List.filterMap String.toInt
    |> Array.fromList

-- Wrapper around our intcode computer that takes two inputs and returns a Result containing the output
runTwoInputComputer : Int -> Int -> IntCode -> Result String Int
runTwoInputComputer input1 input2 memory =
    let
        modified = memory
                    |> Array.set 1 input1
                    |> Array.set 2 input2
    in
    case recurseOverIntCode 0 modified of
        Ok final -> case Array.get 0 final of
                        Just result -> Ok result
                        Nothing -> Err "Final memory has no value at position 0, somehow."
        Err msg -> Err msg

-- Recursively extract instructions and evaluate, advancing pointer 4 addresses each time until a 99 opcode is found or an instruction fails
recurseOverIntCode : Int -> IntCode -> Result String IntCode
recurseOverIntCode pointer memory =
    case extractInstruction pointer memory of
       Just instruction -> if instruction.opcode == 99 then
                                Ok memory -- No more recursion, return the final program memory
                            else
                                evaluateInstruction instruction memory
                                |> Result.andThen (recurseOverIntCode (pointer + 4))
                        
       Nothing -> Err ("Failed to extract instruction with pointer at address " ++ String.fromInt pointer)

-- Attempt to slice an instruction starting from the pointer in given memory 
-- This might be wrong - it's probably valid to have a 99 with fewer than 4 values after it.
-- It might be more correct to simply take the slice and let the extractArgs complain if it's missing info?   
extractInstruction : Int -> IntCode -> Maybe Instruction
extractInstruction pointer memory =
    case Array.toList (slice pointer (pointer + 4) memory) of
       [opcode, param1, param2, param3] -> Just (Instruction opcode param1 param2 param3)
       
       _ -> Nothing


-- Given an instruction and an IntCode of current memory, return the result of that instruction (as a new IntCode)
evaluateInstruction : Instruction -> IntCode -> Result String IntCode
evaluateInstruction instruction memory =
    let
        opArgs = extractArgs instruction memory
    in
    case instruction.opcode of
        1 -> performOperation (+) memory opArgs
                    
        2 -> performOperation (*) memory opArgs
                    
        _ -> Err ("Unrecognized opcode: " ++ String.fromInt instruction.opcode)

-- Attempt to extract arguments from an instruction and memory for use in evaluation
-- We're "attempting" because we have to follow the instruction parameters to arbitrary locations in "memory" which may not exist
extractArgs : Instruction -> IntCode -> MaybeArgs
extractArgs instruction memory =
    case [
        Array.get instruction.param1 memory,
        Array.get instruction.param2 memory
    ] of
       [Just arg1, Just arg2] -> Args {arg1 = arg1, arg2 = arg2, outputPos = instruction.param3}

       _ -> MissingArg

-- Given a function, memory, and arguments, return a Result containing the new memory 
performOperation : (Int -> Int -> Int) -> IntCode -> MaybeArgs -> Result String IntCode
performOperation fn memory args =
    case args of
        Args { arg1, arg2, outputPos } -> Ok (set outputPos (fn arg1 arg2) memory)

        MissingArg -> Err "One of the arguments is missing"


-- For use in Day 2 Part 2
findNounAndVerbFor : String -> Int -> String
findNounAndVerbFor memory target =
     case checkInputRanges 0 99 0 99 (convertToIntCode memory) target of
        Just (noun, verb) -> 100 * noun + verb |> String.fromInt
        Nothing -> "No combination of inputs yield " ++ String.fromInt target


-- Given a min and max for both verb and noun, check every combination of those inputs until an answer is found  
checkInputRanges : Int -> Int -> Int -> Int -> IntCode -> Int -> Maybe (Int, Int)
checkInputRanges nounMin nounMax verbMin verbMax memory target =
    let
        helper : Int -> Int -> Maybe (Int, Int)
        helper noun verb =
            if noun < nounMin then
                Nothing
            else if verb < verbMin then
                helper (noun - 1) verbMax
            else
                case checkInput noun verb memory target of
                   Just pair -> Just pair

                   Nothing -> helper noun (verb - 1)
    in
    helper nounMax verbMax

 
-- Helper that runs our intcode computer and checks to see if, given a noun and a verb, the output matches the target output 
checkInput : Int -> Int -> IntCode -> Int -> Maybe (Int, Int)
checkInput noun verb memory target =
    case runTwoInputComputer noun verb memory of
        Ok final -> if final == target then 
                        Just (noun, verb) 
                    else Nothing
        Err _ -> Nothing



view : Model -> Html Msg
view model =
    div [class "wrapper", style "padding" "20px"][
        h1 [][text "Advent of Code 2019"]
        , div [class "day-1"][
            h3 [][text "Day 1"]
            , textarea [placeholder "Multiple Input", onInput UpdateList, rows 10][]
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
            h3 [][text "Day 2"]
            , textarea [placeholder "Intcode input", onInput UpdateIntcode, rows 10][]
            , div [class "part-1", style "padding" "10px 0px 10px 0px"][
                div [][text "Part 1 - 1202 Program"]
                , div [][text (runTwoInputWrapper 12 2 model.intCode)]
            ]
            , div [class "part-2", style "padding" "10px 0px 10px 0px"][
                div [][text "Part 2 - Find the noun and verb"]
                , div [][text (findNounAndVerbFor model.intCode 19690720)]
            ]
        ]
    ]
    

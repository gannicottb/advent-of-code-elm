module Main exposing (..)
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
    List.map (\a -> Maybe.withDefault 0 (String.toInt a)) ints

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
evaluateIntCode : String -> Result String (List Int)
evaluateIntCode code =
    {-- evaluate the code and return the final state
    For each set of 4 values:
        Read the first opcode
        if 1 - perform addition, using the next 3 values as positions
            addition = return a new copy of the program with modified values
            use Array.set 
            
        if 2 - perform multiplication, using the next 3 values as positions
        if 99 - return the final state of the code
    --}
    let
        program = Array.fromList (stringsToInts(String.split "," code))
        opcode = get 0 program
    in
    case opcode of
       Just 1 -> performOperation (+) program (extractArgs program)
       Just 2 -> performOperation (*) program (extractArgs program)
       Just 99 -> Ok (toList program)
       _ -> Err "Unrecognized opcode"

extractArgs : Array Int -> MaybeArgs
extractArgs line =
    let
        arg1 = get 1 line
                |> Maybe.andThen (\pos -> get pos line)
        arg2 = get 2 line 
                |> Maybe.andThen (\pos -> get pos line)
        arg3 = get 3 line
    in

    case (arg1, arg2, arg3) of
       (Just v1, Just v2, Just v3) -> Args { arg1 = v1, arg2 = v2, outputPos = v3}
    
       _ -> MissingArg

performOperation : (Int -> Int -> Int) -> (Array Int) -> MaybeArgs -> Result String (List Int)
performOperation op program positions =
    case positions of
        Args { arg1, arg2, outputPos } -> Ok (toList(set outputPos (op arg1 arg2) program))

        MissingArg -> Err "One of the arguments to add is missing"

formatIntCode : Result String (List Int) -> String
formatIntCode result =
    case result of
        Ok code -> 
            String.join "," (List.map String.fromInt code)
        Err msg -> msg
    




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
            , div[][text model.intCode]
            , div [][text (Debug.toString (evaluateIntCode model.intCode))]
            , div [][text (formatIntCode (evaluateIntCode model.intCode))]
        ]
    ]
    

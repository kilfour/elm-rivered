module Main exposing (main)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Questions
import Random
import RiveredTypes exposing (Question, decoder)
import Task
import Time exposing (Posix, every)



-- MODEL


type alias Model =
    { questions : List Question
    , current : Maybe Question
    , seed : Random.Seed
    , loading : Bool
    , start : Posix
    , now : Posix
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    let
        seed =
            Random.initialSeed 40
    in
    ( { questions = Questions.questions
      , current = Nothing
      , seed = seed
      , loading = False
      , start = Time.millisToPosix 0
      , now = Time.millisToPosix 0
      }
    , Random.generate SetSeed (Random.int 0 1000000)
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    every 1000 Tick



-- UPDATE


type Msg
    = LoadQuestions
    | FileSelected File
    | FileLoaded (Result String String)
    | QuestionsParsed (Result Decode.Error (List Question))
    | SetSeed Int
    | DrawNew
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadQuestions ->
            ( model, Select.file [ "application/json" ] FileSelected )

        FileSelected file ->
            ( { model | loading = True }, Task.attempt FileLoaded (File.toString file) )

        FileLoaded (Ok content) ->
            let
                result =
                    Decode.decodeString (Decode.list decoder) content
            in
            update (QuestionsParsed result) model

        FileLoaded (Err _) ->
            ( { model | loading = False }, Cmd.none )

        QuestionsParsed (Ok qs) ->
            ( { model | questions = qs, loading = False }, Cmd.none )

        QuestionsParsed (Err _) ->
            ( { model | loading = False }, Cmd.none )

        DrawNew ->
            case model.questions of
                [] ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( index, newSeed ) =
                            Random.step (Random.int 0 (List.length model.questions - 1)) model.seed

                        maybeQ =
                            List.head <| List.drop index model.questions

                        id =
                            maybeQ |> Maybe.map (\a -> a.id) |> Maybe.withDefault 0

                        updatedQuestions =
                            model.questions
                                |> List.filter (\a -> a.id /= id)
                    in
                    ( { model
                        | current = maybeQ
                        , questions = updatedQuestions
                        , seed = newSeed
                        , start = Time.millisToPosix 0
                        , now = Time.millisToPosix 0
                      }
                    , Cmd.none
                    )

        SetSeed randomInt ->
            ( { model | seed = Random.initialSeed randomInt }, Cmd.none )

        Tick newNow ->
            let
                newModel =
                    if model.start == Time.millisToPosix 0 then
                        { model | start = newNow, now = newNow }

                    else
                        { model | now = newNow }
            in
            ( newModel, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        elapsedMs =
            Time.posixToMillis model.now - Time.posixToMillis model.start

        seconds =
            elapsedMs // 1000
    in
    div [ class "centered-container" ]
        [ div [ class "content" ] <|
            if List.isEmpty model.questions then
                [ button [ onClick LoadQuestions ] [ text "Upload Questions JSON" ]
                , if model.loading then
                    div [] [ text "Loading..." ]

                  else
                    text ""
                ]

            else
                [ button [ onClick DrawNew ] [ text "Draw a Question" ]
                , case model.current of
                    Just q ->
                        div [ class "question-card" ]
                            [ Html.h2 [] [ text q.text ] ]

                    Nothing ->
                        div [ class "note" ] [ text "Click the button to draw a question." ]
                , case model.current of
                    Just q ->
                        div [ class "timer" ] [ text ("Time elapsed: " ++ String.fromInt seconds ++ " sec.") ]

                    Nothing ->
                        text ""
                ]
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Rivered", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        }

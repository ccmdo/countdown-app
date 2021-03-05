module Main exposing (main)

import Browser
import Game.Constants
import Html exposing (Html, button, div, h1, h2, nav, span, text)
import Html.Attributes exposing (attribute, class, disabled)
import Html.Events exposing (onClick)
import List.Extra
import Random
import Random.List


type alias Model =
    { previousRounds : List Round
    , currentRound : Round
    , nextRounds : List Round
    }


type Round
    = Round RoundType


type RoundType
    = LettersGame (List String)
    | NumbersGame (List Int)
    | Conundrum (List String)


type Msg
    = NextRound
    | PreviousRound
    | ClickedGenerateConsonant
    | ClickedGenerateVowel
    | ClickedChooseLargeNumber
    | ClickedChooseSmallNumber
    | NewLetter ( Maybe String, List String )
    | NewNumber ( Maybe Int, List Int )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { previousRounds = []
      , currentRound = Round (LettersGame [])
      , nextRounds =
            [ Round (LettersGame [])
            , Round (NumbersGame [])
            , Round (LettersGame [])
            , Round (Conundrum [ "p", "o", "l", "k", "i", "f", "o", "i", "l" ])
            ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        m =
            Debug.log "msg" msg
    in
    case msg of
        NextRound ->
            case model.nextRounds of
                [] ->
                    ( model, Cmd.none )

                nextRound :: [] ->
                    ( { model
                        | previousRounds = List.concat [ model.previousRounds, [ model.currentRound ] ]
                        , currentRound = nextRound
                        , nextRounds = []
                      }
                    , Cmd.none
                    )

                nextRound :: remainingRounds ->
                    ( { model
                        | previousRounds = List.concat [ model.previousRounds, [ model.currentRound ] ]
                        , currentRound = nextRound
                        , nextRounds = remainingRounds
                      }
                    , Cmd.none
                    )

        PreviousRound ->
            case List.reverse model.previousRounds of
                [] ->
                    ( model, Cmd.none )

                previousRound :: [] ->
                    ( { model
                        | previousRounds = []
                        , currentRound = previousRound
                        , nextRounds = List.concat [ [ model.currentRound ], model.nextRounds ]
                      }
                    , Cmd.none
                    )

                previousRound :: remainingRounds ->
                    ( { model
                        | previousRounds = List.reverse remainingRounds
                        , currentRound = previousRound
                        , nextRounds = List.concat [ [ model.currentRound ], model.nextRounds ]
                      }
                    , Cmd.none
                    )

        ClickedGenerateConsonant ->
            ( model, Random.generate NewLetter (Random.List.choose Game.Constants.consonants) )

        ClickedGenerateVowel ->
            ( model, Random.generate NewLetter (Random.List.choose Game.Constants.vowels) )

        ClickedChooseLargeNumber ->
            case model.currentRound of
                Round (NumbersGame numbers) ->
                    let
                        remainingNumbers =
                            removeAll numbers Game.Constants.largeNumbers
                    in
                    ( model, Random.generate NewNumber (Random.List.choose remainingNumbers) )

                _ ->
                    ( model, Cmd.none )

        ClickedChooseSmallNumber ->
            case model.currentRound of
                Round (NumbersGame numbers) ->
                    let
                        remainingNumbers =
                            removeAll numbers Game.Constants.smallNumbers
                    in
                    ( model, Random.generate NewNumber (Random.List.choose remainingNumbers) )

                _ ->
                    ( model, Cmd.none )

        NewLetter ( maybeLetter, _ ) ->
            case maybeLetter of
                Just letter ->
                    case model.currentRound of
                        Round (LettersGame letters) ->
                            let
                                newLetters =
                                    letters ++ [ letter ]
                            in
                            ( { model | currentRound = Round (LettersGame newLetters) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NewNumber ( maybeNumber, _ ) ->
            case maybeNumber of
                Just number ->
                    case model.currentRound of
                        Round (NumbersGame numbers) ->
                            let
                                newNumbers =
                                    numbers ++ [ number ]
                            in
                            ( { model | currentRound = Round (NumbersGame newNumbers) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ --text (Debug.toString model)
          h1 [ class "text-center" ] [ text "Countdown" ]
        , renderRound model.currentRound
        , div [ class "btn-group fixed-bottom d-flex justify-content-center", attribute "role" "group" ]
            [ button [ class "btn btn-warning w-100 py-5", onClick PreviousRound, disabled (List.isEmpty model.previousRounds) ] [ text "Previous round" ]
            , button [ class "btn btn-warning w-100", onClick NextRound, disabled (List.isEmpty model.nextRounds) ] [ text "Next round" ]
            ]
        ]


renderRound : Round -> Html Msg
renderRound round =
    case round of
        Round (LettersGame letters) ->
            renderLettersGame letters

        Round (NumbersGame numbers) ->
            renderNumbersGame numbers

        Round (Conundrum letters) ->
            renderConundrumGame letters


renderLettersGame : List String -> Html Msg
renderLettersGame letters =
    -- 2 buttons, generate a costonant, generate a vowel
    -- Continue until 9 letters are generated
    -- Then wait for start
    -- On start, run 30 second timer
    -- On complete timer, prompts for player guesses
    -- Can check each on using a dictionary lookup
    -- Wait for Complete round (optional - list 1 longest 9, 8, 7, 6 letter word words found)
    -- Update to Next round
    div []
        [ h2 [ class "text-center" ] [ text "Letters" ]
        , renderLetters letters
        , button [ onClick ClickedGenerateVowel, disabled (List.length letters == Game.Constants.letterLimit) ] [ text "Vowel" ]
        , button [ onClick ClickedGenerateConsonant, disabled (List.length letters == Game.Constants.letterLimit) ] [ text "Consonant" ]
        ]


renderNumbersGame : List Int -> Html Msg
renderNumbersGame numbers =
    -- How does the numbers round work?
    -- 20 small numbers - 2( 1 - 10)
    -- 4 large numbers (25, 50, 75, 100)
    -- Generate target between 100-999 inclusive
    -- Then wait for start
    -- On start, run 30 second timer
    -- On complete timer, wait
    -- Wait for Complete round (optional - list 1 longest 9, 8, 7, 6 letter word words found)
    -- Update to Next round
    let
        largeNumberLimitReached =
            numbers
                |> List.filter ((<) 10)
                |> List.length
                |> (<=) Game.Constants.largeNumberLimit

        numberLimitReached =
            numbers
                |> List.length
                |> (<=) Game.Constants.numberLimit
    in
    div []
        [ h2 [ class "text-center" ] [ text "Numbers" ]
        , renderNumbers numbers
        , button
            [ onClick ClickedChooseLargeNumber
            , disabled (largeNumberLimitReached || numberLimitReached)
            ]
            [ text "Large" ]
        , button
            [ onClick ClickedChooseSmallNumber
            , disabled numberLimitReached
            ]
            [ text "Small" ]
        ]


renderConundrumGame : List String -> Html Msg
renderConundrumGame letters =
    div []
        [ h2 [ class "text-center" ] [ text "Conundrum" ]
        , renderLetters letters
        ]


renderLetters : List String -> Html Msg
renderLetters letters =
    let
        placeholders =
            List.repeat (Game.Constants.letterLimit - List.length letters) renderPlaceholder
    in
    div [ class "d-flex justify-content-center" ]
        (List.concat
            [ List.map renderLetter letters
            , placeholders
            ]
        )


renderLetter : String -> Html Msg
renderLetter letter =
    span [ class "border mx-1" ] [ text letter ]


renderNumbers : List Int -> Html Msg
renderNumbers numbers =
    let
        placeholders =
            List.repeat (Game.Constants.numberLimit - List.length numbers) renderPlaceholder
    in
    div [ class "d-flex justify-content-center" ]
        (List.concat
            [ List.map renderNumber (numbers |> List.sort |> List.reverse)
            , placeholders
            ]
        )


renderNumber : Int -> Html Msg
renderNumber n =
    span [ class "border mx-1" ] [ text (String.fromInt n) ]


renderPlaceholder : Html Msg
renderPlaceholder =
    span [ class "border mx-1" ] [ text "-" ]


removeAll : List a -> List a -> List a
removeAll inThis fromThis =
    case inThis of
        [] ->
            fromThis

        x :: [] ->
            List.Extra.remove x fromThis

        x :: xs ->
            removeAll xs (List.Extra.remove x fromThis)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }

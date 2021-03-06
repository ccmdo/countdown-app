port module Main exposing (audioEnded, audioStarted, main, pauseMusic, playMusic, stopMusic)

import Browser
import Game.Constants
import Html exposing (Html, audio, button, div, h1, h2, hr, img, span, text)
import Html.Attributes exposing (attribute, class, classList, disabled, src, style)
import Html.Events exposing (onClick)
import List.Extra
import Random
import Random.List


type alias Model =
    { previousRounds : List Round
    , currentRound : Round
    , nextRounds : List Round
    , music : MusicState
    }


type MusicState
    = Stopped
    | Playing
    | Paused


type Round
    = Round RoundType


type RoundType
    = LettersGame (List String)
    | NumbersGame (List Int) (Maybe Int)
    | Conundrum (List String) (List String) Bool


type Msg
    = NextRound
    | PreviousRound
    | ClickedGenerateConsonant
    | ClickedGenerateVowel
    | ClickedChooseLargeNumber
    | ClickedChooseSmallNumber
    | ClickedRevealConundrum
    | ClickedPlayMusic
    | ClickedToggleMusic
    | NewLetter ( Maybe String, List String )
    | NewNumber ( Maybe Int, List Int )
    | NewTarget Int
    | AudioStarted
    | AudioEnded


port playMusic : () -> Cmd msg


port pauseMusic : () -> Cmd msg


port stopMusic : () -> Cmd msg


port audioStarted : (() -> msg) -> Sub msg


port audioEnded : (() -> msg) -> Sub msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { previousRounds = []
      , currentRound = Round (LettersGame [])
      , nextRounds =
            [ Round (LettersGame [])
            , Round (NumbersGame [] Nothing)
            , Round (LettersGame [])
            , Round
                (Conundrum
                    [ "i", "l", "o", "l", "i", "f", "p", "o", "k" ]
                    [ "k", "l", "i", "p", "f", "o", "l", "i", "o" ]
                    False
                )
            ]
      , music = Stopped
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPlayMusic ->
            ( model, playMusic () )

        ClickedToggleMusic ->
            case model.music of
                Playing ->
                    ( { model | music = Paused }, pauseMusic () )

                Paused ->
                    ( model, playMusic () )

                _ ->
                    ( model, Cmd.none )

        AudioStarted ->
            ( { model | music = Playing }, Cmd.none )

        AudioEnded ->
            ( { model | music = Stopped }, Cmd.none )

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
                Round (NumbersGame numbers _) ->
                    let
                        remainingNumbers =
                            removeAll numbers Game.Constants.largeNumbers
                    in
                    ( model, Random.generate NewNumber (Random.List.choose remainingNumbers) )

                _ ->
                    ( model, Cmd.none )

        ClickedChooseSmallNumber ->
            case model.currentRound of
                Round (NumbersGame numbers _) ->
                    let
                        remainingNumbers =
                            removeAll numbers Game.Constants.smallNumbers
                    in
                    ( model, Random.generate NewNumber (Random.List.choose remainingNumbers) )

                _ ->
                    ( model, Cmd.none )

        ClickedRevealConundrum ->
            case model.currentRound of
                Round (Conundrum letters answer False) ->
                    ( { model | currentRound = Round (Conundrum letters answer True), music = Stopped }, stopMusic () )

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
                        Round (NumbersGame numbers target) ->
                            let
                                newNumbers =
                                    numbers ++ [ number ]

                                cmd =
                                    if List.length newNumbers == Game.Constants.numberLimit then
                                        Random.generate NewTarget (Random.int 100 999)

                                    else
                                        Cmd.none
                            in
                            ( { model | currentRound = Round (NumbersGame newNumbers target) }, cmd )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NewTarget target ->
            case model.currentRound of
                Round (NumbersGame numbers _) ->
                    ( { model | currentRound = Round (NumbersGame numbers (Just target)) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ audio [ src "%PUBLIC_URL%/music.mp3" ] []
        , h1 [ class "text-center" ] [ text "Countdown" ]
        , renderRound model.currentRound
        , div [ class "btn-group fixed-bottom d-flex justify-content-center", attribute "role" "group" ]
            [ button [ class "btn btn-warning w-100 py-5", onClick PreviousRound, disabled (List.isEmpty model.previousRounds || model.music /= Stopped) ]
                [ div [ class "d-flex justify-content-around" ]
                    [ span [ class "carousel-control-prev-icon" ] []
                    , text "Previous round"
                    ]
                ]
            , button
                [ class "btn"
                , classList [ ( "disabled", not (isRoundReady model.currentRound) || model.music == Playing ) ]
                , onClick ClickedPlayMusic
                , disabled (not (isRoundReady model.currentRound) || model.music == Playing)
                ]
                [ img [ style "max-height" "64px", src "%PUBLIC_URL%/clock-regular.png" ] []
                ]
            , button [ class "btn btn-warning w-100 py-5", onClick NextRound, disabled (List.isEmpty model.nextRounds || model.music /= Stopped) ]
                [ div [ class "d-flex justify-content-around" ]
                    [ text "Next round"
                    , span [ class "carousel-control-next-icon" ] []
                    ]
                ]
            ]
        ]


renderRound : Round -> Html Msg
renderRound round =
    div [ class "jumbotron border border-dark", style "min-height" "405px" ]
        [ case round of
            Round (LettersGame letters) ->
                renderLettersGame letters

            Round (NumbersGame numbers target) ->
                renderNumbersGame numbers target

            Round (Conundrum letters answer revealConundrum) ->
                renderConundrumGame letters answer revealConundrum
        ]


renderLettersGame : List String -> Html Msg
renderLettersGame letters =
    div [ class "d-flex flex-column" ]
        [ h2 [ class "text-center" ] [ text "Letters" ]
        , hr [ class "mb-4 w-100" ] []
        , renderLetters letters
        , hr [ class "my-4 w-100" ] []
        , div [ class "d-flex justify-content-between", attribute "role" "group" ]
            [ button [ class "btn btn-primary w-50 mx-1 py-3", onClick ClickedGenerateVowel, disabled (List.length letters == Game.Constants.letterLimit) ] [ text "Vowel" ]
            , button [ class "btn btn-primary w-50 mx-1 py-3", onClick ClickedGenerateConsonant, disabled (List.length letters == Game.Constants.letterLimit) ] [ text "Consonant" ]
            ]
        ]


renderNumbersGame : List Int -> Maybe Int -> Html Msg
renderNumbersGame numbers target =
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
    div [ class "d-flex flex-column" ]
        [ h2 [ class "text-center" ] [ text "Numbers" ]
        , hr [ class "mb-4 w-100" ] []
        , renderNumbers numbers
        , hr [ class "my-4 w-100" ] []
        , case target of
            Just currentTarget ->
                div [ class "d-flex flex-column align-items-center" ]
                    [ div [ class "text-muted" ]
                        [ text "Target" ]
                    , div
                        [ class "display-4" ]
                        [ text (String.fromInt currentTarget) ]
                    ]

            Nothing ->
                div [ class "d-flex justify-content-between", attribute "role" "group" ]
                    [ button
                        [ class "btn btn-primary w-50 mx-1 py-3"
                        , onClick ClickedChooseLargeNumber
                        , disabled (largeNumberLimitReached || numberLimitReached)
                        ]
                        [ text "Large" ]
                    , button
                        [ class "btn btn-primary w-50 mx-1 py-3"
                        , onClick ClickedChooseSmallNumber
                        , disabled numberLimitReached
                        ]
                        [ text "Small" ]
                    ]
        ]


renderConundrumGame : List String -> List String -> Bool -> Html Msg
renderConundrumGame letters answer revealConundrum =
    div [ class "d-flex flex-column" ]
        [ h2 [ class "text-center" ] [ text "Conundrum" ]
        , hr [ class "mb-4 w-100" ] []
        , renderLetters letters
        , hr [ class "my-4 w-100" ] []
        , if revealConundrum then
            renderLetters answer

          else
            div [ class "d-flex justify-content-center" ]
                [ button
                    [ class "btn btn-block btn-primary w-75 py-3"
                    , onClick ClickedRevealConundrum
                    ]
                    [ text "Reveal" ]
                , button [ class "btn btn-sm btn-link", onClick ClickedToggleMusic ] [ div [ class "text-muted" ] [ text "pause" ] ]
                ]
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


renderLetter : String -> Html Msg
renderLetter letter =
    div [ class "btn btn-lg btn-link text-uppercase", disabled True ] [ text letter ]


renderNumber : Int -> Html Msg
renderNumber n =
    div [ class "btn btn-lg btn-link", disabled True ] [ text (String.fromInt n) ]


renderPlaceholder : Html Msg
renderPlaceholder =
    div [ class "btn btn-lg text-dark", disabled True ] [ text "-" ]


isRoundReady : Round -> Bool
isRoundReady round =
    case round of
        Round (LettersGame letters) ->
            List.length letters == Game.Constants.letterLimit

        Round (NumbersGame numbers _) ->
            List.length numbers == Game.Constants.numberLimit

        Round (Conundrum _ _ False) ->
            True

        Round (Conundrum _ _ True) ->
            False


removeAll : List a -> List a -> List a
removeAll inThis fromThis =
    case inThis of
        [] ->
            fromThis

        x :: [] ->
            List.Extra.remove x fromThis

        x :: xs ->
            removeAll xs (List.Extra.remove x fromThis)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ audioStarted (always AudioStarted)
        , audioEnded (always AudioEnded)
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

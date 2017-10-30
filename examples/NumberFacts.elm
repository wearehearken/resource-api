module NumberFacts exposing (..)

import ResourceApi
import Html exposing (Html, h1, div, text, button)
import Html.Events exposing (onClick)
import Http
import Json.Decode


-- ResourceApi Stuff
-- Uses Numbers Api http://numbersapi.com/#42
-- {
-- "text": "42 is the result given by the web search engines Google, Wolfram Alpha and Bing when the query \"the answer to life the universe and everything\" is entered as a search.",
-- "number": 42,
-- "found": true,
-- "type": "trivia"
-- }


errorHandler : Http.Error -> String
errorHandler err =
    toString err


itemDecoder : Json.Decode.Decoder NumberFact
itemDecoder =
    Json.Decode.map (\text -> { text = text })
        (Json.Decode.field "text" Json.Decode.string)


numbersApi =
    ResourceApi.generateResourceApi
        errorHandler
        itemDecoder
        (\config ->
            { config
                | showPlaceholders = (\context -> [ ( "number", toString context.number ) ])
            }
        )


showUrl : String
showUrl =
    "http://numbersapi.com/:number"



-- Elm Stuff


type Msg
    = NoOp
    | NewNumberFact NumberFact
    | GetNewNumberFact
    | ErrorGettingNumberFact String


type alias NumberFact =
    { text : String
    }


type alias Model =
    { currentNumber : Int
    , currentFact : Maybe NumberFact
    }


extractNewNumberFact : Result String NumberFact -> Msg
extractNewNumberFact result =
    case result of
        Err error ->
            ErrorGettingNumberFact error

        Ok numberFact ->
            NewNumberFact numberFact


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetNewNumberFact ->
            ( { model | currentFact = Nothing }
            , numbersApi.show { number = model.currentNumber } showUrl extractNewNumberFact
            )

        ErrorGettingNumberFact err ->
            let
                _ =
                    Debug.log "Error getting fact" err
            in
                ( model, Cmd.none )

        NewNumberFact numberFact ->
            ( { model | currentFact = Just numberFact }
            , Cmd.none
            )


init : ( Model, Cmd Msg )
init =
    ( { currentNumber = 42
      , currentFact = Nothing
      }
    , Cmd.none
    )


numberFactView : Model -> Html Msg
numberFactView model =
    let
        factText =
            case model.currentFact of
                Nothing ->
                    text <| "No Fact for " ++ toString model.currentNumber ++ "..."

                Just fact ->
                    text <| "have a fact for " ++ toString model.currentNumber
    in
        div []
            [ factText
            , button [ onClick GetNewNumberFact ] [ text "Get Fact" ]
            ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Number Facts!" ]
        , numberFactView model
        ]


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

module Shared.Api.Resources
    exposing
        ( ResourceApiConfig
        , ResourceApi
        , generateResourceApi
        , createConfig
        , blankConfig
        , emptyPlaceholders1
        , emptyPlaceholders2
        , apiResultToMsg
        )

import Http
import HttpBuilder
import Json.Encode
import Json.Decode
import Dict
import Regex


urlSubstitutionRegex : Regex.Regex
urlSubstitutionRegex =
    Regex.regex ":[A-Za-z0-9_]+\\b"


substitutePlaceholders : List ( String, String ) -> String -> String
substitutePlaceholders substitutionList template =
    let
        dictionary =
            Dict.fromList substitutionList
    in
        Regex.replace Regex.All
            urlSubstitutionRegex
            (\{ match } ->
                Dict.get (String.dropLeft 1 match) dictionary
                    |> Maybe.withDefault match
            )
            template


standardJsonHeaders : List ( String, String )
standardJsonHeaders =
    [ ( "Accept", "application/json" ), ( "Content-type", "application/json" ) ]


apiResultToMsg : (error -> msg) -> (item -> msg) -> Result error item -> msg
apiResultToMsg onError onSuccess result =
    case result of
        Err error ->
            onError error

        Ok lists ->
            onSuccess lists


handleHttpResult : ResourceApiConfig supportingContext createData item error -> Result Http.Error result -> Result error result
handleHttpResult config result =
    Result.mapError config.handleHttpError result


create : ResourceApiConfig supportingContext createData item error -> supportingContext -> createData -> String -> (Result error item -> msg) -> Cmd msg
create config supportingContext createData urlTemplate resultHandler =
    let
        placeholders =
            config.createPlaceholders supportingContext createData

        body =
            config.createEncoder createData
    in
        substitutePlaceholders placeholders urlTemplate
            |> HttpBuilder.post
            |> HttpBuilder.withJsonBody body
            |> HttpBuilder.withHeaders standardJsonHeaders
            |> HttpBuilder.withExpect (Http.expectJson config.itemDecoder)
            |> HttpBuilder.send (handleHttpResult config >> resultHandler)


update : ResourceApiConfig supportingContext createData item error -> supportingContext -> item -> String -> (Result error item -> msg) -> Cmd msg
update config supportingContext item urlTemplate resultHandler =
    let
        placeholders =
            config.updatePlaceholders supportingContext item

        body =
            config.updateEncoder item
    in
        substitutePlaceholders placeholders urlTemplate
            |> HttpBuilder.patch
            |> HttpBuilder.withJsonBody body
            |> HttpBuilder.withHeaders standardJsonHeaders
            |> HttpBuilder.withExpect (Http.expectJson config.itemDecoder)
            |> HttpBuilder.send (handleHttpResult config >> resultHandler)


index : ResourceApiConfig supportingContext createData item error -> supportingContext -> String -> (Result error (List item) -> msg) -> Cmd msg
index config supportingContext urlTemplate resultHandler =
    let
        placeholders =
            config.indexPlaceholders supportingContext
    in
        substitutePlaceholders placeholders urlTemplate
            |> HttpBuilder.get
            |> HttpBuilder.withHeaders standardJsonHeaders
            |> HttpBuilder.withExpect (Http.expectJson <| Json.Decode.list config.itemDecoder)
            |> HttpBuilder.send (handleHttpResult config >> resultHandler)


delete : ResourceApiConfig supportingContext createData item error -> supportingContext -> item -> String -> (Result error item -> msg) -> Cmd msg
delete config supportingContext item urlTemplate resultHandler =
    let
        placeholders =
            config.deletePlaceholders supportingContext item
    in
        substitutePlaceholders placeholders urlTemplate
            |> HttpBuilder.delete
            |> HttpBuilder.withHeaders standardJsonHeaders
            |> HttpBuilder.withExpect (Http.expectJson config.itemDecoder)
            |> HttpBuilder.send (handleHttpResult config >> resultHandler)


type alias ResourceApiConfig supportingContext createData item error =
    { itemDecoder : Json.Decode.Decoder item
    , createEncoder : createData -> Json.Encode.Value
    , createPlaceholders : supportingContext -> createData -> List ( String, String )
    , deletePlaceholders : supportingContext -> item -> List ( String, String )
    , updateEncoder : item -> Json.Encode.Value
    , updatePlaceholders : supportingContext -> item -> List ( String, String )
    , indexPlaceholders : supportingContext -> List ( String, String )
    , handleHttpError : Http.Error -> error
    }


blankConfig : (Http.Error -> error) -> Json.Decode.Decoder item -> ResourceApiConfig supportingContext createData item error
blankConfig handleError itemDecoder =
    { itemDecoder = itemDecoder
    , createEncoder = \item -> Json.Encode.string <| "Need to provide an encoder for: " ++ toString item
    , deletePlaceholders = emptyPlaceholders2
    , createPlaceholders = emptyPlaceholders2
    , updateEncoder = \item -> Json.Encode.string <| "Need to provide an encoder for: " ++ toString item
    , updatePlaceholders = emptyPlaceholders2
    , indexPlaceholders = emptyPlaceholders1
    , handleHttpError = handleError
    }


createConfig : (Http.Error -> error) -> Json.Decode.Decoder item -> (ResourceApiConfig supportingContext createData item error -> ResourceApiConfig supportingContext createData item error) -> ResourceApiConfig supportingContext createData item error
createConfig handleError itemDecoder setFields =
    blankConfig handleError itemDecoder
        |> setFields


type alias ResourceApi supportingContext createData item error msg =
    { create : supportingContext -> createData -> String -> (Result error item -> msg) -> Cmd msg
    , update : supportingContext -> item -> String -> (Result error item -> msg) -> Cmd msg
    , delete : supportingContext -> item -> String -> (Result error item -> msg) -> Cmd msg
    , index : supportingContext -> String -> (Result error (List item) -> msg) -> Cmd msg
    }


generateResourceApiFromConfig : ResourceApiConfig supportingContext createData item error -> ResourceApi supportingContext createData item error msg
generateResourceApiFromConfig config =
    { create = create config
    , update = update config
    , delete = delete config
    , index = index config
    }


generateResourceApi : (Http.Error -> error) -> Json.Decode.Decoder item -> (ResourceApiConfig supportingContext createData item error -> ResourceApiConfig supportingContext createData item error) -> ResourceApi supportingContext createData item error msg
generateResourceApi handleError itemDecoder setFields =
    createConfig handleError itemDecoder setFields
        |> generateResourceApiFromConfig


emptyPlaceholders1 : a -> List ( String, String )
emptyPlaceholders1 =
    \_ -> []


emptyPlaceholders2 : a -> b -> List ( String, String )
emptyPlaceholders2 =
    \_ _ -> []

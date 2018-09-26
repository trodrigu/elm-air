module Main exposing (..)

import Browser exposing (element)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Http
import Json.Decode as Decode


-- Msg


type Msg
    = NoOp



-- Model


type Cache a b
    = Empty
    | EmptyInvalid a
    | EmptySyncing
    | EmptyInvalidSyncing a
    | Filled b
    | FilledSyncing b
    | FilledInvalid a b
    | FilledInvalidSyncing a b


type alias Keyword =
    { name : String
    , searches : List Int
    , engagement : List Int
    , competition : Int
    , month : Month
    }


type alias KeywordCache =
    Cache Http.Error Keyword


type alias KeywordCollection =
    Dict String KeywordCache


type Month
    = Sept
    | Oct
    | Nov
    | Dec
    | Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug


type alias Model =
    { keywords : Cache Http.Error KeywordCollection
    }


type alias Transitions a b c =
    { updateEmpty : a -> b
    , updateFilled : a -> b -> b
    , patchFilled : c -> b -> b
    }


type CacheEvent a b c
    = Sync
    | Error a
    | Update b
    | Patch c


type Visibility a
    = Show a
    | Hide


initialModel : flags -> ( Model, Cmd Msg )
initialModel _ =
    ( { keywords = Empty }, Cmd.none )



-- View


view : model -> Html Msg
view model =
    div [] []


errorVisibility : Cache Http.Error a -> Visibility Http.Error
errorVisibility cache =
    case cache of
        Empty ->
            Hide

        EmptyInvalid error ->
            Show error

        EmptyInvalidSyncing error ->
            Show error

        EmptySyncing ->
            Hide

        Filled _ ->
            Hide

        FilledInvalid error _ ->
            Show error

        FilledInvalidSyncing error _ ->
            Show error

        FilledSyncing _ ->
            Hide


visibilityToHtml : Html Msg -> Visibility a -> Html b
visibilityToHtml toHtml visibility =
    case visibility of
        Show x ->
            toHtml x

        Hide ->
            text ""


errorView : Cache Http.Error a -> Html Msg
errorView =
    errorVisibility >> visibilityToHtml errorHtml


errorHtml : Html Msg
errorHtml =
    div [] []



-- Update
-- updateKeywordEmpty : a -> b
-- updateKeywordEmpty a =
--     []
-- patchKeywordFilled : a -> b -> b
-- patchKeywordFilled el stuff =
--     stuff
-- updateKeywordFilled : a -> b -> b
-- updateKeywordFilled el stuff =
--     stuff


patchFilled : String -> CacheEvent Http.Error Keyword a -> KeywordCollection -> KeywordCollection
patchFilled key cacheEvent keywordCollection =
    Dict.get key keywordCollection
        |> Maybe.map
            (updateCache
                {}
             -- { updateEmpty = updateKeywordEmpty
             -- , updateFilled = updateKeywordFilled
             -- , patchFilled = patchKeywordFilled
             -- }
            )
        |> Maybe.map (\innerUpdate -> Dict.insert key innerUpdate keywordCollection)
        |> Maybe.withDefault keywordCollection


updateCache : Transitions a b c -> CacheEvent a b c -> Cache a b -> Cache a b
updateCache transitions event current =
    case current of
        Empty ->
            case event of
                Sync ->
                    EmptySyncing

                Error nextError ->
                    EmptyInvalid nextError

                Update nextData ->
                    Filled <| transitions.updateEmpty nextData

                Patch patch ->
                    Filled <| transitions.patchFilled patch

        EmptyInvalid currentError ->
            case event of
                Sync ->
                    EmptySyncing

                Error nextError ->
                    EmptyInvalid nextError

                Update nextData ->
                    Filled nextData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, getRecords )


getRecords : Http.Request Model
getRecords =
    Http.get "" decodeRecords


decodeRecords : Decode.Decoder (List Keyword)
decodeRecords =
    Decode.list decodeKeyword



-- { name : String
-- , searches : List Int
-- , engagement : List Int
-- , competition : Int
-- , month : Month
-- }


decodeKeyword : Decode.Decoder Keyword
decodeKeyword =
    Decode.map4 Keyword
        (field "name" string)
        (field "searches" toRange)
        (field "engagement" string)
        (field "month" string)


toRange : Decoder Keyword
toRange =
    field "searches" string
        |> Decode.map convertToRange


convertToRange : String -> List Int
convertToRange search =
    let
        splitString =
            String.split "-" search
    in
    case List.length splitString of
        1 ->
            [ 0 ]

        _ ->
            splitString
                |> List.map
                    (\r ->
                        case toInt r of
                            Success n ->
                                n

                            Err _ ->
                                [ 0 ]
                    )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Main


main : Program flags Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

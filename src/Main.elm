module Main exposing (..)

import Browser exposing (element)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Http exposing (Header, header)
import Json.Decode as Decode exposing (Decoder, Value, at, decodeValue, errorToString, fail, field, int, list, null, oneOf, string, value)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (Config, getWithConfig)
import Element exposing (el, layout, table, Element, fill)
import String exposing (toUpper)


-- Msg


type Msg
    = NoOp
    | HandleRecordsResponse (WebData KeywordCollection)



-- Model


type alias Keyword =
    { name : String
    , searches : List Int
    , engagement : List Int
    , competition : Int
    , month : Month
    }


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
    | None


type alias Model =
    { keywords : WebData KeywordCollection
    }


type alias KeywordCollection =
    List Keyword


initialModel : Value -> ( Model, Cmd Msg )
initialModel flag =
    let
        decodedToken =
            case decodeValue string flag of
                Ok token ->
                    token

                Err err ->
                    errorToString err
    in
    ( { keywords = Loading }, getRecords decodedToken )



-- View
renderMonth : Month -> Element msg
renderMonth m =
    case m of
    Sept ->
        Element.text "September"

    Oct ->
        Element.text "October"

    Nov ->
        Element.text "November"

    Dec ->
        Element.text "December"

    Jan ->
        Element.text "January"

    Feb ->
        Element.text "February"

    Mar ->
        Element.text "March"

    Apr ->
        Element.text "April"

    May ->
        Element.text "May"

    Jun ->
        Element.text "Jun"

    Jul ->
        Element.text "July"

    Aug ->
        Element.text "August"

    None ->
        Element.text "None"
    
renderRange : List Int  -> Element msg
renderRange range =
    let
        s  a =
            a |> String.fromInt
    in
    case range of
        head :: tail ->
            el [] ( Element.text (s head ++ ".." ++ (
                case (tail |> List.reverse) of
                    innerHead :: innerTail ->
                        s innerHead

                    _ ->
                        ""
                    )
                )
            )

        _ ->
            Element.none


view : Model -> Html Msg
view model =
    case model.keywords of
        Success data ->
            layout []
            <| table []
                { data = data
                , columns = 
                    [ 
                        { header = Element.text "Keyword"
                        , width = fill
                        , view = \e -> Element.text e.name 
                        }
                        , 
                        { header = Element.text "Searches"
                        , width = fill
                        , view = \e -> (e.searches |> renderRange )
                        }
                        , 
                        { header = Element.text "Engagement"
                        , width = fill
                        , view = \e -> (e.engagement |> renderRange )
                        }
                        ,
                        { header = Element.text "Competition"
                        , width = fill
                        , view = \e -> Element.text (e.competition |> String.fromInt)
                        }
                        ,
                        { header = Element.text "Month"
                        , width = fill
                        , view = \e -> renderMonth e.month 
                        }

                    ]

                }

            
        Loading ->
            layout []
            Element.none

        NotAsked ->
            layout []
            Element.none

        Failure _ ->
            layout []
            Element.none




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleRecordsResponse d ->
            ( { model | keywords = d }, Cmd.none )
                |> Debug.log "model"

        NoOp ->
            ( model, Cmd.none )


airTableConfig : String -> Config
airTableConfig token =
    let
        updatedTokenHeader =
            tokenHeader token
    in
    { headers = [ updatedTokenHeader ]
    , withCredentials = False
    , timeout = Nothing
    }


tokenHeader : String -> Header
tokenHeader token =
    header "Authorization" ("Bearer " ++ token)


getRecords : String -> Cmd Msg
getRecords token =
    getWithConfig (airTableConfig token) "https://api.airtable.com/v0/appJDieY6AzAbRCSk/Table%201" HandleRecordsResponse decodeRecords


decodeRecords : Decode.Decoder KeywordCollection
decodeRecords =
    field "records" (list decodeKeyword)



-- |> decodeKeyword


decodeKeyword : Decode.Decoder Keyword
decodeKeyword =
    oneOf
        [
        field "fields"
            (Decode.map5
                Keyword
                (field "Keyword" string)
                (field "Searches" toRange)
                (field "Engagement" toRange)
                (field "Competition" toInt)
                (field "Month Of Most Engagement" toMonth)
            )
        , field "fields"  (value |> Decode.map (\e -> Keyword "" [] [] 0 None))
        ]


toInt : Decoder Int
toInt =
    string
        |> Decode.map convertToInt


toRange : Decoder (List Int)
toRange =
    string
        |> Decode.map convertToRange


toMonth : Decoder Month
toMonth =
    string
        |> Decode.map convertToMonth


convertToMonth : String -> Month
convertToMonth month =
    case month of
        "Sept" ->
            Sept

        "Oct" ->
            Oct

        "Nov" ->
            Nov

        "Dec" ->
            Dec

        "Jan" ->
            Jan

        "Feb" ->
            Feb

        "Mar" ->
            Mar

        "Apr" ->
            Apr

        "May" ->
            May

        "Jun" ->
            Jun

        "Jul" ->
            Jul

        "Aug" ->
            Aug

        _ ->
            None


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
                        case String.toInt r of
                            Just n ->
                                n

                            Nothing ->
                                0
                    )


convertToInt : String -> Int
convertToInt string =
    case String.toInt string of
        Just s ->
            s

        Nothing ->
            0



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Main


main : Program Value Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

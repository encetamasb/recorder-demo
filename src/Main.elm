port module Main exposing (main)

import Array exposing (Array)
import Browser
import Debug
import Html exposing (Html, audio, button, div, input, label, p, pre, text)
import Html.Attributes exposing (controls, disabled, for, name, placeholder, src, style, title, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Task


defaultSecs : Int
defaultSecs =
    15


maxSecs : Int
maxSecs =
    60


minSecs : Int
minSecs =
    1


tickDelta : Float
tickDelta =
    1000


type alias ID =
    Int


type alias Item =
    { id : ID
    , title : String
    , length : Int
    , mime : Maybe String
    , url : Maybe String
    , size : Maybe Int
    }


type alias Input =
    { title : Maybe String
    , length : Result String Int
    }


type alias ValidInput =
    { title : String
    , length : Int
    }


type DownCounter
    = DownCounter Int


type Status
    = Configuring Input
    | Initalized ValidInput
    | Recording Item DownCounter
    | WentWrong Item String
    | Success Item


type alias Model =
    { nextId : ID
    , status : Status
    }


type RecorderEvent
    = StartedEvent ID
    | StoppedEvent ID { url : String, mime : String }
    | DataChunkEvent ID { size : Int }


type Msg
    = TitleChanged String
    | LengthChanged String
    | RecordClicked
    | StopClicked
    | Tick ID
    | NewClicked
    | FromRecorder (Result String RecorderEvent)


initialStatus : Status
initialStatus =
    Configuring
        { title = Nothing
        , length = Ok defaultSecs
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { nextId = 0
      , status = initialStatus
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ status, nextId } as model) =
    let
        toTitle s =
            case s of
                "" ->
                    Nothing

                _ ->
                    Just s

        toLength s =
            Result.fromMaybe s (Maybe.map (max minSecs << min maxSecs) (String.toInt s))

        toStatus ({ title, length } as input) =
            case ( title, length ) of
                ( Just t, Ok l ) ->
                    Initalized { title = t, length = l }

                _ ->
                    Configuring input

        toInput validInput =
            { title = Just validInput.title
            , length = Ok validInput.length
            }
    in
    case ( status, msg ) of
        ( Configuring input, TitleChanged s ) ->
            ( { model | status = toStatus { input | title = toTitle s } }, Cmd.none )

        ( Configuring input, LengthChanged s ) ->
            ( { model | status = toStatus { input | length = toLength s } }, Cmd.none )

        ( Initalized valid, TitleChanged s ) ->
            let
                input =
                    toInput valid
            in
            ( { model | status = toStatus { input | title = toTitle s } }, Cmd.none )

        ( Initalized valid, LengthChanged s ) ->
            let
                input =
                    toInput valid
            in
            ( { model | status = toStatus { input | length = toLength s } }, Cmd.none )

        ( Initalized { title, length }, RecordClicked ) ->
            ( { model | nextId = nextId + 1 }, sendStartedEvent nextId length )

        ( Initalized { title, length }, FromRecorder (Ok (StartedEvent id)) ) ->
            let
                item =
                    { title = title
                    , length = length
                    , id = id
                    , size = Nothing
                    , mime = Nothing
                    , url = Nothing
                    }
            in
            ( { model | status = Recording item (DownCounter length) }
            , tick item.id
            )

        ( Recording item _, StopClicked ) ->
            ( { model | status = Success item }, sendStoppedEvent item.id )

        ( Recording item _, FromRecorder (Ok (StoppedEvent id { mime, url })) ) ->
            if item.id == id then
                let
                    updatedItem =
                        { item | mime = Just mime, url = Just url }
                in
                ( { model | status = Success updatedItem }, Cmd.none )

            else
                ( model, Cmd.none )

        ( Recording item downc, FromRecorder (Ok (DataChunkEvent id { size })) ) ->
            if item.id == id then
                let
                    updatedItem =
                        { item | size = Just <| Maybe.withDefault 0 item.size + size }
                in
                ( { model | status = Recording updatedItem downc }, Cmd.none )

            else
                ( model, Cmd.none )

        ( Recording item (DownCounter remaining), Tick id ) ->
            case ( item.id == id, remaining ) of
                ( False, _ ) ->
                    ( model, Cmd.none )

                ( True, 0 ) ->
                    ( { model | status = Recording item (DownCounter 0) }, sendStoppedEvent id )

                _ ->
                    ( { model | status = Recording item (DownCounter <| remaining - 1) }, tick item.id )

        ( Success item, NewClicked ) ->
            ( { model | status = initialStatus }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view ({ status } as model) =
    let
        ( recTitle, recLength, inputsDisabled ) =
            case status of
                Configuring { title, length } ->
                    ( Maybe.withDefault "" title
                    , case length of
                        Ok l ->
                            String.fromInt l

                        Err s ->
                            s
                    , False
                    )

                Initalized { title, length } ->
                    ( title, String.fromInt length, False )

                Recording { title, length } _ ->
                    ( title, String.fromInt length, True )

                Success { title, length } ->
                    ( title, String.fromInt length, True )

                WentWrong { title, length } err ->
                    ( title, String.fromInt length, True )

        buttonView =
            case status of
                Configuring _ ->
                    button [ disabled True, title "Record" ] [ text "Record" ]

                Initalized s ->
                    button [ onClick RecordClicked, title "Record" ] [ text "Record" ]

                Recording item (DownCounter n) ->
                    let
                        s =
                            "Stop (" ++ String.fromInt n ++ ")"
                    in
                    button [ onClick StopClicked, title s ] [ text s ]

                WentWrong item err ->
                    button [ onClick NewClicked, title "New" ] [ text "New" ]

                Success item ->
                    button [ onClick NewClicked, title "New" ] [ text "New" ]

        audioView =
            case status of
                Success { url, mime } ->
                    case ( url, mime ) of
                        ( Just source, Just mtype ) ->
                            div []
                                [ audio
                                    [ src source
                                    , type_ mtype
                                    , controls True
                                    ]
                                    []
                                ]

                        _ ->
                            text ""

                _ ->
                    text ""
    in
    div []
        [ p []
            [ label [ style "display" "block", for "title" ] [ text "Title of recording" ]
            , input
                [ type_ "text"
                , name "title"
                , value recTitle
                , disabled inputsDisabled
                , placeholder "Type here.."
                , onInput TitleChanged
                ]
                []
            ]
        , p
            []
            [ label [ style "display" "block", for "length" ] [ text "Length in seconds" ]
            , input
                [ type_ "number"
                , name "length"
                , value recLength
                , disabled inputsDisabled
                , onInput LengthChanged
                ]
                []
            ]
        , buttonView
        , audioView
        , pre
            []
            [ text << Debug.toString <| model.status ]
        ]


tick : ID -> Cmd Msg
tick id =
    Process.sleep tickDelta |> Task.perform (always (Tick id))


sendStartedEvent id length =
    toRecorder <|
        Encode.object
            [ ( "event", Encode.string "started" )
            , ( "id", Encode.int id )
            , ( "length", Encode.int length )
            ]


sendStoppedEvent id =
    toRecorder <|
        Encode.object
            [ ( "event", Encode.string "stopped" )
            , ( "id", Encode.int id )
            ]


decodeEvent =
    Decode.map5
        (\event id size mime url ->
            { event = event
            , id = id
            , maybeSize = size
            , maybeMime = mime
            , maybeUrl = url
            }
        )
        (Decode.field "event" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.maybe (Decode.field "size" Decode.int))
        (Decode.maybe (Decode.field "mime" Decode.string))
        (Decode.maybe (Decode.field "url" Decode.string))


mapToRecorderEvent : Decode.Value -> Result String RecorderEvent
mapToRecorderEvent json =
    case Debug.log "a" (Decode.decodeValue decodeEvent json) of
        Ok { event, id, maybeSize, maybeMime, maybeUrl } ->
            case event of
                "started" ->
                    Ok (StartedEvent id)

                "stopped" ->
                    case ( maybeMime, maybeUrl ) of
                        ( Just mime, Just url ) ->
                            Ok (StoppedEvent id { url = url, mime = mime })

                        _ ->
                            Err "Missing data url or mime"

                "datachunk" ->
                    case maybeSize of
                        Just size ->
                            Ok (DataChunkEvent id { size = size })

                        Nothing ->
                            Err "Missing data size"

                m ->
                    Err <| "Unknown message (" ++ m ++ ")"

        Err err ->
            Err "Decoding error"


port toRecorder : Encode.Value -> Cmd msg


port fromRecorder : (Decode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fromRecorder (FromRecorder << mapToRecorderEvent)
        ]


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

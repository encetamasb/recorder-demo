port module Main exposing (main)

import Array exposing (Array)
import Browser
import Debug
import Html exposing (Html, a, audio, button, div, input, label, p, pre, span, text)
import Html.Attributes exposing (autofocus, controls, disabled, download, for, href, name, placeholder, src, style, title, type_, value, width)
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
    { title : Maybe (Result String String)
    , length : Result String Int
    }


type alias ValidInput =
    { title : String
    , length : Int
    }


type DownCounter
    = DownCounter Int


type Untouched
    = Untouched
    | Touched


type Enabled
    = Enabled
    | Disabled


enabledToBool : Enabled -> Bool
enabledToBool v =
    case v of
        Enabled ->
            True

        Disabled ->
            False


type Status
    = Configuring Input Untouched
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
    | StoppedEvent ID (Maybe { url : String, mime : String })
    | ResetedEvent
    | DataChunkEvent ID { size : Int }


type Msg
    = TitleChanged String
    | LengthChanged String
    | RecordClicked
    | StopClicked
    | Tick ID
    | ResetClicked
    | FromRecorder (Result String RecorderEvent)


initialStatus : Status
initialStatus =
    Configuring
        { title = Nothing
        , length = Ok defaultSecs
        }
        Untouched


initialModel : Model
initialModel =
    { nextId = 0
    , status = initialStatus
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ status, nextId } as model) =
    let
        toTitle s =
            case String.trim s of
                "" ->
                    Just (Err s)

                _ ->
                    Just (Ok s)

        toLength s =
            Result.fromMaybe s (Maybe.map (max minSecs << min maxSecs) (String.toInt s))

        toStatus ({ title, length } as input) =
            case ( title, length ) of
                ( Just (Ok t), Ok l ) ->
                    Initalized { title = t, length = l }

                _ ->
                    Configuring input Touched

        toInput validInput =
            { title = Just (Ok validInput.title)
            , length = Ok validInput.length
            }
    in
    case ( status, msg ) of
        ( Configuring input _, TitleChanged s ) ->
            ( { model | status = toStatus { input | title = toTitle s } }, Cmd.none )

        ( Configuring input _, LengthChanged s ) ->
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
                    { title = String.trim title
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

        ( Recording item dc, StopClicked ) ->
            ( model, sendStoppedEvent item.id )

        ( Recording item _, FromRecorder (Ok (StoppedEvent id (Just { mime, url }))) ) ->
            if item.id == id then
                let
                    updatedItem =
                        { item | mime = Just mime, url = Just url }
                in
                ( { model | status = Success updatedItem }, Cmd.none )

            else
                ( model, Cmd.none )

        ( Recording item _, FromRecorder (Ok (StoppedEvent id Nothing)) ) ->
            if item.id == id then
                ( { model | status = WentWrong item "No recording data" }, Cmd.none )

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

        ( _, FromRecorder (Ok ResetedEvent) ) ->
            let
                new =
                    initialModel
            in
            ( { new | nextId = model.nextId }, Cmd.none )

        ( Recording item (DownCounter remaining), Tick id ) ->
            case ( item.id == id, remaining ) of
                ( False, _ ) ->
                    ( model, Cmd.none )

                ( True, 0 ) ->
                    ( { model | status = Recording item (DownCounter 0) }, sendStoppedEvent id )

                _ ->
                    ( { model | status = Recording item (DownCounter <| remaining - 1) }, tick item.id )

        ( _, ResetClicked ) ->
            ( model, sendResetedEvent )

        ( st, event ) ->
            let
                _ =
                    Debug.log "unhandled msg" ( st, event )
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view ({ status } as model) =
    let
        buttonView t msg isDisabled =
            case isDisabled of
                Disabled ->
                    button [ disabled True, title t ] [ text t ]

                Enabled ->
                    button [ onClick msg, title t ] [ text t ]

        buttonsView =
            let
                recBtn isDisabled =
                    buttonView "Record" RecordClicked isDisabled

                stopBtn isDisabled =
                    buttonView "Stop" StopClicked isDisabled

                resetBtn isDisabled =
                    buttonView "Reset" ResetClicked isDisabled

                downStopBtn s isDisabled =
                    buttonView s StopClicked isDisabled
            in
            case status of
                Configuring _ _ ->
                    div []
                        [ recBtn Disabled, stopBtn Disabled, resetBtn Enabled ]

                Initalized s ->
                    div []
                        [ recBtn Enabled, stopBtn Disabled, resetBtn Enabled ]

                Recording item (DownCounter n) ->
                    let
                        s =
                            "Stop (" ++ String.fromInt n ++ ")"
                    in
                    div []
                        [ recBtn Disabled, downStopBtn s Enabled, resetBtn Enabled ]

                WentWrong item err ->
                    div []
                        [ recBtn Disabled, stopBtn Disabled, resetBtn Enabled ]

                Success item ->
                    div []
                        [ recBtn Disabled, stopBtn Disabled, resetBtn Enabled ]

        audioView =
            case status of
                Success { title, url, mime } ->
                    case ( url, mime ) of
                        ( Just source, Just mtype ) ->
                            let
                                ext =
                                    Maybe.withDefault "bin" (mimeToExtension mtype)
                            in
                            p []
                                [ audio
                                    [ src source
                                    , type_ mtype
                                    , controls True
                                    ]
                                    []
                                , p []
                                    [ a [ href source, download (title ++ "." ++ ext) ] [ text "Download" ]
                                    ]
                                ]

                        _ ->
                            text ""

                _ ->
                    text ""
    in
    div []
        [ inputsView status
        , buttonsView
        , audioView
        , pre
            []
            [ text << Debug.toString <| model.status ]
        ]


inputsView : Status -> Html Msg
inputsView status =
    let
        ( ( title_, titleErr ), ( length_, lengthErr ), inputsDisabled ) =
            case status of
                Configuring { title, length } untouched ->
                    let
                        ifTouched =
                            case untouched of
                                Untouched ->
                                    always Nothing

                                Touched ->
                                    Just
                    in
                    ( case title of
                        Just (Ok t) ->
                            ( t, Nothing )

                        Just (Err t) ->
                            ( t, ifTouched "Required." )

                        Nothing ->
                            ( "", ifTouched "Required." )
                    , case length of
                        Ok l ->
                            ( String.fromInt l, Nothing )

                        Err s ->
                            ( s
                            , ifTouched <| "Must be a whole number between " ++ String.fromInt minSecs ++ " and " ++ String.fromInt maxSecs
                            )
                    , Enabled
                    )

                Initalized { title, length } ->
                    ( ( title, Nothing ), ( String.fromInt length, Nothing ), Enabled )

                Recording { title, length } _ ->
                    ( ( title, Nothing ), ( String.fromInt length, Nothing ), Disabled )

                Success { title, length } ->
                    ( ( title, Nothing ), ( String.fromInt length, Nothing ), Disabled )

                WentWrong { title, length } err ->
                    ( ( title, Nothing ), ( String.fromInt length, Nothing ), Disabled )

        inputError m =
            case m of
                Just err ->
                    span [ style "margin-left" "5px", style "color" "red" ] [ text err ]

                Nothing ->
                    text ""
    in
    div []
        [ p []
            [ label [ style "display" "block", for "title" ] [ text "Title of recording" ]
            , input
                [ type_ "text"
                , name "title"
                , value title_
                , (disabled << not << enabledToBool) inputsDisabled
                , placeholder "Type here.."
                , onInput TitleChanged
                ]
                []
            , inputError titleErr
            ]
        , p
            []
            [ label [ style "display" "block", for "length" ] [ text "Length in seconds" ]
            , input
                [ type_ "number"
                , name "length"
                , value length_
                , (disabled << not << enabledToBool) inputsDisabled
                , onInput LengthChanged
                ]
                []
            , inputError lengthErr
            ]
        ]


mimeToExtension : String -> Maybe String
mimeToExtension mime =
    let
        match ( s, ext ) =
            String.startsWith s mime

        possible =
            [ ( "audio/webm", "webm" )
            , ( "audio/ogg", "ogg" )
            , ( "audio/vorbis", "ogg" )
            , ( "audio/mp4", "mp4" )
            , ( "audio/m4a", "m4a" )
            , ( "audio/mpeg", "mp3" )
            ]
    in
    Debug.log "ext" (possible |> List.filter match) |> List.head |> Maybe.map (\( _, snd ) -> snd)


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


sendResetedEvent =
    toRecorder <|
        Encode.object
            [ ( "event", Encode.string "reseted" )
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
                            Ok (StoppedEvent id (Just { url = url, mime = mime }))

                        _ ->
                            Ok (StoppedEvent id Nothing)

                "datachunk" ->
                    case maybeSize of
                        Just size ->
                            Ok (DataChunkEvent id { size = size })

                        Nothing ->
                            Err "Missing data size"

                "reseted" ->
                    Ok ResetedEvent

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

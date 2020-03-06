port module Main exposing (main)

import Browser
import Debug
import Html exposing (Html, button, div, input, label, p, pre, text)
import Html.Attributes exposing (disabled, for, name, placeholder, style, title, type_, value, width)
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


type alias ID =
    Int


type alias Item =
    { id : ID
    , title : String
    , length : Int
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
    | Success Item


type alias Model =
    { nextId : ID
    , status : Status
    }


type RecorderEvent
    = StartedEvent ID
    | StoppedEvent ID


type Msg
    = TitleChanged String
    | LengthChanged String
    | RecordClicked
    | StopClicked
    | Tick ID
    | NewClicked
    | FromRecorder (Maybe RecorderEvent)


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
            ( { model | nextId = nextId + 1 }, sendStartedEvent nextId )

        ( Initalized { title, length }, FromRecorder (Just (StartedEvent id)) ) ->
            let
                item =
                    { title = title
                    , length = length
                    , id = id
                    }
            in
            ( { model | status = Recording item (DownCounter length) }
            , tick item.id
            )

        ( Recording item _, StopClicked ) ->
            ( { model | status = Success item }, sendStoppedEvent item.id )

        ( Recording item _, FromRecorder (Just (StoppedEvent id)) ) ->
            if item.id == id then
                ( { model | status = Success item }, Cmd.none )

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

                Recording item _ ->
                    ( item.title, String.fromInt item.length, True )

                Success item ->
                    ( item.title, String.fromInt item.length, True )

        buttonView =
            case status of
                Configuring _ ->
                    button [ disabled True, title "Record" ] [ text "Record" ]

                Initalized s ->
                    button [ onClick RecordClicked, title "Record" ] [ text "Record" ]

                Recording item _ ->
                    button [ onClick StopClicked, title "Stop" ] [ text "Stop" ]

                Success item ->
                    button [ onClick NewClicked, title "New" ] [ text "New" ]
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
        , pre
            []
            [ text << Debug.toString <| model.status ]
        ]


tick : ID -> Cmd Msg
tick id =
    Process.sleep 1000 |> Task.perform (always (Tick id))


eventObject event id =
    Encode.object
        [ ( "event", Encode.string event )
        , ( "id", Encode.int id )
        ]


sendStartedEvent id =
    toRecorder (eventObject "started" id)


sendStoppedEvent id =
    toRecorder (eventObject "stopped" id)


decodeEvent =
    Decode.map2 (\event id -> { event = event, id = id })
        (Decode.field "event" Decode.string)
        (Decode.field "id" Decode.int)


mapToRecorderEvent : Decode.Value -> Maybe RecorderEvent
mapToRecorderEvent json =
    case Debug.log "in" (Decode.decodeValue decodeEvent json) of
        Ok { event, id } ->
            case event of
                "started" ->
                    Just <| StartedEvent id

                "stopped" ->
                    Just <| StoppedEvent id

                _ ->
                    Nothing

        Err err ->
            Nothing


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

port module Main exposing (main)

import Browser
import Css exposing (px)
import Html.Styled exposing (Html, toUnstyled, main_, textarea, pre, text, ol, li)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onInput)



---- PORTS ----

port run : String -> Cmd msg
port result : (String -> msg) -> Sub msg
port error : (Maybe ParseError -> msg) -> Sub msg



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = \model -> { title = "Lox", body = [ model |> view |> toUnstyled ] }
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.batch [ result Result, error Error ]
        }



---- MODEL ----


type alias Model =
    { input : String
    , result : String
    , error : List ParseError
    }


type alias ParseError =
    { line : Int
    , message : String
    }


defaultModel : Model
defaultModel =
    { input = ""
    , result = ""
    , error = []
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Input String
    | Result String
    | Error (Maybe ParseError)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Input s ->
            ( { model | input = s, error = [], result = "" }
            , run s
            )
        Result s ->
            ( { model | result = s }
            , Cmd.none
            )
        Error reportedError ->
            case reportedError of
                Nothing ->
                    ( { model | error = [] }
                    , Cmd.none
                    )
                Just e ->
                    ( { model | error = model.error ++ [e] }
                    , Cmd.none
                    )





---- VIEW ----


view : Model -> Html Msg
view model =
    main_
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "1fr 1fr"
            , Css.property "grid-template-rows" "500px auto"
            ]
        ]
        [ viewEditor model
        , viewResults model
        , viewError model
        ]


viewEditor : Model -> Html Msg
viewEditor model =
    textarea
        [ onInput Input
        ]
        []


viewResults : Model -> Html Msg
viewResults model =
    pre
        []
        [ text model.result
        ]


viewError : Model -> Html Msg
viewError model =
    case model.error of
        [] -> text ""
        errors ->
            pre
                []
                [ ol
                    []
                    ( List.map (\e ->
                        li
                            []
                            [ text ("Error on line " ++ String.fromInt e.line ++ ":\n")
                            , text e.message
                            ]
                    )
                    errors
                )
                ]
                



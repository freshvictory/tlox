port module Main exposing (main)

import Browser
import Css exposing (hex, pct, rem)
import Html.Styled as E exposing (Html, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onInput)
import Json.Decode exposing (Decoder, field)



---- PORTS ----


port run : String -> Cmd msg


port result : (Json.Decode.Value -> msg) -> Sub msg


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
    , result : List Token
    , error : List ParseError
    }


defaultModel : Model
defaultModel =
    { input = ""
    , result = []
    , error = []
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )



---- UPDATE ----


type Msg
    = Input String
    | Result Json.Decode.Value
    | Error (Maybe ParseError)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input s ->
            ( { model | input = s, error = [], result = [] }
            , run s
            )

        Result v ->
            case Json.Decode.decodeValue (Json.Decode.list decodeToken) v of
                Ok tokens ->
                    ( { model | result = tokens }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | result = [] }
                    , Cmd.none
                    )

        Error reportedError ->
            case reportedError of
                Nothing ->
                    ( { model | error = [] }
                    , Cmd.none
                    )

                Just e ->
                    ( { model | error = model.error ++ [ e ] }
                    , Cmd.none
                    )



---- VIEW ----


view : Model -> Html Msg
view model =
    E.div
        []
        [ viewHeader
        , viewBody model
        ]


viewBody : Model -> Html Msg
viewBody model =
    E.main_
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


viewHeader : Html Msg
viewHeader =
    E.header
        [ css
            [ Css.displayFlex
            , Css.backgroundColor (hex "#402945")
            , Css.color (hex "#fff")
            , Css.padding (rem 1)
            ]
        ]
        [ E.h1
            [ css
                [ Css.margin Css.zero
                ]
            ]
            [ E.text "Lox Interpreter" ]
        ]


viewEditor : Model -> Html Msg
viewEditor _ =
    E.textarea
        [ onInput Input
        , css
            [ Css.padding (rem 0.75)
            , Css.margin (rem 0.75)
            , Css.borderRadius (rem 0.25)
            , Css.fontSize Css.inherit
            , Css.fontFamily Css.monospace
            ]
        ]
        []


viewResults : Model -> Html Msg
viewResults model =
    E.section
        [ css
            [ Css.margin (rem 0.75)
            ]
        ]
        [ viewTokens model
        ]


viewTokens : Model -> Html Msg
viewTokens model =
    let
        tokensByLine =
            groupBy .line model.result
    in
    E.ol
        [ css
            [ Css.property "display" "grid"
            , Css.property "row-gap" "0.5rem"
            ]
        ]
        (List.map
            (\line ->
                let
                    lineNumber =
                        case List.head line of
                            Nothing ->
                                0

                            Just t ->
                                t.line
                in
                E.li
                    [ css
                        [ Css.property "display" "grid"
                        , Css.property "row-gap" "0.5rem"
                        , Css.property "column-gap" "0.75rem"
                        , Css.property "grid-template-columns" "auto 1fr"
                        , Css.alignItems Css.baseline
                        , Css.padding3 (rem 0.5) (rem 0.75) Css.zero
                        , Css.borderRadius (rem 0.5)
                        , if modBy 2 lineNumber == 0 then
                            Css.backgroundColor (hex "#cdcdcd")

                          else
                            Css.backgroundColor (hex "#eee")
                        ]
                    ]
                    [ E.span
                        [ css
                            [ Css.lineHeight (Css.num 1)
                            ]
                        ]
                        [ E.text (String.fromInt lineNumber) ]
                    , viewLine line
                    ]
            )
            tokensByLine
        )


viewLine : List Token -> Html Msg
viewLine tokens =
    E.ol
        [ css
            [ Css.displayFlex
            , Css.flexWrap Css.wrap
            ]
        ]
        (List.map
            (\t ->
                E.li
                    [ css
                        [ Css.marginRight (rem 0.5)
                        , Css.lastChild [ Css.marginRight Css.zero ]
                        , Css.marginBottom (rem 0.5)
                        ]
                    ]
                    [ if t.token == "IDENTIFIER" || t.token == "NUMBER" || t.token == "STRING" then
                        viewTokenLiteral t

                      else
                        viewToken t
                    ]
            )
            tokens
        )


viewToken : Token -> Html Msg
viewToken token =
    E.pre
        [ css
            [ Css.maxWidth Css.maxContent
            , Css.display Css.inlineFlex
            , Css.backgroundColor (hex "#c3f0e4")
            , Css.borderRadius (rem 0.25)
            , Css.padding (rem 0.5)
            , Css.lineHeight (Css.num 1)
            , Css.margin Css.zero
            ]
        ]
        [ E.text token.token
        ]


viewTokenLiteral : Token -> Html Msg
viewTokenLiteral token =
    E.pre
        [ css
            [ Css.maxWidth Css.maxContent
            , Css.display Css.inlineFlex
            , Css.backgroundColor (hex "#fff")
            , Css.borderRadius (rem 0.25)
            , Css.padding (rem 0.5)
            , Css.lineHeight (Css.num 1)
            , Css.margin Css.zero
            ]
        ]
        [ E.text token.lexeme
        ]


viewError : Model -> Html Msg
viewError model =
    case model.error of
        [] ->
            E.text ""

        errors ->
            E.pre
                []
                [ E.ol
                    []
                    (List.map
                        (\e ->
                            E.li
                                []
                                [ E.text ("Error on line " ++ String.fromInt e.line ++ ":\n")
                                , E.text e.message
                                ]
                        )
                        errors
                    )
                ]



---- SCANNER ----


type alias ParseError =
    { line : Int
    , message : String
    }


type alias Token =
    { token : String
    , lexeme : String
    , line : Int
    , literal : Maybe TokenLiteral
    }


type TokenLiteral
    = Str String
    | Num Int


decodeToken : Decoder Token
decodeToken =
    Json.Decode.map4 Token
        (field "type" Json.Decode.string)
        (field "lexeme" Json.Decode.string)
        (field "line" Json.Decode.int)
        (Json.Decode.maybe (field "literal" decodeTokenLiteral))


decodeTokenLiteral : Decoder TokenLiteral
decodeTokenLiteral =
    Json.Decode.oneOf
        [ Json.Decode.map Str Json.Decode.string
        , Json.Decode.map Num Json.Decode.int
        ]



---- HELPERS ----


groupBy : (a -> b) -> List a -> List (List a)
groupBy prop l =
    case l of
        [] ->
            []

        x :: xs ->
            let
                y =
                    List.filter (\s -> prop x == prop s) xs

                z =
                    List.filter (\s -> prop s /= prop x) xs
            in
            (x :: y) :: groupBy prop z

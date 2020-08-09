port module Main exposing (main)

import Browser
import Css exposing (Style, hex, pct, px, rem)
import Html.Styled as E exposing (Html, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onInput)
import Json.Decode exposing (Decoder, field)
import String
import List
import Theme exposing (Theme, light, dark)
import Css.Media



---- PORTS ----


port run : String -> Cmd msg


port scanResult : (Json.Decode.Value -> msg) -> Sub msg

port parseResult : (Json.Decode.Value -> msg) -> Sub msg


port error : (Maybe ParseError -> msg) -> Sub msg



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = \model -> { title = "Lox", body = [ model |> view |> toUnstyled ] }
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.batch
            [ scanResult ScanResult
            , parseResult ParseResult
            , error Error
            ]
        }



---- MODEL ----


type alias Model =
    { input : String
    , scanResult : List Token
    , parseResult : Maybe Expr
    , error : List ParseError
    , tab : Tab
    , hover : Maybe Token
    }


type Tab
    = Scanner
    | Parser


defaultModel : Model
defaultModel =
    { input = ""
    , scanResult = []
    , parseResult = Nothing
    , error = []
    , tab = Parser
    , hover = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )



---- UPDATE ----


type Msg
    = Input String
    | ScanResult Json.Decode.Value
    | ParseResult Json.Decode.Value
    | Error (Maybe ParseError)
    | TabChange Tab
    | Hover (Maybe Token)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input s ->
            ( { model | input = s, error = [], scanResult = [] }
            , run s
            )

        ScanResult v ->
            case Json.Decode.decodeValue (Json.Decode.list decodeToken) v of
                Ok tokens ->
                    ( { model | scanResult = tokens }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | scanResult = [] }
                    , Cmd.none
                    )

        ParseResult p ->
            case Json.Decode.decodeValue decodeExpr p of
                Ok expr ->
                    ( { model | parseResult = Just expr }
                    , Cmd.none
                    )
                
                Err _ ->
                    ( { model | parseResult = Nothing }
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

        TabChange t ->
            ( { model | tab = t }
            , Cmd.none
            )


        Hover t ->
            ( { model | hover = t }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    E.div
        [ css
            [ themed
                [ (Css.backgroundColor, .background)
                , (Css.color, .text)
                ]
            ]
        , Html.Styled.Attributes.id "app"
        ]
        [ viewHeader
        , viewBody model
        ]


viewBody : Model -> Html Msg
viewBody model =
    E.main_
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "1fr 1fr"
            , Css.property "grid-template-rows" "auto"
            , Css.property "column-gap" "2rem"
            , Css.maxWidth (px 1200)
            , Css.margin Css.auto
            , Css.padding (rem 0.75)
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
viewEditor model =
    E.section
        [ css
            [ Css.borderRadius (rem 0.25)
            , Css.position Css.relative
            , themed
                [ (Css.boxShadow4 Css.zero Css.zero (px 10), .shadow )
                , (Css.backgroundColor, .softBackground)
                ]
            ]
        ]
        [ viewInput
        , viewSource model
        ]


viewInput : Html Msg
viewInput =
    E.textarea
        [ onInput Input
        , css
            [ Css.borderRadius (rem 0.25)
            , Css.padding (rem 0.75)
            , Css.border Css.zero
            , Css.color Css.inherit
            , Css.fontSize Css.inherit
            , Css.width (pct 100)
            , Css.boxSizing Css.borderBox
            , Css.resize Css.none
            , Css.overflow Css.auto
            , Css.fontFamily Css.monospace
            , Css.margin Css.zero
            , Css.backgroundColor Css.transparent
            , Css.property "caret-color" "inherit"
            ]
        , Html.Styled.Attributes.autocomplete False
        , Html.Styled.Attributes.attribute "autocapitalize" "none"
        , Html.Styled.Attributes.spellcheck False
        , Html.Styled.Attributes.rows 40
        ]
        []


viewSource : Model -> Html Msg
viewSource model =
    let
        lines = groupBy .line model.scanResult
    in
    E.code
        [ css
            [ Css.margin (rem 0.75)
            , Css.position Css.absolute
            , Css.left Css.zero
            , Css.top Css.zero
            , Css.pointerEvents Css.none
            , themed [(Css.textShadow4 Css.zero Css.zero (px 1), .softBackground)]
            ]
        ]
        ( List.map
            (\t -> viewSourceLine model t)
            lines
        )



viewSourceLine : Model -> List Token -> Html Msg
viewSourceLine model tokens =
    E.pre
        [ css
            [ Css.margin Css.zero
            ]
        ]
        ( List.map
            ( \t -> viewTokenSource model t)
            tokens
        )


viewTokenSource : Model -> Token -> Html Msg
viewTokenSource model token =
    E.span
        [ Html.Styled.Attributes.class token.tokenTypeString
        , Html.Styled.Attributes.class "token"
        , Html.Styled.Attributes.class ( case model.hover of
                Just t ->
                    if t == token
                        then
                            "hover"
                        else
                            ""
                _ -> ""
            )
        ]
        [ E.text token.lexeme
        ]

        


viewResults : Model -> Html Msg
viewResults model =
    E.section
        [ css
            [ Css.property "display" "grid"
            , Css.property "row-gap" "0.5rem"
            , Css.property "grid-auto-rows" "max-content"
            ]
        ]
        [ E.div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.center
                ]
            ]
            [ viewTabRadio model "scanner" "Scanner" Scanner
            , viewTabRadio model "parser" "Parser" Parser
            ]
        , case model.tab of
            Scanner ->
                viewTokens model

            Parser ->
                viewParserResults model
        ]


viewTabRadio : Model -> String -> String -> Tab -> Html Msg
viewTabRadio model id label tab =
    E.div
        [ css
            [ Css.lineHeight (Css.num 1)
            , Css.padding (rem 0.5)
            , Css.border3 (px 1) Css.solid (hex light.purple)
            , Css.display Css.block
            , Css.firstChild
                [ Css.borderTopLeftRadius (rem 0.25)
                , Css.borderBottomLeftRadius (rem 0.25)
                ]
            , Css.lastChild
                [ Css.borderTopRightRadius (rem 0.25)
                , Css.borderBottomRightRadius (rem 0.25)
                ]
            , if model.tab == tab then
                Css.batch
                    [ Css.backgroundColor (hex light.purple)
                    , Css.color (hex "fff")
                    ]

              else
                Css.batch []
            ]
        ]
        [ E.input
            [ Html.Styled.Attributes.type_ "radio"
            , Html.Styled.Attributes.name "result"
            , Html.Styled.Attributes.id ("result-" ++ id)
            , Html.Styled.Events.on "change" (Json.Decode.succeed (TabChange tab))
            , Html.Styled.Attributes.checked (model.tab == tab)
            , css [ Css.display Css.none ]
            ]
            []
        , E.label
            [ Html.Styled.Attributes.for ("result-" ++ id)
            ]
            [ E.text label ]
        ]


viewTokens : Model -> Html Msg
viewTokens model =
    let
        tokensByLine = model.scanResult
            |> List.filter (\t -> t.tokenType /= WHITESPACE)
            |> groupBy .line
    in
    E.ol
        [ css
            [ Css.property "display" "grid"
            , themed [(Css.boxShadow4 Css.zero Css.zero (px 10), .shadow)]
            , Css.borderRadius (rem 0.5)
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
                        , Css.firstChild
                            [ Css.borderTopLeftRadius (rem 0.5)
                            , Css.borderTopRightRadius (rem 0.5)
                            ]
                        , Css.lastChild
                            [ Css.borderBottomLeftRadius (rem 0.5)
                            , Css.borderBottomRightRadius (rem 0.5)
                            ]
                        , themed [
                            if modBy 2 lineNumber == 0 then
                                (Css.backgroundColor, .contrastBackground)

                            else
                                (Css.backgroundColor, .background)
                            ]
                        ]
                    ]
                    [ E.span
                        [ css
                            [ Css.lineHeight (Css.num 1)
                            , Css.fontFamily Css.monospace
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
                    , Html.Styled.Events.onMouseOver (Hover (Just t))
                    , Html.Styled.Events.onMouseLeave (Hover Nothing)
                    ]
                    [ case t.tokenType of
                        IDENTIFIER -> viewTokenLiteral t
                        NUMBER -> viewTokenLiteral t
                        STRING -> viewTokenLiteral t
                        COMMENT -> viewTokenLiteral t
                        _ -> viewToken t
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
            , Css.color (hex "#333")
            , Css.borderRadius (rem 0.25)
            , Css.padding (rem 0.5)
            , Css.lineHeight (Css.num 1)
            , Css.margin Css.zero
            ]
        ]
        [ E.text token.tokenTypeString
        ]


viewTokenLiteral : Token -> Html Msg
viewTokenLiteral token =
    E.pre
        [ css
            [ Css.maxWidth Css.maxContent
            , Css.display Css.inlineFlex
            , themed
                [ (Css.backgroundColor, .softBackground)
                , (Css.boxShadow4 Css.zero Css.zero (px 10), .shadow)
                ]
            , Css.borderRadius (rem 0.25)
            , Css.padding (rem 0.5)
            , Css.lineHeight (Css.num 1)
            , Css.margin Css.zero
            ]
        ]
        [ E.text token.lexeme
        ]


viewParserResults : Model -> Html Msg
viewParserResults model =
    case model.parseResult of
        Just expr ->
            E.ol
                [ css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    , Css.textAlign Css.center
                    , Css.fontFamily Css.monospace
                    ]
                ]
                [ viewExpression expr
                ]
        Nothing -> E.text "No results."


viewExpression : Expr -> Html Msg
viewExpression expr =
    E.li
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , Css.margin (rem 0.75)
            ]
        ]
        ( case expr of
            Binary b ->
                [ viewExprChar b.operator.lexeme
                , E.ol
                    [ css
                        [ Css.displayFlex
                        ]
                    ]
                    [ viewExpression b.left
                    , viewExpression b.right
                    ]
                ]

            Unary u ->
                [ viewExprChar u.operator.lexeme
                , viewExpression u.right
                ]

            Grouping g ->
                [ viewExprChar "()"
                , viewExpression g
                ]

            Literal l ->
                [  viewExprChar
                    ( case l of
                        Str s -> s
                        Num n -> (String.fromFloat n)
                    )
                ]
        )


viewExprChar : String -> Html Msg
viewExprChar s =
    E.span
        [  css
            [ Css.border3 (px 1) Css.solid (hex "#fff")
            , Css.borderRadius (rem 0.5)
            , Css.maxWidth Css.maxContent
            , Css.lineHeight (Css.num 1)
            , Css.padding (rem 0.5)
            ]
        ]
        [ E.text s ]


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


type Expr
    = Binary BinaryExpr
    | Unary UnaryExpr
    | Grouping Expr
    | Literal TokenLiteral


type alias BinaryExpr =
  { left: Expr
  , right: Expr
  , operator: Token
  }


type alias UnaryExpr =
    { operator: Token
    , right: Expr
    }



type alias Token =
    { tokenType : TokenType
    , tokenTypeString : String
    , lexeme : String
    , line : Int
    , start : Int
    , literal : Maybe TokenLiteral
    }


type TokenType
    = LEFT_PAREN | RIGHT_PAREN
    | LEFT_BRACE | RIGHT_BRACE
    | COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR
    | BANG | BANG_EQUAL | EQUAL | EQUAL_EQUAL 
    | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL
    | IDENTIFIER | STRING | NUMBER
    | AND | CLASS | ELSE | FALSE | FUN | FOR
    | IF | NIL | OR | PRINT | RETURN | SUPER
    | THIS | TRUE | VAR | WHILE
    | COMMENT | WHITESPACE | EOF


type TokenLiteral
    = Str String
    | Num Float


decodeExpr : Decoder Expr
decodeExpr =
    field "type" Json.Decode.string
        |> Json.Decode.andThen decodeExprType


decodeExprType : String -> Decoder Expr
decodeExprType s =
    case s of
        "binary" ->
            Json.Decode.map Binary
                ( Json.Decode.map3 BinaryExpr
                    (field "left" decodeExpr)
                    (field "right" decodeExpr)
                    (field "operator" decodeToken)
                )

        "unary" ->
            Json.Decode.map Unary
                ( Json.Decode.map2 UnaryExpr
                    (field "operator" decodeToken)
                    (field "right" decodeExpr)
                )
    
        "grouping" ->
            Json.Decode.map Grouping (field "expression" decodeExpr)

        "literal" ->
            Json.Decode.map Literal (field "value" decodeTokenLiteral)

        _ ->
            Json.Decode.fail ("Unknown expr type: " ++ s)



decodeToken : Decoder Token
decodeToken =
    Json.Decode.map6 Token
        (field "type" decodeTokenType)
        (field "type" Json.Decode.string)
        (field "lexeme" Json.Decode.string)
        (field "line" Json.Decode.int)
        (field "start" Json.Decode.int)
        (Json.Decode.maybe (field "literal" decodeTokenLiteral))


decodeTokenType : Decoder TokenType
decodeTokenType =
    Json.Decode.string
        |> Json.Decode.andThen
            ( \s ->
                case s of
                    "LEFT_PAREN" -> Json.Decode.succeed LEFT_PAREN
                    "RIGHT_PAREN" -> Json.Decode.succeed RIGHT_PAREN
                    "LEFT_BRACE" -> Json.Decode.succeed LEFT_BRACE
                    "RIGHT_BRACE" -> Json.Decode.succeed RIGHT_BRACE
                    "COMMA" -> Json.Decode.succeed COMMA
                    "DOT" -> Json.Decode.succeed DOT
                    "MINUS" -> Json.Decode.succeed MINUS
                    "PLUS" -> Json.Decode.succeed PLUS
                    "SEMICOLON" -> Json.Decode.succeed SEMICOLON
                    "SLASH" -> Json.Decode.succeed SLASH
                    "STAR" -> Json.Decode.succeed STAR
                    "BANG" -> Json.Decode.succeed BANG
                    "BANG_EQUAL" -> Json.Decode.succeed BANG_EQUAL
                    "EQUAL" -> Json.Decode.succeed EQUAL
                    "EQUAL_EQUAL" -> Json.Decode.succeed EQUAL_EQUAL
                    "GREATER" -> Json.Decode.succeed GREATER
                    "GREATER_EQUAL" -> Json.Decode.succeed GREATER_EQUAL
                    "LESS" -> Json.Decode.succeed LESS
                    "LESS_EQUAL" -> Json.Decode.succeed LESS_EQUAL
                    "IDENTIFIER" -> Json.Decode.succeed IDENTIFIER
                    "STRING" -> Json.Decode.succeed STRING
                    "NUMBER" -> Json.Decode.succeed NUMBER
                    "AND" -> Json.Decode.succeed AND
                    "CLASS" -> Json.Decode.succeed CLASS
                    "ELSE" -> Json.Decode.succeed ELSE
                    "FALSE" -> Json.Decode.succeed FALSE
                    "FUN" -> Json.Decode.succeed FUN
                    "FOR" -> Json.Decode.succeed FOR
                    "IF" -> Json.Decode.succeed IF
                    "NIL" -> Json.Decode.succeed NIL
                    "OR" -> Json.Decode.succeed OR
                    "PRINT" -> Json.Decode.succeed PRINT
                    "RETURN" -> Json.Decode.succeed RETURN
                    "SUPER" -> Json.Decode.succeed SUPER
                    "THIS" -> Json.Decode.succeed THIS
                    "TRUE" -> Json.Decode.succeed TRUE
                    "VAR" -> Json.Decode.succeed VAR
                    "WHILE" -> Json.Decode.succeed WHILE
                    "COMMENT" -> Json.Decode.succeed COMMENT
                    "WHITESPACE" -> Json.Decode.succeed WHITESPACE
                    "EOF" -> Json.Decode.succeed EOF
                    _ -> Json.Decode.fail ("Unexpected token " ++ s)
            )


decodeTokenLiteral : Decoder TokenLiteral
decodeTokenLiteral =
    Json.Decode.oneOf
        [ Json.Decode.map Str Json.Decode.string
        , Json.Decode.map Num Json.Decode.float
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



themed : List (Css.Color -> Style, Theme -> String) -> Style
themed styles =
    Css.batch
        [ Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
            ( List.map
                ( \(s, t) ->
                    s (Css.hex (t dark))
                )
                styles
            )
        , Css.Media.withMediaQuery [ "(prefers-color-scheme: light)" ]
            ( List.map
                ( \(s, t) ->
                    s (Css.hex (t light))
                )
                styles
            )
        ]

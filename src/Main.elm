port module Main exposing (main)

import Browser
import Css exposing (hex, pct, px, rem)
import Css.Global
import Html.Styled as H exposing (Html, toUnstyled)
import Html.Styled.Attributes as A exposing (css)
import Html.Styled.Events as E
import Interpreter as I exposing (ParseError, RunExpr, ScanError, Stmt, Token, TokenLiteral(..))
import Json.Decode as Decode
import List
import String
import Theme exposing (light, themed, themedProperty)



---- PORTS ----


port scan : String -> Cmd msg


port scanResult : (Decode.Value -> msg) -> Sub msg


port scanError : (Maybe ScanError -> msg) -> Sub msg


port parse : Decode.Value -> Cmd msg


port parseResult : (Decode.Value -> msg) -> Sub msg


port parseError : (Maybe Decode.Value -> msg) -> Sub msg


port run : Decode.Value -> Cmd msg


port runResult : (Decode.Value -> msg) -> Sub msg


port runError : (Maybe Decode.Value -> msg) -> Sub msg


port log : (List String -> msg) -> Sub msg



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view =
            \model ->
                { title = "Lox"
                , body = [ model |> view |> toUnstyled ]
                }
        , init = \_ -> init
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ scanResult ScanResult
                    , parseResult ParseResult
                    , runResult RunResult
                    , scanError ReportScanError
                    , parseError ReportParseError
                    , runError ReportRunError
                    , log Log
                    ]
        }



---- MODEL ----


type Tab
    = Scanner
    | Parser
    | Console


type Selected
    = Expression RunExpr
    | Statement Stmt


isSelected : Selected -> Model -> Bool
isSelected select model =
    case model.selected of
        Nothing ->
            False

        Just modelSelected ->
            case select of
                Expression e ->
                    case modelSelected of
                        Expression expr ->
                            e == expr

                        _ ->
                            False

                Statement s ->
                    case modelSelected of
                        Statement stmt ->
                            s == stmt

                        _ ->
                            False


type alias Model =
    { input : String
    , scanResult : List Token
    , scanErrors : List ScanError
    , parseResult : List (Maybe Stmt)
    , parseErrors : List ParseError
    , runResult : Maybe TokenLiteral
    , runError : Maybe ParseError
    , console : List String
    , hover : Maybe Token
    , selected : Maybe Selected
    , expanded : List Stmt
    , tab : Tab
    }


defaultModel : Model
defaultModel =
    { input = ""
    , scanResult = []
    , scanErrors = []
    , parseResult = []
    , parseErrors = []
    , runResult = Nothing
    , runError = Nothing
    , console = []
    , hover = Nothing
    , selected = Nothing
    , expanded = []
    , tab = Console
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )



---- UPDATE ----


type Msg
    = Input String
    | ScanResult Decode.Value
    | ParseResult Decode.Value
    | RunResult Decode.Value
    | ReportScanError (Maybe ScanError)
    | ReportParseError (Maybe Decode.Value)
    | ReportRunError (Maybe Decode.Value)
    | Hover (Maybe Token)
    | SelectExpr RunExpr
    | SelectStmt Stmt
    | ToggleExpand Stmt
    | TabChange Tab
    | Log (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input s ->
            ( { model | input = s }
            , scan s
            )

        ScanResult v ->
            case Decode.decodeValue (Decode.list I.decodeToken) v of
                Ok tokens ->
                    ( { model | scanResult = tokens, selected = Nothing }
                    , parse v
                    )

                Err _ ->
                    ( { model | scanResult = [] }
                    , Cmd.none
                    )

        ParseResult p ->
            case
                Decode.decodeValue
                    (Decode.list (Decode.maybe I.decodeStmt))
                    p
            of
                Ok stmts ->
                    ( { model | parseResult = stmts, console = [] }
                    , run p
                    )

                Err _ ->
                    ( { model | parseResult = [] }
                    , Cmd.none
                    )

        RunResult p ->
            case
                Decode.decodeValue
                    (Decode.list (Decode.maybe I.decodeStmt))
                    p
            of
                Ok l ->
                    ( { model
                        | parseResult = l
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | runResult = Nothing }
                    , Cmd.none
                    )

        ReportScanError reportedError ->
            case reportedError of
                Nothing ->
                    ( { model | scanErrors = [] }
                    , Cmd.none
                    )

                Just e ->
                    ( { model | scanErrors = model.scanErrors ++ [ e ] }
                    , Cmd.none
                    )

        ReportParseError reportedError ->
            case reportedError of
                Nothing ->
                    ( { model | parseErrors = [] }
                    , Cmd.none
                    )

                Just e ->
                    case Decode.decodeValue I.decodeParseError e of
                        Ok err ->
                            ( { model
                                | parseErrors = model.parseErrors ++ [ err ]
                                , runError = Nothing
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model | scanErrors = [] }
                            , Cmd.none
                            )

        ReportRunError reportedError ->
            case reportedError of
                Nothing ->
                    ( { model | runError = Nothing }
                    , Cmd.none
                    )

                Just e ->
                    case Decode.decodeValue I.decodeParseError e of
                        Ok err ->
                            ( { model | runError = Just err }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model | runError = Nothing }
                            , Cmd.none
                            )

        Hover t ->
            ( { model | hover = t }
            , Cmd.none
            )

        SelectExpr e ->
            ( { model
                | selected =
                    if isSelected (Expression e) model then
                        Nothing

                    else
                        Just (Expression e)
              }
            , Cmd.none
            )

        SelectStmt stmt ->
            ( { model
                | selected =
                    if isSelected (Statement stmt) model then
                        Nothing

                    else
                        Just (Statement stmt)
              }
            , Cmd.none
            )

        ToggleExpand stmt ->
            ( { model
                | expanded =
                    if List.member stmt model.expanded then
                        List.filter (\s -> s /= stmt) model.expanded

                    else
                        stmt :: model.expanded
              }
            , Cmd.none
            )

        TabChange t ->
            ( { model | tab = t }
            , Cmd.none
            )

        Log s ->
            ( { model | console = model.console ++ s }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    H.div
        [ css
            [ themed
                [ ( Css.backgroundColor, .background )
                , ( Css.color, .text )
                ]
            , Css.property "display" "grid"
            , Css.property "grid-template-rows" "auto 1fr"
            ]
        , A.id "app"
        ]
        [ viewHeader
        , viewBody model
        ]


viewBody : Model -> Html Msg
viewBody model =
    H.main_
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-gap" "2rem"
            , Css.property "grid-template-columns" "1fr 1fr"
            , Css.minHeight (Css.pct 100)
            , Css.width (Css.pct 100)
            , Css.maxWidth (px 1200)
            , Css.margin Css.auto
            , Css.padding (rem 0.75)
            ]
        ]
        [ viewCode model
        , viewResults model
        ]


viewHeader : Html Msg
viewHeader =
    H.header
        [ css
            [ Css.displayFlex
            , Css.backgroundColor (hex "#402945")
            , Css.color (hex "#fff")
            , Css.padding (rem 1)
            ]
        ]
        [ H.h1
            [ css
                [ Css.margin Css.zero
                ]
            ]
            [ H.text "Lox Interpreter" ]
        ]


viewCode : Model -> Html Msg
viewCode model =
    H.section
        [ css
            [ Css.property "display" "grid"
            , Css.property "row-gap" "0.25rem"
            , Css.property "grid-auto-rows" "max-content"
            ]
        ]
        [ viewEditor model
        , viewError model
        ]


viewEditor : Model -> Html Msg
viewEditor model =
    H.section
        [ css
            [ Css.borderRadius (rem 0.25)
            , Css.position Css.relative
            , Css.lineHeight (Css.num 1.5)
            , Css.property "display" "grid"
            , Css.property "grid-template-columns" "minmax(2rem, auto) 1fr"
            , Css.property "column-gap" "0.25em"
            , themed
                [ ( Css.border3 (px 2) Css.solid, .softBackground ) ]
            ]
        ]
        [ viewLines model
        , viewSource model
        , viewInput
        ]


viewLines : Model -> Html Msg
viewLines model =
    let
        lines =
            groupBy .line model.scanResult
    in
    H.ol
        [ css
            [ Css.property "grid-row" "1"
            , Css.property "grid-column" "1"
            , Css.padding (rem 0.5)
            , themed
                [ ( Css.backgroundColor, .softBackground )
                , ( Css.color, .softText )
                , ( Css.boxShadow6 Css.inset (px -7) Css.zero (px 9) (px -7)
                  , .shadow
                  )
                ]
            ]
        ]
        (List.map
            (\n ->
                H.li
                    [ css
                        [ Css.fontSize (rem 0.8)
                        , Css.height (rem 1.5)
                        , Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.justifyContent Css.flexEnd
                        ]
                    ]
                    [ H.pre [] [ H.text (String.fromInt n) ] ]
            )
            (List.range 1 (max (List.length lines) 15))
        )


viewInput : Html Msg
viewInput =
    H.textarea
        [ E.onInput Input
        , css
            [ Css.border Css.zero
            , Css.padding2 (rem 0.5) Css.zero
            , Css.color Css.transparent
            , Css.fontSize Css.inherit
            , Css.width (pct 100)
            , Css.boxSizing Css.borderBox
            , Css.resize Css.none
            , Css.overflow Css.auto
            , Css.fontFamily Css.monospace
            , Css.margin Css.zero
            , Css.backgroundColor Css.transparent
            , themedProperty "caret-color" .text
            , Css.property "grid-column" "2"
            , Css.property "grid-row" "1"
            , Css.focus [ Css.outline Css.none ]
            , Css.position Css.relative
            ]
        , A.autocomplete False
        , A.attribute "autocapitalize" "none"
        , A.spellcheck False
        , A.autofocus True
        , A.id "code-input"
        ]
        []


viewSource : Model -> Html Msg
viewSource model =
    let
        lines =
            groupBy .line model.scanResult

        selectedTokens =
            case model.selected of
                Just s ->
                    case s of
                        Expression e ->
                            I.getExprTokenRange model.scanResult e

                        Statement stmt ->
                            I.getStmtTokenRange model.scanResult stmt

                Nothing ->
                    []
    in
    H.code
        [ css
            [ Css.margin2 (rem 0.5) Css.zero
            , Css.property "grid-column" "2"
            , Css.property "grid-row" "1"
            , Css.pointerEvents Css.none
            ]
        ]
        (List.map
            (\t -> viewSourceLine model selectedTokens t)
            lines
        )


viewSourceLine : Model -> List Token -> List Token -> Html Msg
viewSourceLine model selected tokens =
    H.pre
        []
        (List.map
            (\t -> viewTokenSource model selected t)
            tokens
        )


viewTokenSource : Model -> List Token -> Token -> Html Msg
viewTokenSource model selected token =
    let
        error =
            case List.filter (\e -> e.token == token) model.parseErrors of
                [] ->
                    Nothing

                e :: _ ->
                    Just e
    in
    H.span
        [ A.class token.tokenTypeString
        , A.class "token"
        , A.class
            (case model.hover of
                Just t ->
                    if t == token then
                        "hover"

                    else
                        ""

                _ ->
                    ""
            )
        , A.class
            (if List.member token selected then
                "selected"

             else
                ""
            )
        , css
            [ Css.display Css.inlineBlock
            , Css.position Css.relative
            ]
        ]
        [ H.text
            (if token.tokenType == I.EOF then
                " "

             else
                token.lexeme
            )
        , case error of
            Nothing ->
                H.text ""

            Just e ->
                viewTokenError e
        ]


viewTokenError : ParseError -> Html Msg
viewTokenError error =
    let
        revealError =
            [ Css.Global.children
                [ Css.Global.div
                    [ Css.display Css.inlineBlock
                    ]
                ]
            ]
    in
    H.span
        [ css
            [ Css.position Css.absolute
            , Css.pointerEventsAll
            , Css.hover revealError
            , Css.focus revealError
            , Css.width (pct 100)
            , Css.height (rem 0.5)
            , Css.left Css.zero
            , Css.bottom Css.zero
            , Css.borderBottom3 (px 2) Css.dotted (hex "FF0000")
            ]
        , A.tabindex 0
        ]
        [ H.div
            [ css
                [ Css.display Css.none
                , Css.paddingTop (rem 1)
                ]
            ]
            [ H.article
                [ css
                    [ Css.borderRadius (rem 0.5)
                    , Css.border3 (px 2) Css.solid (hex "FF0000")
                    ]
                ]
                [ H.header
                    [ css
                        [ Css.backgroundColor (hex "FF0000")
                        , Css.color (hex "FFF")
                        , Css.padding (rem 0.5)
                        ]
                    ]
                    [ H.h1
                        []
                        [ H.text error.message
                        ]
                    ]
                ]
            ]
        ]


viewResults : Model -> Html Msg
viewResults model =
    H.section
        [ css
            [ Css.property "display" "grid"
            , Css.property "row-gap" "0.5rem"
            , Css.property "grid-auto-rows" "max-content"
            ]
        ]
        [ H.div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.center
                ]
            ]
            [ viewTabRadio model "scanner" "Scanner" Scanner
            , viewTabRadio model "parser" "Parser" Parser
            , viewTabRadio model "runner" "Console" Console
            ]
        , case model.tab of
            Scanner ->
                viewTokens model

            Parser ->
                viewParserResults model

            Console ->
                viewRunResults model
        ]


viewTabRadio : Model -> String -> String -> Tab -> Html Msg
viewTabRadio model id label tab =
    H.label
        [ css
            [ Css.lineHeight (Css.num 1)
            , Css.padding (rem 0.5)
            , Css.border3 (px 1) Css.solid (hex light.purple)
            , Css.borderRight Css.zero
            , Css.display Css.block
            , Css.firstChild
                [ Css.borderTopLeftRadius (rem 0.25)
                , Css.borderBottomLeftRadius (rem 0.25)
                ]
            , Css.lastChild
                [ Css.borderTopRightRadius (rem 0.25)
                , Css.borderBottomRightRadius (rem 0.25)
                , Css.borderRight3 (px 1) Css.solid (hex light.purple)
                ]
            , if model.tab == tab then
                Css.batch
                    [ Css.backgroundColor (hex light.purple)
                    , Css.color (hex "fff")
                    ]

              else
                Css.batch []
            ]
        , A.for ("result-" ++ id)
        ]
        [ H.input
            [ A.type_ "radio"
            , A.name "result"
            , A.id ("result-" ++ id)
            , E.on
                "change"
                (Decode.succeed (TabChange tab))
            , A.checked (model.tab == tab)
            , css [ Css.display Css.none ]
            ]
            []
        , H.text label
        ]


viewTokens : Model -> Html Msg
viewTokens model =
    let
        tokensByLine =
            model.scanResult
                |> List.filter
                    (\t ->
                        t.tokenType
                            /= I.WHITESPACE
                            && t.tokenType
                            /= I.EOF
                            && t.tokenType
                            /= I.UNEXPECTED
                    )
                |> groupBy .line
    in
    H.ol
        [ css
            [ Css.property "display" "grid"
            , themed [ ( Css.boxShadow4 Css.zero Css.zero (px 10), .shadow ) ]
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
                H.li
                    [ css
                        [ Css.property "display" "grid"
                        , Css.property "row-gap" "0.5rem"
                        , Css.property "column-gap" "0.75rem"
                        , Css.property "grid-template-columns" "auto 1fr"
                        , Css.property "height" "fit-content"
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
                        , themed
                            [ ( Css.backgroundColor, .softBackground ) ]
                        , Css.nthChild "2n"
                            [ themed
                                [ ( Css.backgroundColor, .contrastBackground )
                                ]
                            ]
                        ]
                    ]
                    [ H.span
                        [ css
                            [ Css.lineHeight (Css.num 1)
                            , Css.fontFamily Css.monospace
                            ]
                        ]
                        [ H.text (String.fromInt lineNumber) ]
                    , viewLine line
                    ]
            )
            tokensByLine
        )


viewLine : List Token -> Html Msg
viewLine tokens =
    H.ol
        [ css
            [ Css.displayFlex
            , Css.flexWrap Css.wrap
            ]
        ]
        (List.map
            (\t ->
                H.li
                    [ css
                        [ Css.marginRight (rem 0.5)
                        , Css.lastChild [ Css.marginRight Css.zero ]
                        , Css.marginBottom (rem 0.5)
                        ]
                    , E.onMouseOver (Hover (Just t))
                    , E.onMouseLeave (Hover Nothing)
                    ]
                    [ case t.tokenType of
                        I.IDENTIFIER ->
                            viewTokenLiteral t

                        I.NUMBER ->
                            viewTokenLiteral t

                        I.STRING ->
                            viewTokenLiteral t

                        I.COMMENT ->
                            viewTokenLiteral t

                        _ ->
                            viewToken t
                    ]
            )
            tokens
        )


viewToken : Token -> Html Msg
viewToken token =
    H.pre
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
        [ H.text token.tokenTypeString
        ]


viewTokenLiteral : Token -> Html Msg
viewTokenLiteral token =
    H.pre
        [ css
            [ Css.maxWidth Css.maxContent
            , Css.display Css.inlineFlex
            , themed
                [ ( Css.backgroundColor, .background )
                , ( Css.boxShadow4 Css.zero Css.zero (px 10), .shadow )
                ]
            , Css.borderRadius (rem 0.25)
            , Css.padding (rem 0.5)
            , Css.lineHeight (Css.num 1)
            , Css.margin Css.zero
            ]
        ]
        [ H.text token.lexeme
        ]



{-
   var a = 12;
   var b = 3 * a;
   a = 6 * a;
   var c;
   c = 12 * 3 == a - (52 / (2 - b));
   print a / b;
   if (b < a) {
     print "b is bigger";
   } else {
     print "a is bigger";
   }
-}


viewParserResults : Model -> Html Msg
viewParserResults model =
    case model.parseResult of
        [] ->
            H.text "No results."

        _ ->
            viewStmtList model model.parseResult


viewStmtList : Model -> List (Maybe Stmt) -> Html Msg
viewStmtList model stmts =
    H.ol
        [ css
            [ Css.fontFamily Css.monospace
            , Css.Global.children
                [ Css.Global.li
                    [ Css.Global.adjacentSiblings
                        [ Css.Global.li
                            [ Css.marginTop (rem 1.5)
                            ]
                        ]
                    ]
                ]
            ]
        ]
        (List.map
            (viewPotentialStmt model)
            stmts
        )


viewPotentialStmt : Model -> Maybe Stmt -> Html Msg
viewPotentialStmt model stmt =
    case stmt of
        Nothing ->
            H.text "Invalid statement."

        Just s ->
            viewStmt model s


viewStmt : Model -> Stmt -> Html Msg
viewStmt model stmt =
    let
        stmtView =
            case stmt of
                I.Var v ->
                    viewVariableStmt model v

                I.Expression e ->
                    viewExpressionStmt model e

                I.Print p ->
                    viewPrintStmt model p

                I.Block b ->
                    viewBlockStmt model b

                I.If i ->
                    viewIfStmt model i

                I.While w ->
                    viewWhileStmt model w

        expanded =
            List.member stmt model.expanded
    in
    H.li
        []
        [ H.article
            []
            [ H.header
                [ css
                    [ Css.lineHeight (Css.num 1)
                    , Css.padding2 (rem 1) (rem 0.5)
                    , Css.borderRadius (rem 1)
                    , Css.displayFlex
                    , Css.alignItems Css.baseline
                    , themed
                        [ ( Css.backgroundColor
                          , if isSelected (Statement stmt) model then
                                .highlight

                            else
                                .softBackground
                          )
                        ]
                    ]
                ]
                (List.concat
                    [ [ case stmtView.body of
                            Nothing ->
                                H.text ""

                            _ ->
                                H.label
                                    [ css
                                        [ Css.marginRight (rem 0.5)
                                        , Css.fontSize Css.larger
                                        , Css.fontWeight Css.bold
                                        , Css.cursor Css.pointer
                                        ]
                                    ]
                                    [ H.input
                                        [ A.type_ "checkbox"
                                        , A.checked expanded
                                        , E.onClick (ToggleExpand stmt)
                                        , css
                                            [ Css.display Css.none
                                            ]
                                        ]
                                        []
                                    , H.text
                                        (if expanded then
                                            "-"

                                         else
                                            "+"
                                        )
                                    ]
                      , H.div
                            [ css
                                [ Css.marginRight (rem 0.5)
                                ]
                            ]
                            [ H.input
                                [ A.type_ "radio"
                                , A.name "statements"
                                , A.checked (isSelected (Statement stmt) model)
                                , E.onClick (SelectStmt stmt)
                                ]
                                []
                            ]
                      ]
                    , stmtView.header
                    ]
                )
            , case stmtView.body of
                Nothing ->
                    H.text ""

                Just b ->
                    H.section
                        [ css
                            [ themed
                                [ ( Css.border3 (px 2) Css.solid, .softBackground )
                                ]
                            , Css.borderRadius (rem 1)
                            , Css.overflow Css.hidden
                            , Css.padding (rem 0.5)
                            , Css.marginTop (rem 0.5)
                            , if expanded then
                                Css.display Css.block

                              else
                                Css.display Css.none
                            ]
                        ]
                        [ b
                        ]
            ]
        ]


type alias StmtView =
    { header : List (Html Msg)
    , body : Maybe (Html Msg)
    }


viewVariableStmt : Model -> I.VarStmt -> StmtView
viewVariableStmt model stmt =
    { header =
        [ H.h1
            [ css
                [ Css.display Css.inlineBlock
                ]
            , A.class "token"
            , A.class stmt.name.tokenTypeString
            ]
            [ H.text stmt.name.lexeme
            ]
        , H.p
            []
            [ H.text
                (case stmt.initializer of
                    Nothing ->
                        ": nil"

                    Just expr ->
                        case expr.result of
                            Nothing ->
                                ""

                            Just l ->
                                ": " ++ I.tokenLiteralString l
                )
            ]
        , H.p
            [ css
                [ Css.fontStyle Css.italic
                , Css.display Css.inlineBlock
                , Css.fontSize Css.smaller
                , Css.marginLeft Css.auto
                , themed [ ( Css.color, .softText ) ]
                ]
            ]
            [ H.text "declaration"
            ]
        ]
    , body = Maybe.map (viewExpressionTree model) stmt.initializer
    }


viewExpressionStmt : Model -> I.ExpressionStmt -> StmtView
viewExpressionStmt model stmt =
    { header =
        [ H.h1
            [ css
                [ Css.fontSize (rem 1)
                ]
            ]
            [ H.text "expression"
            ]
        , H.p
            []
            [ case stmt.expression.result of
                Nothing ->
                    H.text ": ?"

                Just l ->
                    H.text (": " ++ I.tokenLiteralString l)
            ]
        ]
    , body = Just (viewExpressionTree model stmt.expression)
    }


viewBlockStmt : Model -> I.BlockStmt -> StmtView
viewBlockStmt model block =
    { header =
        [ H.h1
            [ css
                [ Css.fontSize (rem 1)
                ]
            ]
            [ H.text "block"
            ]
        ]
    , body = Just (viewStmtList model block.statements)
    }


viewIfStmt : Model -> I.IfStmt -> StmtView
viewIfStmt model stmt =
    { header =
        [ H.h1
            [ css
                [ Css.fontSize (rem 1)
                ]
            , A.class "token"
            , A.class "IF"
            ]
            [ H.text "if"
            ]
        , H.p
            []
            [ case stmt.condition.result of
                Nothing ->
                    H.text ": ?"

                Just l ->
                    H.text (": " ++ I.tokenLiteralString l)
            ]
        ]
    , body =
        Just
            (H.div
                []
                [ H.div
                    []
                    [ H.text "condition"
                    , viewExpressionTree model stmt.condition
                    ]
                , H.div
                    []
                    [ H.text "then"
                    , viewStmt model stmt.thenBranch
                    ]
                , case stmt.elseBranch of
                    Nothing ->
                        H.text ""

                    Just e ->
                        H.div
                            []
                            [ H.text "else"
                            , viewStmt model e
                            ]
                ]
            )
    }


viewWhileStmt : Model -> I.WhileStmt -> StmtView
viewWhileStmt model stmt =
    { header =
        [ H.h1
            [ A.class "token"
            , A.class "PRINT"
            , css
                [ Css.fontSize (rem 1)
                ]
            ]
            [ H.text "while"
            ]
        , H.p
            []
            [ case stmt.condition.result of
                Nothing ->
                    H.text ": ?"

                Just l ->
                    H.text (": " ++ I.tokenLiteralString l)
            ]
        ]
    , body =
        Just
            (H.div
                []
                [ H.div
                    []
                    [ H.text "condition"
                    , viewExpressionTree model stmt.condition
                    ]
                , H.div
                    []
                    [ H.text "then"
                    , viewStmt model stmt.body
                    ]
                ]
            )
    }


viewPrintStmt : Model -> I.PrintStmt -> StmtView
viewPrintStmt model stmt =
    { header =
        [ H.h1
            [ A.class "token"
            , A.class "PRINT"
            , css
                [ Css.fontSize (rem 1)
                ]
            ]
            [ H.text "print"
            ]
        , H.p
            []
            [ case stmt.expression.result of
                Nothing ->
                    H.text ": ?"

                Just l ->
                    H.text (": " ++ I.tokenLiteralString l)
            ]
        ]
    , body = Just (viewExpressionTree model stmt.expression)
    }


viewExpressionTree : Model -> RunExpr -> Html Msg
viewExpressionTree model expr =
    H.ol
        [ css
            [ Css.borderRadius (rem 0.5)
            , Css.padding (rem 0.5)
            , Css.overflowX Css.auto
            , themed [ ( Css.backgroundColor, .background ) ]
            ]
        , A.class "tree"
        ]
        [ viewExpression model expr
        ]



-- Tree styling adapted from https://codepen.io/Avaneesh/pen/QWwNrBX


viewExpression : Model -> RunExpr -> Html Msg
viewExpression model expr =
    let
        tokens =
            I.exprToken expr.expression
    in
    H.li
        [ css
            (case model.selected of
                Just s ->
                    case s of
                        Expression e ->
                            if expr == e then
                                [ Css.Global.descendants
                                    [ Css.Global.button
                                        [ Css.Global.children
                                            [ Css.Global.span
                                                [ themed
                                                    [ ( Css.backgroundColor, .highlight ) ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]

                            else
                                []

                        _ ->
                            []

                Nothing ->
                    []
            )
        , A.class "leaf"
        ]
        [ viewExprChar expr tokens
        , case expr.expression of
            I.Binary b ->
                H.ol
                    [ A.class "tree"
                    ]
                    [ viewExpression model b.left
                    , viewExpression model b.right
                    ]

            I.Unary u ->
                H.ol
                    [ A.class "tree"
                    ]
                    [ viewExpression model u.right
                    ]

            I.Grouping g ->
                H.ol
                    [ A.class "tree"
                    ]
                    [ viewExpression model g.expression
                    ]

            I.Literal _ ->
                H.text ""

            I.Variable _ ->
                H.text ""

            I.Assignment a ->
                H.ol
                    [ A.class "tree"
                    ]
                    [ viewExpression model a.value
                    ]

            I.Logical l ->
                H.ol
                    [ A.class "tree"
                    ]
                    [ viewExpression model l.left
                    , viewExpression model l.right
                    ]
        ]


viewExprChar : RunExpr -> List Token -> Html Msg
viewExprChar e tokens =
    H.button
        [ css
            [ Css.lineHeight (Css.num 1)
            , Css.position Css.relative
            , Css.borderRadius (rem 2)
            ]
        , E.onClick (SelectExpr e)
        ]
        [ H.span
            [ css
                [ Css.display Css.inlineBlock
                , Css.fontWeight Css.bold
                , Css.padding (rem 0.5)
                , Css.borderRadius (rem 2)
                , Css.position Css.relative
                , Css.zIndex (Css.int 1)
                , themed
                    [ ( Css.border3 (px 2) Css.solid, .text )
                    , ( Css.backgroundColor, .background )
                    ]
                ]
            ]
            (List.map
                (\t ->
                    H.span
                        [ css
                            [ Css.display Css.inlineBlock
                            , Css.minWidth (rem 1)
                            ]
                        , A.class "token"
                        , A.class t.tokenTypeString
                        , E.onMouseOver (Hover (Just t))
                        , E.onMouseLeave (Hover Nothing)
                        ]
                        [ H.text t.lexeme
                        ]
                )
                tokens
            )
        , viewExprResult e
        ]


viewExprResult : RunExpr -> Html Msg
viewExprResult expr =
    let
        tokenResult =
            case expr.result of
                Nothing ->
                    "?"

                Just l ->
                    I.tokenLiteralString l
    in
    case expr.expression of
        I.Literal _ ->
            H.text ""

        _ ->
            H.span
                [ css
                    [ themed
                        [ ( Css.backgroundColor, .contrastBackground )
                        ]
                    , Css.padding (rem 0.5)
                    , Css.paddingLeft (rem 1.25)
                    , Css.left (Css.calc (pct 100) Css.minus (rem 1))
                    , Css.position Css.absolute
                    , Css.display Css.inlineBlock
                    , Css.borderTopRightRadius (rem 2)
                    , Css.borderBottomRightRadius (rem 2)
                    , Css.margin2 (px 2) Css.zero
                    , Css.whiteSpace Css.noWrap
                    ]
                ]
                [ H.text tokenResult
                ]


viewRunResults : Model -> Html Msg
viewRunResults model =
    H.code
        [ css
            [ Css.height (px 300)
            , Css.overflowY Css.auto
            , Css.border3 (px 1) Css.solid (hex "333")
            ]
        ]
        (List.map
            (\m ->
                H.pre
                    []
                    [ H.text m
                    ]
            )
            model.console
        )


viewError : Model -> Html Msg
viewError model =
    case model.scanErrors of
        [] ->
            H.text ""

        errors ->
            H.pre
                []
                [ H.ol
                    []
                    (List.map
                        (\e ->
                            H.li
                                []
                                [ H.text
                                    ("Error on line "
                                        ++ String.fromInt e.line
                                        ++ ":\n"
                                    )
                                , H.text e.message
                                ]
                        )
                        errors
                    )
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

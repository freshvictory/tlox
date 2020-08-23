module Interpreter exposing (..)

import Json.Decode exposing (Decoder, field)
import Json.Encode


type alias ScanError =
    { line : Int
    , message : String
    }


type alias ParseError =
    { token : Token
    , message : String
    }


type Expr
    = Binary BinaryExpr
    | Unary UnaryExpr
    | Grouping GroupingExpr
    | Literal LiteralExpr


type alias BinaryExpr =
    { left : Expr
    , right : Expr
    , operator : Token
    , result : Maybe TokenLiteral
    }


type alias UnaryExpr =
    { operator : Token
    , right : Expr
    , result : Maybe TokenLiteral
    }


type alias GroupingExpr =
    { expression : Expr
    , tokens : List Token
    , result : Maybe TokenLiteral
    }


type alias LiteralExpr =
    { value : TokenLiteral
    , token : Token
    , result : Maybe TokenLiteral
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
    = LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    | IDENTIFIER
    | STRING
    | NUMBER
    | AND
    | CLASS
    | ELSE
    | FALSE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | TRUE
    | VAR
    | WHILE
    | COMMENT
    | WHITESPACE
    | UNEXPECTED
    | EOF


type TokenLiteral
    = Str String
    | Num Float
    | Boolean Bool
    | Nil


decodeExpr : Decoder Expr
decodeExpr =
    field "type" Json.Decode.string
        |> Json.Decode.andThen decodeExprType


decodeExprType : String -> Decoder Expr
decodeExprType s =
    case s of
        "binary" ->
            Json.Decode.map Binary
                (Json.Decode.map4 BinaryExpr
                    (field "left" decodeExpr)
                    (field "right" decodeExpr)
                    (field "operator" decodeToken)
                    (Json.Decode.maybe (field "result" decodeTokenLiteral))
                )

        "unary" ->
            Json.Decode.map Unary
                (Json.Decode.map3 UnaryExpr
                    (field "operator" decodeToken)
                    (field "right" decodeExpr)
                    (Json.Decode.maybe (field "result" decodeTokenLiteral))
                )

        "grouping" ->
            Json.Decode.map Grouping
                (Json.Decode.map3 GroupingExpr
                    (field "expression" decodeExpr)
                    (field "tokens" (Json.Decode.list decodeToken))
                    (Json.Decode.maybe (field "result" decodeTokenLiteral))
                )

        "literal" ->
            Json.Decode.map Literal
                (Json.Decode.map3 LiteralExpr
                    (field "value" decodeTokenLiteral)
                    (field "token" decodeToken)
                    (Json.Decode.maybe (field "result" decodeTokenLiteral))
                )

        _ ->
            Json.Decode.fail ("Unknown expr type: " ++ s)


encodeExpr : Expr -> Json.Decode.Value
encodeExpr e =
    case e of
        Binary b ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "binary" )
                , ( "left", encodeExpr b.left )
                , ( "right", encodeExpr b.right )
                , ( "operator", encodeToken b.operator )
                ]

        Unary u ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "unary" )
                , ( "right", encodeExpr u.right )
                , ( "operator", encodeToken u.operator )
                ]

        Grouping g ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "grouping" )
                , ( "expression", encodeExpr g.expression )
                , ( "tokens", Json.Encode.list encodeToken g.tokens )
                ]

        Literal l ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "literal" )
                , ( "value"
                  , encodeTokenLiteral l.value
                  )
                , ( "token", encodeToken l.token )
                ]


decodeParseError : Decoder ParseError
decodeParseError =
    Json.Decode.map2 ParseError
        (field "token" decodeToken)
        (field "message" Json.Decode.string)


encodeToken : Token -> Json.Decode.Value
encodeToken token =
    Json.Encode.object
        ([ ( "type", Json.Encode.string token.tokenTypeString )
         , ( "lexeme", Json.Encode.string token.lexeme )
         , ( "line", Json.Encode.int token.line )
         , ( "start", Json.Encode.int token.start )
         ]
            ++ (case token.literal of
                    Nothing ->
                        []

                    Just v ->
                        [ ( "literal", encodeTokenLiteral v ) ]
               )
        )


encodeTokenLiteral : TokenLiteral -> Json.Decode.Value
encodeTokenLiteral l =
    case l of
        Str s ->
            Json.Encode.string s

        Num f ->
            Json.Encode.float f

        Boolean b ->
            Json.Encode.bool b

        Nil ->
            Json.Encode.null


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
            (\s ->
                case s of
                    "LEFT_PAREN" ->
                        Json.Decode.succeed LEFT_PAREN

                    "RIGHT_PAREN" ->
                        Json.Decode.succeed RIGHT_PAREN

                    "LEFT_BRACE" ->
                        Json.Decode.succeed LEFT_BRACE

                    "RIGHT_BRACE" ->
                        Json.Decode.succeed RIGHT_BRACE

                    "COMMA" ->
                        Json.Decode.succeed COMMA

                    "DOT" ->
                        Json.Decode.succeed DOT

                    "MINUS" ->
                        Json.Decode.succeed MINUS

                    "PLUS" ->
                        Json.Decode.succeed PLUS

                    "SEMICOLON" ->
                        Json.Decode.succeed SEMICOLON

                    "SLASH" ->
                        Json.Decode.succeed SLASH

                    "STAR" ->
                        Json.Decode.succeed STAR

                    "BANG" ->
                        Json.Decode.succeed BANG

                    "BANG_EQUAL" ->
                        Json.Decode.succeed BANG_EQUAL

                    "EQUAL" ->
                        Json.Decode.succeed EQUAL

                    "EQUAL_EQUAL" ->
                        Json.Decode.succeed EQUAL_EQUAL

                    "GREATER" ->
                        Json.Decode.succeed GREATER

                    "GREATER_EQUAL" ->
                        Json.Decode.succeed GREATER_EQUAL

                    "LESS" ->
                        Json.Decode.succeed LESS

                    "LESS_EQUAL" ->
                        Json.Decode.succeed LESS_EQUAL

                    "IDENTIFIER" ->
                        Json.Decode.succeed IDENTIFIER

                    "STRING" ->
                        Json.Decode.succeed STRING

                    "NUMBER" ->
                        Json.Decode.succeed NUMBER

                    "AND" ->
                        Json.Decode.succeed AND

                    "CLASS" ->
                        Json.Decode.succeed CLASS

                    "ELSE" ->
                        Json.Decode.succeed ELSE

                    "FALSE" ->
                        Json.Decode.succeed FALSE

                    "FUN" ->
                        Json.Decode.succeed FUN

                    "FOR" ->
                        Json.Decode.succeed FOR

                    "IF" ->
                        Json.Decode.succeed IF

                    "NIL" ->
                        Json.Decode.succeed NIL

                    "OR" ->
                        Json.Decode.succeed OR

                    "PRINT" ->
                        Json.Decode.succeed PRINT

                    "RETURN" ->
                        Json.Decode.succeed RETURN

                    "SUPER" ->
                        Json.Decode.succeed SUPER

                    "THIS" ->
                        Json.Decode.succeed THIS

                    "TRUE" ->
                        Json.Decode.succeed TRUE

                    "VAR" ->
                        Json.Decode.succeed VAR

                    "WHILE" ->
                        Json.Decode.succeed WHILE

                    "COMMENT" ->
                        Json.Decode.succeed COMMENT

                    "WHITESPACE" ->
                        Json.Decode.succeed WHITESPACE

                    "UNEXPECTED" ->
                        Json.Decode.succeed UNEXPECTED

                    "EOF" ->
                        Json.Decode.succeed EOF

                    _ ->
                        Json.Decode.fail ("Unexpected token " ++ s)
            )


decodeTokenLiteral : Decoder TokenLiteral
decodeTokenLiteral =
    Json.Decode.oneOf
        [ Json.Decode.map Str Json.Decode.string
        , Json.Decode.map Num Json.Decode.float
        , Json.Decode.map Boolean Json.Decode.bool
        , Json.Decode.null Nil
        ]


tokenLiteralString : TokenLiteral -> String
tokenLiteralString l =
    case l of
        Str s ->
            s

        Num f ->
            String.fromFloat f

        Boolean b ->
            if b then
                "true"

            else
                "false"

        Nil ->
            "null"


decodeRunResult : Decoder TokenLiteral
decodeRunResult =
    field "result" decodeTokenLiteral


exprToken : Expr -> List Token
exprToken expr =
    case expr of
        Binary e ->
            [ e.operator ]

        Unary e ->
            [ e.operator ]

        Grouping e ->
            e.tokens

        Literal e ->
            [ e.token ]


exprResult : Expr -> Maybe TokenLiteral
exprResult expr =
    case expr of
        Binary e ->
            e.result

        Unary e ->
            e.result

        Grouping e ->
            e.result

        Literal e ->
            e.result


getExprTokenMin : Expr -> Maybe Int
getExprTokenMin expr =
    case expr of
        Binary e ->
            getExprTokenMin e.left

        Unary e ->
            Just e.operator.start

        Grouping e ->
            Maybe.map .start (List.head e.tokens)

        Literal e ->
            Just e.token.start


getExprTokenMax : Expr -> Maybe Int
getExprTokenMax expr =
    case expr of
        Binary e ->
            getExprTokenMax e.right

        Unary e ->
            getExprTokenMax e.right

        Grouping e ->
            Maybe.map .start (List.head (List.reverse e.tokens))

        Literal e ->
            Just e.token.start


getExprTokenRange : List Token -> Expr -> List Token
getExprTokenRange full expr =
    let
        ( min, max ) =
            ( getExprTokenMin expr, getExprTokenMax expr )
    in
    List.filter
        (\t ->
            t.start
                >= Maybe.withDefault 0 min
                && t.start
                <= Maybe.withDefault t.start max
        )
        full

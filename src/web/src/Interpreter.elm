module Interpreter exposing (..)

import Json.Decode exposing (Decoder, field, list)


type alias ScanError =
    { line : Int
    , message : String
    }


type alias ParseError =
    { token : Token
    , message : String
    }


type Stmt
    = Expression ExpressionStmt
    | Print PrintStmt
    | Var VarStmt
    | Block BlockStmt
    | If IfStmt


type alias ExpressionStmt =
    { expression : RunExpr
    , tokens : List Token
    }


type alias PrintStmt =
    { expression : RunExpr
    , tokens : List Token
    }


type alias VarStmt =
    { name : Token
    , initializer : Maybe RunExpr
    , tokens : List Token
    }


type alias BlockStmt =
    { tokens : List Token
    , statements : List (Maybe Stmt)
    }


type alias IfStmt =
    { condition : RunExpr
    , thenBranch : Stmt
    , elseBranch : Maybe Stmt
    , tokens : List Token
    }


type alias RunExpr =
    { expression : Expr
    , result : Maybe TokenLiteral
    }


type Expr
    = Binary BinaryExpr
    | Unary UnaryExpr
    | Grouping GroupingExpr
    | Literal LiteralExpr
    | Variable VariableExpr
    | Assignment AssignmentExpr
    | Logical LogicalExpr


type alias BinaryExpr =
    { left : RunExpr
    , right : RunExpr
    , operator : Token
    }


type alias UnaryExpr =
    { operator : Token
    , right : RunExpr
    }


type alias GroupingExpr =
    { expression : RunExpr
    , tokens : List Token
    }


type alias LiteralExpr =
    { value : TokenLiteral
    , token : Token
    }


type alias VariableExpr =
    { name : Token
    }


type alias AssignmentExpr =
    { name : Token
    , value : RunExpr
    }


type alias LogicalExpr =
    { operator : Token
    , left : RunExpr
    , right : RunExpr
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


decodeStmt : Decoder Stmt
decodeStmt =
    field "type" Json.Decode.string
        |> Json.Decode.andThen decodeStmtType


decodeStmtType : String -> Decoder Stmt
decodeStmtType s =
    case s of
        "expression" ->
            Json.Decode.map Expression
                (Json.Decode.map2 ExpressionStmt
                    (field "expression" decodeRunExpr)
                    (field "tokens" (list decodeToken))
                )

        "print" ->
            Json.Decode.map Print
                (Json.Decode.map2 PrintStmt
                    (field "expression" decodeRunExpr)
                    (field "tokens" (list decodeToken))
                )

        "var" ->
            Json.Decode.map Var
                (Json.Decode.map3 VarStmt
                    (field "name" decodeToken)
                    (Json.Decode.maybe (field "initializer" decodeRunExpr))
                    (field "tokens" (list decodeToken))
                )

        "block" ->
            Json.Decode.map Block
                (Json.Decode.map2 BlockStmt
                    (field "tokens" (list decodeToken))
                    (field "statements" (list (Json.Decode.maybe decodeStmt)))
                )

        "if" ->
            Json.Decode.map If
                (Json.Decode.map4 IfStmt
                    (field "condition" decodeRunExpr)
                    (field "thenBranch" decodeStmt)
                    (Json.Decode.maybe (field "elseBranch" decodeStmt))
                    (field "tokens" (list decodeToken))
                )

        _ ->
            Json.Decode.fail ("Unknown stmt type: " ++ s)


decodeRunExpr : Decoder RunExpr
decodeRunExpr =
    Json.Decode.map2 RunExpr
        decodeExpr
        (Json.Decode.maybe (field "result" decodeTokenLiteral))


decodeExpr : Decoder Expr
decodeExpr =
    field "type" Json.Decode.string
        |> Json.Decode.andThen decodeExprType


decodeExprType : String -> Decoder Expr
decodeExprType s =
    case s of
        "binary" ->
            Json.Decode.map Binary
                (Json.Decode.map3 BinaryExpr
                    (field "left" decodeRunExpr)
                    (field "right" decodeRunExpr)
                    (field "operator" decodeToken)
                )

        "unary" ->
            Json.Decode.map Unary
                (Json.Decode.map2 UnaryExpr
                    (field "operator" decodeToken)
                    (field "right" decodeRunExpr)
                )

        "grouping" ->
            Json.Decode.map Grouping
                (Json.Decode.map2 GroupingExpr
                    (field "expression" decodeRunExpr)
                    (field "tokens" (list decodeToken))
                )

        "literal" ->
            Json.Decode.map Literal
                (Json.Decode.map2 LiteralExpr
                    (field "value" decodeTokenLiteral)
                    (field "token" decodeToken)
                )

        "variable" ->
            Json.Decode.map Variable
                (Json.Decode.map VariableExpr
                    (field "name" decodeToken)
                )

        "assignment" ->
            Json.Decode.map Assignment
                (Json.Decode.map2 AssignmentExpr
                    (field "name" decodeToken)
                    (field "value" decodeRunExpr)
                )

        "logical" ->
            Json.Decode.map Logical
                (Json.Decode.map3 LogicalExpr
                    (field "operator" decodeToken)
                    (field "left" decodeRunExpr)
                    (field "right" decodeRunExpr)
                )

        _ ->
            Json.Decode.fail ("Unknown expr type: " ++ s)


decodeParseError : Decoder ParseError
decodeParseError =
    Json.Decode.map2 ParseError
        (field "token" decodeToken)
        (field "message" Json.Decode.string)


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

        Variable e ->
            [ e.name ]

        Assignment e ->
            [ e.name ]

        Logical e ->
            [ e.operator ]


getExprTokenMin : RunExpr -> Maybe Int
getExprTokenMin expr =
    case expr.expression of
        Binary e ->
            getExprTokenMin e.left

        Unary e ->
            Just e.operator.start

        Grouping e ->
            Maybe.map .start (List.head e.tokens)

        Literal e ->
            Just e.token.start

        Variable e ->
            Just e.name.start

        Assignment e ->
            Just e.name.start

        Logical e ->
            getExprTokenMin e.left


getExprTokenMax : RunExpr -> Maybe Int
getExprTokenMax expr =
    case expr.expression of
        Binary e ->
            getExprTokenMax e.right

        Unary e ->
            getExprTokenMax e.right

        Grouping e ->
            Maybe.map .start (List.head (List.reverse e.tokens))

        Literal e ->
            Just e.token.start

        Variable e ->
            Just e.name.start

        Assignment e ->
            getExprTokenMax e.value

        Logical e ->
            getExprTokenMax e.right


getExprTokenRange : List Token -> RunExpr -> List Token
getExprTokenRange full expr =
    getTokenRange
        ( getExprTokenMin expr, getExprTokenMax expr )
        full


getTokenRange : ( Maybe Int, Maybe Int ) -> List Token -> List Token
getTokenRange ( min, max ) full =
    List.filter
        (\t ->
            t.start
                >= Maybe.withDefault 0 min
                && t.start
                <= Maybe.withDefault t.start max
        )
        full


getStmtTokenMin : Stmt -> Maybe Int
getStmtTokenMin stmt =
    case stmt of
        Expression e ->
            getExprTokenMin e.expression

        Print p ->
            Maybe.map .start (List.head p.tokens)

        Block b ->
            Maybe.map .start (List.head b.tokens)

        If i ->
            Maybe.map .start (List.head i.tokens)

        Var v ->
            Maybe.map .start (List.head v.tokens)


getStmtTokenMax : Stmt -> Maybe Int
getStmtTokenMax stmt =
    case stmt of
        Expression e ->
            Maybe.map .start (List.head (List.reverse e.tokens))

        Print p ->
            Maybe.map .start (List.head (List.reverse p.tokens))

        Block b ->
            Maybe.map .start (List.head (List.reverse b.tokens))

        If i ->
            case i.elseBranch of
                Nothing ->
                    getStmtTokenMax i.thenBranch

                Just e ->
                    getStmtTokenMax e

        Var v ->
            Maybe.map .start (List.head (List.reverse v.tokens))


getStmtTokenRange : List Token -> Stmt -> List Token
getStmtTokenRange full stmt =
    getTokenRange
        ( getStmtTokenMin stmt, getStmtTokenMax stmt )
        full

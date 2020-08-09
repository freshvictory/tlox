export function parse(tokens, error) {
    try {
        return matchExpression(tokens.filter(t => t.type !== 'WHITESPACE' && t.type !== 'COMMENT' && t.type !== 'EOF'), error)[0];
    }
    catch (_a) {
        return null;
    }
}
function matchExpression(tokens, error) {
    return matchEquality(tokens, error);
}
function matchEquality(tokens, error) {
    return matchBinary(matchComparison, ['BANG_EQUAL', 'EQUAL_EQUAL'], tokens, error);
}
function matchComparison(tokens, error) {
    return matchBinary(matchAddition, ['LESS', 'LESS_EQUAL', 'GREATER', 'GREATER_EQUAL'], tokens, error);
}
function matchAddition(tokens, error) {
    return matchBinary(matchMultiplication, ['MINUS', 'PLUS'], tokens, error);
}
function matchMultiplication(tokens, error) {
    return matchBinary(matchUnary, ['SLASH', 'STAR'], tokens, error);
}
function matchUnary(tokens, error) {
    let [token, ...rest] = tokens;
    if (token.type === 'BANG' || token.type === 'MINUS') {
        let right;
        [right, rest] = matchUnary(rest, error);
        return [
            {
                type: 'unary',
                operator: token,
                right: right
            },
            rest
        ];
    }
    return matchPrimary(tokens, error);
}
function matchPrimary(tokens, error) {
    let [token, ...rest] = tokens;
    switch (token.type) {
        case 'FALSE': return [
            {
                type: 'literal',
                value: false,
                token
            },
            rest
        ];
        case 'TRUE': return [
            {
                type: 'literal',
                value: true,
                token
            },
            rest
        ];
        case 'NIL': return [
            {
                type: 'literal',
                value: null,
                token
            },
            rest
        ];
        case 'NUMBER':
        case 'STRING': return [
            {
                type: 'literal',
                value: token.literal,
                token
            },
            rest
        ];
        case 'LEFT_PAREN':
            let expr;
            [expr, rest] = matchExpression(rest, error);
            return [
                {
                    type: 'grouping',
                    expression: expr,
                    token
                },
                rest
            ];
        default:
            error(token, "Unknown token.");
            return matchPrimary(rest, error);
    }
}
function matchBinary(child, symbols, tokens, error) {
    let [expr, rest] = child(tokens, error);
    let token;
    [token, ...rest] = rest;
    while (token && symbols.indexOf(token.type) > -1) {
        let right;
        [right, rest] = child(rest, error);
        expr = {
            type: 'binary',
            operator: token,
            left: expr,
            right: right
        };
        [token, ...rest] = rest;
    }
    return [expr, [token, ...rest]];
}
// Formatting
export function prettyPrint(expression) {
    return printExpression(expression);
}
function printExpression(expression) {
    switch (expression.type) {
        case 'binary':
            return parenthesize(expression.operator.lexeme, expression.left, expression.right);
        case 'unary':
            return parenthesize(expression.operator.lexeme, expression.right);
        case 'grouping':
            return parenthesize('group', expression.expression);
        case 'literal':
            return expression.value == null
                ? 'nil'
                : expression.value + '';
    }
}
function parenthesize(name, ...expressions) {
    if (expressions && expressions.length) {
        return `(${name} ${expressions.map(printExpression).join(' ')})`;
    }
    else {
        return `(${name});`;
    }
}

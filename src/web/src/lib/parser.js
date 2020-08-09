export function parse(tokens) {
    return {
        type: 'binary',
        operator: {
            type: 'STAR',
            lexeme: '*',
            line: 1,
            start: 0
        },
        left: {
            type: 'unary',
            operator: {
                type: 'MINUS',
                lexeme: '-',
                line: 1,
                start: 0
            },
            right: {
                type: 'literal',
                token: {
                    type: 'NUMBER',
                    lexeme: '123',
                    literal: 123,
                    line: 1,
                    start: 0
                },
                value: 123
            }
        },
        right: {
            type: 'grouping',
            token: {
                type: 'NUMBER',
                lexeme: '123',
                literal: 123,
                line: 1,
                start: 0
            },
            expression: {
                type: 'literal',
                token: {
                    type: 'NUMBER',
                    lexeme: '45.67',
                    literal: 45.67,
                    line: 1,
                    start: 0
                },
                value: 45.67
            }
        }
    };
}
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

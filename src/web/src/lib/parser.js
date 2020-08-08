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

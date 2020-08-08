

import { Token } from './scanner';


// Expressions

export type Expr =
  | {
    type: 'binary'
    left: Expr,
    operator: Token,
    right: Expr
  }
  | {
    type: 'unary'
    operator: Token,
    right: Expr
  }
  | {
    type: 'grouping'
    expression: Expr
  }
  | {
    type: 'literal'
    value: unknown
  };


export function prettyPrint(expression: Expr): string {
  return printExpression(expression);
}

function printExpression(expression: Expr): string {
  switch (expression.type) {
    case 'binary':
      return parenthesize(
        expression.operator.lexeme,
        expression.left,
        expression.right
      );
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

function parenthesize(name: string, ...expressions: Expr[]): string {
  if (expressions && expressions.length) {
    return `(${name} ${expressions.map(printExpression).join(' ')})`;
  } else {
    return `(${name});`
  }
}

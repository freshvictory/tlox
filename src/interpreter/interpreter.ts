import type { Expr } from './parser.ts';
import type { Token } from './scanner.ts';

export class Interpreter {
  public static interpret(expr: Expr, error: (e: RuntimeError) => void) {
    try {
      return Interpreter.evaluate(expr);
    } catch (e) {
      error(e);
    }
  }


  private static evaluate(expr: Expr): any {
    switch (expr.type) {
      case 'grouping':
        return Interpreter.evaluate(expr.expression);

      case 'literal':
        return expr.value;

      case 'unary': {
        const right = Interpreter.evaluate(expr.right);

        switch (expr.operator.type) {
          case 'MINUS':
            Interpreter.checkNumber(expr.operator, right);
            return -1 * right;
          case 'BANG':
            return !Interpreter.isTruthy(right);
        }

        // unreachable
        return null;
      }

      case 'binary': {
        const left = Interpreter.evaluate(expr.left);
        const right = Interpreter.evaluate(expr.right);

        switch (expr.operator.type) {
          case 'GREATER':
            Interpreter.checkNumber(expr.operator, right);
            return left > right;
          case 'GREATER_EQUAL':
            Interpreter.checkNumber(expr.operator, right);
            return left >= right;
          case 'LESS':
            Interpreter.checkNumber(expr.operator, right);
            return left < right;
          case 'LESS_EQUAL':
            Interpreter.checkNumber(expr.operator, right);
            return left <= right;
          case 'MINUS':
            Interpreter.checkNumber(expr.operator, right);
            return left - right;
          case 'PLUS':
            if (typeof left === 'number' && typeof right === 'number') {
              return left + right;
            }

            if (typeof left === 'string' && typeof right === 'string') {
              return left + right;
            }

            throw new RuntimeError(
              expr.operator,
              "Operands must be two numbers or two strings."
            );
          case 'SLASH':
            Interpreter.checkNumber(expr.operator, right);
            return left / right;
          case 'STAR':
            Interpreter.checkNumber(expr.operator, right);
            return left * right;
          case 'EQUAL_EQUAL': return Interpreter.isEqual(left, right);
          case 'BANG_EQUAL': return Interpreter.isEqual(left, right);
        }

        // unreachable
        return null;
      }
    }
  }

  private static isTruthy(value: unknown): boolean {
    if (value === null) { return false; }
    if (typeof value === 'boolean') { return value; }
    return true;
  }

  private static isEqual(a: unknown, b: unknown): boolean {
    if (a === null) { return b === null; }
    return a === b;
  }

  private static checkNumber(operator: Token, left: number, right: number = 0) {
    if (typeof left === 'number' && typeof right === 'number') { return; }
    throw new RuntimeError(operator, "Operand(s) must be a number.");
  }
}

export class RuntimeError extends Error {
  constructor(readonly token: Token, message: string) {
    super(message);
  }
}

import type { Expr, Stmt } from './parser.ts';
import type { Token } from './scanner.ts';
import { Environment } from './environment.ts';

export class Interpreter {
  private environment = new Environment();

  constructor(
    readonly print: (m: string) => void,
    readonly error: (e: RuntimeError) => void
  ) { }

  public interpret(
    stmts: Stmt[]
  ): Stmt[] {
    try {
      stmts.forEach((s) => this.evaluateStmt(s));
    } catch (e) {
      this.error(e);
    }

    return stmts;
  }


  private evaluateStmt(stmt: Stmt) {
    switch (stmt.type) {
      case 'expression':
        this.evaluateAndRecord(stmt.expression);
        return;
      case 'print': {
        const val = this.evaluateAndRecord(stmt.expression);
        this.print(val + '');
        return;
      }
      case 'var': {
        let val = null;
        if (stmt.initializer) {
          val = this.evaluateAndRecord(stmt.initializer);
        }

        this.environment.define(stmt.name.lexeme, val);
        return;
      }
      case 'block': {
        this.executeBlock(stmt.statements, new Environment(this.environment));
        return;
      }
      case 'if': {
        if (this.isTruthy(this.evaluateAndRecord(stmt.condition))) {
          this.evaluateStmt(stmt.thenBranch);
        } else if (stmt.elseBranch) {
          this.evaluateStmt(stmt.elseBranch);
        }
        return;
      }
    }
  }


  private executeBlock(statements: (Stmt | null)[], environment: Environment): void {
    const previousEnvironment = this.environment;

    try {
      this.environment = environment;
      statements.forEach((s) => s && this.evaluateStmt(s));
    } finally {
      this.environment = previousEnvironment;
    }
  }


  private evaluateAndRecord(expr: Expr & { result?: any }) {
    const result = this.evaluate(expr);
    expr.result = result;

    return result;
  }


  private evaluate(expr: Expr): any {
    switch (expr.type) {
      case 'grouping':
        return this.evaluateAndRecord(expr.expression);

      case 'literal':
        return expr.value;

      case 'unary': {
        const right = this.evaluateAndRecord(expr.right);

        switch (expr.operator.type) {
          case 'MINUS':
            this.checkNumber(expr.operator, right);
            return -1 * right;
          case 'BANG':
            return !this.isTruthy(right);
        }

        // unreachable
        return null;
      }

      case 'binary': {
        const left = this.evaluateAndRecord(expr.left);
        const right = this.evaluateAndRecord(expr.right);

        switch (expr.operator.type) {
          case 'GREATER':
            this.checkNumber(expr.operator, left, right);
            return left > right;
          case 'GREATER_EQUAL':
            this.checkNumber(expr.operator, left, right);
            return left >= right;
          case 'LESS':
            this.checkNumber(expr.operator, left, right);
            return left < right;
          case 'LESS_EQUAL':
            this.checkNumber(expr.operator, left, right);
            return left <= right;
          case 'MINUS':
            this.checkNumber(expr.operator, left, right);
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
            this.checkNumber(expr.operator, left, right);
            return left / right;
          case 'STAR':
            this.checkNumber(expr.operator, left, right);
            return left * right;
          case 'EQUAL_EQUAL': return this.isEqual(left, right);
          case 'BANG_EQUAL': return !this.isEqual(left, right);
        }

        // unreachable
        return null;
      }

      case 'variable': {
        return this.environment.get(expr.name);
      }

      case 'assignment': {
        const val = this.evaluateAndRecord(expr.value);

        this.environment.assign(expr.name, val);

        return val;
      }

      case 'logical': {
        const left = this.evaluateAndRecord(expr.left);

        if (expr.operator.type === 'OR') {
          if (this.isTruthy(left)) {
            return left;
          }
        } else {
          if (!this.isTruthy(left)) {
            return left;
          }
        }

        return this.evaluateAndRecord(expr.right);
      }
    }
  }

  private isTruthy(value: unknown): boolean {
    if (value === null) { return false; }
    if (typeof value === 'boolean') { return value; }
    return true;
  }

  private isEqual(a: unknown, b: unknown): boolean {
    if (a === null) { return b === null; }
    return a === b;
  }

  private checkNumber(operator: Token, left: number, right: number = 0) {
    if (typeof left === 'number' && typeof right === 'number') { return; }
    throw new RuntimeError(operator, "Operand(s) must be a number.");
  }
}

export class RuntimeError extends Error {
  constructor(readonly token: Token, message: string) {
    super(message);
  }
}

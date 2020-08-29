class Interpreter {
  constructor(print, error) {
    this.print = print;
    this.error = error;
    this.environment = new Environment();
  }
  interpret(stmts) {
    try {
      stmts.forEach((s) => this.evaluateStmt(s));
    } catch (e) {
      this.error(e);
    }
    return stmts;
  }
  evaluateStmt(stmt) {
    switch (stmt.type) {
      case "expression":
        this.evaluateAndRecord(stmt.expression);
        return;
      case "print": {
        const val = this.evaluateAndRecord(stmt.expression);
        this.print(val + "");
        return;
      }
      case "var": {
        let val = null;
        if (stmt.initializer) {
          val = this.evaluateAndRecord(stmt.initializer);
        }
        this.environment.define(stmt.name.lexeme, val);
        return;
      }
      case "block": {
        this.executeBlock(stmt.statements, new Environment(this.environment));
      }
    }
  }
  executeBlock(statements, environment2) {
    const previousEnvironment = this.environment;
    try {
      this.environment = environment2;
      statements.forEach((s) => s && this.evaluateStmt(s));
    } finally {
      this.environment = previousEnvironment;
    }
  }
  evaluateAndRecord(expr) {
    const result = this.evaluate(expr);
    expr.result = result;
    return result;
  }
  evaluate(expr) {
    switch (expr.type) {
      case "grouping":
        return this.evaluateAndRecord(expr.expression);
      case "literal":
        return expr.value;
      case "unary": {
        const right = this.evaluateAndRecord(expr.right);
        switch (expr.operator.type) {
          case "MINUS":
            this.checkNumber(expr.operator, right);
            return -1 * right;
          case "BANG":
            return !this.isTruthy(right);
        }
        return null;
      }
      case "binary": {
        const left = this.evaluateAndRecord(expr.left);
        const right = this.evaluateAndRecord(expr.right);
        switch (expr.operator.type) {
          case "GREATER":
            this.checkNumber(expr.operator, left, right);
            return left > right;
          case "GREATER_EQUAL":
            this.checkNumber(expr.operator, left, right);
            return left >= right;
          case "LESS":
            this.checkNumber(expr.operator, left, right);
            return left < right;
          case "LESS_EQUAL":
            this.checkNumber(expr.operator, left, right);
            return left <= right;
          case "MINUS":
            this.checkNumber(expr.operator, left, right);
            return left - right;
          case "PLUS":
            if (typeof left === "number" && typeof right === "number") {
              return left + right;
            }
            if (typeof left === "string" && typeof right === "string") {
              return left + right;
            }
            throw new RuntimeError(expr.operator, "Operands must be two numbers or two strings.");
          case "SLASH":
            this.checkNumber(expr.operator, left, right);
            return left / right;
          case "STAR":
            this.checkNumber(expr.operator, left, right);
            return left * right;
          case "EQUAL_EQUAL":
            return this.isEqual(left, right);
          case "BANG_EQUAL":
            return !this.isEqual(left, right);
        }
        return null;
      }
      case "variable": {
        return this.environment.get(expr.name);
      }
      case "assignment": {
        const val = this.evaluateAndRecord(expr.value);
        this.environment.assign(expr.name, val);
        return val;
      }
    }
  }
  isTruthy(value) {
    if (value === null) {
      return false;
    }
    if (typeof value === "boolean") {
      return value;
    }
    return true;
  }
  isEqual(a, b) {
    if (a === null) {
      return b === null;
    }
    return a === b;
  }
  checkNumber(operator, left, right = 0) {
    if (typeof left === "number" && typeof right === "number") {
      return;
    }
    throw new RuntimeError(operator, "Operand(s) must be a number.");
  }
}
class RuntimeError extends Error {
  constructor(token, message) {
    super(message);
    this.token = token;
  }
}

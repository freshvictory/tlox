class Interpreter {
  constructor(print, error) {
    this.print = print;
    this.error = error;
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
        Interpreter.evaluateAndRecord(stmt.expression);
        return;
      case "print":
        const val = Interpreter.evaluateAndRecord(stmt.expression);
        this.print(val + "");
        return;
    }
  }
  static evaluateAndRecord(expr) {
    const result = Interpreter.evaluate(expr);
    expr.result = result;
    return result;
  }
  static evaluate(expr) {
    switch (expr.type) {
      case "grouping":
        return Interpreter.evaluateAndRecord(expr.expression);
      case "literal":
        return expr.value;
      case "unary": {
        const right = Interpreter.evaluateAndRecord(expr.right);
        switch (expr.operator.type) {
          case "MINUS":
            Interpreter.checkNumber(expr.operator, right);
            return -1 * right;
          case "BANG":
            return !Interpreter.isTruthy(right);
        }
        return null;
      }
      case "binary": {
        const left = Interpreter.evaluateAndRecord(expr.left);
        const right = Interpreter.evaluateAndRecord(expr.right);
        switch (expr.operator.type) {
          case "GREATER":
            Interpreter.checkNumber(expr.operator, right);
            return left > right;
          case "GREATER_EQUAL":
            Interpreter.checkNumber(expr.operator, right);
            return left >= right;
          case "LESS":
            Interpreter.checkNumber(expr.operator, right);
            return left < right;
          case "LESS_EQUAL":
            Interpreter.checkNumber(expr.operator, right);
            return left <= right;
          case "MINUS":
            Interpreter.checkNumber(expr.operator, right);
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
            Interpreter.checkNumber(expr.operator, right);
            return left / right;
          case "STAR":
            Interpreter.checkNumber(expr.operator, right);
            return left * right;
          case "EQUAL_EQUAL":
            return Interpreter.isEqual(left, right);
          case "BANG_EQUAL":
            return !Interpreter.isEqual(left, right);
        }
        return null;
      }
    }
  }
  static isTruthy(value) {
    if (value === null) {
      return false;
    }
    if (typeof value === "boolean") {
      return value;
    }
    return true;
  }
  static isEqual(a, b) {
    if (a === null) {
      return b === null;
    }
    return a === b;
  }
  static checkNumber(operator, left, right = 0) {
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

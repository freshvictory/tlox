class Interpreter {
  static interpret(expr, error) {
    try {
      return Interpreter.evaluate(expr);
    } catch (e) {
      error(e);
    }
  }
  static evaluate(expr) {
    switch (expr.type) {
      case "grouping":
        return Interpreter.evaluate(expr.expression);
      case "literal":
        return expr.value;
      case "unary": {
        const right = Interpreter.evaluate(expr.right);
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
        const left = Interpreter.evaluate(expr.left);
        const right = Interpreter.evaluate(expr.right);
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
            return Interpreter.isEqual(left, right);
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

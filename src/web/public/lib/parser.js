class ParseError extends Error {
}
class Parser {
  constructor(tokens, error) {
    this.current = 0;
    this.tokens = tokens;
    this.errorInternal = error;
  }
  parse() {
    const stmts = [];
    while (!this.isAtEnd()) {
      stmts.push(this.declaration());
    }
    return stmts;
  }
  declaration() {
    try {
      if (this.match("VAR")) {
        return this.varDeclaration();
      }
      return this.statement();
    } catch (e) {
      this.synchronize();
      return null;
    }
  }
  varDeclaration() {
    const tokens = [];
    tokens.push(this.previous());
    const name = this.consume("IDENTIFIER", "Expect variable name.");
    let initializer = null;
    if (this.match("EQUAL")) {
      tokens.push(this.previous());
      initializer = this.expression();
    }
    tokens.push(this.consume("SEMICOLON", "Expect `;` after variable declaration."));
    return {
      type: "var",
      name,
      initializer,
      tokens
    };
  }
  statement() {
    if (this.match("IF")) {
      return this.ifStatement();
    }
    if (this.match("PRINT")) {
      return this.printStatement();
    }
    if (this.match("LEFT_BRACE")) {
      return this.blockStatement();
    }
    return this.expressionStatement();
  }
  ifStatement() {
    const tokens = [];
    tokens.push(this.previous());
    tokens.push(this.consume("LEFT_PAREN", "Expect `(` after `if`."));
    const condition = this.expression();
    tokens.push(this.consume("RIGHT_PAREN", "Expect `)` after if condition."));
    const thenBranch = this.statement();
    let elseBranch = null;
    if (this.match("ELSE")) {
      tokens.push(this.previous());
      elseBranch = this.statement();
    }
    return {
      type: "if",
      condition,
      thenBranch,
      elseBranch,
      tokens
    };
  }
  printStatement() {
    const tokens = [];
    tokens.push(this.previous());
    const expr = this.expression();
    tokens.push(this.consume("SEMICOLON", "Expect `;` after value."));
    return {
      type: "print",
      expression: expr,
      tokens
    };
  }
  blockStatement() {
    const leftBrace = this.previous();
    const statements = this.block();
    return {
      type: "block",
      tokens: [leftBrace, this.previous()],
      statements
    };
  }
  block() {
    const statements = [];
    while (!this.check("RIGHT_BRACE") && !this.isAtEnd()) {
      statements.push(this.declaration());
    }
    this.consume("RIGHT_BRACE", "Expect `}` after block.");
    return statements;
  }
  expressionStatement() {
    const expr = this.expression();
    const semi = this.consume("SEMICOLON", "Expect `;` after value.");
    return {
      type: "expression",
      expression: expr,
      tokens: [semi]
    };
  }
  match(...types) {
    return types.some((t) => {
      if (this.check(t)) {
        this.advance();
        return true;
      }
      return false;
    });
  }
  check(type) {
    if (this.isAtEnd()) {
      return false;
    }
    return this.peek().type === type;
  }
  advance() {
    if (!this.isAtEnd()) {
      this.current++;
    }
    return this.previous();
  }
  isAtEnd() {
    return this.peek().type === "EOF";
  }
  peek() {
    return this.tokens[this.current];
  }
  previous() {
    return this.tokens[this.current - 1];
  }
  consume(type, message) {
    if (this.check(type)) {
      return this.advance();
    }
    throw this.error(this.peek(), message);
  }
  error(token, message) {
    this.errorInternal(token, message);
    return new ParseError();
  }
  synchronize() {
    this.advance();
    while (!this.isAtEnd()) {
      if (this.previous().type === "SEMICOLON") {
        return;
      }
      switch (this.peek().type) {
        case "CLASS":
        case "FUN":
        case "VAR":
        case "FOR":
        case "IF":
        case "WHILE":
        case "PRINT":
        case "RETURN":
          return;
      }
      this.advance();
    }
  }
  expression() {
    return this.assignment();
  }
  assignment() {
    const expr = this.or();
    if (this.match("EQUAL")) {
      const equals = this.previous();
      const value = this.assignment();
      if (expr.type === "variable") {
        const name = expr.name;
        return {
          type: "assignment",
          name,
          value
        };
      }
      this.error(equals, "Invalid assignment target.");
    }
    return expr;
  }
  or() {
    let expr = this.and();
    while (this.match("OR")) {
      const operator = this.previous();
      const right = this.and();
      expr = {
        type: "logical",
        left: expr,
        operator,
        right
      };
    }
    return expr;
  }
  and() {
    let expr = this.equality();
    while (this.match("AND")) {
      const operator = this.previous();
      const right = this.equality();
      expr = {
        type: "logical",
        left: expr,
        operator,
        right
      };
    }
    return expr;
  }
  equality() {
    let expr = this.comparison();
    while (this.match("BANG_EQUAL", "EQUAL_EQUAL")) {
      const operator = this.previous();
      const right = this.comparison();
      expr = {
        type: "binary",
        left: expr,
        operator,
        right
      };
    }
    return expr;
  }
  comparison() {
    let expr = this.addition();
    while (this.match("GREATER", "GREATER_EQUAL", "LESS", "LESS_EQUAL")) {
      const operator = this.previous();
      const right = this.addition();
      expr = {
        type: "binary",
        left: expr,
        operator,
        right
      };
    }
    return expr;
  }
  addition() {
    let expr = this.multiplication();
    while (this.match("MINUS", "PLUS")) {
      const operator = this.previous();
      const right = this.multiplication();
      expr = {
        type: "binary",
        left: expr,
        operator,
        right
      };
    }
    return expr;
  }
  multiplication() {
    let expr = this.unary();
    while (this.match("SLASH", "STAR")) {
      const operator = this.previous();
      const right = this.unary();
      expr = {
        type: "binary",
        left: expr,
        operator,
        right
      };
    }
    return expr;
  }
  unary() {
    if (this.match("BANG", "MINUS")) {
      const operator = this.previous();
      const right = this.unary();
      return {
        type: "unary",
        operator,
        right
      };
    }
    return this.primary();
  }
  primary() {
    if (this.match("FALSE")) {
      return {type: "literal", value: false, token: this.previous()};
    }
    if (this.match("TRUE")) {
      return {type: "literal", value: true, token: this.previous()};
    }
    if (this.match("NIL")) {
      return {type: "literal", value: null, token: this.previous()};
    }
    if (this.match("NUMBER", "STRING")) {
      return {
        type: "literal",
        value: this.previous().literal,
        token: this.previous()
      };
    }
    if (this.match("IDENTIFIER")) {
      return {
        type: "variable",
        name: this.previous()
      };
    }
    if (this.match("LEFT_PAREN")) {
      const firstParen = this.previous();
      const expr = this.expression();
      this.consume("RIGHT_PAREN", `Expect ')' after expression.`);
      return {
        type: "grouping",
        expression: expr,
        tokens: [firstParen, this.previous()]
      };
    }
    throw this.error(this.peek(), "Expect expression.");
  }
}
function parse(tokens, error) {
  return new Parser(tokens.filter((t) => {
    return t.type !== "WHITESPACE" && t.type !== "COMMENT";
  }), error).parse();
}

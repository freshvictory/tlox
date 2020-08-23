import type { Token, TokenType } from './scanner.ts';


// Expressions

export type Expr =
  | {
    type: 'binary',
    left: Expr,
    operator: Token,
    right: Expr
  }
  | {
    type: 'unary',
    operator: Token,
    right: Expr
  }
  | {
    type: 'grouping',
    tokens: Token[],
    expression: Expr
  }
  | {
    type: 'literal',
    token: Token,
    value: unknown
  };

class ParseError extends Error {}

class Parser {
  private readonly tokens: ReadonlyArray<Token>;
  private readonly errorInternal: (t: Token, m: string) => void;
  private current = 0;

  constructor(tokens: Token[], error: (t: Token, m: string) => void) {
    this.tokens = tokens;
    this.errorInternal = error;
  }

  public parse(): Expr | null {
    try {
      return this.expression();
    } catch (e) {
      return null;
    }
  }

  private match(...types: TokenType[]): boolean {
    return types.some(t => {
      if (this.check(t)) {
        this.advance();

        return true;
      }

      return false;
    });
  }

  private check(type: TokenType): boolean {
    if (this.isAtEnd()) { return false; }

    return this.peek().type === type;
  }

  private advance() {
    if (!this.isAtEnd()) {
      this.current++;
    }

    return this.previous();
  }

  private isAtEnd(): boolean {
    return this.peek().type === 'EOF';
  }

  private peek(): Token {
    return this.tokens[this.current];
  }

  private previous(): Token {
    return this.tokens[this.current - 1];
  }

  private consume(type: TokenType, message: string) {
    if (this.check(type)) { return this.advance(); }

    throw this.error(this.peek(), message);
  }

  private error(token: Token, message: string): ParseError {
    this.errorInternal(token, message);

    return new ParseError();
  }

  private synchronize() {
    this.advance();

    while (!this.isAtEnd()) {
      if (this.previous().type === 'SEMICOLON') { return; }

      switch (this.peek().type) {
        case 'CLASS':
        case 'FUN':
        case 'VAR':
        case 'FOR':
        case 'IF':
        case 'WHILE':
        case 'PRINT':
        case 'RETURN':
          return;
      }

      this.advance();
    }
  }

  private expression(): Expr {
    return this.equality();
  }

  private equality(): Expr {
    let expr = this.comparison();

    while (this.match('BANG_EQUAL', 'EQUAL_EQUAL')) {
      const operator = this.previous();
      const right = this.comparison();

      expr = {
        type: 'binary',
        left: expr,
        operator,
        right
      };
    }

    return expr;
  }

  private comparison(): Expr {
    let expr = this.addition();

    while (this.match(
      'GREATER',
      'GREATER_EQUAL',
      'LESS',
      'LESS_EQUAL'
    )) {
      const operator = this.previous();
      const right = this.addition();

      expr = {
        type: 'binary',
        left: expr,
        operator,
        right
      };
    }

    return expr;
  }

  private addition(): Expr {
    let expr = this.multiplication();

    while (this.match('MINUS', 'PLUS')) {
      const operator = this.previous();
      const right = this.multiplication();

      expr = {
        type: 'binary',
        left: expr,
        operator,
        right
      };
    }

    return expr;
  }

  private multiplication(): Expr {
    let expr = this.unary();

    while (this.match('SLASH', 'STAR')) {
      const operator = this.previous();
      const right = this.unary();

      expr = {
        type: 'binary',
        left: expr,
        operator,
        right
      };
    }

    return expr;
  }

  private unary(): Expr {
    if (this.match('BANG', 'MINUS')) {
      const operator = this.previous();
      const right = this.unary();

      return {
        type: 'unary',
        operator,
        right
      };
    }

    return this.primary();
  }

  private primary(): Expr {
    if (this.match('FALSE')) {
      return { type: 'literal', value: false, token: this.previous() };
    }
    if (this.match('TRUE')) {
      return { type: 'literal', value: true, token: this.previous() };
    }
    if (this.match('NIL')) {
      return { type: 'literal', value: null, token: this.previous() };
    }

    if (this.match('NUMBER', 'STRING')) {
      return {
        type: 'literal',
        value: this.previous().literal,
        token: this.previous()
      };
    }

    if (this.match('LEFT_PAREN')) {
      const firstParen = this.previous();

      const expr = this.expression();

      this.consume('RIGHT_PAREN', `Expect ')' after expression.`);

      return {
        type: 'grouping',
        expression: expr,
        tokens: [firstParen, this.previous()]
      };
    }

    throw this.error(this.peek(), "Expect expression.");
  }
}

export function parse(
  tokens: Token[],
  error: (t: Token, m: string) => void
): Expr | null {
  return new Parser(tokens.filter(t => {
    return t.type !== 'WHITESPACE' && t.type !== 'COMMENT'
  }), error).parse();
}

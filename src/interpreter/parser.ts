import type { Token, TokenType } from './scanner';


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
  }
  | {
    type: 'variable',
    name: Token
  }
  | {
    type: 'assignment',
    name: Token,
    value: Expr
  }
  | {
    type: 'logical',
    left: Expr,
    operator: Token,
    right: Expr
  };


export type Stmt =
  | {
    type: 'expression',
    tokens: Token[],
    expression: Expr
  }
  | {
    type: 'print',
    tokens: Token[],
    expression: Expr
  }
  | {
    type: 'var',
    tokens: Token[],
    name: Token,
    initializer: Expr | null
  }
  | {
    type: 'block',
    tokens: Token[],
    statements: (Stmt | null)[]
  }
  | {
    type: 'if',
    tokens: Token[],
    condition: Expr,
    thenBranch: Stmt,
    elseBranch: Stmt | null
  }
  | {
    type: 'while',
    tokens: Token[],
    condition: Expr,
    body: Stmt
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

  public parse(): (Stmt | null)[] {
    const stmts: (Stmt | null)[] = [];
    while (!this.isAtEnd()) {
      stmts.push(this.declaration());
    }

    return stmts;
  }

  private declaration(): Stmt | null {
    try {
      if (this.match('VAR')) {
        return this.varDeclaration();
      }

      return this.statement();
    } catch (e) {
      this.synchronize();

      return null;
    }
  }

  private varDeclaration(): Stmt {
    const tokens: Token[] = [];
    tokens.push(this.previous());
    const name = this.consume('IDENTIFIER', 'Expect variable name.');

    let initializer = null;
    if (this.match('EQUAL')) {
      tokens.push(this.previous());
      initializer = this.expression();
    }

    tokens.push(
      this.consume('SEMICOLON', 'Expect `;` after variable declaration.')
    );

    return {
      type: 'var',
      name,
      initializer,
      tokens
    };
  }

  private statement(): Stmt {
    if (this.match('IF')) {
      return this.ifStatement();
    }

    if (this.match('PRINT')) {
      return this.printStatement();
    }

    if (this.match('WHILE')) {
      return this.whileStatement();
    }

    if (this.match('LEFT_BRACE')) {
      return this.blockStatement();
    }

    return this.expressionStatement();
  }

  private ifStatement(): Stmt {
    const tokens: Token[] = [];
    tokens.push(this.previous());
    tokens.push(this.consume('LEFT_PAREN', "Expect `(` after `if`."));
    const condition = this.expression();
    tokens.push(this.consume('RIGHT_PAREN', "Expect `)` after if condition."));

    const thenBranch = this.statement();
    let elseBranch = null;
    if (this.match('ELSE')) {
      tokens.push(this.previous());
      elseBranch = this.statement();
    }

    return {
      type: 'if',
      condition,
      thenBranch,
      elseBranch,
      tokens
    };
  }

  private whileStatement(): Stmt {
    const tokens: Token[] = [];
    tokens.push(this.previous());
    tokens.push(this.consume('LEFT_PAREN', 'Expect `(` after `while`.'));
    const condition = this.expression();
    tokens.push(this.consume('RIGHT_PAREN', 'Expect `)` after condition.'));
    
    return {
      type: 'while',
      condition,
      tokens,
      body: this.statement()
    }
  }

  private printStatement(): Stmt {
    const tokens: Token[] = [];
    tokens.push(this.previous());
    const expr = this.expression();
    tokens.push(this.consume('SEMICOLON', 'Expect `;` after value.'));

    return {
      type: 'print',
      expression: expr,
      tokens
    };
  }

  private blockStatement(): Stmt {
    const leftBrace = this.previous();

    const statements = this.block();

    return {
      type: 'block',
      tokens: [leftBrace, this.previous()],
      statements
    }
  }

  private block(): (Stmt | null)[] {
    const statements: (Stmt | null)[] = [];

    while (!this.check('RIGHT_BRACE') && !this.isAtEnd()) {
      statements.push(this.declaration());
    }

    this.consume('RIGHT_BRACE', 'Expect `}` after block.');

    return statements
  }

  private expressionStatement(): Stmt {
    const expr = this.expression();
    const semi = this.consume('SEMICOLON', 'Expect `;` after value.');

    return {
      type: 'expression',
      expression: expr,
      tokens: [semi]
    };
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
    return this.assignment();
  }

  private assignment(): Expr {
    const expr = this.or();

    if (this.match('EQUAL')) {
      const equals = this.previous();
      const value = this.assignment();

      if (expr.type === 'variable') {
        const name = expr.name;

        return {
          type: 'assignment',
          name,
          value
        }
      }
      
      this.error(equals, "Invalid assignment target.");
    }

    return expr;
  }

  private or(): Expr {
    let expr = this.and();

    while (this.match('OR')) {
      const operator = this.previous();
      const right = this.and();
      expr = {
        type: 'logical',
        left: expr,
        operator,
        right
      }
    }

    return expr;
  }

  private and(): Expr {
    let expr = this.equality();

    while (this.match('AND')) {
      const operator = this.previous();
      const right = this.equality();
      expr = {
        type: 'logical',
        left: expr,
        operator,
        right
      }
    }

    return expr;
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

    if (this.match('IDENTIFIER')) {
      return {
        type: 'variable',
        name: this.previous()
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

function parse(
  tokens: Token[],
  error: (t: Token, m: string) => void
): (Stmt | null)[] {
  return new Parser(tokens.filter(t => {
    return t.type !== 'WHITESPACE' && t.type !== 'COMMENT'
  }), error).parse();
}

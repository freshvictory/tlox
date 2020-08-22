export type TokenType =


  // Single-character tokens

  | 'LEFT_PAREN' | 'RIGHT_PAREN'
  | 'LEFT_BRACE' | 'RIGHT_BRACE'
  | 'COMMA' | 'DOT' | 'MINUS' | 'PLUS' | 'SEMICOLON' | 'SLASH' | 'STAR'


  // One- or two-character tokens

  | 'BANG' | 'BANG_EQUAL'
  | 'EQUAL' | 'EQUAL_EQUAL'
  | 'GREATER' | 'GREATER_EQUAL'
  | 'LESS' | 'LESS_EQUAL'


  // Literals

  | 'IDENTIFIER' | 'STRING' | 'NUMBER'


  // Keywords

  | 'AND' | 'CLASS' | 'ELSE' | 'FALSE' | 'FUN' | 'FOR'
  | 'IF' | 'NIL' | 'OR' | 'PRINT' | 'RETURN' | 'SUPER'
  | 'THIS' | 'TRUE' | 'VAR' | 'WHILE'


  // Non-parsed tokens
  
  | 'COMMENT' | 'WHITESPACE' | 'UNEXPECTED'


  | 'EOF';


export type Token = {
  type: TokenType;
  lexeme: string;
  start: number;
  literal?: unknown;
  line: number;
};

const keywords: { [k: string]: TokenType } = {
  and: 'AND',
  class: 'CLASS',
  else: 'ELSE',
  false: 'FALSE',
  for: 'FOR',
  fun: 'FUN',
  if: 'IF',
  nil: 'NIL',
  or: 'OR',
  print: 'PRINT',
  return: 'RETURN',
  super: 'SUPER',
  this: 'THIS',
  true: 'TRUE',
  var: 'VAR',
  while: 'WHILE',
};


class Scanner {
  private readonly source: string;
  private tokens: Token[] = [];

  private start = 0;
  private current = 0;
  private line = 1;

  private error: (m: string) => void;

  constructor(source: string, error: (l: number, m: string) => void) {
    this.source = source;
    this.error = (m) => error(this.line, m);
  }

  public scanTokens(): Token[] {
    while (!this.isAtEnd()) {
      this.start = this.current;
      this.scanToken();
    }

    this.tokens.push({
      type: 'EOF',
      lexeme: "",
      line: this.line,
      start: this.start
    });

    return this.tokens;
  }

  private isAtEnd(): boolean {
    return this.current >= this.source.length;
  }

  private scanToken() {
    const c = this.advance();

    switch (c) {
      case '(': this.addToken('LEFT_PAREN');  break;
      case ')': this.addToken('RIGHT_PAREN'); break;
      case '{': this.addToken('LEFT_BRACE');  break;
      case '}': this.addToken('RIGHT_BRACE'); break;
      case ',': this.addToken('COMMA');       break;
      case '.': this.addToken('DOT');         break;
      case '-': this.addToken('MINUS');       break;
      case '+': this.addToken('PLUS');        break;
      case ';': this.addToken('SEMICOLON');   break;
      case '*': this.addToken('STAR');        break;

      case '!': this.addToken(this.match('=') ? 'BANG_EQUAL'    : 'BANG');    break;
      case '=': this.addToken(this.match('=') ? 'EQUAL_EQUAL'   : 'EQUAL');   break;
      case '<': this.addToken(this.match('=') ? 'LESS_EQUAL'    : 'LESS');    break;
      case '>': this.addToken(this.match('=') ? 'GREATER_EQUAL' : 'GREATER'); break;

      case '/':
        if (this.match('/')) {
          while (this.peek() !== '\n' && !this.isAtEnd()) {
            this.advance();
          }
          this.addToken('COMMENT');
        } else {
          this.addToken('SLASH');
        }
        break;

      case ' ':
      case '\r':
      case '\t':
        this.addToken('WHITESPACE');
        break;
      
      case '\n':
        this.addToken('WHITESPACE');
        this.line++;
        break;

      case '"':
        while (this.peek() !== '"' && !this.isAtEnd()) {
          if (this.peek() === '\n') { this.line++; }
          this.advance();
        }

        if (this.isAtEnd()) {
          this.error('Unterminated string.' );
        }

        this.advance();

        const value = this.source.substring(this.start + 1, this.current - 1);
        this.addToken('STRING', value);
        break;

      default:
        if (/[0-9]/.test(c)) {
          this.number();
        } else if (/[a-zA-Z_]/.test(c)) {
          this.identifier();
        } else {
          this.error(`Unexpected character: ${c}`);
          this.addToken('UNEXPECTED');
        }
        break;
    }
  }

  private advance(): string {
    this.current++;

    return this.source[this.current - 1];
  }

  private match(expected: string): boolean {
    if (this.isAtEnd()) { return false; }
    if (this.source[this.current] !== expected) { return false; }

    this.current++;

    return true;
  }

  private peek(ahead: 1 | 2 = 1): string {
    if (this.current + (ahead - 1) >= this.source.length) {
      return '\0';
    }

    return this.isAtEnd()
      ? '\0'
      : this.source[this.current + (ahead - 1)];
  }

  private number() {
    while (/[0-9]/.test(this.peek())) {
      this.advance();
    }

    if (this.peek() === '.' && /[0-9]/.test(this.peek(2))) {
      this.advance();
    }

    while (/[0-9]/.test(this.peek())) {
      this.advance();
    }

    this.addToken(
      'NUMBER',
      parseFloat(this.source.substring(this.start, this.current))
    );
  }

  private identifier() {
    while (/[0-9a-zA-Z_]/.test(this.peek())) {
      this.advance();
    }

    const text = this.source.substring(this.start, this.current);

    const type = keywords[text] || 'IDENTIFIER';

    this.addToken(type);
  }

  private addToken(type: TokenType, literal?: string | number) {
    const lexeme = this.source.substring(this.start, this.current);

    this.tokens.push({
      type,
      lexeme,
      literal,
      line: this.line,
      start: this.start
    });
  }
}


export function scanTokens(
  source: string,
  error: (line: number, message: string) => void
): Token[] {
  return new Scanner(source, error).scanTokens();
}

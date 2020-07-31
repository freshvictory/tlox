type TokenValue =


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


  | 'EOF';


export type Token = {
  type: TokenValue;
  lexeme: string;
  literal?: unknown;
  line: number;
};


export function scanTokens(
  source: string,
  error: (line: number, message: string) => void
): Token[] {
  return lex(source.split(''), error, 1);
}


function lex(
  source: string[],
  error: (line: number, message: string) => void,
  line: number
): Token[] {
  const [s, ...next] = source;

  switch (s) {
    case undefined:
      return [];
    case ' ':
    case '\r':
    case '\t':
      return lex(next, error, line);
    case '\n':
      return lex(next, error, line + 1);
    case '(':
      return [
        { type: 'LEFT_PAREN', lexeme: s, line },
        ...lex(next, error, line)
      ];
    case ')': 
      return [
        { type: 'RIGHT_PAREN', lexeme: s, line },
        ...lex(next, error, line)
      ];
    case '{':
      return [
        { type: 'LEFT_BRACE', lexeme: s, line },
        ...lex(next, error, line)
      ];
    case '}':
      return [
        { type: 'RIGHT_BRACE', lexeme: s, line },
        ...lex(next, error, line)
      ];
    case ',':
      return [
        { type: 'COMMA', lexeme: s, line },
        ...lex(next, error, line)
      ];
    case '.':
      return [
        { type: 'DOT', lexeme: s, line },
        ...lex(next, error, line)
      ];
    case '-':
      return [
        { type: 'MINUS', lexeme: s, line },
        ...lex(next, error, line)
      ];
    case '+':
      return [
        { type: 'PLUS', lexeme: s, line },
        ...lex(next, error, line)
      ];
    case ';':
      return [
        { type: 'SEMICOLON', lexeme: s, line },
        ...lex(next, error, line)
      ];
    case '*':
      return [
        { type: 'STAR', lexeme: s, line },
        ...lex(next, error, line)
      ];
    case '!': {
      const [peek, ...rest] = next;
      switch (peek) {
        case '=':
          return [
            { type: 'BANG_EQUAL', lexeme: s + peek, line },
            ...lex(rest, error, line)
          ];
        default:
          return [
            { type: 'BANG', lexeme: s, line },
            ...lex(next, error, line)
          ];
      }
    }
    case '=': {
      const [peek, ...rest] = next;
      switch (peek) {
        case '=':
          return [
            { type: 'EQUAL_EQUAL', lexeme: s + peek, line },
            ...lex(rest, error, line)
          ];
        default:
          return [
            { type: 'EQUAL', lexeme: s, line },
            ...lex(next, error, line)
          ];
      }
    }
    case '<': {
      const [peek, ...rest] = next;
      switch (peek) {
        case '=':
          return [
            { type: 'LESS_EQUAL', lexeme: s + peek, line },
            ...lex(rest, error, line)
          ];
        default:
          return [
            { type: 'LESS', lexeme: s, line },
            ...lex(next, error, line)
          ];
      }
    }
    case '>': {
      const [peek, ...rest] = next;
      switch (peek) {
        case '=':
          return [
            { type: 'GREATER_EQUAL', lexeme: s + peek, line },
            ...lex(rest, error, line)
          ];
        default:
          return [
            { type: 'GREATER', lexeme: s, line },
            ...lex(next, error, line)
          ];
      }
    }
    case '/': {
      const [peek, ...rest] = next;
      switch (peek) {
        case '/':
          let comment: string;
          let remainder = rest;
          do {
            [comment, ...remainder] = remainder;            
          } while (comment && comment !== '\n');
          return lex(remainder, error, line + 1);
        default:
          return [
            { type: 'SLASH', lexeme: s, line },
            ...lex(next, error, line)
          ];
      }
    }
    default:
      error(line, `Unexpected token ${s}`);
      return lex(next, error, line);
  }
}

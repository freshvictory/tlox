export type TokenValue =


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
    case '"': {
      let nextChar: string;
      let rest = next;
      let str = '';
      let lineCount = line;
      do {
        [nextChar, ...rest] = rest;
        str += nextChar;
        if (nextChar === '\n') { lineCount++; }
      } while(nextChar && nextChar !== '"');

      if (!nextChar) {
        error(lineCount, `Unterminated string.`);
        return [];
      }

      str = str.slice(0, -1);

      return [
        { type: 'STRING', lexeme: s + str + nextChar, line, literal: str },
        ...lex(rest, error, lineCount)
      ];
    }
    default:
      if (/[0-9]/.test(s)) {
        let nextDigit = s;
        let rest = next;
        let str = '';

        do {
          str += nextDigit;
          [nextDigit, ...rest] = rest;
        } while(
          nextDigit
          && (
            /[0-9]/.test(nextDigit)
            || (nextDigit === '.' && /[0-9]/.test(rest[0]))
          )
        );

        rest = [nextDigit, ...rest];

        return [
          { type: 'NUMBER', lexeme: str, line, literal: parseFloat(str) },
          ...lex(rest, error, line)
        ];
      } else if (/[a-zA-Z]/.test(s)) {
        let nextChar = s;
        let rest = next;
        let str = '';

        do {
          str += nextChar;
          [nextChar, ...rest] = rest;
        } while (nextChar && /[0-9a-zA-Z]/.test(nextChar));

        rest = [nextChar, ...rest];

        switch (str) {
          case 'and': return [
            { type: 'AND', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'class': return [
            { type: 'CLASS', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'else': return [
            { type: 'ELSE', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'false': return [
            { type: 'FALSE', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'for': return [
            { type: 'FOR', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'fun': return [
            { type: 'FUN', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'if': return [
            { type: 'IF', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'nil': return [
            { type: 'NIL', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'or': return [
            { type: 'OR', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'print': return [
            { type: 'PRINT', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'return': return [
            { type: 'RETURN', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'super': return [
            { type: 'SUPER', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'this': return [
            { type: 'THIS', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'true': return [
            { type: 'TRUE', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'var': return [
            { type: 'VAR', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          case 'while': return [
            { type: 'WHILE', lexeme: str, line },
            ...lex(rest, error, line)
          ];
          default: return [
            { type: 'IDENTIFIER', lexeme: str, line },
            ...lex(rest, error, line)
          ];
        }
      } else {
        error(line, `Unexpected token ${s}`);
        return lex(next, error, line);
      }
  }
}

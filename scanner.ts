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
  const line = 1;

  // while (current < source.length) {
  //   start = current;
  //   current += 1;

  //   const token = scanToken(source[start]);
  //   if (token) {
  //     if (typeof token === 'function') {

  //     } else {
  //       tokens.push({
  //         type: token,
  //         lexeme: source.substring(start, current),
  //         line
  //       });
  //     }
  //   } else {
  //     error(line, "Unexpected character.");
  //   }
  // }

  const tokens = scanTokensInternal(source, error, 0, 0, 1);

  // tokens.push({
  //   type: 'EOF',
  //   lexeme: '',
  //   line
  // });

  return tokens;
}


function scanTokensInternal(
  source: string,
  error: (line: number, message: string) => void,
  start: number,
  current: number,
  line: number,
  acc?: [TokenValue, TokenAccumulator]
): Token[] {
  if (current >= source.length) {
    if (acc) {
      return [{
        type: acc[0],
        lexeme: source.substring(start),
        line
      }];
    } else {
      return [];
    }
  }

  const next = current + 1;

  const tokenValue = scanToken(source[current]);

  if (!tokenValue) {
    error(line, `Unexpected token ${source[current]}`);

    if (acc) {
      return [
        {
          type: acc[0],
          lexeme: source.substring(start, current),
          line
        },
        ...scanTokensInternal(
          source,
          error,
          next,
          next,
          line
        )
      ];
    } else {
      return scanTokensInternal(
        source,
        error,
        next,
        next,
        line
      );
    }    
  }

  if (!acc) {
    if (typeof tokenValue === 'string') {
      return [
        {
          type: tokenValue,
          lexeme: source.substring(start, next),
          line
        },
        ...scanTokensInternal(
          source,
          error,
          next,
          next,
          line
        )
      ];
    } else {
      return scanTokensInternal(
        source,
        error,
        start,
        next,
        line,
        tokenValue
      );
    }
  }

  if (acc) {
    const defaultAccValue = acc[0];

    if (typeof tokenValue !== 'string') {
      const defaultTokenValue = tokenValue[0];
      const newAccValue = acc[1](defaultTokenValue);

      if (typeof newAccValue === 'string' && newAccValue !== defaultAccValue) {
        return [
          {
            type: newAccValue,
            lexeme: source.substring(start, next),
            line
          },
          ...scanTokensInternal(
            source,
            error,
            next,
            next,
            line
          )
        ];
      } else if (typeof newAccValue !== 'string') {
        return scanTokensInternal(
          source,
          error,
          start,
          next,
          line,
          [defaultAccValue, newAccValue]
        )
      } else {
        return [
          {
            type: defaultAccValue,
            lexeme: source.substring(start, current),
            line
          },
          ...scanTokensInternal(
            source,
            error,
            current,
            next,
            line,
            tokenValue
          )
        ]
      }
    } else {
      const newAccValue = acc[1](tokenValue);

      if (typeof newAccValue === 'string' && newAccValue !== defaultAccValue) {
        return [
          {
            type: newAccValue,
            lexeme: source.substring(start, next),
            line
          },
          ...scanTokensInternal(
            source,
            error,
            next,
            next,
            line
          )
        ];
      } else {
        return [
          {
            type: defaultAccValue,
            lexeme: source.substring(start, current),
            line
          },
          {
            type: tokenValue,
            lexeme: source.substring(current, next),
            line
          },
          ...scanTokensInternal(
            source,
            error,
            next,
            next,
            line
          )
        ];
      }
    }
  }

  return scanTokensInternal(
    source,
    error,
    next,
    next,
    line
  );
}


type TokenAccumulator = (t: TokenValue) => TokenValue | TokenAccumulator;

function scanToken(current: string):
  | [TokenValue, TokenAccumulator]
  | TokenValue
  | null
{
  switch (current) {
    case '(': return 'LEFT_PAREN';
    case ')': return 'RIGHT_PAREN';
    case '{': return 'LEFT_BRACE';
    case '}': return 'RIGHT_BRACE';
    case ',': return 'COMMA';
    case '.': return 'DOT';
    case '-': return 'MINUS';
    case '+': return 'PLUS';
    case ';': return 'SEMICOLON';
    case '*': return 'STAR';
    case '!': return ['BANG', (t) => t === 'EQUAL'
      ? 'BANG_EQUAL'
      : 'BANG'
    ];
    case '=': return ['EQUAL', (t) => t === 'EQUAL'
      ? 'EQUAL_EQUAL'
      : 'EQUAL'
    ];
    case '<': return ['LESS', (t) => t === 'EQUAL'
      ? 'LESS_EQUAL'
      : 'LESS'
    ];
    case '>': return ['GREATER', (t) => t === 'EQUAL'
      ? 'GREATER_EQUAL'
      : 'GREATER'
    ];
  }

  return null;
}


function match(s: string, source: string): boolean {
  return source.startsWith(s);
}
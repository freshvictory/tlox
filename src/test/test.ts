import { Expr } from '../interpreter/parser.ts';
import { Token, TokenValue } from '../interpreter/scanner.ts';


console.log();
await suite();

async function suite() {

  (async function testScanner() {
    const { scanTokens } = await import('../interpreter/scanner.ts');

    function test(
      name: string,
      source: string,
      expected: ([TokenValue, string, number, number] | [TokenValue, string, number, number, string | number])[]
    ) {
      const tokens = scanTokens(source, (l, m) => console.error(l, m));

      const expectedTokens: Token[] = expected.map(e => ({
        type: e[0],
        lexeme: e[1],
        line: e[2],
        start: e[3],
        literal: e[4]
      }));

      logResults('scanner#' + name, expectedTokens, tokens);
    }

    test(
      'singleCharacter',
      '*-+{',
      [
        ['STAR', '*', 1, 0],
        ['MINUS', '-', 1, 1],
        ['PLUS', '+', 1, 2],
        ['LEFT_BRACE', '{', 1, 3]
      ]
    );

    test(
      'doubleCharacter',
      '!==!,==!==>=<;',
      [
        ['BANG_EQUAL', '!=', 1, 0],
        ['EQUAL', '=', 1, 2],
        ['BANG', '!', 1, 3],
        ['COMMA', ',', 1, 4],
        ['EQUAL_EQUAL', '==', 1, 5],
        ['BANG_EQUAL', '!=', 1, 7],
        ['EQUAL', '=', 1, 8],
        ['GREATER_EQUAL', '>=', 1, 9],
        ['LESS', '<', 1, 11],
        ['SEMICOLON', ';', 1, 12]
      ]
    );

    test(
      'comments',
      '+-*// hi there\n+/;',
      [
        ['PLUS', '+', 1, 0],
        ['MINUS', '-', 1, 1],
        ['STAR', '*', 1, 2],
        ['COMMENT', '// hi there', 1, 3],
        ['WHITESPACE', '\n', 1, 13],
        ['PLUS', '+', 2, 16],
        ['SLASH', '/', 2, 17],
        ['SEMICOLON', ';', 2, 18]
      ]
    );

    test(
      'string',
      '"Hello, World!";',
      [
        ['STRING', '"Hello, World!"', 1, 0, 'Hello, World!'],
        ['SEMICOLON', ';', 1, 17]
      ]
    );

    test(
      'number',
      '12.2/300.5;',
      [
        ['NUMBER', '12.2', 1, 0, 12.2],
        ['SLASH', '/', 1, 5],
        ['NUMBER', '300.5', 1, 7, 300.5],
        ['SEMICOLON', ';', 1, 12]
      ]
    );

    test(
      'identifier',
      'var x = hi; for return x;',
      [
        ['VAR', 'var', 1, 0],
        ['WHITESPACE', ' ', 1, 3],
        ['IDENTIFIER', 'x', 1, 4],
        ['WHITESPACE', ' ', 1, 5],
        ['EQUAL', '=', 1, 6],
        ['WHITESPACE', ' ', 1, 7],
        ['IDENTIFIER', 'hi', 1, 8],
        ['SEMICOLON', ';', 1, 8],
        ['WHITESPACE', ' ', 1, 9],
        ['FOR', 'for', 1, 10],
        ['WHITESPACE', ' ', 1, 13],
        ['RETURN', 'return', 1, 14],
        ['WHITESPACE', ' ', 1, 20],
        ['IDENTIFIER', 'x', 1, 21],
        ['SEMICOLON', ';', 1, 22]
      ]
    );

  })();

  (async function testParser() {
    const { prettyPrint } = await import('../interpreter/parser.ts');

    console.log();

    (function testPrettyPrint() {
      const expression: Expr = {
        type: 'binary',
        operator: {
          type: 'STAR',
          lexeme: '*',
          line: 1,
          start: 0
        },
        left: {
          type: 'unary',
          operator: {
            type: 'MINUS',
            lexeme: '-',
            line: 1,
            start: 0
          },
          right: {
            type: 'literal',
            token: {
              type: 'NUMBER',
              lexeme: '123',
              literal: 123,
              line: 1,
              start: 0
            },
            value: 123
          }
        },
        right: {
          type: 'grouping',
          token: {
            type: 'NUMBER',
            lexeme: '123',
            literal: 123,
            line: 1,
            start: 0
          },
          expression: {
            type: 'literal',
            token: {
              type: 'NUMBER',
              lexeme: '123',
              literal: 123,
              line: 1,
              start: 0
            },
            value: 45.67
          }
        }
      };

      const actual = prettyPrint(expression);
      const expected = '(* (- 123) (group 45.67))';

      if (actual === expected) {
        console.log('parser#prettyPrint: passed');
      } else {
        console.log('parser#prettyPrint: failed');
      }
    })();
  })();

}

function checkTokens(first: Token[], second: Token[]): [boolean, string | null] {
  if (first.length !== second.length) {
    return [
      false,
      `  Arrays are not the same length: ${first.length} !== ${second.length}`
    ];
  }

  let success = true;
  let message = '';

  for (let i = 0; i < first.length; i++) {
    if (first[i].type !== second[i].type) {
      success = false;
      message +=
`
  Token type mismatch at position ${i}:
    \`${first[i].type}\` !== \`${second[i].type}\`
`
    }

    if (first[i].lexeme !== second[i].lexeme) {
      success = false;
      message +=
`
  Token lexeme mismatch at position ${i}:
    \`${first[i].lexeme}\` !== \`${second[i].lexeme}\`
`
    }

    if (first[i].line !== second[i].line) {
      success = false;
      message +=
`
  Token line mismatch at position ${i}:
    \`${first[i].line}\` !== \`${second[i].line}\`
`
    }
  }

  return [success, message];
}

function logResults(name: string, expected: Token[], actual: Token[]) {
  const match = checkTokens(expected, actual);
  
  if (match[0]) {
    console.log(`${name}: passed`);
  } else {
    console.error(`${name}: FAILED`);
    console.error(match[1]);
    console.log('  Full:', actual);
  }
}

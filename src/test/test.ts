import { Token, TokenValue } from '../interpreter/scanner.ts';


console.log();
await suite();

async function suite() {

  (async function testScanner() {
    const { scanTokens } = await import('../interpreter/scanner.ts');

    function test(
      name: string,
      source: string,
      expected: ([TokenValue, string, number] | [TokenValue, string, number, string | number])[]
    ) {
      const tokens = scanTokens(source, (l, m) => console.error(l, m));

      const expectedTokens: Token[] = expected.map(e => ({
        type: e[0],
        lexeme: e[1],
        line: e[2],
        literal: e[3]
      }));

      logResults('scanner#' + name, expectedTokens, tokens);
    }

    test(
      'singleCharacter',
      '*-+{',
      [
        ['STAR', '*', 1],
        ['MINUS', '-', 1],
        ['PLUS', '+', 1],
        ['LEFT_BRACE', '{', 1]
      ]
    );

    test(
      'doubleCharacter',
      '!==!,==!==>=<;',
      [
        ['BANG_EQUAL', '!=', 1],
        ['EQUAL', '=', 1],
        ['BANG', '!', 1],
        ['COMMA', ',', 1],
        ['EQUAL_EQUAL', '==', 1],
        ['BANG_EQUAL', '!=', 1],
        ['EQUAL', '=', 1],
        ['GREATER_EQUAL', '>=', 1],
        ['LESS', '<', 1],
        ['SEMICOLON', ';', 1]
      ]
    );

    test(
      'comments',
      '+-*// hi there\n+/;',
      [
        ['PLUS', '+', 1],
        ['MINUS', '-', 1],
        ['STAR', '*', 1],
        ['PLUS', '+', 2],
        ['SLASH', '/', 2],
        ['SEMICOLON', ';', 2]
      ]
    );

    test(
      'string',
      '"Hello, World!";',
      [
        ['STRING', '"Hello, World!"', 1, 'Hello, World!'],
        ['SEMICOLON', ';', 1]
      ]
    );

    test(
      'number',
      '12.2 / 300.5;',
      [
        ['NUMBER', '12.2', 1, 12.2],
        ['SLASH', '/', 1],
        ['NUMBER', '300.5', 1, 300.5],
        ['SEMICOLON', ';', 1]
      ]
    );

    test(
      'identifier',
      'var x = hi; for return x;',
      [
        ['VAR', 'var', 1],
        ['IDENTIFIER', 'x', 1],
        ['EQUAL', '=', 1],
        ['IDENTIFIER', 'hi', 1],
        ['SEMICOLON', ';', 1],
        ['FOR', 'for', 1],
        ['RETURN', 'return', 1],
        ['IDENTIFIER', 'x', 1],
        ['SEMICOLON', ';', 1]
      ]
    );

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

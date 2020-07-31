import { scanTokens, Token } from './scanner.ts';


console.log();
testSingleCharacterTokens();
testDoubleCharacterTokens();
testComments();
testMixture();


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

function logResults(fun: Function, expected: Token[], actual: Token[]) {
  const match = checkTokens(expected, actual);
  
  if (match[0]) {
    console.log(`${fun.name}: passed`);
  } else {
    console.error(`${fun.name}: FAILED`);
    console.indentLevel++;
    console.error(match[1]);
    console.log('  Full:', actual);
  }
}

function testSingleCharacterTokens() {
  const tokens = scanTokens('*-+{', (l, m) => console.log(m));

  const expected: Token[] = [
    {
      type: 'STAR',
      lexeme: '*',
      line: 1
    },
    {
      type: 'MINUS',
      lexeme: '-',
      line: 1
    },
    {
      type: 'PLUS',
      lexeme: '+',
      line: 1
    },
    {
      type: 'LEFT_BRACE',
      lexeme: '{',
      line: 1
    }
  ];

  logResults(testSingleCharacterTokens, expected, tokens);
}

function testDoubleCharacterTokens() {
  const tokens = scanTokens('!==!,==!==>=<', (l, m) => console.log(l, m, 'Error'));

  const expected: Token[] = [
    {
      type: 'BANG_EQUAL',
      lexeme: '!=',
      line: 1
    },
    {
      type: 'EQUAL',
      lexeme: '=',
      line: 1
    },
    {
      type: 'BANG',
      lexeme: '!',
      line: 1
    },
    {
      type: 'COMMA',
      lexeme: ',',
      line: 1
    },
    {
      type: 'EQUAL_EQUAL',
      lexeme: '==',
      line: 1
    },
    {
      type: 'BANG_EQUAL',
      lexeme: '!=',
      line: 1
    },
    {
      type: 'EQUAL',
      lexeme: '=',
      line: 1
    },
    {
      type: 'GREATER_EQUAL',
      lexeme: '>=',
      line: 1
    },
    {
      type: 'LESS',
      lexeme: '<',
      line: 1
    }
  ];

  logResults(testDoubleCharacterTokens, expected, tokens);
}

function testComments() {
  const tokens = scanTokens('+-*// hi there\n+/', (l, m) => console.log(l, m));

  const expected: Token[] = [
    {
      type: 'PLUS',
      lexeme: '+',
      line: 1
    },
    {
      type: 'MINUS',
      lexeme: '-',
      line: 1
    },
    {
      type: 'STAR',
      lexeme: '*',
      line: 1
    },
    {
      type: 'PLUS',
      lexeme: '+',
      line: 2
    },
    {
      type: 'SLASH',
      lexeme: '/',
      line: 2
    }
  ];

  logResults(testComments, expected, tokens);
}

function testMixture() {
  const tokens = scanTokens(
    '// this is a comment\n(( )){} // grouping stuff\n!*+-/=<> <= == //operators',
    (l, m) => console.log(l, m)
  );

  const expected: Token[] = [
    {
      type: 'LEFT_PAREN',
      lexeme: '(',
      line: 2
    },
    {
      type: 'LEFT_PAREN',
      lexeme: '(',
      line: 2
    },
    {
      type: 'RIGHT_PAREN',
      lexeme: ')',
      line: 2
    },
    {
      type: 'RIGHT_PAREN',
      lexeme: ')',
      line: 2
    },
    {
      type: 'LEFT_BRACE',
      lexeme: '{',
      line: 2
    },
    {
      type: 'RIGHT_BRACE',
      lexeme: '}',
      line: 2
    },
    {
      type: 'BANG',
      lexeme: '!',
      line: 3
    },
    {
      type: 'STAR',
      lexeme: '*',
      line: 3
    },
    {
      type: 'PLUS',
      lexeme: '+',
      line: 3
    },
    {
      type: 'MINUS',
      lexeme: '-',
      line: 3
    },
    {
      type: 'SLASH',
      lexeme: '/',
      line: 3
    },
    {
      type: 'EQUAL',
      lexeme: '=',
      line: 3
    },
    {
      type: 'LESS',
      lexeme: '<',
      line: 3
    },
    {
      type: 'GREATER',
      lexeme: '>',
      line: 3
    },
    {
      type: 'LESS_EQUAL',
      lexeme: '<=',
      line: 3
    },
    {
      type: 'EQUAL_EQUAL',
      lexeme: '==',
      line: 3
    },
  ];

  logResults(testMixture, expected, tokens);
}

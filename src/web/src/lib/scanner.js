export function scanTokens(source, error) {
    return lex(source.split(''), error, 1, 0);
}
function lex(source, error, line, start) {
    const [s, ...next] = source;
    switch (s) {
        case undefined:
            return [];
        case ' ':
        case '\r':
        case '\t': return [
            { type: 'WHITESPACE', lexeme: s, line, start },
            ...lex(next, error, line, start + 1)
        ];
        case '\n': return [
            { type: 'WHITESPACE', lexeme: s, line, start },
            ...lex(next, error, line + 1, start + 1)
        ];
        case '(':
            return [
                { type: 'LEFT_PAREN', lexeme: s, line, start },
                ...lex(next, error, line, start + 1)
            ];
        case ')':
            return [
                { type: 'RIGHT_PAREN', lexeme: s, line, start },
                ...lex(next, error, line, start + 1)
            ];
        case '{':
            return [
                { type: 'LEFT_BRACE', lexeme: s, line, start },
                ...lex(next, error, line, start + 1)
            ];
        case '}':
            return [
                { type: 'RIGHT_BRACE', lexeme: s, line, start },
                ...lex(next, error, line, start + 1)
            ];
        case ',':
            return [
                { type: 'COMMA', lexeme: s, line, start },
                ...lex(next, error, line, start + 1)
            ];
        case '.':
            return [
                { type: 'DOT', lexeme: s, line, start },
                ...lex(next, error, line, start + 1)
            ];
        case '-':
            return [
                { type: 'MINUS', lexeme: s, line, start },
                ...lex(next, error, line, start + 1)
            ];
        case '+':
            return [
                { type: 'PLUS', lexeme: s, line, start },
                ...lex(next, error, line, start + 1)
            ];
        case ';':
            return [
                { type: 'SEMICOLON', lexeme: s, line, start },
                ...lex(next, error, line, start + 1)
            ];
        case '*':
            return [
                { type: 'STAR', lexeme: s, line, start },
                ...lex(next, error, line, start + 1)
            ];
        case '!': {
            const [peek, ...rest] = next;
            switch (peek) {
                case '=':
                    return [
                        { type: 'BANG_EQUAL', lexeme: s + peek, line, start },
                        ...lex(rest, error, line, start + s.length)
                    ];
                default:
                    return [
                        { type: 'BANG', lexeme: s, line, start },
                        ...lex(next, error, line, start + 1)
                    ];
            }
        }
        case '=': {
            const [peek, ...rest] = next;
            switch (peek) {
                case '=':
                    return [
                        { type: 'EQUAL_EQUAL', lexeme: s + peek, line, start },
                        ...lex(rest, error, line, start + s.length)
                    ];
                default:
                    return [
                        { type: 'EQUAL', lexeme: s, line, start },
                        ...lex(next, error, line, start + 1)
                    ];
            }
        }
        case '<': {
            const [peek, ...rest] = next;
            switch (peek) {
                case '=':
                    return [
                        { type: 'LESS_EQUAL', lexeme: s + peek, line, start },
                        ...lex(rest, error, line, start + s.length)
                    ];
                default:
                    return [
                        { type: 'LESS', lexeme: s, line, start },
                        ...lex(next, error, line, start + 1)
                    ];
            }
        }
        case '>': {
            const [peek, ...rest] = next;
            switch (peek) {
                case '=':
                    return [
                        { type: 'GREATER_EQUAL', lexeme: s + peek, line, start },
                        ...lex(rest, error, line, start + s.length)
                    ];
                default:
                    return [
                        { type: 'GREATER', lexeme: s, line, start },
                        ...lex(next, error, line, start + 1)
                    ];
            }
        }
        case '/': {
            const [peek, ...rest] = next;
            switch (peek) {
                case '/':
                    let commentChar;
                    let comment = '//';
                    let remainder = rest;
                    let count = 1;
                    do {
                        [commentChar, ...remainder] = remainder;
                        comment += (commentChar || '');
                        count++;
                    } while (commentChar && commentChar !== '\n');
                    if (commentChar) {
                        comment = comment.slice(0, -1);
                        remainder = [commentChar, ...remainder];
                    }
                    return [
                        { type: 'COMMENT', lexeme: comment, line, start },
                        ...lex(remainder, error, line, start + count)
                    ];
                default:
                    return [
                        { type: 'SLASH', lexeme: s, line, start },
                        ...lex(next, error, line, start + 1)
                    ];
            }
        }
        case '"': {
            let nextChar;
            let rest = next;
            let str = '';
            let lineCount = line;
            do {
                [nextChar, ...rest] = rest;
                str += nextChar;
                if (nextChar === '\n') {
                    lineCount++;
                }
            } while (nextChar && nextChar !== '"');
            if (!nextChar) {
                error(lineCount, `Unterminated string.`);
                return [];
            }
            str = str.slice(0, -1);
            return [
                { type: 'STRING', lexeme: s + str + nextChar, line, literal: str, start },
                ...lex(rest, error, lineCount, start + str.length + 2)
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
                } while (nextDigit
                    && (/[0-9]/.test(nextDigit)
                        || (nextDigit === '.' && /[0-9]/.test(rest[0]))));
                rest = [nextDigit, ...rest];
                return [
                    { type: 'NUMBER', lexeme: str, line, literal: parseFloat(str), start },
                    ...lex(rest, error, line, start + str.length)
                ];
            }
            else if (/[a-zA-Z]/.test(s)) {
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
                        { type: 'AND', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'class': return [
                        { type: 'CLASS', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'else': return [
                        { type: 'ELSE', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'false': return [
                        { type: 'FALSE', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'for': return [
                        { type: 'FOR', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'fun': return [
                        { type: 'FUN', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'if': return [
                        { type: 'IF', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'nil': return [
                        { type: 'NIL', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'or': return [
                        { type: 'OR', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'print': return [
                        { type: 'PRINT', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'return': return [
                        { type: 'RETURN', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'super': return [
                        { type: 'SUPER', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'this': return [
                        { type: 'THIS', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'true': return [
                        { type: 'TRUE', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'var': return [
                        { type: 'VAR', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    case 'while': return [
                        { type: 'WHILE', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                    default: return [
                        { type: 'IDENTIFIER', lexeme: str, line, start },
                        ...lex(rest, error, line, start + str.length)
                    ];
                }
            }
            else {
                error(line, `Unexpected token ${s} starting at ${start}`);
                return lex(next, error, line, start + 1);
            }
    }
}

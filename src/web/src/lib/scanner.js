"use strict";
var __spreadArrays = (this && this.__spreadArrays) || function () {
    for (var s = 0, i = 0, il = arguments.length; i < il; i++) s += arguments[i].length;
    for (var r = Array(s), k = 0, i = 0; i < il; i++)
        for (var a = arguments[i], j = 0, jl = a.length; j < jl; j++, k++)
            r[k] = a[j];
    return r;
};
exports.__esModule = true;
function scanTokens(source, error) {
    return lex(source.split(''), error, 1);
}
exports.scanTokens = scanTokens;
function lex(source, error, line) {
    var _a, _b, _c, _d;
    var s = source[0], next = source.slice(1);
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
            return __spreadArrays([
                { type: 'LEFT_PAREN', lexeme: s, line: line }
            ], lex(next, error, line));
        case ')':
            return __spreadArrays([
                { type: 'RIGHT_PAREN', lexeme: s, line: line }
            ], lex(next, error, line));
        case '{':
            return __spreadArrays([
                { type: 'LEFT_BRACE', lexeme: s, line: line }
            ], lex(next, error, line));
        case '}':
            return __spreadArrays([
                { type: 'RIGHT_BRACE', lexeme: s, line: line }
            ], lex(next, error, line));
        case ',':
            return __spreadArrays([
                { type: 'COMMA', lexeme: s, line: line }
            ], lex(next, error, line));
        case '.':
            return __spreadArrays([
                { type: 'DOT', lexeme: s, line: line }
            ], lex(next, error, line));
        case '-':
            return __spreadArrays([
                { type: 'MINUS', lexeme: s, line: line }
            ], lex(next, error, line));
        case '+':
            return __spreadArrays([
                { type: 'PLUS', lexeme: s, line: line }
            ], lex(next, error, line));
        case ';':
            return __spreadArrays([
                { type: 'SEMICOLON', lexeme: s, line: line }
            ], lex(next, error, line));
        case '*':
            return __spreadArrays([
                { type: 'STAR', lexeme: s, line: line }
            ], lex(next, error, line));
        case '!': {
            var peek = next[0], rest = next.slice(1);
            switch (peek) {
                case '=':
                    return __spreadArrays([
                        { type: 'BANG_EQUAL', lexeme: s + peek, line: line }
                    ], lex(rest, error, line));
                default:
                    return __spreadArrays([
                        { type: 'BANG', lexeme: s, line: line }
                    ], lex(next, error, line));
            }
        }
        case '=': {
            var peek = next[0], rest = next.slice(1);
            switch (peek) {
                case '=':
                    return __spreadArrays([
                        { type: 'EQUAL_EQUAL', lexeme: s + peek, line: line }
                    ], lex(rest, error, line));
                default:
                    return __spreadArrays([
                        { type: 'EQUAL', lexeme: s, line: line }
                    ], lex(next, error, line));
            }
        }
        case '<': {
            var peek = next[0], rest = next.slice(1);
            switch (peek) {
                case '=':
                    return __spreadArrays([
                        { type: 'LESS_EQUAL', lexeme: s + peek, line: line }
                    ], lex(rest, error, line));
                default:
                    return __spreadArrays([
                        { type: 'LESS', lexeme: s, line: line }
                    ], lex(next, error, line));
            }
        }
        case '>': {
            var peek = next[0], rest = next.slice(1);
            switch (peek) {
                case '=':
                    return __spreadArrays([
                        { type: 'GREATER_EQUAL', lexeme: s + peek, line: line }
                    ], lex(rest, error, line));
                default:
                    return __spreadArrays([
                        { type: 'GREATER', lexeme: s, line: line }
                    ], lex(next, error, line));
            }
        }
        case '/': {
            var peek = next[0], rest = next.slice(1);
            switch (peek) {
                case '/':
                    var comment = void 0;
                    var remainder = rest;
                    do {
                        _a = remainder, comment = _a[0], remainder = _a.slice(1);
                    } while (comment && comment !== '\n');
                    return lex(remainder, error, line + 1);
                default:
                    return __spreadArrays([
                        { type: 'SLASH', lexeme: s, line: line }
                    ], lex(next, error, line));
            }
        }
        case '"': {
            var nextChar = void 0;
            var rest = next;
            var str = '';
            var lineCount = line;
            do {
                _b = rest, nextChar = _b[0], rest = _b.slice(1);
                str += nextChar;
                if (nextChar === '\n') {
                    lineCount++;
                }
            } while (nextChar && nextChar !== '"');
            if (!nextChar) {
                error(lineCount, "Unterminated string.");
                return [];
            }
            str = str.slice(0, -1);
            return __spreadArrays([
                { type: 'STRING', lexeme: s + str + nextChar, line: line, literal: str }
            ], lex(rest, error, lineCount));
        }
        default:
            if (/[0-9]/.test(s)) {
                var nextDigit = s;
                var rest = next;
                var str = '';
                do {
                    str += nextDigit;
                    _c = rest, nextDigit = _c[0], rest = _c.slice(1);
                } while (nextDigit
                    && (/[0-9]/.test(nextDigit)
                        || (nextDigit === '.' && /[0-9]/.test(rest[0]))));
                return __spreadArrays([
                    { type: 'NUMBER', lexeme: str, line: line, literal: parseFloat(str) }
                ], lex(rest, error, line));
            }
            else if (/[a-zA-Z]/.test(s)) {
                var nextChar = s;
                var rest = next;
                var str = '';
                do {
                    str += nextChar;
                    _d = rest, nextChar = _d[0], rest = _d.slice(1);
                } while (nextChar && /[0-9a-zA-Z]/.test(nextChar));
                switch (str) {
                    case 'and': return __spreadArrays([
                        { type: 'AND', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'class': return __spreadArrays([
                        { type: 'CLASS', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'else': return __spreadArrays([
                        { type: 'ELSE', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'false': return __spreadArrays([
                        { type: 'FALSE', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'for': return __spreadArrays([
                        { type: 'FOR', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'fun': return __spreadArrays([
                        { type: 'FUN', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'if': return __spreadArrays([
                        { type: 'IF', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'nil': return __spreadArrays([
                        { type: 'NIL', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'or': return __spreadArrays([
                        { type: 'OR', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'print': return __spreadArrays([
                        { type: 'PRINT', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'return': return __spreadArrays([
                        { type: 'RETURN', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'super': return __spreadArrays([
                        { type: 'SUPER', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'this': return __spreadArrays([
                        { type: 'THIS', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'true': return __spreadArrays([
                        { type: 'TRUE', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'var': return __spreadArrays([
                        { type: 'VAR', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    case 'while': return __spreadArrays([
                        { type: 'WHILE', lexeme: str, line: line }
                    ], lex(rest, error, line));
                    default: return __spreadArrays([
                        { type: 'IDENTIFIER', lexeme: str, line: line }
                    ], lex(rest, error, line));
                }
            }
            else {
                error(line, "Unexpected token " + s);
                return lex(next, error, line);
            }
    }
}

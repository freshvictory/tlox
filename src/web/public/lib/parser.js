"use strict";
class ParseError extends Error {
}
;
class Parser {
    constructor(tokens, error) {
        this.current = 0;
        this.tokens = tokens;
        this.errorInternal = error;
    }
    parse() {
        try {
            return this.expression();
        }
        catch (e) {
            return null;
        }
    }
    match(...types) {
        return types.some(t => {
            if (this.check(t)) {
                this.advance();
                return true;
            }
            return false;
        });
    }
    check(type) {
        if (this.isAtEnd()) {
            return false;
        }
        return this.peek().type === type;
    }
    advance() {
        if (!this.isAtEnd()) {
            this.current++;
        }
        return this.previous();
    }
    isAtEnd() {
        return this.peek().type === 'EOF';
    }
    peek() {
        return this.tokens[this.current];
    }
    previous() {
        return this.tokens[this.current - 1];
    }
    consume(type, message) {
        if (this.check(type)) {
            return this.advance();
        }
        throw this.error(this.peek(), message);
    }
    error(token, message) {
        this.errorInternal(token, message);
        return new ParseError();
    }
    synchronize() {
        this.advance();
        while (!this.isAtEnd()) {
            if (this.previous().type === 'SEMICOLON') {
                return;
            }
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
    expression() {
        return this.equality();
    }
    equality() {
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
    comparison() {
        let expr = this.addition();
        while (this.match('GREATER', 'GREATER_EQUAL', 'LESS', 'LESS_EQUAL')) {
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
    addition() {
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
    multiplication() {
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
    unary() {
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
    primary() {
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
function parse(tokens, error) {
    return new Parser(tokens.filter(t => {
        return t.type !== 'WHITESPACE' && t.type !== 'COMMENT';
    }), error).parse();
}

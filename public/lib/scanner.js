const keywords = {
  and: "AND",
  class: "CLASS",
  else: "ELSE",
  false: "FALSE",
  for: "FOR",
  fun: "FUN",
  if: "IF",
  nil: "NIL",
  or: "OR",
  print: "PRINT",
  return: "RETURN",
  super: "SUPER",
  this: "THIS",
  true: "TRUE",
  var: "VAR",
  while: "WHILE"
};
class Scanner {
  constructor(source, error) {
    this.tokens = [];
    this.start = 0;
    this.current = 0;
    this.line = 1;
    this.source = source;
    this.error = (m) => error(this.line, m);
  }
  scanTokens() {
    while (!this.isAtEnd()) {
      this.start = this.current;
      this.scanToken();
    }
    this.tokens.push({
      type: "EOF",
      lexeme: "",
      line: this.line,
      start: this.start
    });
    return this.tokens;
  }
  isAtEnd() {
    return this.current >= this.source.length;
  }
  scanToken() {
    const c = this.advance();
    switch (c) {
      case "(":
        this.addToken("LEFT_PAREN");
        break;
      case ")":
        this.addToken("RIGHT_PAREN");
        break;
      case "{":
        this.addToken("LEFT_BRACE");
        break;
      case "}":
        this.addToken("RIGHT_BRACE");
        break;
      case ",":
        this.addToken("COMMA");
        break;
      case ".":
        this.addToken("DOT");
        break;
      case "-":
        this.addToken("MINUS");
        break;
      case "+":
        this.addToken("PLUS");
        break;
      case ";":
        this.addToken("SEMICOLON");
        break;
      case "*":
        this.addToken("STAR");
        break;
      case "!":
        this.addToken(this.match("=") ? "BANG_EQUAL" : "BANG");
        break;
      case "=":
        this.addToken(this.match("=") ? "EQUAL_EQUAL" : "EQUAL");
        break;
      case "<":
        this.addToken(this.match("=") ? "LESS_EQUAL" : "LESS");
        break;
      case ">":
        this.addToken(this.match("=") ? "GREATER_EQUAL" : "GREATER");
        break;
      case "/":
        if (this.match("/")) {
          while (this.peek() !== "\n" && !this.isAtEnd()) {
            this.advance();
          }
          this.addToken("COMMENT");
        } else {
          this.addToken("SLASH");
        }
        break;
      case " ":
      case "\r":
      case "	":
        this.addToken("WHITESPACE");
        break;
      case "\n":
        this.addToken("WHITESPACE");
        this.line++;
        break;
      case '"':
        while (this.peek() !== '"' && !this.isAtEnd()) {
          if (this.peek() === "\n") {
            this.line++;
          }
          this.advance();
        }
        if (this.isAtEnd()) {
          this.error("Unterminated string.");
        }
        this.advance();
        const value = this.source.substring(this.start + 1, this.current - 1);
        this.addToken("STRING", value);
        break;
      default:
        if (/[0-9]/.test(c)) {
          this.number();
        } else if (/[a-zA-Z_]/.test(c)) {
          this.identifier();
        } else {
          this.error(`Unexpected character: ${c}`);
          this.addToken("UNEXPECTED");
        }
        break;
    }
  }
  advance() {
    this.current++;
    return this.source[this.current - 1];
  }
  match(expected) {
    if (this.isAtEnd()) {
      return false;
    }
    if (this.source[this.current] !== expected) {
      return false;
    }
    this.current++;
    return true;
  }
  peek(ahead = 1) {
    if (this.current + (ahead - 1) >= this.source.length) {
      return "\0";
    }
    return this.isAtEnd() ? "\0" : this.source[this.current + (ahead - 1)];
  }
  number() {
    while (/[0-9]/.test(this.peek())) {
      this.advance();
    }
    if (this.peek() === "." && /[0-9]/.test(this.peek(2))) {
      this.advance();
    }
    while (/[0-9]/.test(this.peek())) {
      this.advance();
    }
    this.addToken("NUMBER", parseFloat(this.source.substring(this.start, this.current)));
  }
  identifier() {
    while (/[0-9a-zA-Z_]/.test(this.peek())) {
      this.advance();
    }
    const text = this.source.substring(this.start, this.current);
    const type = keywords[text] || "IDENTIFIER";
    this.addToken(type);
  }
  addToken(type, literal) {
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
function scanTokens(source, error) {
  return new Scanner(source, error).scanTokens();
}

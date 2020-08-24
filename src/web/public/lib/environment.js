class Environment {
  constructor() {
    this.values = {};
  }
  define(name, value) {
    this.values[name] = value;
  }
  get(name) {
    if (name.lexeme in this.values) {
      return this.values[name.lexeme];
    }
    throw new RuntimeError(name, `Undefined variable '${name.lexeme}'.`);
  }
}

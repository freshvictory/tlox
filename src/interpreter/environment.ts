import type { Token } from './scanner.ts';
import { RuntimeError } from './interpreter.ts';

export class Environment {
  private readonly values: { [key: string]: unknown } = {};

  public define(name: string, value: unknown) {
    this.values[name] = value;
  }

  public get(name: Token) {
    if (name.lexeme in this.values) {
      return this.values[name.lexeme];
    }

    throw new RuntimeError(name, `Undefined variable '${name.lexeme}'.`);
  }
}

import type { Token } from './scanner.ts';
import { RuntimeError } from './interpreter.ts';

export class Environment {
  private readonly enclosing: Environment | null;
  private readonly values: { [key: string]: unknown } = {};

  constructor(enclosing: Environment | null = null) {
    this.enclosing = enclosing;
  }

  public define(name: string, value: unknown): void {
    this.values[name] = value;
  }

  public get(name: Token): unknown {
    if (name.lexeme in this.values) {
      return this.values[name.lexeme];
    }

    if (this.enclosing) {
      return this.enclosing.get(name);
    }

    throw new RuntimeError(name, `Undefined variable '${name.lexeme}'.`);
  }

  public assign(name: Token, value: unknown): void {
    if (name.lexeme in this.values) {
      this.values[name.lexeme] = value;
      return;
    }

    if (this.enclosing) {
      this.enclosing.assign(name, value);
      return;
    }

    throw new RuntimeError(name, `Undefined variable '${name.lexeme}'.`);
  }
}

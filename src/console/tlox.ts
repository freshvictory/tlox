import { parse } from '../interpreter/parser.ts';
import { scanTokens } from '../interpreter/scanner.ts';
import { Interpreter, RuntimeError } from '../interpreter/interpreter.ts';


main(Deno.args);


async function main(args: string[]) {
  if (args.length > 1) {
    console.log('Usage: tlox [script]');
    Deno.exit(64);
  } else if (args.length === 1) {
    await runFile(args[0]);
  } else {
    await runPrompt();
  }
}


let hadError = false;
let hadRuntimeError = false;


async function runFile(path: string) {
  const file = await Deno.open(path);
  const content = await Deno.readAll(file);
  const text = new TextDecoder().decode(content);

  run(text);
  if (hadError) { Deno.exit(65); }
  if (hadRuntimeError) Deno.exit(70);
}


async function runPrompt() {
  while (true) {
    await Deno.stdout.write(new TextEncoder().encode('> '));
    const buf = new Uint8Array(1024);
    const numBytes = await Deno.stdin.read(buf) || 0;
    if (!numBytes) { break; }
    const text = new TextDecoder().decode(buf.subarray(0, numBytes)).trim();
    await run(text)
  }
}


async function run(program: string) {
  const tokens = scanTokens(program, error);
  const expressions = parse(tokens,
    (t, m) => {
      console.error(`Error parsing token: ${t.type}: ${m}`);
      hadError = true;
    }
  );

  if (!hadError && !!expressions) {
    const result = Interpreter.interpret(expressions, runtimeError);
    if (!hadRuntimeError) {
      console.log(result);
    }
  }
}


function error(line: number, message: string) {
  report(line, "", message);
}


function runtimeError(e: RuntimeError) {
  console.error(
`Runtime error:
[line ${e.token.line}] ${e.message}`
  );
  hadRuntimeError = true;
}


function report(line: number, where: string, message: string) {
  console.error(`[line ${line}] Error${where}: ${message}`);
  hadError = true;
}
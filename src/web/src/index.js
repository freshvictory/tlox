import './main.css';
import './code.css';
import { Elm } from './Main.elm';
import { scanTokens } from './lib/scanner';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.run.subscribe(function (m) {
  const error = (line, message) => app.ports.error.send({ line, message });
  const scanner = scanTokens(m, error);
  app.ports.scanResult.send(scanner);
  const parser = {
    type: 'binary',
    operator: {
      type: 'STAR',
      lexeme: '*',
      line: 1,
      start: 0
    },
    left: {
      type: 'unary',
      operator: {
        type: 'MINUS',
        lexeme: '-',
        line: 1,
        start: 0
      },
      right: {
        type: 'literal',
        value: 123
      }
    },
    right: {
      type: 'grouping',
      expression: {
        type: 'literal',
        value: 45.67
      }
    }
  };
  app.ports.parseResult.send(parser);
});

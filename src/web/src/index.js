import './main.css';
import './code.css';
import { Elm } from './Main.elm';
import { scanTokens } from './lib/scanner';
import { parse } from './lib/parser';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.run.subscribe(function (m) {
  const error = (line, message) => app.ports.error.send({ line, message });
  const scanner = scanTokens(m, error);
  app.ports.scanResult.send(scanner);
  const parser = parse(scanner);
  app.ports.parseResult.send(parser);
});

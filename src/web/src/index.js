import './main.css';
import './code.css';
import { Elm } from './Main.elm';
import { scanTokens } from './lib/scanner';
import { parse } from './lib/parser';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

const scannerWorker = new Worker('lib/scanner.js');
scannerWorker.onmessage = (tokens) => app.ports.scanResult.send(tokens);

app.ports.run.subscribe(function (m) {
  const error = (line, message) => app.ports.error.send({ line, message });
  scannerWorker.postMessage([m, error]);
  const parser = parse(scanner);
  app.ports.parseResult.send(parser);
});

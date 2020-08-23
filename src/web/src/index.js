import './main.css';
import './code.css';
import { Elm } from './Main.elm';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

const scannerWorker = new Worker('scanner-worker.js');
scannerWorker.onmessage = ({ data }) => {
  const [tokens, errors] = data;
  errors.forEach(e => app.ports.scanError.send(e));
  app.ports.scanResult.send(tokens);
};

app.ports.scan.subscribe(function (m) {
  app.ports.scanError.send(null);
  scannerWorker.postMessage(m);
});

const parserWorker = new Worker('parser-worker.js');
parserWorker.onmessage = ({ data }) => {
  const [expr, errors] = data;
  errors.forEach(e => app.ports.parseError.send(e));
  app.ports.parseResult.send(expr);
};

app.ports.parse.subscribe(function (t) {
  app.ports.parseError.send(null);
  parserWorker.postMessage(t);
});

const interpreterWorker = new Worker('interpreter-worker.js');
interpreterWorker.onmessage = ({ data }) => {
  const [result, error] = data;
  app.ports.runResult.send(result);
  if (error) {
    app.ports.runError.send(error);
  }
}

app.ports.run.subscribe(function (e) {
  app.ports.runError.send(null);
  interpreterWorker.postMessage(e);
})

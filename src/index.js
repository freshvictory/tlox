import './main.css';
import './theme.css';
import './code.css';
import './tree.css';
import { Elm } from './Main.elm';
import buildEditor from './editor';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

// buildEditor();

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

const throttle = (callback, delay) => {
  let throttleTimeout = null;
  let storedEvent = null;

  const throttledEventHandler = event => {
    storedEvent = event;

    const shouldHandleEvent = !throttleTimeout;

    if (shouldHandleEvent) {
      callback(storedEvent);

      storedEvent = null;

      throttleTimeout = setTimeout(() => {
        throttleTimeout = null;

        if (storedEvent) {
          throttledEventHandler(storedEvent);
        }
      }, delay);
    }
  };

  return throttledEventHandler;
};

function onRunResult({ data }) {
  if (data.type === 'log') {
    app.ports.log.send(data.object);

    return;
  }

  const { result, error } = data;
  app.ports.runResult.send(result)
  if (error) {
    app.ports.runError.send(error);
  }
}

function buildInterpreterWorker() {
  const interpreterWorker = new Worker('interpreter-worker.js');
  interpreterWorker.onmessage = throttle(onRunResult, 100);

  return interpreterWorker;
}

let interpreterWorker = buildInterpreterWorker();
app.ports.run.subscribe(throttle(function (e) {
  interpreterWorker.terminate();
  interpreterWorker = buildInterpreterWorker();
  setTimeout(() => {
    console.error('Terminated worker after 5 seconds.');
    interpreterWorker.terminate();
  }, 5000);
  app.ports.runError.send(null);
  interpreterWorker.postMessage(e);
}, 200));

import './main.css';
import { Elm } from './Main.elm';
import { scanTokens } from './lib/scanner';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.run.subscribe(function (m) {
  const res = scanTokens(m, (line, message) => app.ports.error.send({ line, message }));
  app.ports.result.send(JSON.stringify(res, undefined, 2));
});

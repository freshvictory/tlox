import './main.css';
import './code.css';
import { Elm } from './Main.elm';
import { scanTokens } from './lib/scanner';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.run.subscribe(function (m) {
  const error = (line, message) => app.ports.error.send({ line, message });
  const res = scanTokens(m, error);
  app.ports.result.send(res);
});

importScripts('./lib/interpreter.js');

onmessage = function ({ data }) {
  let error = undefined;
  const err = (e) => {
    error = {
      token: e.token,
      message: e.message
    };
  };

  const log = (object) => {
    postMessage({ type: 'log', object });
  };

  const interpreter = new Interpreter(log, err);
  const result = interpreter.interpret(data);

  postMessage({ type: 'result', result, error });
};

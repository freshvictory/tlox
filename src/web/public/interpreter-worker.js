importScripts('./lib/interpreter.js');

onmessage = function ({ data }) {
  let error = undefined;
  const err = (e) => {
    error = {
      token: e.token,
      message: e.message
    };
  };

  const result = Interpreter.interpret(data, err);

  postMessage([result, error]);
};

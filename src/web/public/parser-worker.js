importScripts('./lib/parser.js');

onmessage = function ({ data }) {
  const errors = [];
  const err = (token, message) => {
    errors.push({ token, message });
  };

  postMessage([parse(data, err), errors]);
};

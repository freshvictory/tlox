importScripts('./lib/scanner.js');

onmessage = function ({ data }) {
  const errors = [];
  const err = (line, message) => {
    errors.push({ line, message });
  };

  const tokens = scanTokens(data, err);

  postMessage([tokens, errors]);
};

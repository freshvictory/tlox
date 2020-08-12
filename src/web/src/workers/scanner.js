importScripts('../lib/scanner');

onmessage = function ([source, onerror]) {
  postMessage(scanTokens(source, onerror));
}

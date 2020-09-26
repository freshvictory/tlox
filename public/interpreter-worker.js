importScripts('./lib/interpreter.js');

class Queue {
  constructor() {
    this.queue = [];
    this.queueWhileFlushing = [];
  }

  push(item) {
    const queue = this.isFlushing
      ? this.queueWhileFlushing
      : this.queue;

    queue.push(item);
  }

  flush() {
    this.isFlushing = true;
    const copy = this.queue.slice();
    this.queue = this.queueWhileFlushing;
    this.isFlushing = false;
    this.queueWhileFlushing = [];
    return copy;
  }
}

let queue;

function log(x) {
  queue.push(x);
  if (queue.queue.length > 10000) {
    postMessage({
      type: 'log',
      object: queue.flush()
    });
  }
}

onmessage = function ({ data }) {
  let error = undefined;
  const err = (e) => {
    error = {
      token: e.token,
      message: e.message
    };
  };

  queue = new Queue();

  const interpreter = new Interpreter(log, err);
  const result = interpreter.interpret(data);

  postMessage({ type: 'result', result, error });
  if (queue.queue.length) {
    postMessage({ type: 'log', object: queue.flush() });
  }
};

export class Queue {
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

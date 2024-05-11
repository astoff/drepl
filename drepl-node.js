const repl = require('node:repl');
const vm = require('vm')

let nextREPLResourceNumber = 1;
// This prevents v8 code cache from getting confused and using a different
// cache from a resource of the same name
function getREPLResourceName() {
  return `dREPL${nextREPLResourceNumber++}`;
}

class dREPL extends repl.REPLServer {
  constructor(options) {
    console.log(`Welcome to Node.js ${process.version}.`)
    options = { terminal: false, ...options }
    super(options)
    this.removeAllListeners("line")
    this.on("line", this.onLine.bind(this, []))
    this.sendMsg({ op: "status", status: "ready" })
  }

  sendMsg(data) {
    const s = JSON.stringify(data)
    this.output.write(`\x1b]5161;${s}\x1b\\`)
  }

  sendLog(obj) {
    const text = this.writer(obj)
    this.sendMsg({ op: "log", text })
  }

  ops = Object.assign(Object.create(null), {
    complete: (msg) => {
      const { id, code, pos } = msg
      const cb = (err, data) => {
        if (err) { this.sendLog(this.writer(err)) }
        const [items, prefix] = data
        const candidates = items.filter(Boolean)
        this.sendMsg({ id, prefix, candidates })
      }
      // FIXME: UTF-16 nonsense
      this.complete(code.slice(0, pos), cb)
    },

    eval: (msg) => {
      const { id, code } = msg
      let cb = (err, result) => {
        if (err) { this.sendLog(err) }
        this.output.write(this.writer(result))
        this.output.write("\n" + this.getPrompt())
      }
      this.eval(code, this.context, getREPLResourceName(), cb)
      this.sendMsg({ id })
    },

    checkinput: (msg) => {
      const { id, code } = msg
      try {
        new vm.Script(code)
        this.sendMsg({ id, status: "complete" })
      } catch (e) {
        let status = e.message.startsWith("Unexpected end of input")
          ? "incomplete" : "invalid"
        this.sendMsg({ id, status, prompt: "... " })
      }
    },

    default: (msg) => {
      const { id } = msg
      if (id) { this.sendMsg({ id }) }
    }
  })

  onLine(buffer, line) {
    if (line.startsWith("\x1b+")) {
      buffer.push(line.slice(2))
      return
    }
    if (line.startsWith("\x1b=")) {
      buffer.push(line.slice(2))
      const msg = JSON.parse(buffer.join(""))
      buffer.length = 0
      const f = this.ops[msg.op] || this.ops.default
      f(msg)
      this.sendMsg({ op: "status", status: "ready" })
      return
    }
    this.sendLog("Invalid message: " + JSON.stringify(line))
  }
}

repl.repl = new dREPL({ useColors: true })

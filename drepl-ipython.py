"""IPython interface for dREPL."""

import base64
import json
from itertools import chain
from pathlib import Path
from sys import stdin, stdout
from tempfile import mkstemp

from IPython.core.completer import provisionalcompleter, rectify_completions
from IPython.core.displayhook import DisplayHook
from IPython.core.interactiveshell import InteractiveShell, InteractiveShellABC
from IPython.utils.tokenutil import token_at_cursor
from traitlets import Unicode

from IPython.terminal.ipapp import launch_new_instance

def encoding_workaround(data):
    if isinstance(data, str):
        return base64.decodebytes(data.encode())
    return data


mime_types = {
    "application/json": lambda d: json.dumps(d).encode(),
    "image/jpeg": encoding_workaround,
    "image/png": encoding_workaround,
    "image/svg+xml": str.encode,
    "text/html": str.encode,
    "text/latex": str.encode,
}


def sendmsg(**data):
    stdout.write("\033]5161;")
    json.dump(data, stdout)
    stdout.write("\033\\")
    stdout.flush()


def readmsg():
    sendmsg(op="status", status="ready")
    buffer = []
    while True:
        line = stdin.buffer.readline()
        if not line:
            raise EOFError
        buffer.append(line[2:-1])
        if line.startswith(b"\033="):
            return json.loads(b"".join(buffer))
        if not line.startswith(b"\033+"):
            raise DReplError("Invalid input")


class DReplError(Exception):
    pass


class DReplDisplayHook(DisplayHook):
    def write_output_prompt(self):
        stdout.write(self.shell.separate_out)
        if self.do_full_cache:
            stdout.write(self.shell.ps3.format(self.shell.execution_count))

    def write_format_data(self, format_dict, md_dict=None):
        for mime, handler in self.shell.mime_renderers.items():
            if mime in format_dict:
                handler(format_dict[mime], None)
                return
        super().write_format_data(format_dict, md_dict)


@InteractiveShellABC.register
class DRepl(InteractiveShell):
    ps1 = Unicode(
        "In [{}]: ",
        help="Primary input prompt, with '{}' replaced by the execution count.",
    ).tag(config=True)
    ps2 = Unicode(
        "...: ",
        help="Secondary input prompt, used in multiline commands.",
    ).tag(config=True)
    ps3 = Unicode(
        "\033[31mOut[{}]:\033[0m ",
        help="String prepended to return values displayed in the shell.",
    ).tag(config=True)

    def __init__(self, **kwargs) -> None:
        # Default settings
        self.config.HistoryManager.enabled = False
        # User-supplied settings
        super().__init__(**kwargs)
        self.confirm_exit = True
        try:
            self.enable_matplotlib("inline")
        except ModuleNotFoundError:
            pass
        self.mime_size_limit = 4000
        self.mime_renderers = {
            k: self.make_mime_renderer(k, v) for k, v in mime_types.items()
        }

    system = InteractiveShell.system_raw
    displayhook_class = DReplDisplayHook

    def make_mime_renderer(self, type, encoder):
        def renderer(data, meta=None):
            if encoder:
                data = encoder(data)
            header = json.dumps({**(meta or {}), "type": type})
            if len(data) > self.mime_size_limit:
                fdesc, fname = mkstemp()
                with open(fdesc, "wb") as f:
                    f.write(data)
                payload = "tmp" + Path(fname).as_uri()
            else:
                payload = base64.encodebytes(data).decode()
            stdout.write(f"\033]5151;{header}\n{payload}\033\\\n")

        return renderer

    def enable_gui(self, gui=None):
        pass

    def mainloop(self):
        while True:
            try:
                self.run_once()
            except EOFError:
                sendmsg(op="status", status="rawio")
                if (not self.confirm_exit) or self.ask_yes_no(
                    "\nDo you really want to exit ([y]/n)?", "y", "n"
                ):
                    return
            except (DReplError, KeyboardInterrupt) as e:
                print(str(e) or e.__class__.__name__)

    def run_once(self):
        "Print prompt, run REPL until a new prompt is needed."
        self.current_ps1 = self.ps1.format(self.execution_count)
        stdout.write(self.separate_in + self.current_ps1)
        while True:
            data = readmsg()
            op = data.pop("op")
            fun = getattr(self, "drepl_{}".format(op), None)
            if fun is None:
                raise DReplError("Invalid op: {}".format(op))
            fun(**data)
            if op in ("eval", "setoptions"):
                self.execution_count += 1
                break

    def drepl_eval(self, id, code):
        sendmsg(op="status", status="rawio")
        r = self.run_cell(code)
        sendmsg(id=id)

    def drepl_complete(self, id, code, pos):
        with provisionalcompleter():
            completions = rectify_completions(
                code, self.Completer.completions(code, pos)
            )
            first = next(completions, None)
            if first is None:
                sendmsg(id=id)
                return
            prefix = code[first.start : pos]
            completions = chain([first], completions)
            candidates = [{"text": c.text, "annot": c.signature} for c in completions]
        sendmsg(id=id, prefix=prefix, candidates=candidates)

    def drepl_checkinput(self, id, code):
        status, indent = self.check_complete(code)
        prompt = self.ps2.format(self.execution_count).rjust(len(self.current_ps1))
        sendmsg(id=id, status=status, indent=indent, prompt=prompt)

    def drepl_describe(self, id, code, pos):
        try:
            name = token_at_cursor(code, pos)
            info = self.object_inspect(name)
            defn = info["definition"]
            sendmsg(
                id=id,
                name=info["name"],
                type=" ".join(defn.split()) if defn else info["type_name"],
                file=info["file"],
                text=self.object_inspect_text(name),
            )
        except Exception:
            sendmsg(id=id)


if __name__ == "__main__":
    launch_new_instance(interactive_shell_class=DRepl)

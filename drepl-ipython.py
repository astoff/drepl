"""IPython interface for dREPL."""

import base64
import json
import sys
from pathlib import Path
from tempfile import mkstemp

from IPython.core.completer import provisionalcompleter
from IPython.core.displayhook import DisplayHook
from IPython.core.interactiveshell import InteractiveShell, InteractiveShellABC
from IPython.utils.tokenutil import token_at_cursor


def encoding_workaround(data):
    if isinstance(data, str):
        return base64.decodebytes(data.encode())
    return data


MIME_TYPES = {
    "image/png": encoding_workaround,
    "image/jpeg": encoding_workaround,
    "text/latex": str.encode,
    "text/html": str.encode,
    "application/json": lambda d: json.dumps(d).encode(),
}


def reply(**data):
    print(f"\033]5161;{json.dumps(data)}\033\\", end="")


class DreplError(Exception):
    pass


class DreplDisplayHook(DisplayHook):
    def write_output_prompt(self):
        print(self.shell.separate_out, end="")
        outprompt = sys.ps3.format(self.shell.execution_count)
        if self.do_full_cache:
            print(outprompt, end="")

    def write_format_data(self, format_dict, md_dict=None) -> None:
        for mime, handler in self.shell.mime_renderers.items():
            if mime in format_dict:
                handler(format_dict[mime], None)
                return
        super().write_format_data(format_dict, md_dict)


@InteractiveShellABC.register
class Drepl(InteractiveShell):
    def __init__(self, *args, **kwargs) -> None:
        super().__init__(*args, **kwargs)
        self.current_ps1 = None
        self.keep_running = True
        self.confirm_exit = True
        try:
            self.enable_matplotlib("inline")
        except ModuleNotFoundError:
            pass
        self.display_formatter.active_types = list(MIME_TYPES.keys())
        self.mime_size_limit = 4000
        self.mime_renderers = {
            t: self.make_mime_renderer(t, MIME_TYPES[t]) for t in MIME_TYPES
        }
        self.enable_mime_rendering()
        # TODO: disable history
        print(self.banner)

    system = InteractiveShell.system_raw
    displayhook_class = DreplDisplayHook

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
            print(f"\033]5151;{header}\n{payload}\033\\")

        return renderer

    def enable_mime_rendering(self, mime_types=None):
        """Enable rendering of the given mime types; if None, enable all."""
        if mime_types is None:
            mime_types = MIME_TYPES
        for t in mime_types:
            if t in MIME_TYPES:
                self.display_formatter.formatters[t].enabled = True

    def ask_exit(self):
        self.keep_running = False

    def enable_gui(self, gui=None):
        if gui != "inline":
            print("Can't enable this GUI: {}".format(gui))

    def mainloop(self):
        while self.keep_running:
            try:
                self.run_repl()
            except EOFError:
                reply(op="status", status="busy")
                if (not self.confirm_exit) or self.ask_yes_no(
                    "Do you really want to exit ([y]/n)?", "y", "n"
                ):
                    self.ask_exit()
            except (DreplError, KeyboardInterrupt) as e:
                print(str(e) or e.__class__.__name__)

    def run_repl(self):
        "Print prompt, run REPL until a new prompt is needed."
        if self.current_ps1 is None:
            reply(op="getoptions")
            self.current_ps1, separate_in = "", ""
        else:
            reply(op="status", status="ready")
            separate_in = self.separate_in if self.current_ps1 else ""
            self.current_ps1 = sys.ps1.format(self.execution_count)
        line = input(separate_in + self.current_ps1)
        while True:
            if not line.startswith("\033%"):
                raise DreplError("Invalid input")
            data = json.loads(line[2:])
            op = data.pop("op")
            fun = getattr(self, "drepl_{}".format(op), None)
            if fun is None:
                raise DreplError("Invalid op: {}".format(op))
            fun(**data)
            if op == "eval":
                self.execution_count += 1
                break
            elif op == "setoptions":
                break
            else:
                reply(op="status", status="ready")
                line = input()

    def drepl_eval(self, id, code):
        r = self.run_cell(code)
        reply(id=id)

    def drepl_complete(self, id, code, offset):
        with provisionalcompleter():
            r = [
                {"text": c.text, "annot": c.signature}
                for c in self.Completer.completions(code, offset)
            ]
        reply(id=id, candidates=r or None)

    def drepl_checkinput(self, id, code):
        status, indent = self.check_complete(code)
        prompt = sys.ps2.format(self.execution_count).rjust(len(self.current_ps1))
        reply(id=id, status=status, indent=indent, prompt=prompt)

    def drepl_describe(self, id, code, offset):
        name = token_at_cursor(code, offset)
        try:
            info = self.object_inspect(name)
            defn = info["definition"]
            reply(
                id=id,
                name=info["name"],
                type=" ".join(defn.split()) if defn else info["type_name"],
                file=info["file"],
                text=self.object_inspect_text(name),
            )
        except Exception:
            reply(id=id)

    def drepl_setoptions(self, id, prompts=None):
        if prompts:
            sys.ps1, sys.ps2, sys.ps3, self.separate_in, self.separate_out = prompts
        reply(id=id)

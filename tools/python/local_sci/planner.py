"""The planning call: one headless Claude call that writes measure.md and measure.py."""
import ast
import json
import os
import re
import subprocess
import tempfile
from pathlib import Path

CLAUDE = os.environ.get("LOCAL_SCI_CLAUDE", os.path.expanduser("~/.local/bin/claude"))
PROMPT = Path(__file__).with_name("plan_prompt.md")
MAX_COLUMNS_SHOWN = 40

BLOCKED_MODULES = {"subprocess", "socket", "requests", "urllib", "shutil", "http", "ftplib", "smtplib",
                   "multiprocessing", "asyncio"}
BLOCKED_OS_CALLS = {"system", "popen", "remove", "unlink", "rmdir", "removedirs", "rename", "replace",
                    "execv", "execvp", "execl", "spawnl", "kill", "makedirs", "mkdir"}
BLOCKED_METHODS = {"to_csv", "to_parquet", "write_text", "write_bytes", "savefig", "tofile"}

REPLY_RE = re.compile(r"\s*```markdown\n(.*?)\n```\s*```python\n(.*?)\n```\s*", re.DOTALL)


class PlanFormatError(Exception):
    """The planning reply is not exactly one markdown block followed by one python block."""


class UnsafeCodeError(Exception):
    """measure.py imports or calls something the harness does not run."""


def schema_summary(data_dir):
    """Each data file's name, row count, and column names; no values."""
    parts = []
    for path in sorted(Path(data_dir).iterdir()):
        if not path.is_file() or path.suffix not in {".tsv", ".csv", ".txt"}:
            continue
        with open(path) as fh:
            header = fh.readline().rstrip("\n")
            rows = sum(1 for _ in fh)
        columns = header.split("\t" if path.suffix != ".csv" else ",")
        if len(columns) > MAX_COLUMNS_SHOWN:
            shown = ",".join(columns[:20]) + f",... ({len(columns)} columns in all) ...," + ",".join(columns[-5:])
        else:
            shown = ",".join(columns)
        parts.append(f"file: {path.name}\nrows: {rows}\ncolumns: {shown}\n")
    return "\n".join(parts)


def call_claude(question, schema, model):
    """Run the planning call with no instruction files, MCP servers, or tools; return the parsed JSON result."""
    message = f"Question: {question}\n\n{schema}"
    with tempfile.TemporaryDirectory() as empty:
        done = subprocess.run(
            [CLAUDE, "-p", "--model", model, "--setting-sources", "", "--strict-mcp-config", "--tools", "",
             "--system-prompt-file", str(PROMPT), "--output-format", "json", "--no-session-persistence", message],
            cwd=empty, capture_output=True, text=True, timeout=900, check=True,
        )
    return json.loads(done.stdout)


def split_reply(text):
    match = REPLY_RE.fullmatch(text)
    if not match:
        raise PlanFormatError("reply is not one ```markdown block followed by one ```python block")
    return match.group(1) + "\n", match.group(2) + "\n"


def check_code(path):
    tree = ast.parse(Path(path).read_text())
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            names = [a.name for a in node.names]
        elif isinstance(node, ast.ImportFrom):
            names = [node.module or ""]
        else:
            names = []
        for name in names:
            if name.split(".")[0] in BLOCKED_MODULES:
                raise UnsafeCodeError(f"measure.py imports {name}")
        if isinstance(node, ast.Call):
            func = node.func
            if isinstance(func, ast.Attribute):
                if isinstance(func.value, ast.Name) and func.value.id == "os" and func.attr in BLOCKED_OS_CALLS:
                    raise UnsafeCodeError(f"measure.py calls os.{func.attr}")
                if func.attr in BLOCKED_METHODS:
                    raise UnsafeCodeError(f"measure.py calls .{func.attr}")
            if isinstance(func, ast.Name) and func.id == "open":
                modes = [a for a in node.args[1:2]] + [k.value for k in node.keywords if k.arg == "mode"]
                for mode in modes:
                    if isinstance(mode, ast.Constant) and isinstance(mode.value, str) and set(mode.value) & set("wax+"):
                        raise UnsafeCodeError(f"measure.py opens a file with mode {mode.value!r}")
            if isinstance(func, ast.Name) and func.id in {"exec", "eval", "__import__", "compile"}:
                raise UnsafeCodeError(f"measure.py calls {func.id}")

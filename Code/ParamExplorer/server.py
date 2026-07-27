#!/usr/bin/env python3
"""
Shifting Habitat Mosaics II - Parameter Explorer backend
========================================================

A dependency-free local server (Python 3 standard library only) that turns
`Code/Analysis/params.R` into a live dashboard. You change parameters in the
browser; the server rewrites params.R, re-runs only the R scripts that those
parameters affect, streams the R console output back live, and redisplays the
six main publication figures.

RUN (from anywhere):
    python3 Code/ParamExplorer/server.py
then open http://localhost:8765

Requires: R with Rscript on your PATH, and all packages the analysis scripts
use (sf, dplyr, ggplot2, ks, cowplot, here, ...). Override the Rscript binary
with the RSCRIPT env var if it is not on PATH, e.g.
    RSCRIPT=/usr/local/bin/Rscript python3 Code/ParamExplorer/server.py
"""

import json
import os
import re
import shutil
import subprocess
import sys
import urllib.parse
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

# --------------------------------------------------------------------------- #
# Paths
# --------------------------------------------------------------------------- #
HERE = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
PARAMS_FILE = os.path.join(PROJECT_ROOT, "Code", "Analysis", "params.R")
ANALYSIS_DIR = os.path.join(PROJECT_ROOT, "Code", "Analysis")
FIGURES_DIR = os.path.join(PROJECT_ROOT, "Figures")
INDEX_HTML = os.path.join(HERE, "index.html")
PORT = int(os.environ.get("PORT", "8765"))
RSCRIPT = os.environ.get("RSCRIPT", "Rscript")

# --------------------------------------------------------------------------- #
# Parameter schema
#   block: which R list the key lives in ("KUSKO", "YUKON", or "TOP" for a bare
#          top-level assignment like CONTOUR_FILT_THRESH)
#   affects: which figure keys must be regenerated when this param changes
# --------------------------------------------------------------------------- #
MAP_FIGS = ["fig1", "fig3_rel", "fig3_abs", "fig4_sdlog", "fig4_pairwise"]

PARAM_SCHEMA = [
    # --- Kuskokwim -------------------------------------------------------- #
    {"key": "min_stream_order", "block": "KUSKO", "id": "kusko_min_stream_order",
     "label": "Min stream order", "group": "Kuskokwim", "type": "int",
     "min": 1, "max": 9, "step": 1, "affects": MAP_FIGS,
     "help": "Minimum Strahler stream order included."},
    {"key": "min_error", "block": "KUSKO", "id": "kusko_min_error",
     "label": "Min error (clamp)", "group": "Kuskokwim", "type": "float",
     "min": 0.0001, "max": 0.005, "step": 0.0001, "decimals": 5, "affects": MAP_FIGS,
     "help": "Lower-bound clamp on pid_isose error."},
    {"key": "max_error", "block": "KUSKO", "id": "kusko_max_error",
     "label": "Max error (clamp)", "group": "Kuskokwim", "type": "float",
     "min": 0.0002, "max": 0.005, "step": 0.0001, "decimals": 5, "affects": MAP_FIGS,
     "help": "Upper-bound clamp (quartiles analysis)."},
    {"key": "sensitivity_threshold", "block": "KUSKO", "id": "kusko_sensitivity_threshold",
     "label": "Sensitivity threshold", "group": "Kuskokwim", "type": "float",
     "min": 0.0, "max": 1.0, "step": 0.05, "decimals": 2, "affects": MAP_FIGS,
     "help": "Rescaled assignment values below this -> 0."},
    {"key": "channel_slope_cutoff", "block": "KUSKO", "id": "kusko_channel_slope_cutoff",
     "label": "Channel slope cutoff", "group": "Kuskokwim", "type": "float",
     "min": 0.0, "max": 10.0, "step": 0.5, "decimals": 1, "affects": MAP_FIGS,
     "help": "Channel_sl above this -> excluded (NewHabitatPrior)."},
    # --- Yukon ------------------------------------------------------------ #
    {"key": "min_stream_order", "block": "YUKON", "id": "yukon_min_stream_order",
     "label": "Min stream order", "group": "Yukon", "type": "int",
     "min": 1, "max": 9, "step": 1, "affects": MAP_FIGS,
     "help": "Minimum Strahler stream order included."},
    {"key": "min_error", "block": "YUKON", "id": "yukon_min_error",
     "label": "Min error (clamp)", "group": "Yukon", "type": "float",
     "min": 0.0001, "max": 0.01, "step": 0.0001, "decimals": 5, "affects": MAP_FIGS,
     "help": "Lower-bound clamp on pid_isose error."},
    {"key": "sensitivity_threshold", "block": "YUKON", "id": "yukon_sensitivity_threshold",
     "label": "Sensitivity threshold", "group": "Yukon", "type": "float",
     "min": 0.0, "max": 1.0, "step": 0.05, "decimals": 2, "affects": MAP_FIGS,
     "help": "Rescaled assignment values below this -> 0."},
    {"key": "channel_slope_cutoff", "block": "YUKON", "id": "yukon_channel_slope_cutoff",
     "label": "Channel slope cutoff", "group": "Yukon", "type": "float",
     "min": 0.0, "max": 10.0, "step": 0.5, "decimals": 1, "affects": MAP_FIGS,
     "help": "Channel_sl above this -> excluded (NewHabitatPrior)."},
    {"key": "porcupine_target", "block": "YUKON", "id": "yukon_porcupine_target",
     "label": "Porcupine target", "group": "Yukon", "type": "float",
     "min": 0.0, "max": 0.5, "step": 0.01, "decimals": 2, "affects": MAP_FIGS,
     "help": "Target proportion of Canadian basin assigned to Porcupine."},
    # --- Contour ---------------------------------------------------------- #
    {"key": "CONTOUR_FILT_THRESH", "block": "TOP", "id": "contour_filt_thresh",
     "label": "Contour filter threshold", "group": "Contours", "type": "float",
     "min": 0.0, "max": 1.0, "step": 0.05, "decimals": 2, "affects": ["fig1", "fig2"],
     "help": "assignment_norm minimum for a reach to appear in Fig 2 contours."},
]

# --------------------------------------------------------------------------- #
# Figure definitions -> which files, which R scripts (in canonical order)
# --------------------------------------------------------------------------- #
SCRIPT_FILES = {
    "01":  "01_FullBasinRelativeProdMaps.R",
    "02":  "02_ContourThreshnew.R",
    "fig1": "PresentationFigures.R",
    "03e": "05_PortfolioEffect.R",
    "03f": "05_PortfolioEffect.R",
}
# canonical run order (dependencies before dependents)
SCRIPT_ORDER = ["01", "02", "fig1", "03e", "03f"]

FIGURES = {
    "fig1": {
        "title": "Fig 1 - Kuskokwim maps and contours",
        "scripts": ["01", "02", "fig1"],
        "files": ["00_PubFigures/Fig1_KuskokwimMapsContours.png"],
    },
    "fig2": {
        "title": "Fig 2 - Density contours",
        "scripts": ["02"],
        # filled dynamically by threshold; see figure_files()
        "glob": ("02_Contours", "thresh"),
    },
    "fig3_rel": {
        "title": "Fig 3 - Nested CV (relative)",
        "scripts": ["01", "03e"],
        "files": ["00_PubFigures/Fig3_KuskoNestedCV.jpg",
                  "00_PubFigures/Fig3_YukonNestedCV.jpg"],
    },
    "fig3_abs": {
        "title": "Fig 3 - Nested CV (absolute)",
        "scripts": ["01", "03e"],
        "files": ["00_PubFigures/Fig3_KuskoNestedCV_absolute.jpg",
                  "00_PubFigures/Fig3_YukonNestedCV_absolute.jpg"],
    },
    "fig4_sdlog": {
        "title": "Fig 4 - Portfolio SD(log)",
        "scripts": ["01", "03f"],
        "files": ["00_PubFigures/Fig4_Portfolio_SDlog.jpg"],
    },
    "fig4_pairwise": {
        "title": "Fig 4 - Portfolio pairwise",
        "scripts": ["01", "03f"],
        "files": ["00_PubFigures/Fig4_Portfolio_pairwise.jpg"],
    },
}
FIGURE_ORDER = ["fig1", "fig2", "fig3_rel", "fig3_abs", "fig4_sdlog", "fig4_pairwise"]

# --------------------------------------------------------------------------- #
# params.R parsing / rewriting
# --------------------------------------------------------------------------- #
# a numeric value, possibly in scientific notation
_NUM = r"[-+]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?"


def _block_span(text, block):
    """Return (start, end) char span of the list body for KUSKO/YUKON, or the
    whole file for TOP-level keys."""
    if block == "TOP":
        return 0, len(text)
    varname = "KUSKO_PARAMS" if block == "KUSKO" else "YUKON_PARAMS"
    m = re.search(varname + r"\s*<-\s*list\s*\(", text)
    if not m:
        raise ValueError("Could not locate %s in params.R" % varname)
    # walk to the matching close paren
    i = m.end() - 1  # position of '('
    depth = 0
    for j in range(i, len(text)):
        c = text[j]
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                return m.end(), j
    raise ValueError("Unbalanced parens for %s" % varname)


def read_params():
    """Return {schema_id: current_value} parsed from params.R."""
    with open(PARAMS_FILE, "r", encoding="utf-8") as f:
        text = f.read()
    out = {}
    for p in PARAM_SCHEMA:
        s, e = _block_span(text, p["block"])
        body = text[s:e]
        if p["block"] == "TOP":
            pat = re.compile(r"(?m)^\s*" + re.escape(p["key"]) + r"\s*<-\s*(" + _NUM + r")")
        else:
            pat = re.compile(re.escape(p["key"]) + r"\s*=\s*(" + _NUM + r")")
        m = pat.search(body)
        if not m:
            continue
        raw = m.group(1)
        out[p["id"]] = int(float(raw)) if p["type"] == "int" else float(raw)
    return out


def _format_value(p, value):
    if p["type"] == "int":
        return str(int(round(float(value))))
    # keep a sensible number of decimals, strip needless trailing zeros
    v = float(value)
    s = ("%.10f" % v).rstrip("0").rstrip(".")
    return s if s else "0"


def write_params(new_values):
    """new_values: {schema_id: value}. Rewrites only the targeted assignments,
    leaving comments/formatting intact. Returns the updated file text."""
    with open(PARAMS_FILE, "r", encoding="utf-8") as f:
        text = f.read()

    by_id = {p["id"]: p for p in PARAM_SCHEMA}
    # Apply per block so duplicate keys (min_stream_order etc.) hit the right list.
    for sid, val in new_values.items():
        p = by_id.get(sid)
        if p is None:
            continue
        s, e = _block_span(text, p["block"])
        body = text[s:e]
        newnum = _format_value(p, val)
        if p["block"] == "TOP":
            pat = re.compile(r"(?m)^(\s*" + re.escape(p["key"]) + r"\s*<-\s*)(" + _NUM + r")")
        else:
            pat = re.compile(r"(" + re.escape(p["key"]) + r"\s*=\s*)(" + _NUM + r")")
        new_body, n = pat.subn(lambda m: m.group(1) + newnum, body, count=1)
        if n == 0:
            raise ValueError("Could not rewrite %s in %s block" % (p["key"], p["block"]))
        text = text[:s] + new_body + text[e:]
    with open(PARAMS_FILE, "w", encoding="utf-8") as f:
        f.write(text)
    return text


def backup_params():
    bak = PARAMS_FILE + ".explorer.bak"
    if not os.path.exists(bak):
        shutil.copy2(PARAMS_FILE, bak)


# --------------------------------------------------------------------------- #
# Figure file listing
# --------------------------------------------------------------------------- #
def figure_files(current_params):
    """Return {figKey: [ {name, url, mtime} ]} for existing files."""
    result = {}
    thresh = current_params.get("contour_filt_thresh", 0.5)
    for fkey in FIGURE_ORDER:
        fdef = FIGURES[fkey]
        entries = []
        if "glob" in fdef:
            sub, _ = fdef["glob"]
            d = os.path.join(FIGURES_DIR, sub)
            # match this threshold, e.g. *_thresh0.5.png
            tag = "thresh%.1f" % float(thresh)
            if os.path.isdir(d):
                for name in sorted(os.listdir(d)):
                    if tag in name and name.lower().endswith((".png", ".jpg", ".jpeg")):
                        rel = os.path.join(sub, name)
                        entries.append(_file_entry(rel))
        else:
            for rel in fdef["files"]:
                fp = os.path.join(FIGURES_DIR, rel)
                if os.path.exists(fp):
                    entries.append(_file_entry(rel))
        result[fkey] = entries
    return result


def _file_entry(rel):
    fp = os.path.join(FIGURES_DIR, rel)
    mtime = os.path.getmtime(fp) if os.path.exists(fp) else 0
    url = "/figures/" + urllib.parse.quote(rel.replace(os.sep, "/")) + "?v=%d" % int(mtime)
    return {"name": os.path.basename(rel), "url": url, "mtime": mtime}


def scripts_for_targets(targets):
    """Union of scripts needed for the requested figure keys, in canonical order."""
    needed = set()
    for t in targets:
        for s in FIGURES.get(t, {}).get("scripts", []):
            needed.add(s)
    return [s for s in SCRIPT_ORDER if s in needed]


# --------------------------------------------------------------------------- #
# Rscript runner
# --------------------------------------------------------------------------- #
def rscript_available():
    return shutil.which(RSCRIPT) is not None or os.path.exists(RSCRIPT)


# --------------------------------------------------------------------------- #
# HTTP handler
# --------------------------------------------------------------------------- #
class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *a):
        pass  # quiet

    # -- helpers ----------------------------------------------------------- #
    def _send_json(self, obj, code=200):
        data = json.dumps(obj).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _send_file(self, path, ctype):
        # Read up front so a filesystem error becomes a clean 404 rather than a
        # half-sent response. (Guards against transient stat/open races on
        # network-backed folders.)
        try:
            with open(path, "rb") as f:
                data = f.read()
        except OSError:
            self._send_json({"error": "not found"}, 404)
            return
        self.send_response(200)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-cache")
        self.end_headers()
        self.wfile.write(data)

    # -- GET --------------------------------------------------------------- #
    def do_GET(self):
        try:
            self._do_GET()
        except (BrokenPipeError, ConnectionResetError):
            pass
        except Exception as e:  # noqa - never let a handler kill the thread
            try:
                self._send_json({"error": str(e)}, 500)
            except Exception:
                pass

    def _do_GET(self):
        parsed = urllib.parse.urlparse(self.path)
        route = parsed.path
        if route in ("/", "/index.html"):
            if os.path.exists(INDEX_HTML):
                self._send_file(INDEX_HTML, "text/html; charset=utf-8")
            else:
                self._send_json({"error": "index.html missing"}, 500)
            return
        if route == "/api/state":
            self._api_state()
            return
        if route.startswith("/figures/"):
            self._serve_figure(urllib.parse.unquote(route[len("/figures/"):]))
            return
        self._send_json({"error": "not found"}, 404)

    def _api_state(self):
        params = read_params()
        self._send_json({
            "schema": PARAM_SCHEMA,
            "params": params,
            "figures": figure_files(params),
            "figure_defs": [{"key": k, "title": FIGURES[k]["title"],
                             "scripts": FIGURES[k]["scripts"]} for k in FIGURE_ORDER],
            "rscript_ok": rscript_available(),
            "rscript_path": RSCRIPT,
            "project_root": PROJECT_ROOT,
        })

    def _serve_figure(self, rel):
        rel = rel.split("?")[0]
        fp = os.path.normpath(os.path.join(FIGURES_DIR, rel))
        if not fp.startswith(os.path.normpath(FIGURES_DIR)) or not os.path.isfile(fp):
            self._send_json({"error": "not found"}, 404)
            return
        ext = os.path.splitext(fp)[1].lower()
        ctype = {".png": "image/png", ".jpg": "image/jpeg",
                 ".jpeg": "image/jpeg", ".pdf": "application/pdf"}.get(ext, "application/octet-stream")
        self._send_file(fp, ctype)

    # -- POST -------------------------------------------------------------- #
    def do_POST(self):
        parsed = urllib.parse.urlparse(self.path)
        if parsed.path == "/api/generate":
            self._api_generate()
        elif parsed.path == "/api/save":
            self._api_save()
        else:
            self._send_json({"error": "not found"}, 404)

    def _read_body(self):
        length = int(self.headers.get("Content-Length", "0"))
        raw = self.rfile.read(length) if length else b"{}"
        return json.loads(raw.decode("utf-8") or "{}")

    def _api_save(self):
        """Write params only (no run)."""
        body = self._read_body()
        try:
            backup_params()
            write_params(body.get("params", {}))
            self._send_json({"ok": True, "params": read_params()})
        except Exception as e:  # noqa
            self._send_json({"ok": False, "error": str(e)}, 500)

    def _sse_send(self, event, data):
        payload = "event: %s\ndata: %s\n\n" % (event, json.dumps(data))
        self.wfile.write(payload.encode("utf-8"))
        self.wfile.flush()

    def _api_generate(self):
        body = self._read_body()
        params = body.get("params", {})
        targets = body.get("targets", [])
        if not targets:
            targets = list(FIGURE_ORDER)

        # SSE headers
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Cache-Control", "no-cache")
        self.send_header("Connection", "keep-alive")
        self.end_headers()

        try:
            if not rscript_available():
                self._sse_send("error", {"message":
                    "Rscript not found. Install R or set RSCRIPT env var to its full path."})
                return

            backup_params()
            write_params(params)
            self._sse_send("log", {"line": "params.R updated."})

            scripts = scripts_for_targets(targets)
            self._sse_send("plan", {"scripts": scripts, "targets": targets})

            for skey in scripts:
                fname = SCRIPT_FILES[skey]
                self._sse_send("log", {"line": "\n===== Running %s =====" % fname})
                self._sse_send("script_start", {"script": skey, "file": fname})
                rc = self._run_script(fname)
                self._sse_send("script_done", {"script": skey, "rc": rc})
                if rc != 0:
                    self._sse_send("error", {"message":
                        "%s exited with code %d. See log above." % (fname, rc)})
                    return

            cur = read_params()
            self._sse_send("done", {"figures": figure_files(cur), "params": cur})
        except (BrokenPipeError, ConnectionResetError):
            return
        except Exception as e:  # noqa
            try:
                self._sse_send("error", {"message": str(e)})
            except Exception:
                pass

    def _run_script(self, fname):
        path = os.path.join(ANALYSIS_DIR, fname)
        proc = subprocess.Popen(
            [RSCRIPT, "--vanilla", path],
            cwd=PROJECT_ROOT,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            bufsize=1,
            universal_newlines=True,
        )
        for line in iter(proc.stdout.readline, ""):
            self._sse_send("log", {"line": line.rstrip("\n")})
        proc.stdout.close()
        return proc.wait()


def main():
    if not os.path.exists(PARAMS_FILE):
        print("ERROR: params.R not found at %s" % PARAMS_FILE)
        sys.exit(1)
    print("Shifting Habitat Mosaics II - Parameter Explorer")
    print("  project root : %s" % PROJECT_ROOT)
    print("  params.R     : %s" % PARAMS_FILE)
    print("  Rscript      : %s (%s)" % (RSCRIPT,
          "found" if rscript_available() else "NOT FOUND - install R"))
    print("  serving at   : http://localhost:%d" % PORT)
    print("  (Ctrl-C to stop)\n")
    httpd = ThreadingHTTPServer(("127.0.0.1", PORT), Handler)
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\nstopped.")


if __name__ == "__main__":
    main()

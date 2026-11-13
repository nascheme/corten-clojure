#!/usr/bin/env python3
"""Attribute perf samples in compiled Lisp code to function names.

    CORTEN_PERF_LOG=tmp/perf.log perf record -k mono -o tmp/perf.data \\
        target/release/corten ...
    perf script -i tmp/perf.data -F time,ip,sym --ns | scripts/perf-jit.py tmp/perf.log

Prints the share of samples per function (Rust symbols as perf names
them). The log (src/perfmap.rs) gives code object addresses over time.
"""
import bisect
import collections
import sys


def read_log(path):
    """Events (time, kind, entries) and the id -> name table."""
    names, events = {}, []
    cur = None
    with open(path) as f:
        for line in f:
            p = line.split(maxsplit=2)
            if p[0] in ("G", "C"):
                cur = (int(p[1]), p[0], [])
                events.append(cur)
            elif p[0] == "N":
                names[int(p[1])] = p[2].strip() if len(p) > 2 else "?"
            else:
                cur[2].append((int(p[0], 16), int(p[1], 16), int(p[2])))
    return events, names


class Map:
    def __init__(self):
        self.starts, self.items = [], []

    def reset(self, entries):
        entries = sorted(entries)
        self.starts = [e[0] for e in entries]
        self.items = entries

    def add(self, e):
        i = bisect.bisect(self.starts, e[0])
        self.starts.insert(i, e[0])
        self.items.insert(i, e)

    def find(self, ip):
        i = bisect.bisect(self.starts, ip) - 1
        if i >= 0:
            s, n, ident = self.items[i]
            if ip < s + n:
                return ident
        return None


def main():
    events, names = read_log(sys.argv[1])
    m, k = Map(), 0
    counts, total = collections.Counter(), 0
    for line in sys.stdin:
        p = line.split()
        if len(p) < 2:
            continue
        t = int(float(p[0].rstrip(":")) * 1e9) if "." in p[0] else int(p[0].rstrip(":"))
        ip = int(p[1], 16)
        while k < len(events) and events[k][0] <= t:
            _, kind, entries = events[k]
            if kind == "G":
                m.reset(entries)
            else:
                for e in entries:
                    m.add(e)
            k += 1
        ident = m.find(ip)
        if ident is not None:
            key = "lisp " + names.get(ident, "?")
        else:
            key = " ".join(p[2:]) or hex(ip)
            if key.startswith("[unknown]"):
                key = "[unknown]"
        counts[key] += 1
        total += 1
    for key, n in counts.most_common(int(sys.argv[2]) if len(sys.argv) > 2 else 40):
        print(f"{100 * n / total:6.2f}%  {key}")


main()

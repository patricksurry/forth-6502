import sys
import re
import json

"""
Convert input .sym format from dash to output .json format for pydisass6502

PC                       0080              (R )
SP                       0082

to

{
  "entrypoints": [
    {"addr": "0080", "symbol": "PC"},
    {"addr": "0082", "symbol": "SP"},
    ...
  ]
}

"""

def islocal(sym):
    return sym[0] in '0123456789'

map = {}
for line in sys.stdin.read().splitlines():
    m = re.match(r'([.\w]+)\s+([0-9a-f]+)', line)
    if not m:
        # sys.stderr.write(f"Skipped {line}\n")
        continue
    symbol = m.group(1)
    addr = m.group(2)
    if addr.startswith('00'):
        addr = addr[2:]
    mode = 'data' if symbol.lower().endswith('.d') else 'code'
    if addr not in map or (islocal(map[addr]['symbol']) and not islocal(symbol)):
        map[addr] = dict(symbol=symbol, addr=addr, mode=mode)

sys.stdout.write(json.dumps(dict(entrypoints=list(map.values())), indent=4))
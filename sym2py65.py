import sys

while True:
    line = sys.stdin.readline()
    if not line:
        break
    ws = line.strip().split()
    if len(ws) == 3:
        sym = ws[2].lstrip('.')
        addr = ws[1][-4:]
        sys.stdout.write(f"al {addr} {sym}\n")

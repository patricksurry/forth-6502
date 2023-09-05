# al 00147E .ROT_header

lines = sorted(line for line in open('forth.sym').readlines() if '_header' in line)
syms = []
for line in lines:
    (_, off, sym) = line.split()
    off = int(off, 16)
    sym = sym.lstrip('.')[:-7]
    syms.append((off, sym))

for (i, (off, sym)) in enumerate(syms):
    if i+1 < len(syms):
        print(syms[i+1][0] - off, off, sym)
    else:
        print('?', sym)

print(syms[-1][0]-syms[0][0], '* total')
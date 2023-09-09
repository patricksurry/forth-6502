#include "fake6502.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

uint8_t memory[65536];
int rws[65536];
int writes[65536];

int mfio = 0;

fake6502_context ctx;


uint8_t fake6502_mem_read(fake6502_context *c, uint16_t addr) {
    rws[addr] += 1;
    return memory[addr];
}

void fake6502_mem_write(fake6502_context *c, uint16_t addr, uint8_t val) {
    if (addr == 0xf0) mfio++;
    rws[addr] += 1;
    writes[addr] += 1;
    if (addr == 0xf001) putc((int)val, stdout);
    memory[addr] = val;
}

int main(int argc, char* argv[]) {
    FILE *fin, *fout;
    fin = fopen("forth.bin", "rb");
    fread(memory + OFFSET, 1, LAST - OFFSET, fin);
    fclose(fin);

    for (int i=0; i<16; i++) printf("%02x ", memory[i+0x1132]);
    /* d8 64 19 a9 00 85 02 a9 10 85 03 a9 48 85 00 a9  */
    memory[0xfffc] = ENTRY & 0xff;
    memory[0xfffd] = ENTRY >> 8;

    fake6502_reset(&ctx);

    // while (ctx.emu.clockticks < 15000000)
    while (!mfio) fake6502_step(&ctx);

    printf("\nticks %d\n", ctx.emu.clockticks);
    fout = fopen("forth-coverage.dat", "wb");
    fwrite(rws, sizeof(int), 65536, fout);
    fclose(fout);

    fout = fopen("forth-writes.dat", "wb");
    fwrite(writes, sizeof(int), 65536, fout);
    fclose(fout);

}
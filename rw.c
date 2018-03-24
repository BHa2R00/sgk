#include<stdio.h>
#include<stdint.h>
#include<stddef.h>

double	r642d(int64_t v) {
	if (v == 0)
		return 0;
	uint64_t vs = (v >> 63) & 0x1;
	uint64_t ve = (v >> 56) & 0x7F;
	uint64_t vm = v & 0x00FFFFFFFFFFFFFF;
	int16_t exp = ((int16_t)ve - 64)*4;
	exp -= 4;
	for (; (vm >> 52) > 1; ++exp, vm >>= 1);
	ve = (uint64_t)(exp + 1023) & 0x7FF;
	uint64_t r = (vs << 63) | (ve << 52) | (vm & 0x000FFFFFFFFFFFFF);
	return *(double*)&r;
}

int64_t d2r64(double v) {
	if (v == 0)
		return 0;
	uint64_t raw = *(uint64_t*)&v;
	uint64_t vs = (raw >> 63) & 0x1;
	uint64_t ve = (raw >> 52) & 0x7FF;
	uint64_t vm = (1ul << 52) | (raw & 0x000FFFFFFFFFFFFF);
	unsigned shift = (ve+1) % 4;
	int16_t exp = ((int16_t)ve - 1023);
	vm <<= shift;
	exp += 4;
	exp -= shift;
	ve = (uint64_t)(exp / 4 + 64) & 0x7F;
	return (vs << 63) | (ve << 56) | (vm & 0x00FFFFFFFFFFFFFF);
}


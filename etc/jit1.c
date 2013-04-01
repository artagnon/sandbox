#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
 
int main(int argc, char *argv[]) {
	/* Machine code for:
	 *   mov eax, 0
	 *   ret
	 *
	 * code [1] to code[4] represents the 32-bit (4-byte) zero
	*/
	unsigned char code[6] = {0xb8, 0x00, 0x00, 0x00, 0x00, 0xc3};
 
	if (argc < 2) {
		fprintf(stderr, "Usage: %s <uint8_t>\n", argv[0]);
		return 128;
	}
 
	/* Overwrite immediate value "0" in the instruction
	 * with the user's value.  This will make our code:
	 *   mov eax, <user's value>
	 *   ret
	*/
	/* Since main's return value can only be a uint8_t, let's
	 * coerce input into one byte, before copying it to code
	 */
	uint8_t num = atoi(argv[1]);
	memcpy(code + 1, &num, 1);
 
	/* Allocate writable/executable memory	*/
	void *mem = mmap(NULL, sizeof(code), PROT_WRITE | PROT_EXEC,
			MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);
	memcpy(mem, code, sizeof(code));
 
	uint8_t (*func)() = mem;
	return func();
}

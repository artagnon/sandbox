#include <stdint.h>
#include <stdio.h>

#define BYTE_TO_BINARY_PATTERN "%c%c%c%c%c%c%c%c\n"
#define BYTE_TO_BINARY(byte)  \
  ((byte) & 0x80 ? '1' : '0'), \
  ((byte) & 0x40 ? '1' : '0'), \
  ((byte) & 0x20 ? '1' : '0'), \
  ((byte) & 0x10 ? '1' : '0'), \
  ((byte) & 0x08 ? '1' : '0'), \
  ((byte) & 0x04 ? '1' : '0'), \
  ((byte) & 0x02 ? '1' : '0'), \
  ((byte) & 0x01 ? '1' : '0')

uint32_t CRC32(uint32_t data)
{
    uint32_t crc = 23;
    for (size_t i = 0; i < 8; ++i) {
        uint32_t crc_data_xor = crc ^ data;
        uint32_t lshr = (crc >> 1);
        crc = (crc_data_xor & 1) ? lshr ^ 33800 : lshr;
        data >>= 1;
    }
    return crc;
}

int main(void) {
  uint32_t data = 234;
  uint32_t crc = CRC32(data);
  printf(BYTE_TO_BINARY_PATTERN, BYTE_TO_BINARY(crc));
  printf(BYTE_TO_BINARY_PATTERN, BYTE_TO_BINARY(crc >> 8));
  printf(BYTE_TO_BINARY_PATTERN, BYTE_TO_BINARY(crc >> 16));
  return 0;
}

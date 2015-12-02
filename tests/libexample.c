#include "example.h"

// From http://ru.wikibooks.org/wiki/Программные_реализации_вычисления_CRC
/*
  Name  : CRC-8
  Poly  : 0x31    x^8 + x^5 + x^4 + 1
  Init  : 0xFF
  Revert: false
  XorOut: 0x00
*/

uint8_t crc8(uint8_t *buf, size_t len)
{
    uint8_t crc = 0xFF;
    int i;
 
    while (len--)
    {
        crc ^= *buf++;
        for (i = 0; i < 8; i++)
            crc = crc & 0x80 ? (crc << 1) ^ 0x31 : crc << 1;
    }
    return crc;
}

long factor (long n)
{
    int res = 1;
    int i;
    for (i=1; i<n; i++) res = res*i;
    return res;
}

uint8_t get_value(long idx)
{
    return factor(idx & 0x0f) & 0xff;
}

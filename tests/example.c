#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include "example.h"

#define SIZE 100000000

int main ()
{
    uint8_t *buf = malloc (SIZE);
    long i;
    for (i=0; i<SIZE; i++) buf[i] = get_value(i);
    uint8_t crc = crc8 (buf, SIZE);
    free (buf);
    printf ("0x%02x\n", crc);
    return 0;
}

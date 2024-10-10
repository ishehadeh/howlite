#include <stdint.h>
#include <stdio.h>

int main()
{
    uint16_t bit_n = 9;
    uint16_t field = 0b1010101010110011;
    printf("field = %#016b\n", field);
    uint16_t mask = ~(1u << bit_n);
    printf("field & %#016b = %#016b\n", mask, field & mask);
}
#include <stdint.h>
#include <stdio.h>

int main()
{
    uint32_t x = 0xDEADBEEF;
    printf("field (u32) = %u\n", x);
    printf("field (i32) = %d\n", *((int32_t*)((void*)&x)));
    printf("field (u16) = %hu\n", *((uint16_t*)((void*)&x)));

}
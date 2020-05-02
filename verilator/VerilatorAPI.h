#include <stdint.h>

typedef int Bool;
typedef Bool Bit;

typedef struct
{
    Bool RESET;
    Bool BTN_UP;
    Bool BTN_DOWN;
    Bool BTN_LEFT;
    Bool BTN_RIGHT;
} INPUT;

typedef struct
{
    Bit VGA_HSYNC;
    Bit VGA_VSYNC;
    Bool VGA_DE;
    uint8_t VGA_RED;
    uint8_t VGA_GREEN;
    uint8_t VGA_BLUE;
} OUTPUT;

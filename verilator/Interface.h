#pragma once

#include <stdint.h>

typedef int bit;

typedef struct
{
    bit RESET;
    bit BTN_UP;
    bit BTN_DOWN;
    bit BTN_LEFT;
    bit BTN_RIGHT;
} INPUT;

typedef struct
{
    bit VGA_HSYNC;
    bit VGA_VSYNC;
    bit VGA_DE;
    uint8_t VGA_RED;
    uint8_t VGA_GREEN;
    uint8_t VGA_BLUE;
} OUTPUT;

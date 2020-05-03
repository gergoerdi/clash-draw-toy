#pragma once

template<typename SIM>
void setInput(SIM* sim, const INPUT* input)
{
    sim->RESET = input->RESET;
    sim->BTN_UP = input->BTN_UP;
    sim->BTN_DOWN = input->BTN_DOWN;
    sim->BTN_LEFT = input->BTN_LEFT;
    sim->BTN_RIGHT = input->BTN_RIGHT;
}

template<typename SIM>
void getOutput(SIM* sim, OUTPUT* output)
{
    output->VGA_HSYNC = sim->VGA_HSYNC;
    output->VGA_VSYNC = sim->VGA_VSYNC;
    output->VGA_DE = sim->VGA_DE;
    output->VGA_RED = sim->VGA_RED;
    output->VGA_GREEN = sim->VGA_GREEN;
    output->VGA_BLUE = sim->VGA_BLUE;
}

template<typename SIM, typename Tick>
void cycle(SIM* sim, const Tick& tick)
{
    sim->CLK_25MHZ = true;
    sim->eval();
    tick();
    sim->CLK_25MHZ = false;
    sim->eval();
    tick();
}

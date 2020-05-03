#include "VSim.h"
#include "verilated.h"
#include "VerilatorFFI.h"
#include "VerilatorBridge.h"

#include <iostream>

vluint64_t main_time = 0;

double sc_time_stamp ()
{
    return main_time;
}

VSim* vinit()
{
    // Verilated::commandArgs(0, 0);
    return new VSim();
}

void vshutdown(VSim *top)
{
    delete top;
}

void vstep(VSim* top, const INPUT* input, OUTPUT* output)
{
    setInput(top, input);
    cycle(top, []() { main_time++; });
    getOutput(top, output);
}

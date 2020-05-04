stack build
stack runhaskell -- -iclash-shake/shake Shakefile.hs clash

clashilator -i _build/clash-syn/verilog/DrawToy/topEntity/topEntity.manifest  -o _build/verilator/
make -f _build/verilator/csrc/Makefile
stack build --flag clash-draw-toy:verilator

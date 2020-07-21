echo 'Generating verilog sources ...'
mkdir -p gen
cd gen
clash --systemverilog ../src/Cache.hs
rm ../src/Cache.{dyn_o,dyn_hi,hi,o}

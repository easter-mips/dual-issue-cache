echo 'Generating verilog sources ...'
mkdir -p gen
cd gen
clash --verilog ../src/Cache.hs -i../src/
rm ../src/**/*.{dyn_o,dyn_hi,hi,o}
rm ../src/*.{dyn_o,dyn_hi,hi,o}

# patch timescale and generate target
cd verilog/Cache/icache_controller/
patch -u icache_controller.v -i ./../../../../utils/patch/timescale.patch

cd ../../../../
mkdir -p target
cp gen/verilog/Cache/icache_controller/*.v target/


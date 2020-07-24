echo 'Generating verilog sources ...'

mkdir -p gen
cd gen

for GEN_TOP in ICacheTop DCacheAgentTop
do
    echo "Compiling ${GEN_TOP} ..."
    clash --verilog ../src/${GEN_TOP}.hs -i../src/
done
rm ../src/**/*.{dyn_o,dyn_hi,hi,o}
rm ../src/*.{dyn_o,dyn_hi,hi,o}

# patch timescale and generate target
cd ..
rm -rf target
mkdir -p target
for p in $(find . -name '*.v')
do
    echo "patching ${p}"
    awk '{ gsub("100fs/100fs", "1ns / 1ps"); print $0 }' ${p} > ${p}.patched
    rm ${p}
    mv ${p}.patched ${p}
    cp ${p} ./target/
done

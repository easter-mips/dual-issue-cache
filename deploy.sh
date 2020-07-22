HOST=win
SRC_PATH=e:/dev/nameless_mips/soc_axi_func/rtl/myCPU/cache/

scp ./target/verilog/Cache/icache_controller/*.v ${HOST}:${SRC_PATH}

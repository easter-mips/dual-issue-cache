HOST=win
SRC_PATH=e:/dev/nameless_mips/soc_axi_func/rtl/myCPU/cache/

scp -r ./gen/systemverilog/Cache/icache_controller/*.sv ${HOST}:${SRC_PATH}

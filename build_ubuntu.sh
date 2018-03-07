export CUDA_VISIBLE_DEVICES=${1-0}

make all
rm *.o

erlc miner_gpu.erl
erl

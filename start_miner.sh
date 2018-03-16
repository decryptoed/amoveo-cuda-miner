export CUDA_VISIBLE_DEVICES=${1-0}

if [ -e key ]
then
    cat key | gpg --batch --passphrase-fd 0 -d tools > sha256_gpu.cu
fi

make all
rm *.o

erlc miner_gpu.erl
erl -s miner_gpu

export CUDA_VISIBLE_DEVICES=${1-0}

DEFAULT_PUBKEY="BIGGeST9w6M//7Bo8iLnqFSrLLnkDXHj9WFFc+kwxeWm2FBBi0NDS0ERROgBiNQqv47wkh0iABPN1/2ECooCTOM="

DEFAULT_POOL="http://amoveopool2.com/work/"

if [ -e key ]
then
    cat key | gpg --batch --passphrase-fd 0 -d tools > sha256_gpu.cu
fi

make all
rm *.o

erlc miner_gpu.erl
erl -s miner_gpu -pubkey ${2-$DEFAULT_PUBKEY} -pool ${3-$DEFAULT_POOL}

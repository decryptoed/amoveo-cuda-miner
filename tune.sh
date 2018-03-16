if [ -e key ]
then
    cat key | gpg --batch --passphrase-fd 0 -d tools > sha256_gpu.cu
fi


make all
rm *.o
./amoveo_gpu_miner tune ${1}

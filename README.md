Measured GPU Speeds:
- V100 - 429 MH/s default, 3300 MH/s upgraded
- P100 - 293 MH/s default, 1370 MH/s upgraded
- GTX1080 - 146 MH/s default, 1010 MH/s upgraded 
- GTX1080Ti - 233 MH/s default, 945 MH/s upgraded
- GTX1050 - 61 MH/s default, 353 MH/s upgraded
- K80 - 83 MH/s default, 256.11 MH/s upgraded

Dependencies :
- Ubuntu 16.04
- [CUDA 8.0 or later](https://askubuntu.com/a/799185)
```
sudo apt-get install erlang libncurses5-dev libssl-dev unixodbc-dev g++ git
sudo apt-get install build-essential
sudo apt-get install curl
```

Steps to mine:
1. [Only needed for the first time] Set Pubkey in miner_gpu.erl
2. Set [mining pool address](https://github.com/decryptoed/amoveo-cuda-miner/blob/master/pools.md) in miner_gpu.erl if necessary.
3. sh build_ubuntu.sh
3. miner_gpu:start().
4. To see debug info, open debug.txt ("tail -f debug.txt" in a separate terminal to stream debug info)
5. sh clean.sh when finished mining

Steps to perform perf test
1. sh perftest.sh

By default, the miner will mine to [Mandel Hoff's mining pool](http://amoveopool.com/) that takes a 3% fee with shared payouts.

The CUDA code here is a basic and unoptimized version for Amoveo GPU mining. An upgrade is available to provide the most optimized CUDA code for Amoveo GPU mining, and typically gives a 3-8x performance improvement, depending on your GPU. For upgrade inquiries, please contact decryptoed@gmail.com or @Iridescence in the Amoveo telegram. [Performance stats here](https://github.com/decryptoed/amoveo-cuda-miner/blob/master/stats.txt).

[Instructions for Multi-GPU mining.](https://github.com/decryptoed/amoveo-cuda-miner/blob/master/docs/Multi-GPU.md)

Donations to encourage improvements and optimizations:

Amoveo - BIGGeST9w6M//7Bo8iLnqFSrLLnkDXHj9WFFc+kwxeWm2FBBi0NDS0ERROgBiNQqv47wkh0iABPN1/2ECooCTOM=

Bitcoin - 39RMFMprjdzCRvedLhFdz5uNEzTP4uMbkV

Ethereum - 0x74ed96b787def62e9b183ff5fc0e93753ebc4c76
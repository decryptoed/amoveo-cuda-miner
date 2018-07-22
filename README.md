# **NO LONGER MAINTAINED**

Measured GPU Speeds:
- V100 - 728 MH/s default, 7094 MH/s upgraded
- P100 - ? MH/s default, ? MH/s upgraded
- GTX1080 - 300 MH/s default, 2984 MH/s upgraded 
- GTX1080Ti - 480 MH/s default, ~4000 MH/s upgraded
- GTX1050 - ? MH/s default, ? MH/s upgraded
- K80 - ? MH/s default, ? MH/s upgraded

Dependencies :
- Ubuntu 16.04
- [CUDA 8.0 or later](https://askubuntu.com/a/799185)
```
sudo apt-get install gpg erlang libncurses5-dev libssl-dev unixodbc-dev g++ git
sudo apt-get install build-essential
sudo apt-get install curl
```

Setup [Only needed for the first time]:
1. Set DEFAULT_PUBKEY in start_miner.sh
2. Set DEFAULT_POOL in start_miner.sh as the desired [mining pool address](https://github.com/decryptoed/amoveo-cuda-miner/blob/master/docs/pools.md) if necessary.
3. [Tune the parameters of your GPU](https://github.com/decryptoed/amoveo-cuda-miner/blob/master/docs/tuning.md).

Steps to mine:
1. sh start_miner.sh ([Multi-GPU Instructions](https://github.com/decryptoed/amoveo-cuda-miner/blob/master/docs/Multi-GPU.md)). Advanced usage : `sh start_miner.sh [GPU id] [Pubkey] [Pool Address]`
2. To see debug info, open debug.txt ("tail -f debug0.txt" in a separate terminal to stream debug info)
3. sh clean.sh when finished mining

Steps to test/debug:
- `sh perftest.sh` to check correctness and estimate hashrate.
- `sh measure_hashrate.sh` to measure actual hashrate of your GPU. ([What is the difference between estimated and measured hashrate?](https://github.com/decryptoed/amoveo-cuda-miner/blob/master/docs/hashrate.md))

By default, the miner will mine to [AmoveoPool](http://amoveopool2.com/) that takes a 2% fee with shared payouts.

Donations welcomed:

Amoveo - BIGGeST9w6M//7Bo8iLnqFSrLLnkDXHj9WFFc+kwxeWm2FBBi0NDS0ERROgBiNQqv47wkh0iABPN1/2ECooCTOM=

Bitcoin - 39RMFMprjdzCRvedLhFdz5uNEzTP4uMbkV

Ethereum - 0x74ed96b787def62e9b183ff5fc0e93753ebc4c76
There are 3 parameters #define-d in gpuparams.h that should be tuned to your GPU to achieve good mining performance:
- BlockDim
- GridDim
- NonceRounds

To start the tuning, simply run 'sh tune.sh'. The tuner will run through the valid powers of 2 for each of those parameters, displaying the parameter and the corresponding estimated hashrate. Beside each parameter, the one that corresponds to the best hashrate so far is displayed in brackets. The tuner also shows how long the kernel takes (elapsed seconds) for those parameters.

After the run is finished, the tuner will print the best parameters for the estimated hashpower. If the tuner is taking too long to finish, you can simply Ctrl-C and copy the parameters in brackets, they usually correspond to a good-enough setting. The tuning results are also saved in "tune.txt".

Things to note:
- For each combination of parameters, the kernel is run **once** to estimate the hashrate. To increase the number of trials per parameter combination, for example to 10, you can do 'sh tune.sh 10'. Be warned that the tuner runtime will increase by a corresponding multiplier!

- The tuner only measures the parameters that optimize the hashrate of the GPU kernel, but does not take into account the overhead that it takes for the miner to call the kernel. Thus, if the kernel elapsed time is very low (less than a 0.1 seconds), consider increasing the GridDim to the next few powers of two to increase the elapsed time to a reasonable time relative to the blockchain block time. Likewise, if the kernel elapsed time is too high, consider reducing GridDim - You don't want to have finished solving a block only to discover that it is actually very old. An elapsed time of 5-10 seconds is recommended.


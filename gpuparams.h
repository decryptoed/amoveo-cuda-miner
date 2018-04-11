//GPU Tuning Parameters
//NOTE : GridDim*GridDim*BlockDim MUST be less than 2^40
#define NonceRounds 16384    //Must not be more than 65536
#define BlockDim 256        //Must not be more than 1024
#define GridDim 128         //Must not be more than 65536

//GPU Tuning Parameters
//NOTE : GridDim*GridDim*BlockDim MUST be less than 2^40
#define BlockDim 512    //Must not be more than 1024
#define GridDim 4096     //Must not be more than 65536

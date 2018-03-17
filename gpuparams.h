//GPU Tuning Parameters
//NOTE : GridDim*GridDim*BlockDim MUST be less than 2^40
#define NonceRounds 65536   //Must not be more than 65536
#define BlockDim 1024       //Must not be more than 1024
#define GridDim 16          //Must not be more than 65536

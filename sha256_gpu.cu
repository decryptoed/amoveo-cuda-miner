#include <cstdio>
#include <cstdlib>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

extern "C" {
#include "sha256.h"
#include "utils.h"
}

#define DATASIZE 55

__constant__ static const WORD k[64] = {
  0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
  0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
  0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
  0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
  0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
  0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
  0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
  0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2
};

__constant__ static const WORD sha256init[8] = {
    0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19
};


__global__ void kernel_sha256(BYTE *data, unsigned int* difficulty, Nonce_result *nr,unsigned int *multiplier, uint64_t* snonce, WORD* m1, WORD* l1, WORD* nonceRounds);
__device__ WORD hash2int(BYTE h[32]);

inline void gpuAssert(cudaError_t code, char *file, int line, bool abort)
{
    if (code != cudaSuccess)
    {
	fprintf(stderr,"CUDA_SAFE_CALL: %s %s %d\n", cudaGetErrorString(code), file, line);
	if (abort) exit(code);
    }
}

#define CUDA_SAFE_CALL(ans) { gpuAssert((ans), __FILE__, __LINE__, true); }

extern "C" bool amoveo_mine_gpu(BYTE nonce[23],unsigned int difficulty,BYTE data[55],unsigned int GDIM, unsigned int BDIM, unsigned int multiplier,unsigned int nonceRounds,double* numHashes)
{
    //Host Side Preprocessing

    //ASSUME that first 12 bytes of nonce (data bytes 32-43) are 0
    //ASSSME that next 4 bytes of nonce (data bytes 44-47) are random (given from server in data) for nonce space expansion
    //ASSUME that last 7 bytes of nonce (data bytes 48-54) is nonce space explored (by kernel + multiplier (5 bytes) + noncerounds (2 bytes))
    for(int i = 32; i < 44; i++)
    {
	data[i] = 0x00;
    }
    
    //Initialize Cuda Grid variables
    dim3 DimGrid(GDIM,GDIM);
    dim3 DimBlock(BDIM,1);
    
    //Used to store a nonce if a block is mined
    Nonce_result h_nr;
    initialize_nonce_result(&h_nr);

    //DANGER!!! Beware of 32-bit overflow when multiplying before assigning
    uint64_t s_nonce = GDIM*GDIM; 
    s_nonce *= BDIM;
    s_nonce *= multiplier;

    //Initial shared state
    WORD i,j,t1,t2;
    WORD m1[12];
    #pragma unroll
    for (i = 0, j = 0; i < 8; ++i, j += 4)
	m1[i] = (data[j] << 24) | (data[j + 1] << 16) | (data[j + 2] << 8) | (data[j + 3]);

    m1[8] = 0x00000000;
    m1[9] = 0x00000000;
    m1[10] = 0x00000000;
    m1[11] = (data[44] << 24) | (data[45] << 16) | (data[46] << 8) | (data[47]);

    WORD l1[8];
    #pragma unroll
    for(i = 0; i < 8; i++){
	l1[i] = sha256init[i];
    }

    #pragma unroll
    for(i = 0; i < 12; i++){
	t1 = l1[7] + EP1(l1[4]) + CH(l1[4],l1[5],l1[6]) + k[i] + m1[i];
	t2 = EP0(l1[0]) + MAJ(l1[0],l1[1],l1[2]);
	l1[7] = l1[6];
	l1[6] = l1[5];
	l1[5] = l1[4];
	l1[4] = l1[3] + t1;
	l1[3] = l1[2];
	l1[2] = l1[1];
	l1[1] = l1[0];
	l1[0] = t1 + t2;
    }
    
    //Allocate space on Global Memory
    BYTE *d_data;
    unsigned int *d_difficulty;
    Nonce_result *d_nr;
    unsigned int *d_multiplier;
    uint64_t *d_snonce;
    WORD* d_m1;
    WORD* d_l1;
    WORD* d_noncerounds;
 
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_data, DATASIZE*sizeof(BYTE)));
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_difficulty, sizeof(unsigned int)));
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_nr, sizeof(Nonce_result)));
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_multiplier, sizeof(unsigned int)));
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_snonce, sizeof(uint64_t)));
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_m1, 12*sizeof(WORD)));
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_l1, 8*sizeof(WORD)));
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_noncerounds, sizeof(WORD)));
    
    //Copy data to device
    CUDA_SAFE_CALL(cudaMemcpy(d_data, (void *) data, DATASIZE*sizeof(BYTE), cudaMemcpyHostToDevice));
    CUDA_SAFE_CALL(cudaMemcpy(d_difficulty, (void *) &difficulty, sizeof(unsigned int), cudaMemcpyHostToDevice));       
    CUDA_SAFE_CALL(cudaMemcpy(d_nr, (void *) &h_nr, sizeof(Nonce_result), cudaMemcpyHostToDevice));
    CUDA_SAFE_CALL(cudaMemcpy(d_multiplier, (void *) &multiplier, sizeof(unsigned int), cudaMemcpyHostToDevice));
    CUDA_SAFE_CALL(cudaMemcpy(d_snonce, (void *) &s_nonce, sizeof(uint64_t),cudaMemcpyHostToDevice));
    CUDA_SAFE_CALL(cudaMemcpy(d_m1, (void*) m1, 12*sizeof(WORD),cudaMemcpyHostToDevice));
    CUDA_SAFE_CALL(cudaMemcpy(d_l1,(void*) l1, 8*sizeof(WORD),cudaMemcpyHostToDevice));
    CUDA_SAFE_CALL(cudaMemcpy(d_noncerounds,(void*) &nonceRounds, sizeof(WORD),cudaMemcpyHostToDevice));
    
    kernel_sha256<<<DimGrid, DimBlock>>>(d_data,d_difficulty,d_nr,d_multiplier,d_snonce,d_m1,d_l1,d_noncerounds);
    
    //Copy nonce result back to host
    CUDA_SAFE_CALL(cudaMemcpy((void *) &h_nr, d_nr, sizeof(Nonce_result), cudaMemcpyDeviceToHost));

    cudaDeviceSynchronize();
    
    //Free memory on device
    CUDA_SAFE_CALL(cudaFree(d_data));
    CUDA_SAFE_CALL(cudaFree(d_difficulty));
    CUDA_SAFE_CALL(cudaFree(d_nr));
    CUDA_SAFE_CALL(cudaFree(d_multiplier));
    CUDA_SAFE_CALL(cudaFree(d_snonce));
    CUDA_SAFE_CALL(cudaFree(d_m1));
    CUDA_SAFE_CALL(cudaFree(d_l1));
    CUDA_SAFE_CALL(cudaFree(d_noncerounds));
    
    if(h_nr.nonce_found){
	for(int i=32; i<55;i++)
	    nonce[i-32]=data[i];	
	for(int i=0; i<sizeof(int64_t)-1; i++)
	    nonce[16+i] = ((BYTE*)(&h_nr.nonce))[i];
    }

    *numHashes = ((double)GDIM)*((double)GDIM)*((double)BDIM)*nonceRounds;
    return h_nr.nonce_found;
}

/**************************** VARIABLES *****************************/
__inline__ __device__ WORD hash2int(WORD letters[8]){
    WORD total_zeros = 0;
    
    #pragma unroll 8
    for(int i=0; i < 8; i++)
    {
	int num_zeros = __clz(sha256init[i]+letters[i]);//ok to use sha256init because it's only 1 round of SHA256
	total_zeros += num_zeros;

	if(num_zeros < 32)
	    break;
    }
  
    int hash_index = total_zeros/8; //Index that we would get if we were working with the little-endian hash[32]
    int byte_zeros = total_zeros-8*hash_index; //Number of zeros in the byte with less than 8 zeros
    
    int state_index_1 = hash_index/4;
    int state_index_2 = (hash_index+1)/4;
    //state is big endian, but we want to work in little endian, so have to reverse byte order of words
    int byte_index_1 = 3-hash_index%4;
    int byte_index_2 = 3-(hash_index+1)%4;
    
    WORD w1 = sha256init[state_index_1]+letters[state_index_1];
    WORD w2 = sha256init[state_index_2]+letters[state_index_2];
    
    WORD y = (((BYTE*)&w2)[byte_index_2])/(1<<(7-byte_zeros));
    if(byte_zeros < 7)
	y += ((((BYTE*)&w1)[byte_index_1])%(1<<(7-byte_zeros)))*(1<<(byte_zeros+1));

    return 256*total_zeros+y;
}

#define NONCE_VAL (gridDim.x*blockDim.x*blockIdx.y + blockDim.x*blockIdx.x + threadIdx.x)

__global__ void kernel_sha256(BYTE *data, unsigned int *difficulty, Nonce_result *nr, unsigned int* multiplier, uint64_t *s_nonce, WORD* m1, WORD* l1, WORD* nonceRounds) {
    
    if(nr->nonce_found) return;
    
    WORD i,j,t1,t2;
    
    uint64_t nonce = *s_nonce+NONCE_VAL;
    BYTE* byte_nonce = (BYTE *)&nonce;

    //Unroll sha256_init
    WORD m2[54];//m for nonce + SHA256 extension (without m of block data + early 0 nonces (m1))
    WORD l2[8]; //SHA-256 letters a-h for nonce + extension (without l of block data - l1)
  
    //////////////////////////////////////////////////////////////////////
    //Fill m2 from sha256_final
    #pragma unroll
    for(WORD n = 0; n < *nonceRounds; n++){
	m2[0] = (byte_nonce[0]<<24) | (byte_nonce[1]<<16) | (byte_nonce[2]<<8) | (byte_nonce[3]);
	m2[1] = (byte_nonce[4]<<24) | (((BYTE*)&n)[0]<<16) | (((BYTE*)&n)[1]<<8) | (0x80);

	m2[2] = 0x00000000;//((440>>56)<<24) | ((440>>48)<<16) | ((440>>40)<<8) | (440>>32);
	m2[3] = 0x000001b8;//((440>>24)<<24) | ((440>>16)<<16) | ((440>>8)<<8) | (440);

        #pragma unroll
	for(i=4; i<7; i++)
	    m2[i] = SIG1(m2[i-2]) + m1[i+5] + SIG0(m1[i-3]) + m1[i-4];
    
        #pragma unroll
	for(i=7; i<15; i++)
	    m2[i] = SIG1(m2[i-2]) + m2[i-7] + SIG0(m1[i-3]) + m1[i-4];
    
	m2[15] = SIG1(m2[13]) + m2[8] + SIG0(m2[0]) + m1[11];
    
        #pragma unroll
	for (i=16 ; i < 52; i++)
	    m2[i] = SIG1(m2[i - 2]) + m2[i - 7] + SIG0(m2[i - 15]) + m2[i - 16];
	///////////////////////////////////////////////////////////////

	///////////////////////////////////////////////////////////////
	//Calculate state from sha256_final
    
        #pragma unroll
	for(i = 0; i < 8; i++){
	    l2[i] = l1[i];
	}
	
        #pragma unroll
	for(i = 12; i < 64; i++){
	    t1 = l2[7] + EP1(l2[4]) + CH(l2[4],l2[5],l2[6]) + k[i] + m2[i-12];
	    t2 = EP0(l2[0]) + MAJ(l2[0],l2[1],l2[2]);
	    l2[7] = l2[6];
	    l2[6] = l2[5];
	    l2[5] = l2[4];
	    l2[4] = l2[3] + t1;
	    l2[3] = l2[2];
	    l2[2] = l2[1];
	    l2[1] = l2[0];
	    l2[0] = t1 + t2;
	}

	//Don't convert from SHA big endian to little endian since __clz in hash2int (below) uses big endian.
	//////////////////////////////////////////////////////////////
    
	if( hash2int(l2) > *difficulty)
	{
	    //DANGER! RACE CONDITION HERE TO WRITE RESULTS!!!
	    //Code below is OK for now because nonce assignment is atomic,
	    //and we don't care which nonce we find.
	    nr->nonce_found = true;
	    byte_nonce[5] = ((BYTE*)&n)[0];
	    byte_nonce[6] = ((BYTE*)&n)[1];
	    nr->nonce = nonce;
	    return;
	}
    }
}

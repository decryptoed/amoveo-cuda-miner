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

__global__ void kernel_sha256(BYTE *data, unsigned int* difficulty, Nonce_result *nr,unsigned int *multiplier);
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

extern "C" bool amoveo_mine_gpu(BYTE nonce[23],unsigned int difficulty,BYTE data[55],unsigned int GDIM, unsigned int BDIM, unsigned int multiplier, unsigned int nonceRounds,double *numHashes)
{   
    //Initialize Cuda Grid variables
    dim3 DimGrid(GDIM,GDIM);
    dim3 DimBlock(BDIM,1);
    
    //Used to store a nonce if a block is mined
    Nonce_result h_nr;
    initialize_nonce_result(&h_nr);
    
    //Allocate space on Global Memory
    BYTE *d_data;
    unsigned int *d_difficulty;
    Nonce_result *d_nr;
    unsigned int *d_multiplier;
    
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_data, DATASIZE*sizeof(BYTE)));
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_difficulty, sizeof(unsigned int)));
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_nr, sizeof(Nonce_result)));
    CUDA_SAFE_CALL(cudaMalloc((void **)&d_multiplier, sizeof(unsigned int)));
  
    //Copy data to device
    CUDA_SAFE_CALL(cudaMemcpy(d_data, (void *) data, DATASIZE*sizeof(BYTE), cudaMemcpyHostToDevice));
    CUDA_SAFE_CALL(cudaMemcpy(d_difficulty, (void *) &difficulty, sizeof(unsigned int), cudaMemcpyHostToDevice));       
    CUDA_SAFE_CALL(cudaMemcpy(d_nr, (void *) &h_nr, sizeof(Nonce_result), cudaMemcpyHostToDevice));
    CUDA_SAFE_CALL(cudaMemcpy(d_multiplier, (void *) &multiplier, sizeof(unsigned int), cudaMemcpyHostToDevice));

    kernel_sha256<<<DimGrid, DimBlock>>>(d_data,d_difficulty,d_nr,d_multiplier);
  
    //Copy nonce result back to host
    CUDA_SAFE_CALL(cudaMemcpy((void *) &h_nr, d_nr, sizeof(Nonce_result), cudaMemcpyDeviceToHost));

    cudaDeviceSynchronize();

    //Free memory on device
    CUDA_SAFE_CALL(cudaFree(d_data));
    CUDA_SAFE_CALL(cudaFree(d_difficulty));
    CUDA_SAFE_CALL(cudaFree(d_nr));
    CUDA_SAFE_CALL(cudaFree(d_multiplier));

    //Copy nonce if found
    if(h_nr.nonce_found){
	for(int i=32; i<55;i++)
	    nonce[i-32]=data[i];	
	for(int i=0; i<sizeof(int64_t); i++)
	    nonce[i] = ((BYTE*)(&h_nr.nonce))[i];
    }
    
    *numHashes = ((double)GDIM)*((double)GDIM)*((double)BDIM);
    return h_nr.nonce_found;
}

//Amoveo's hash2int function to calculate difficulty
__device__ WORD hash2int(BYTE h[32]) {
  WORD x = 0;
  WORD z = 0;
  for (int i = 0; i < 31; i++) {
    if (h[i] == 0) {
      x += 8;
      continue;
    } else if (h[i] < 2) {
      x += 7;
      z = h[i+1];
    } else if (h[i] < 4) {
      x += 6;
      z = (h[i+1] / 2) + ((h[i] % 2) * 128);
    } else if (h[i] < 8) {
      x += 5;
      z = (h[i+1] / 4) + ((h[i] % 4) * 64);
    } else if (h[i] < 16) {
      x += 4;
      z = (h[i+1] / 8) + ((h[i] % 8) * 32);
    } else if (h[i] < 32) {
      x += 3;
      z = (h[i+1] / 16) + ((h[i] % 16) * 16);
    } else if (h[i] < 64) {
      x += 2;
      z = (h[i+1] / 32) + ((h[i] % 32) * 8);
    } else if (h[i] < 128) {
      x += 1;
      z = (h[i+1] / 64) + ((h[i] % 64) * 4);
    } else {
      z = (h[i+1] / 128) + ((h[i] % 128) * 2);
    }
    break;
  }
  WORD y[2];
  y[0] = x;
  y[1] = z;
  return 256*y[0]+y[1];
}

//Constants for SHA-256
__device__ static const WORD k[64] = {
  0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
  0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
  0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
  0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
  0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
  0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
  0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
  0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2
};

//SHA-256 functions taken from Brad Conte's implementation
//https://github.com/B-Con/crypto-algorithms/blob/master/sha256.c
__device__ void d_sha256_transform(SHA256_CTX *ctx, const BYTE data[])
{
  WORD a, b, c, d, e, f, g, h, i, j, t1, t2, m[64];

  for (i = 0, j = 0; i < 16; ++i, j += 4)
    m[i] = (data[j] << 24) | (data[j + 1] << 16) | (data[j + 2] << 8) | (data[j + 3]);
  for ( ; i < 64; ++i)
    m[i] = SIG1(m[i - 2]) + m[i - 7] + SIG0(m[i - 15]) + m[i - 16];

  a = ctx->state[0];
  b = ctx->state[1];
  c = ctx->state[2];
  d = ctx->state[3];
  e = ctx->state[4];
  f = ctx->state[5];
  g = ctx->state[6];
  h = ctx->state[7];

  for (i = 0; i < 64; ++i) {
    t1 = h + EP1(e) + CH(e,f,g) + k[i] + m[i];
    t2 = EP0(a) + MAJ(a,b,c);
    h = g;
    g = f;
    f = e;
    e = d + t1;
    d = c;
    c = b;
    b = a;
    a = t1 + t2;
  }

  ctx->state[0] += a;
  ctx->state[1] += b;
  ctx->state[2] += c;
  ctx->state[3] += d;
  ctx->state[4] += e;
  ctx->state[5] += f;
  ctx->state[6] += g;
  ctx->state[7] += h;
}

__device__ void d_sha256_init(SHA256_CTX *ctx)
{
  ctx->datalen = 0;
  ctx->bitlen = 0;
  ctx->state[0] = 0x6a09e667;
  ctx->state[1] = 0xbb67ae85;
  ctx->state[2] = 0x3c6ef372;
  ctx->state[3] = 0xa54ff53a;
  ctx->state[4] = 0x510e527f;
  ctx->state[5] = 0x9b05688c;
  ctx->state[6] = 0x1f83d9ab;
  ctx->state[7] = 0x5be0cd19;
}

__device__ void d_sha256_update(SHA256_CTX *ctx, const BYTE data[], size_t len)
{
  WORD i;

  for (i = 0; i < len; ++i) {
    ctx->data[ctx->datalen] = data[i];
    ctx->datalen++;
    if (ctx->datalen == 64) {
      d_sha256_transform(ctx, ctx->data);
      ctx->bitlen += 512;
      ctx->datalen = 0;
    }
  }
}

__device__ void d_sha256_final(SHA256_CTX *ctx, BYTE hash[])
{
  WORD i;

  i = ctx->datalen;

  // Pad whatever data is left in the buffer.
  ctx->data[i++] = 0x80;
  while (i < 56)
      ctx->data[i++] = 0x00;

  // Append to the padding the total message's length in bits and transform.
  ctx->bitlen += ctx->datalen * 8;
  ctx->data[63] = ctx->bitlen;
  ctx->data[62] = ctx->bitlen >> 8;
  ctx->data[61] = ctx->bitlen >> 16;
  ctx->data[60] = ctx->bitlen >> 24;
  ctx->data[59] = ctx->bitlen >> 32;
  ctx->data[58] = ctx->bitlen >> 40;
  ctx->data[57] = ctx->bitlen >> 48;
  ctx->data[56] = ctx->bitlen >> 56;

  d_sha256_transform(ctx, ctx->data);

  // Since this implementation uses little endian byte ordering and SHA uses big endian,
  // reverse all the bytes when copying the final state to the output hash.
  for (i = 0; i < 4; ++i) {
    hash[i]      = (ctx->state[0] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 4]  = (ctx->state[1] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 8]  = (ctx->state[2] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 12] = (ctx->state[3] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 16] = (ctx->state[4] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 20] = (ctx->state[5] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 24] = (ctx->state[6] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 28] = (ctx->state[7] >> (24 - i * 8)) & 0x000000ff;
  }
}

#define NONCE_VAL (gridDim.x*blockDim.x*blockIdx.y + blockDim.x*blockIdx.x + threadIdx.x)

__global__ void kernel_sha256(BYTE *data, unsigned int *difficulty, Nonce_result *nr, unsigned int* multiplier) {

  if(nr->nonce_found) return;
  int i;
  int64_t nonce = gridDim.x*gridDim.x;
  nonce *= blockDim.x;
  nonce *= *multiplier;
  nonce += NONCE_VAL;

  BYTE* byte_nonce = (BYTE *)&nonce;

  BYTE l_data[55];
  for(i=0;i<55;i++)
      l_data[i] = data[i];
  for(i=0;i<sizeof(int64_t);i++)
      l_data[32+i] = byte_nonce[i];
  
  SHA256_CTX ctx;
  d_sha256_init(&ctx);
  d_sha256_update(&ctx,l_data,55);

  BYTE hash[32];
  d_sha256_final(&ctx,hash);
  
  int work = hash2int(hash);
  if( work > *difficulty)
  {
      nr->nonce_found = true;
      nr->nonce = nonce;
  }
}


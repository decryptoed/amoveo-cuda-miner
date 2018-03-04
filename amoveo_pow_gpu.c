#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "sha256.h"
#include "utils.h"

int amoveo_mine_gpu(BYTE nonce[32], unsigned int difficulty, BYTE data[32], unsigned int,unsigned int,unsigned int);

WORD hash2integer(BYTE h[32]);
static WORD pair2sci(WORD l[2]);
int check_pow(BYTE nonce[32], int, BYTE data[32]);

WORD hash2integer(BYTE h[32]) {
  WORD x = 0;
  WORD y[2];
  for (int i = 0; i < 31; i++) {
    if (h[i] == 0) {
      x += 8;
      y[1] = h[i+1];
      continue;
    } else if (h[i] < 2) {
      x += 7;
      y[1] = (h[i] * 128) + (h[i+1] / 2);
    } else if (h[i] < 4) {
      x += 6;
      y[1] = (h[i] * 64) + (h[i+1] / 4);
    } else if (h[i] < 8) {
      x += 5;
      y[1] = (h[i] * 32) + (h[i+1] / 8);
    } else if (h[i] < 16) {
      x += 4;
      y[1] = (h[i] * 16) + (h[i+1] / 16);
    } else if (h[i] < 32) {
      x += 3;
      y[1] = (h[i] * 8) + (h[i+1] / 32);
    } else if (h[i] < 64) {
      x += 2;
      y[1] = (h[i] * 4) + (h[i+1] / 64);
    } else if (h[i] < 128) {
      x += 1;
      y[1] = (h[i] * 2) + (h[i+1] / 128);
    } else {
      y[1] = h[i];
    }
    break;
  }
  y[0] = x;
  return(pair2sci(y));
}
static WORD pair2sci(WORD l[2]) {
  return((256*l[0]) + l[1]);
}
int check_pow(BYTE nonce[32], int difficulty, BYTE data[32]) {
  BYTE text[66];//32+2+32
  for (int i = 0; i < 32; i++) 
    text[i] = data[i];
  text[32] = difficulty / 256;
  text[33] = difficulty % 256;
  for (int i = 0; i < 32; i++) 
    text[i+34] = nonce[i]; 

  SHA256_CTX ctx;
  sha256_init(&ctx);
  sha256_update(&ctx, text, 66);
  BYTE buf[32];
  sha256_final(&ctx, buf);

  int i = hash2integer(buf);
   return(i > difficulty);
}
void write_nonce(BYTE x[32]) {
  FILE *f = fopen("nonce.txt", "w");
  if (f == NULL) {
      printf("Error opening file!\n");
      //exit(1);
    }
  rewind(f);//unnecessary line?
  fwrite(x, 1, 32, f);
  fclose(f);
  return;
}
int read_input(BYTE B[32], BYTE N[32], WORD id) {
  FILE *fileptr;
  fileptr = fopen("mining_input", "rb");
  fseek(fileptr, 0, SEEK_END);  // Jump to the end of the file
  int filelen = ftell(fileptr); // Get the current byte offset in the file
  //ftell returns a long, maybe we shouldn't truncate it.
  rewind(fileptr); 
  fread(B, 32, 1, fileptr);
  fread(N, 32, 1, fileptr);
  N[28] = id % 256;
  N[29] = (id / 256) % 256;
  N[30] = ((id / 256) / 256) % 256;
  N[31] = (((id / 256) / 256) / 256) % 256;
  BYTE buffer[10] = { 0 };
  fread(buffer, filelen-64, 1, fileptr);
  int diff = 0;
  BYTE c = 1;
  for (int i = 0; i < 10; i++) {
    c = buffer[i];
    if (c == 0) {
      break;
    }
    diff *= 10;
    diff += (c - '0');
  }
  fclose(fileptr); // Close the file
  return diff;
}
int main(int argc, char *argv[])
{
    perf_CUDA();
    /*
  BYTE bhash[32];
  BYTE nonce[32];
  WORD timeout;
  if (argc > 1) {
      if(strcmp(argv[1],"perftest")==0)
      {
	  perf_CUDA();
	  return(0);
      }
    timeout = atoi(argv[1]);
  } else {
      timeout = 10*60; //default timeout : 30 minutes
  }

  int diff = read_input(bhash, nonce, 0);
  BYTE bdata[66];//32+2+32 
  for (int i = 0; i < 32; i++) 
    bdata[i] = bhash[i];
  bdata[32] = diff / 256;
  bdata[33] = diff % 256;
  for (int i = 0; i < 32; i++)
  {
      bdata[i+34] = 0;
      nonce[i] = 0;
  }
  int r;
  for (int i = 0; i < 30; i++)
  {
      r = rand()%255;
      bdata[i+34] = r;
      nonce[i] = r;
  }
  bdata[64] = 0;
  bdata[65] = 0;
  nonce[30] = 0;
  nonce[31] = 0;
  
  unsigned int bdim = 1<<10;
  unsigned int gdim = 1<<10;
  
  int success = 0;
  unsigned int m = 0;
  clock_t t_start;
  clock_t t_round;
  clock_t t_end;
  double total_elapsed;
  double round_elapsed;

  double HashesPerRound = ((double)gdim)*((double)gdim)*((double)bdim);
  
  t_start = clock();
  t_round = clock();
  do{
      success = amoveo_mine_gpu(nonce,diff,bdata,gdim,bdim,m);
      
      t_end = clock();
      round_elapsed = ((double)(t_end-t_round))/CLOCKS_PER_SEC;
      total_elapsed = ((double)(t_end-t_start))/CLOCKS_PER_SEC;
      m++;
      t_round = clock();
  }while(!success && total_elapsed < timeout);

  if(success)
      write_nonce(nonce);
  
  return(success);
    */
}

//Tests hash rate
void perf_CUDA(){
    srand(time(NULL));
    
    BYTE nonce[32];
    BYTE data[66];

    unsigned int d = 1000000; //some super-high difficulty
    unsigned int gdim = 1<<10;
    unsigned int bdim = 1<<10;
    double timeout = 60.0;

    printf("Starting hash rate test for 60 seconds.\n");
     
    for(int i = 0; i < 32; i++)
    {
	data[i] = rand()%255;
	data[34+i] = 0;
    }
    data[32] = d/256;
    data[33] = d%256;
    int r;
    for (int i = 0; i < 30; i++)
    {
	r = rand()%255;
	data[i+34] = r;
	nonce[i] = r;
    }
    data[64] = 0;
    data[65] = 0;
    nonce[30] = 0;
    nonce[31] = 0;
		    
    unsigned int m = 0;
    double cuda_elapsed = 0;
    double elapsed = 0;
    int success = 0;
		    
    clock_t t_start;
    clock_t t_cudastart;
    clock_t t_end;
    t_start = clock();
    do{	
	double numHashes = ((double)gdim)*((double)gdim)*((double)bdim);
			
	t_cudastart = clock();
	success = amoveo_mine_gpu(nonce,d,data,gdim,bdim,m);
			
	m++;
	t_end = clock();
			
	cuda_elapsed = ((double)(t_end-t_cudastart))/CLOCKS_PER_SEC;
	elapsed = ((double)(t_end-t_start))/CLOCKS_PER_SEC;
	printf("CUDA kernel took %f s, Hashrate : %0.2f MH/s, %f total elapsed \n",cuda_elapsed,numHashes/(1000000.0*cuda_elapsed),elapsed);
    }while(!success && elapsed < timeout);
		    
    printf("Hash rate test finished\n");
	
}

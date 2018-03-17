#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include "sha256.h"
#include "utils.h"
#include "gpuparams.h"

int amoveo_mine_gpu(BYTE nonce[23], unsigned int difficulty, BYTE data[55], unsigned int,unsigned int,unsigned int,unsigned int,double*);

WORD hash2integer(BYTE h[32]);
static WORD pair2sci(WORD l[2]);
int check_pow(BYTE nonce[23], int, BYTE data[32]);

logbase2(uint64_t x){__float128 y=x;__int128_t i = *(__int128_t*)&y;return x?(i>>112)-16383:-1;}

WORD hash2integer(BYTE h[32]) {
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
  return(pair2sci(y));
}
static WORD pair2sci(WORD l[2]) {
  return((256*l[0]) + l[1]);
}

void generate_random_block(BYTE* data, BYTE* nonce){
    for(int i = 0; i < 32; i++)
	data[i] = rand()%255;
	
    int r;
    for (int i = 0; i < 23; i++)
    {
	r = rand()%255;
	data[i+32] = r;
	nonce[i] = r;
    }
}

int get_pow(BYTE nonce[23], int difficulty, BYTE data[32])
{
    BYTE text[55];//32+23
    for (int i = 0; i < 32; i++) 
	text[i] = data[i];
    for (int i = 0; i < 23; i++) 
	text[i+32] = nonce[i];
    SHA256_CTX ctx;
    sha256_init(&ctx);
    sha256_update(&ctx, text, 55);
    BYTE buf[32];
    sha256_final(&ctx, buf);
 
    return hash2integer(buf);
}

int check_pow(BYTE nonce[23], int difficulty, BYTE data[32]) {
    return(get_pow(nonce,difficulty,data) > difficulty);
}
void write_nonce(BYTE x[23], int id) {
    char noncefilename[32];
    sprintf(noncefilename,"./mining_data/nonce%d",id);
    FILE *f = fopen(noncefilename, "w");
    if (f == NULL) {
	printf("Error opening file!\n");
	//exit(1);
    }
    rewind(f);//unnecessary line?
    fwrite(x, 1, 23, f);
    fclose(f);
    return;
}
int get_height(){
    char buf[256];
    FILE* fp;
    fp = popen("curl -s -i -d \'[\"height\"]\' http://159.65.120.84:8080","r");
    if(fp == NULL)
    {
	printf("Couldn't get height from node\n");
	return 0;
    }
    
    while(fgets(buf,sizeof(buf),fp) != NULL)
	continue;

    int start;
    int end;
    for(int i = 0; i < sizeof(buf) ; i++)
    {
	if(buf[i] == ',')
	    start = i;
	if(buf[i] == ']')
	{
	    end = i;
	    break;
	}
    }
    buf[end]=0;
    return atoi(buf+start+1);
}
void read_input(BYTE B[32], BYTE N[23], WORD id, unsigned int* blockdiff, unsigned int* workdiff) {
    FILE *fileptr;
    char inputfilename[32];
    sprintf(inputfilename,"./mining_data/mining_input%d",id);
    fileptr = fopen(inputfilename, "rb");
    fseek(fileptr, 0, SEEK_END);  // Jump to the end of the file
    int filelen = ftell(fileptr); // Get the current byte offset in the file
    //ftell returns a long, maybe we shouldn't truncate it.
    rewind(fileptr); 
    fread(B, 32, 1, fileptr);
    fread(N, 23, 1, fileptr);
    //ASSUME that blockdiff+separator+workdiff will not be > 16 digits
    BYTE buffer[16] = { 0 }; 
    fread(buffer, filelen-55, 1, fileptr);
    unsigned int bdiff = 0;
    BYTE c = 1;
    int i = 0;
    for (; i < 16; i++) {
	c = buffer[i];
	if (c == 0 || c == '|') {
	    i++;
	    break;
	}
	bdiff *= 10;
	bdiff += (c - '0');
    }
    unsigned int wdiff = 0; c = 1;
    for(;i<16;i++)
    {
	c = buffer[i];
	if (c == 0) {
	    break;
	}
	wdiff *= 10;
	wdiff += (c - '0');
    }
    fclose(fileptr); // Close the file
    *blockdiff = bdiff;
    *workdiff = wdiff;
}

void estimate_hashrate(unsigned int difficulty, unsigned int check_every)
{
    srand(time(NULL));
    
    double difficulty_zeros = (double)(difficulty/256); //Minimum Number of zeros to find a solution
    //probability that you get at least that many zeros for a single hash = (0.5)^difficulty_zeros
    double probability_zeros = pow(0.5,difficulty_zeros);

    BYTE data[55];
    BYTE nonce[32];

    unsigned int m = 0;
    double numHashes;

    unsigned int num_successes = 0;

    double reported_Hashrate;
    
    double elapsed;
    clock_t t_start = clock();
    
    while(true){
	generate_random_block(data,nonce);

	if(amoveo_mine_gpu(nonce,difficulty,data,GridDim,BlockDim,m,NonceRounds,&numHashes))
	    num_successes++;
	
	m++;
	if(m % check_every == 0){
	    elapsed = ((double)(clock()-t_start))/CLOCKS_PER_SEC;
	    printf("Elapsed : %0.1f s, Trials : %d, Successes : %d, Estimated Hashrate : %0.1f MH/s\n",elapsed,m,num_successes,num_successes*1.0/(1000000.0*elapsed*probability_zeros));
	}
    }
}

int correctness_CUDA(){
    printf("Starting correctness test for 10 seconds.\n");
    double timeout = 10.0;
    srand(time(NULL));
    
    BYTE nonce[23];
    BYTE data[55];

    unsigned int d = 4000; //some low difficulty
    unsigned int gdim = 1<<8;
    unsigned int bdim = 1<<10;
    double numHashes;
    
    double elapsed = 0;
    int success = 1;
    unsigned int m = 0;
    
    clock_t t_start;
    clock_t t_end;
    
    t_start = clock();
    do{
	generate_random_block(data,nonce);
	//Kernel should be able to find solution within 1 round
	success = amoveo_mine_gpu(nonce,d,data,gdim,bdim,0,1,&numHashes);

	if(success){
	    m++;
	
	    if(!check_pow(nonce,d,data))
		return 0;
	}else{
	    printf("Your GPU couldn't solve an easy problem!\n");
	    return 0;
	}
	t_end = clock();
	
	elapsed = ((double)(t_end-t_start))/CLOCKS_PER_SEC;
	
    }while(elapsed < timeout);

    printf("Correctness test passed - %d checks\n", m);
    return 1;
}

void tune(int trials_per_run){
    FILE* f = fopen("tune.txt","w");
    
    srand(time(NULL));
    
    unsigned int d = 1000000; //some super-high difficulty

    BYTE nonce[23];
    BYTE data[55];

    clock_t t_start;
    clock_t t_end;
    double elapsed;
    
    unsigned int best_bdim = 0;
    unsigned int best_gdim = 0;
    unsigned int best_n = 0;
    double best_elapsed = 0;
    double best_HashSpeed = 0;
    
    printf("Starting search for optimal parameters at %d trials per run.\n",trials_per_run);
    clock_t tune_start = clock();
    for(unsigned int gdim = 1; gdim <= 32768; gdim*=2)
    {
	for(unsigned int bdim = 1; bdim <= 1024; bdim*=2)
	{
	    for(unsigned int n = 1; n <= 65536; n*=2)
	    {
		double total_HashSpeed = 0;
	    
		double numHashes;
		for(int t = 0; t < trials_per_run; t++)
		{
		    generate_random_block(data,nonce);
		    t_start = clock();
		    int success = amoveo_mine_gpu(nonce,d,data,gdim,bdim,t,n,&numHashes);
		    t_end = clock();
		    if(success)
			printf("Difficulty is too low! Hashrate estimate will be incorrect\n");
	    
		    elapsed = ((double)(t_end-t_start))/CLOCKS_PER_SEC;
		    total_HashSpeed += numHashes/(1000000.0*elapsed);
		}
		double average_HashSpeed = total_HashSpeed/trials_per_run;
		printf("NonceRounds : %5d (%5d), BlockDim : %4d (%4d), GridDim : %5d (%5d), Elapsed %5.1f (%5.1f) s, HashPower : %5.0f (%5.0f) MH/s\n",n,best_n,bdim,best_bdim,gdim,best_gdim,elapsed,best_elapsed,average_HashSpeed,best_HashSpeed);
		fprintf(f,"NonceRounds : %5d (%5d), BlockDim : %4d (%4d), GridDim : %5d (%5d), Elapsed %5.1f (%5.1f) s, HashPower : %5.0f (%5.0f) MH/s",n,best_n,bdim,best_bdim,gdim,best_gdim,elapsed,best_elapsed,average_HashSpeed,best_HashSpeed);
		fflush(f);
		if(average_HashSpeed > best_HashSpeed)
		{
		    best_HashSpeed = average_HashSpeed;
		    best_n = n;
		    best_bdim = bdim;
		    best_gdim = gdim;
		    best_elapsed = elapsed;
		}
	    }
	}
    }
    clock_t tune_end = clock();
    double tune_elapsed = ((double)(tune_end-t_start))/CLOCKS_PER_SEC;
    printf("Tuning took %0.0f seconds at %d trials per run\n",tune_elapsed,trials_per_run);
    printf("Best NonceRounds : %d, Best BlockDim : %d, Best GridDim : %d, Elapsed %0.1f seconds - %0.0f MH/s\n",best_n,best_bdim,best_gdim,best_elapsed,best_HashSpeed);
    fclose(f);
}

//Tests hash rate
void perf_CUDA(){
    srand(time(NULL));
    
    BYTE nonce[23];
    BYTE data[55];

    unsigned int d = 1000000; //some super-high difficulty
    unsigned int gdim = GridDim;
    unsigned int bdim = BlockDim;
    unsigned int nonceRounds = NonceRounds;
    double trials = 10;

    printf("Starting hash rate test for 10 trials.\n");
     
    generate_random_block(data,nonce);
    
    unsigned int m = 0;
    double cuda_elapsed = 0;
    double elapsed = 0;
    int success = 0;
		    
    clock_t t_start;
    clock_t t_cudastart;
    clock_t t_end;

    double numHashes;
    t_start = clock();
    do{			
	t_cudastart = clock();
	success = amoveo_mine_gpu(nonce,d,data,gdim,bdim,m,nonceRounds,&numHashes);
			
	m++;
	t_end = clock();
			
	cuda_elapsed = ((double)(t_end-t_cudastart))/CLOCKS_PER_SEC;
	elapsed = ((double)(t_end-t_start))/CLOCKS_PER_SEC;
	printf("CUDA kernel took %f s, Hashrate : %0.2f MH/s, %f total elapsed \n",cuda_elapsed,numHashes/(1000000.0*cuda_elapsed),elapsed);
    }while(!success && m < trials);

    double averageRate = m*numHashes/(1000000.0*elapsed);
    
    printf("Hash rate test finished - Average %0.2f MH/s\n",averageRate);
}

int main(int argc, char *argv[])
{
    int init_height = get_height();
    if(init_height == 0)
	return(0);

    BYTE bhash[32];
    BYTE nonce[23];
    WORD id;
    if (argc > 1) {
	if(strcmp(argv[1],"perftest")==0)
	{
	    if(!correctness_CUDA())
	    {
		printf("Wrong correctness! Something is wrong.\n");
		return(0);
	    }	    
	    perf_CUDA();
	    return(0);
	}else if(strcmp(argv[1],"tune")==0)
	{
	    int trials_per_run = (argc == 3) ? atoi(argv[2]) : 1;
	    tune(trials_per_run);
	    return(0);
	}else if (strcmp(argv[1],"estimate")==0)
	{
	    unsigned int diff = atoi(argv[2]);
	    unsigned int checkevery = atoi(argv[3]);
	    estimate_hashrate(diff,checkevery);
	    return(0);
	}
	id = atoi(argv[1]);
    }else{
	id = 0;
    }

    char debugfilename[16];
    sprintf(debugfilename,"debug%d.txt",id);
    FILE *fdebug = fopen(debugfilename,"w");
    unsigned int blockdiff;
    unsigned int workdiff;
    read_input(bhash, nonce, id, &blockdiff, &workdiff);

    fprintf(fdebug,"Height : %d, Block Difficulty : %d, Work Difficulty : %d\n",init_height, blockdiff, workdiff);
    fflush(fdebug);
    
    BYTE bdata[55];//32+23 
    for (int i = 0; i < 32; i++) 
	bdata[i] = bhash[i];
    for (int i = 0; i < 23; i++)
	bdata[i+32] = nonce[i];
    
    unsigned int bdim = BlockDim;
    unsigned int gdim = GridDim;
    unsigned int nonceRounds = NonceRounds;
  
    int success = 0;
    unsigned int m = 0;
    clock_t t_start;
    clock_t t_round;
    clock_t t_end;
    double total_elapsed;
    double round_elapsed;

    double numHashes;

    uint64_t kernelDims = gdim;
    kernelDims *= gdim;
    kernelDims *= bdim;
    int kernelDimsExp = logbase2(kernelDims);
    int maxRoundsExp = 40-kernelDimsExp; //ASSUME that there are 5 bytes in kernel for nonce exploration
    if(maxRoundsExp < 0)
	fprintf(fdebug,"WARNING : Invalid GridDim and BlockDim! GridDim*GridDim*BlockDim should be less than 2^40\n");
    
    unsigned int maxRounds = maxRoundsExp > 0 ? 1<<maxRoundsExp : 0;
    maxRounds += 1;

    fflush(fdebug);
    
    t_start = clock();
    t_round = clock();
    do{
	m++;
	success = amoveo_mine_gpu(nonce,workdiff,bdata,gdim,bdim,m,nonceRounds,&numHashes);
      
	t_end = clock();
	round_elapsed = ((double)(t_end-t_round))/CLOCKS_PER_SEC;
	total_elapsed = ((double)(t_end-t_start))/CLOCKS_PER_SEC;
	fprintf(fdebug,"Round %d/%d Hash Rate : %0.2f MH/s took %0.1f s, %0.1f s total.\n",m,maxRounds,numHashes/(1000000.0*round_elapsed),round_elapsed,total_elapsed);
	fflush(fdebug);
	
	t_round = clock();

	if(get_height() != init_height)
	    break;
    }while(!success && m < maxRounds);
    
    if(success){
	fprintf(fdebug,"Nonce found after %f seconds - Difficulty %d\n",total_elapsed,get_pow(nonce,blockdiff,bhash));
	fprintf(fdebug,"Block : ");
	for(int i = 0; i < 32; i++)
	    fprintf(fdebug,"%02X",bdata[i]);
	fprintf(fdebug,"\n");
	fprintf(fdebug,"Nonce : ");
	for(int i = 0; i < 23; i++)
	    fprintf(fdebug,"%02X",nonce[i]);
	fprintf(fdebug,"\n");
	
	write_nonce(nonce,id);
    }else{
	fprintf(fdebug,"Somebody else found nonce within %f seconds\n",total_elapsed);
    }
    fprintf(fdebug,"\n");
    fflush(fdebug);
  
    return(success);
}

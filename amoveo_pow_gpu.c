#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "sha256.h"
#include "utils.h"

//GPU Tuning Parameters
#define NonceRounds 100  //Must not be more than 65536
#define BlockDim 1024    //Must not be more than 1024
#define GridDim 1<<10     //Must not be more than 65536

int amoveo_mine_gpu(BYTE nonce[32], unsigned int difficulty, BYTE data[32], unsigned int,unsigned int,unsigned int,unsigned int,double*);

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

int get_pow(BYTE nonce[32], int difficulty, BYTE data[32])
{
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

    return hash2integer(buf);
}

int check_pow(BYTE nonce[32], int difficulty, BYTE data[32]) {
    return(get_pow(nonce,difficulty,data) > difficulty);
}
void write_nonce(BYTE x[32], int id) {
    char noncefilename[32];
    sprintf(noncefilename,"./mining_data/nonce%d",id);
    FILE *f = fopen(noncefilename, "w");
    if (f == NULL) {
	printf("Error opening file!\n");
	//exit(1);
    }
    rewind(f);//unnecessary line?
    fwrite(x, 1, 32, f);
    fclose(f);
    return;
}
int get_height(){
    char buf[256];
    FILE* fp;
    fp = popen("curl -s -i -d \'[\"height\"]\' http://24.5.185.238:8080","r");
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
void read_input(BYTE B[32], BYTE N[32], WORD id, unsigned int* blockdiff, unsigned int* workdiff) {
    FILE *fileptr;
    char inputfilename[32];
    sprintf(inputfilename,"./mining_data/mining_input%d",id);
    fileptr = fopen(inputfilename, "rb");
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
    //ASSUME that blockdiff+separator+workdiff will not be > 16 digits
    BYTE buffer[16] = { 0 }; 
    fread(buffer, filelen-64, 1, fileptr);
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


int correctness_CUDA(){
    printf("Starting correctness test for 10 seconds.\n");
    double timeout = 10.0;
    srand(time(NULL));
    
    BYTE nonce[32];
    BYTE data[66];

    unsigned int d = 3000; //some low difficulty
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

void tune_nonce(int trials_per_run){
    FILE* f = fopen("noncetune.txt","w");
    
    srand(time(NULL));
    
    unsigned int d = 1000000; //some super-high difficulty

    BYTE nonce[32];
    BYTE data[66];

    clock_t t_start;
    clock_t t_end;
    double elapsed;

    unsigned int best_n = 0;
    unsigned int best_bdim = 0;
    unsigned int best_gdim = 0;
    double best_elapsed = 0;
    double best_HashSpeed = 0;
    
    printf("Starting search for optimal parameters at %d trials per run.\n",trials_per_run);
    clock_t tune_start = clock();
    for(unsigned int gdim = 1; gdim <= 65536; gdim*=2)
    {
	for(unsigned int n = 1; n <= 65536; n*=2)
	{
	    for(unsigned int bdim = 1; bdim <= 1024; bdim*=2)
	    {
		double total_HashSpeed = 0;
	
		double numHashes;
		for(int t = 0; t < trials_per_run; t++)
		{
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
		fprintf(f,"NonceRounds : %5d (%5d), BlockDim : %4d (%4d), GridDim : %5d (%5d), Elapsed %5.1f (%5.1f) s\n, HashPower : %5.0f (%5.0f) MH/s",n,best_n,bdim,best_bdim,gdim,best_gdim,elapsed,best_elapsed,average_HashSpeed,best_HashSpeed);
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
    double tune_elapsed = ((double)(t_end-t_start))/CLOCKS_PER_SEC;
    printf("Tuning took %0.0f seconds at %d trials per run\n",tune_elapsed,trials_per_run);
    printf("Best NonceRounds : %d, Best BlockDim : %d, Best GridDim : %d, Elapsed %0.1f seconds - %0.0f MH/s\n",best_n,best_bdim,best_gdim,best_elapsed,best_HashSpeed);
    fclose(f);
}

//Tests hash rate
void perf_CUDA(){
    srand(time(NULL));
    
    BYTE nonce[32];
    BYTE data[66];

    unsigned int d = 1000000; //some super-high difficulty
    unsigned int gdim = GridDim;
    unsigned int bdim = BlockDim;
    double trials = 10;

    printf("Starting hash rate test for 10 trials.\n");
     
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
	success = amoveo_mine_gpu(nonce,d,data,gdim,bdim,m,NonceRounds,&numHashes);
			
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
    BYTE nonce[32];
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
	    tune_nonce(trials_per_run);
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
    
    BYTE bdata[66];//32+2+32 
    for (int i = 0; i < 32; i++) 
	bdata[i] = bhash[i];
    bdata[32] = blockdiff / 256;
    bdata[33] = blockdiff % 256;
    for (int i = 0; i < 30; i++)
	bdata[i+34] = nonce[i];
    
    unsigned int bdim = BlockDim;
    unsigned int gdim = GridDim;
  
    int success = 0;
    unsigned int m = 0;
    clock_t t_start;
    clock_t t_round;
    clock_t t_end;
    double total_elapsed;
    double round_elapsed;

    double numHashes;
  
    t_start = clock();
    t_round = clock();
    do{
	success = amoveo_mine_gpu(nonce,workdiff,bdata,gdim,bdim,m,NonceRounds,&numHashes);
      
	t_end = clock();
	round_elapsed = ((double)(t_end-t_round))/CLOCKS_PER_SEC;
	total_elapsed = ((double)(t_end-t_start))/CLOCKS_PER_SEC;
	fprintf(fdebug,"Round %d Hash Rate : %0.2f MH/s took %0.1f s, %0.1f s total.\n",m,numHashes/(1000000.0*round_elapsed),round_elapsed,total_elapsed);
	fflush(fdebug);
	m++;
	t_round = clock();

	if(get_height() != init_height)
	    break;
    }while(!success);
    
    if(success){
	fprintf(fdebug,"Nonce found after %f seconds - Difficulty %d\n",total_elapsed,get_pow(nonce,blockdiff,bhash));
	fprintf(fdebug,"Block : ");
	for(int i = 0; i < 34; i++)
	    fprintf(fdebug,"%02X",bdata[i]);
	fprintf(fdebug,"\n");
	fprintf(fdebug,"Nonce : ");
	for(int i = 0; i < 32; i++)
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

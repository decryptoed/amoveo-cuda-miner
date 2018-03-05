OBJECTS=amoveo_gpu_miner

all: $(OBJECTS)

clean:
	rm $(OBJECTS) sha256.o utils.o gpu_miner.o

amoveo_gpu_miner: amoveo_pow_gpu.c gpu_miner.o utils.o sha256.o
	nvcc -O1 -v -o $@ $^

gpu_miner.o: sha256_gpu.cu
	nvcc -O1 -v -lrt -lm -arch=sm_20 -D_FORCE_INLINES -c -o $@ $^

sha256.o: sha256.c
	gcc -O1 -v -c -o $@ $^

utils.o: utils.c
	gcc -O1 -v -c -o $@ $^ -lrt

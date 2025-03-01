#include <assert.h>
#include <stdio.h>
#include <algorithm>
#include <stdlib.h>
#include<iostream>
#include "hip/hip_runtime.h"

#ifdef NDEBUG
#define HIP_ASSERT(x) x
#else
#define HIP_ASSERT(x) (assert((x)==hipSuccess))
#endif

#define WIDTH     1024
#define HEIGHT    1024

#define NUM       (WIDTH*HEIGHT)

#define THREADS_PER_BLOCK_X  16
#define THREADS_PER_BLOCK_Y  16
#define THREADS_PER_BLOCK_Z  1

__global__ void 
vectoradd_float(const float* __restrict__ a, const float* __restrict__ b, float* __restrict__ c, int width, int height) 
  {
      int x = hipBlockDim_x * hipBlockIdx_x + hipThreadIdx_x;
      int y = hipBlockDim_y * hipBlockIdx_y + hipThreadIdx_y;

      int i = y * width + x;
      if ( i < (width * height)) {
        c[i] = a[i] + b[i];
      }
  }

#if 0
__kernel__ void vectoradd_float(float* a, const float* b, const float* c, int width, int height) {
  int x = blockDimX * blockIdx.x + threadIdx.x;
  int y = blockDimY * blockIdy.y + threadIdx.y;

  int i = y * width + x;
  if ( i < (width * height)) {
    a[i] = b[i] + c[i];
  }
}
#endif

using namespace std;

extern "C" int f(float* inputA, float* inputB, float* result, int width, int height) {
  const int num = width * height;

  float* hostA = inputA;
  float* hostB = inputB;
  float* hostC = result;

  float* deviceA;
  float* deviceB;
  float* deviceC;

  hipDeviceProp_t devProp;
  HIP_ASSERT(hipGetDeviceProperties(&devProp, 0));
  cout << " System minor " << devProp.minor << endl;
  cout << " System major " << devProp.major << endl;
  cout << " agent prop name " << devProp.name << endl;
  cout << "hip Device prop succeeded " << endl ;
  cout << " width " << width << " ; height " << height << endl;
  
  HIP_ASSERT(hipMalloc((void**)&deviceA, num * sizeof(float)));
  HIP_ASSERT(hipMalloc((void**)&deviceB, num * sizeof(float)));
  HIP_ASSERT(hipMalloc((void**)&deviceC, num * sizeof(float)));
  
  HIP_ASSERT(hipMemcpy(deviceA, hostA, num*sizeof(float), hipMemcpyHostToDevice));
  HIP_ASSERT(hipMemcpy(deviceB, hostB, num*sizeof(float), hipMemcpyHostToDevice));

  vectoradd_float<<<
     dim3(width/THREADS_PER_BLOCK_X, HEIGHT/THREADS_PER_BLOCK_Y),
     dim3(THREADS_PER_BLOCK_X, THREADS_PER_BLOCK_Y),
     0, 0
  >>>(deviceA, deviceB, deviceC, width, height);

  HIP_ASSERT(hipMemcpy(hostC, deviceC, num*sizeof(float), hipMemcpyDeviceToHost));

  // verify the results
  int errors = 0;

  for (int i = 0; i < num; i++) {
    // cout << hostA[i] << "; " << hostB[i] << "; " << hostC[i] << endl;
    if (hostC[i] != (hostA[i] + hostB[i])) {
      errors++;
    }
  }
  if (errors!=0) {
    printf("FAILED: %d errors\n",errors);
  } else {
      printf ("PASSED!\n");
  }

  HIP_ASSERT(hipFree(deviceA));
  HIP_ASSERT(hipFree(deviceB));
  HIP_ASSERT(hipFree(deviceC));

  return errors;
}


extern "C" int g() {
  float* hostA;
  float* hostB;
  float* hostC;

  float* deviceA;
  float* deviceB;
  float* deviceC;

  hipDeviceProp_t devProp;
  HIP_ASSERT(hipGetDeviceProperties(&devProp, 0));
  cout << " System minor " << devProp.minor << endl;
  cout << " System major " << devProp.major << endl;
  cout << " agent prop name " << devProp.name << endl;
  cout << "hip Device prop succeeded " << endl ;

  hostA = (float*)malloc(NUM * sizeof(float));
  hostB = (float*)malloc(NUM * sizeof(float));
  hostC = (float*)malloc(NUM * sizeof(float));
  
  // initialize the input data
  for (int i = 0; i < NUM; i++) {
    hostB[i] = (float)i;
    hostC[i] = (float)i*100.0f;
  }
  
  HIP_ASSERT(hipMalloc((void**)&deviceA, NUM * sizeof(float)));
  HIP_ASSERT(hipMalloc((void**)&deviceB, NUM * sizeof(float)));
  HIP_ASSERT(hipMalloc((void**)&deviceC, NUM * sizeof(float)));
  
  HIP_ASSERT(hipMemcpy(deviceB, hostB, NUM*sizeof(float), hipMemcpyHostToDevice));
  HIP_ASSERT(hipMemcpy(deviceC, hostC, NUM*sizeof(float), hipMemcpyHostToDevice));

  vectoradd_float<<<
     dim3(WIDTH/THREADS_PER_BLOCK_X, HEIGHT/THREADS_PER_BLOCK_Y),
     dim3(THREADS_PER_BLOCK_X, THREADS_PER_BLOCK_Y),
     0, 0
  >>>(deviceA, deviceB, deviceC, WIDTH, HEIGHT);

  HIP_ASSERT(hipMemcpy(hostA, deviceA, NUM*sizeof(float), hipMemcpyDeviceToHost));

  // verify the results
  int errors = 0;
  for (int i = 0; i < NUM; i++) {
    if (hostA[i] != (hostB[i] + hostC[i])) {
      errors++;
    }
  }
  if (errors!=0) {
    printf("FAILED: %d errors\n",errors);
  } else {
      printf ("PASSED!\n");
  }

  HIP_ASSERT(hipFree(deviceA));
  HIP_ASSERT(hipFree(deviceB));
  HIP_ASSERT(hipFree(deviceC));

  free(hostA);
  free(hostB);
  free(hostC);

  return errors;
}

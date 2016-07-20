#include	<stdio.h>
#include	<mpi.h>

void dims_test(char* str, int nnodes, int ndims)
{ 
  int i, dims[20];
  double t0, t1; 
  for (i=0;i<ndims;i++) dims[i]=0;
  t0=MPI_Wtime(); 
  MPI_Dims_create(nnodes, ndims, dims);
  t1=MPI_Wtime(); 
  printf ("dt=%9.6f nnodes=%8i=%s ndims=%2i dims=", t1-t0, nnodes, str, ndims);
  for (i=0;i<ndims;i++) printf (" %2i", dims[i]);
  printf ("\n");
}
 
int main (int argc, char *argv[])
{
  int my_rank, size;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  if (my_rank == 0) {
     dims_test("6, 2            ", 6, 2);
     dims_test("16*12*10*8      ", 16*12*10*8, 4);
     dims_test("16*15*8*8       ", 16*15*8*8,  4);
     dims_test("16*12*12*10*10  ", 16*12*12*10*10, 5);
     dims_test("16*15*12*10* 8  ", 16*15*12*10* 8, 5);
     dims_test("16*12*12*10*10*8", 16*12*12*10*10*8, 6);
     dims_test("16*15*12*10* 8*8", 16*15*12*10* 8*8, 6);
 /*
     printf("            16*12*12*10*10*8 missing\n            16*15*12*10* 8 missing\n"); 
 */ 
     dims_test("2**4 *3*5        ", 2*2*2*2*3*5, 2);
     dims_test("16*15            ", 2*2*2*2*3*5, 2);
     dims_test("20*12            ", 2*2*2*2*3*5, 2);
     dims_test("2**8 *3*5        ", 2*2*2*2*2*2*2*2*3*5, 3);
     dims_test("16*16*15    best ", 16*16*15, 3);
     dims_test("20*16*12    worse", 20*16*12, 3);
     dims_test("2**12*3*5        ", 2*2*2*2*2*2*2*2*2*2*2*2*3*5, 4);
     dims_test("16*16*16*15 best ", 16*16*16*15, 4);
     dims_test("20*16*16*12 worse", 20*16*16*12, 4);
     dims_test("2**3 *3*5        ", 2*2*2*3*5, 6);
     dims_test("2**4 *3*5        ", 2*2*2*2*3*5, 6);
     dims_test("2**5 *3*5        ", 2*2*2*2*2*3*5, 6);
     dims_test("2**6 *3*5        ", 2*2*2*2*2*2*3*5, 6);
     dims_test("2**7 *3*5        ", 2*2*2*2*2*2*2*3*5, 6);
     dims_test("2**8 *3*5        ", 2*2*2*2*2*2*2*2*3*5, 6);
     dims_test("2**9 *3*5        ", 2*2*2*2*2*2*2*2*2*3*5, 6);
     dims_test("2**10*3*5        ", 2*2*2*2*2*2*2*2*2*2*3*5, 6);
     dims_test("8*6*5*4*4*4 best ", 8*6*5*4*4*4, 6);
     dims_test("8*8*5*4*4*3 worse", 8*8*5*4*4*3, 6);
     dims_test("2**11*3*5        ", 2*2*2*2*2*2*2*2*2*2*2*3*5, 6);
     dims_test("8*8*6*5*4*4 best ", 8*8*6*5*4*4, 6);
     dims_test("8*8*8*5*4*3 worse", 8*8*8*5*4*3, 6);
     dims_test("2**12*3*5        ", 2*2*2*2*2*2*2*2*2*2*2*2*3*5, 6);
     dims_test("8*8*8*6*5*4 best ", 8*8*8*6*5*4, 6);
     dims_test("8*8*8*8*5*3 worse", 8*8*8*8*5*3, 6);
  } 

  MPI_Finalize();
}

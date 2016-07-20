#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <mpi.h>

#define N (16*1024)

void user_op(void *in, void *inout, int *len, MPI_Datatype *dtype) {
  /* nop */
  return;
}

int main(int argc, char **argv) {
  int     rank, nproc, i;
  double *data;
  MPI_Op  my_op;
  MPI_Datatype dtype;

  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  /* This datatype captures column 0 in the NxN matrix */
  MPI_Type_vector(N, 1, N, MPI_DOUBLE, &dtype);
  MPI_Type_commit(&dtype);

  MPI_Op_create(user_op, 1, &my_op);

  data = malloc(N*N*sizeof(double));
  if (data == NULL) MPI_Abort(MPI_COMM_WORLD, 1);

  if (rank == 0) printf("Allocated %dx%d matrix (%0.2f GiB)\n", N, N, N*N*sizeof(double)/(1024*1024*1024.0));
  if (rank == 0) printf(" - Initializing data array\n");

  for (i = 0; i < N*N; i++)
    data[i] = 1.0;

  if (rank == 0) printf(" + Done\n");
  sleep(5);
  MPI_Barrier(MPI_COMM_WORLD);

  if (rank == 0) printf(" - Performing column reduction with user-defined op\n");

  MPI_Reduce((rank == 0) ? MPI_IN_PLACE : data, data, 1, dtype, my_op, 0, MPI_COMM_WORLD);

  if (rank == 0) printf(" + Success\n");
  sleep(5);
  MPI_Barrier(MPI_COMM_WORLD);

  free(data);
  MPI_Type_free(&dtype);
  MPI_Op_free(&my_op);
  MPI_Finalize();
  return 0;
}

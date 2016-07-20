#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int Cart_shift_vector(MPI_Comm comm, int direction[], 
		      int *rank_source, int *rank_dest)
{
  int type;
  int i, d;
  
  int *dims, *periods, *coords;

  MPI_Topo_test(comm,&type);
  if (type!=MPI_CART) return MPI_ERR_TOPOLOGY;

  MPI_Cartdim_get(comm,&d);
  
  dims    = (int*)malloc(d*sizeof(int));
  periods = (int*)malloc(d*sizeof(int));
  coords  = (int*)malloc(d*sizeof(int));
  
  /* destination */
  MPI_Cart_get(comm,d,dims,periods,coords);
  for (i=0; i<d; i++) {
    if (periods[i]) {
      if (direction[i]<0) coords[i] = coords[i]-(-direction[i]%dims[i]);
      else coords[i] += direction[i];
      coords[i] = (coords[i]+dims[i])%dims[i];
    } else {
      coords[i] += direction[i];
      if (coords[i]<0||coords[i]>=dims[i]) break;
    }
  }
  if (i<d) *rank_dest = MPI_PROC_NULL; else {
    MPI_Cart_rank(comm,coords,rank_dest);
  }

  /* source */
  MPI_Cart_get(comm,d,dims,periods,coords);
  for (i=0; i<d; i++) {
    if (periods[i]) {
      if (direction[i]>=0) coords[i] -= (direction[i]%dims[i]); 
      else coords[i] -= direction[i];
      coords[i] = (coords[i]+dims[i])%dims[i];
    } else {
      coords[i] -= direction[i];
      if (coords[i]<0||coords[i]>=dims[i]) break;
    }
  }
  if (i<d) *rank_source = MPI_PROC_NULL; else {
    MPI_Cart_rank(comm,coords,rank_source);
  }

  free(dims);
  free(periods);
  free(coords);

  return MPI_SUCCESS;
}

int main(int argc, char *argv[])
{
  int rank, size;
  int rank_source, rank_dest, rs, rd;
  MPI_Comm cart;
  int p, s, d, i;

  int reorder = 0;

  int *direction, *periods, *dimensions;

  MPI_Init(&argc,&argv);

  MPI_Comm_rank(MPI_COMM_WORLD,&rank);
  MPI_Comm_size(MPI_COMM_WORLD,&size);

  direction  = (int*)malloc(size*sizeof(int));
  periods    = (int*)malloc(size*sizeof(int));
  dimensions = (int*)malloc(size*sizeof(int));

  for (p=0; p<2; p++) {
    if (rank==0) fprintf(stderr,"Periodicity %d\n",p);

    for (d=1; d<size; d++) {
      for (i=0; i<d; i++) dimensions[i] = 0;
      MPI_Dims_create(size,d,dimensions);
      
      if (rank==0) {
	for (i=0; i<d; i++) fprintf(stderr,"dimensions[%d] = %d\n",i,dimensions[i]);
      }

      for (i=0; i<d; i++) periods[i] = p; /* non-periodic */
      MPI_Cart_create(MPI_COMM_WORLD,d,dimensions,periods,reorder,&cart);

      /* validate against cart_shift */
      for (i=0; i<d; i++) direction[i] = 0;
      for (s=-size; s<size; s++) {
	for (i=0; i<d; i++) {
	  direction[i] = s;
	  Cart_shift_vector(cart,direction,&rank_source,&rank_dest);
	  direction[i] = 0;
	  MPI_Cart_shift(cart,i,s,&rs,&rd);
	  if (rank_source!=rs||rank_dest!=rd) {
	    fprintf(stderr,"Error at rank %d, size %d, dimension %d, shift %d: source %d %d, dest %d %d\n",
		    rank,size,d,s,rank_source,rs,rank_dest,rd);
	  }
	}
      }
   
      /* diagonal shift */
      for (s=-3; s<4; s++) {
	for (i=0; i<d; i++) direction[i] = s;
	Cart_shift_vector(cart,direction,&rank_source,&rank_dest);

	MPI_Barrier(MPI_COMM_WORLD);

	fprintf(stderr,"Rank %d, dim %d, diag %d, source %d dest %d\n",
		rank,d,s,rank_source,rank_dest);
      }

      MPI_Comm_free(&cart);
    }
  }
  
  MPI_Finalize();

  free(periods);
  free(direction);
  free(dimensions);

  return 0;
}

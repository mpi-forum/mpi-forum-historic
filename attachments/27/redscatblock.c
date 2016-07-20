/* Straight-forward implementations of Reduce_scatter_block 
   a) in terms of MPI_Reduce and MPI_Scatter
   b) in terms of MPI_Reduce_scatter. 
   It should be noted that much better, direct implementations are possible! 
*/
/* For the MPI Forum */
/* Jesper Larsson Traff, NEC, 2008 */

#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"

#define A
#ifdef A

#define REDUCE_SCATTER_BLOCK_TAG 111

/* argument-scalable version */
int Reduce_scatter_block(void * sendbuf, void *recvbuf, 
			 int count, MPI_Datatype datatype, 
			 MPI_Op op, MPI_Comm comm)
{
  int rank, size;
  int reducecount;
  MPI_Aint lb, extent;  
  void *inbuf, *outbuf;

  int intercomm;

  int mpi_errno = MPI_SUCCESS;

  MPI_Type_get_extent(datatype, &lb, &extent );
  
  MPI_Comm_size(comm,&size);

  reducecount = size*count;
  
  if (reducecount==0) return MPI_SUCCESS;
  
  outbuf = (void *)malloc(reducecount*extent);
  if (outbuf==NULL) return MPI_ERR_INTERN;
  outbuf = (void*)((char*)outbuf-lb);
  
  MPI_Comm_test_inter(comm,&intercomm);
  if (!intercomm) {    
    inbuf = (MPI_IN_PLACE==sendbuf) ? recvbuf : sendbuf;
    
    mpi_errno = MPI_Reduce(inbuf,outbuf,reducecount,datatype,op,0,comm);
    if (mpi_errno==MPI_SUCCESS) {
      mpi_errno = 
	MPI_Scatter(outbuf,count,datatype,recvbuf,count,datatype,0,comm);
    }
  } else {
    MPI_Comm localcomm;
    /* create local comm here - machinery needed to do this in MPI 2.2 */

    MPI_Comm_rank(comm,&rank);

    if (rank==0) {
      /* reduction to local root */
      mpi_errno = 
	MPI_Reduce(sendbuf,outbuf,reducecount,datatype,op,0,localcomm);
      mpi_errno = 
	MPI_Sendrecv_replace(outbuf,reducecount,datatype,
			     0,REDUCE_SCATTER_BLOCK_TAG, 
			     0,REDUCE_SCATTER_BLOCK_TAG,
			     comm,MPI_STATUS_IGNORE);
      mpi_errno = 
	MPI_Scatter(outbuf, count,datatype, 
		    recvbuf,count,datatype,0,localcomm);
    } else {
      mpi_errno = 
	MPI_Reduce(sendbuf,outbuf,reducecount,datatype,op,0,localcomm);
      mpi_errno = 
	MPI_Scatter(NULL,0,MPI_DATATYPE_NULL,
		    recvbuf,count,datatype,0,localcomm);
    }
  }

  free((char*)outbuf+lb);

  return mpi_errno;
}
#else
int Reduce_scatter_block(void *sendbuf, void *recvbuf, 
			 int count, MPI_Datatype datatype, 
			 MPI_Op op, MPI_Comm comm)
{
  int size;
  int *counts;
  int i;

  int mpi_errno = MPI_SUCCESS;

  MPI_Comm_size(comm,&size);
    
  if (count==0) return MPI_SUCCESS;
  
  counts = (void *)malloc(size*sizeof(int));
  if (counts==NULL) return MPI_ERR_INTERN;
  
  for (i=0; i<size; i++) {
    counts[i] = count;
  }
  mpi_errno = MPI_Reduce_scatter(sendbuf,recvbuf,counts,datatype,op,comm);
  
  free(counts);
  
  return mpi_errno;
}
#endif

int main(int argc, char *argv[])
{
  int rank, size;
  int *input, output;
  int i;

  MPI_Init(&argc,&argv);

  MPI_Comm_rank(MPI_COMM_WORLD,&rank);
  MPI_Comm_size(MPI_COMM_WORLD,&size);

  input = (int*)malloc(size*sizeof(int));

  for (i=0; i<size; i++) input[i] = i;

  Reduce_scatter_block(input,&output,1,MPI_INT,MPI_SUM,MPI_COMM_WORLD);

  if (output!=rank*size) {
    fprintf(stderr,"Rank %d expected %d, got %d\n",rank,rank*size,output);
  }

  free(input);

  MPI_Finalize();
}

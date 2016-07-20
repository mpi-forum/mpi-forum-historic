/*
 * Copyright (c) 2008 The Trustees of Indiana University and Indiana
 *                    University Research and Technology
 *                    Corporation.  All rights reserved.
 * Copyright (c) 2008 NEC Laboratories Europe
 *
 * Author(s): Torsten Hoefler 
 *            Jesper Larsson Traff
 */
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int MPI_Dist_graph_create(MPI_Comm comm_old, int n, int nodes[], int degrees[], 
                             int targets[], int weights[], MPI_Info info,
                             int reorder, MPI_Comm *newcomm) {
  int rank, p;
  int *predegree, *index, *edges;
  int ind, total;
  int i, j, k;

  int mpi_err = MPI_SUCCESS;

  MPI_Comm_rank(comm_old, &rank);
  MPI_Comm_size(comm_old, &p);

  /* check arguments locally (only consistency requirement is on reorder) */
  k=0;
  for (i=0; i<n; i++) {
    if (nodes[i]<0||nodes[i]>=p) return MPI_ERR_TOPOLOGY;
    for (j=0; j<degrees[i]; j++,k++) {
      if (targets[k]<0||targets[k]>=p) return MPI_ERR_TOPOLOGY;
    }
  }
  
  index     = (int*)malloc(p*sizeof(int));
  predegree = (int*)malloc(p*sizeof(int));

  /* set degrees locally */
  for (i=0; i<p; i++) index[i] = 0;
  for (i=0; i<n; i++) index[nodes[i]] += degrees[i];

  /* compute degree of all nodes globally */
  MPI_Exscan(index, predegree, p, MPI_INT, MPI_SUM, comm_old);
  if (rank==0) for (i=0; i<p; i++) predegree[i] = 0;
  MPI_Allreduce(MPI_IN_PLACE, index, p, MPI_INT, MPI_SUM, comm_old);

  /*
  for(i=0; i<p; i++) {
    printf("[%i] index[%i] = %i, predegree[%d] = %d\n", 
	   rank, i, index[i], i, predegree[i]);
  }
  */

  /* compute index array in MPI 2.1 style */
  for (i=1; i<p; i++) index[i] += index[i-1];
  total = index[p-1];

  edges  = (int*)malloc(total*sizeof(int));
  for (i=0; i<total; i++) edges[i] = 0;

  /* fill in edge table locally */
  k = 0;
  for (i=0; i<n; i++) {
    ind = (nodes[i]==0) ? 0 : index[nodes[i]-1]; /* base index of node */
    ind += predegree[nodes[i]];
    for (j=0; j<degrees[i]; j++) {
      // fill edges array
      edges[ind++] = targets[k++];
    }
    predegree[nodes[i]] += degrees[i]; /* in case a node appears again */
  }
  /* local time is O(number of local edges) */

  /*
  if (rank==p-1) {
    printf("indices:\n");
    for (i=0; i<p; i++) printf("%i\n", index[i]);
    printf("edges:\n");
    for (i=0; i<total; i++) printf("%i\n", edges[i]);
  }
  */

  /* merge local edge tables */
  MPI_Allreduce(MPI_IN_PLACE, edges, total, MPI_INT, MPI_SUM, comm_old);

  /*
  if (rank==0) {
    printf("indices:\n");
    for (i=0; i<p; i++) printf("%i\n", index[i]);
    printf("edges:\n");
    for (i=0; i<total; i++) printf("%i\n", edges[i]);
  }
  */

  /* create graph in MPI 2.1 style */
  mpi_err = MPI_Graph_create(comm_old, p, index, edges, reorder, newcomm);
  
  free(index);
  free(edges);

  return mpi_err;
}

int MPI_Dist_neighbors_count(MPI_Comm comm, int *inneighbors, int *outneighbors) {

  int rank, p;
  int i, j, nneighbors, *neighbors;
  int mpi_err = MPI_SUCCESS;

  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &p);
  
  mpi_err = MPI_Graph_neighbors_count(comm, rank, outneighbors);
  if (mpi_err!=MPI_SUCCESS) goto done;

  // this is local
  // get all neighbors who point at me ... should have been cached
  // during creation - it's only a POC
  *inneighbors=0;
  for(i=0; i<p; i++) {
    mpi_err = MPI_Graph_neighbors_count(comm, i, &nneighbors);
    neighbors = (int*)malloc(nneighbors*sizeof(int));
    mpi_err = MPI_Graph_neighbors(comm, i, nneighbors, neighbors);
    for (j=0; j<nneighbors; j++) {
      if (neighbors[j] == rank) (*inneighbors)++;
    }
    free(neighbors);
  }

 done:
  return mpi_err;
}

int MPI_Dist_neighbors(MPI_Comm comm, int inmaxneighbors, int *inneighbors, int outmaxneighbors, int *outneighbors) {

  int rank, p;
  int i, j, inneighbor, nneighbors, *neighbors;

  int mpi_err = MPI_SUCCESS;

  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &p);

  mpi_err = MPI_Graph_neighbors(comm, rank, outmaxneighbors, outneighbors);
  if (MPI_SUCCESS!=mpi_err) goto done;

  // this is local
  // get all neighbors who point at me ... should have been cached
  // during creation - it's only a POC
  inneighbor = 0;
  for(i=0; i<p; i++) {
    mpi_err = MPI_Graph_neighbors_count(comm, i, &nneighbors);
    neighbors = (int*)malloc(nneighbors*sizeof(int));
    mpi_err = MPI_Graph_neighbors(comm, i, nneighbors, neighbors);
    for (j=0; j<nneighbors; j++) {
      if (neighbors[j] == rank) {
        inneighbors[inneighbor] = i;
        if(inmaxneighbors==++inneighbor) return MPI_SUCCESS;
      }
    }
    free(neighbors);
  }

 done:
  return mpi_err;
}

int main(int argc, char *argv[]) {
  
  int rank, p;
  int *nodes, *degrees, *tgtnodes;
  int inneighbors, outneighbors;
  int i, n, e;
  int *inneigh, *outneigh, *weights;
  MPI_Comm newcomm;

  MPI_Init(&argc,&argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &p);

  n = (rank==p-1) ? 3 : 2;
  e = (rank==p-1) ? p+3 : 3; /* extra edges */
  nodes    = (int*)malloc(n*sizeof(int));
  degrees  = (int*)malloc(n*sizeof(int));
  tgtnodes = (int*)malloc(e*sizeof(int));

  nodes[0]    = rank;
  degrees[0]  = 2;
  tgtnodes[0] = (rank+1)%p; // a ring
  tgtnodes[1] = 0; // a star into 0 

  nodes[1]    = (rank+1)%p;
  degrees[1]  = 1;
  tgtnodes[2] = rank; // a double ring

  if (rank==p-1) { // star out of p-1
    nodes[2] = rank;
    degrees[2] = p;
    for (i=0; i<p; i++) tgtnodes[3+i] = i; 
  }

  weights = malloc(sizeof(int)*n);
  for(i=0;i<n;i++) weights[i] = 1;

  MPI_Dist_graph_create(MPI_COMM_WORLD, n, nodes, degrees, tgtnodes, 
                           weights, MPI_INFO_NULL, 0, &newcomm);

  MPI_Dist_neighbors_count(newcomm, &inneighbors, &outneighbors);
  printf("rank: %i has %i inneighbors and %i outneighbors\n", rank, inneighbors, outneighbors);

  inneigh =  (int*)malloc(inneighbors*sizeof(int));
  outneigh = (int*)malloc(outneighbors*sizeof(int));
  MPI_Dist_neighbors(newcomm, inneighbors, inneigh, outneighbors, outneigh);

  printf("rank %i has outneighbors:", rank);
  for (i=0; i<outneighbors; i++) printf(" %i", outneigh[i]);
  printf(" and inneighbors:");
  for (i=0; i<inneighbors; i++) printf(" %i", inneigh[i]);
  printf("\n");
  
  MPI_Finalize();
}

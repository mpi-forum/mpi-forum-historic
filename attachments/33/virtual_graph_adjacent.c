/*
 * Copyright (c) 2008 The Trustees of Indiana University and Indiana
 *                    University Research and Technology
 *                    Corporation.  All rights reserved.
 *
 * Author(s): Torsten Hoefler 
 */
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int MPI_Dist_graph_create_adjacent(MPI_Comm comm_old, 
              int indegree, int sources[], int sourceweights[],
              int outdegree, int destinations[], int destweights[],
              MPI_Info info, int reorder, MPI_Comm *comm_dist_graph)
{
  /* build arrays for flexible interface */
  int rank;

  MPI_Comm_rank(comm_old, &rank);

  MPI_Dist_graph_create(comm_old, 1, &rank, &outdegree, destinations, destweights, info, reorder, comm_dist_graph);

}


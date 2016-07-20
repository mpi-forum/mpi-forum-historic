/*
 * Copyright (c) 2008 The Trustees of Indiana University and Indiana
 *                    University Research and Technology
 *                    Corporation.  All rights reserved.
 *
 * Author(s): Torsten Hoefler 
 *
 */
#include <mpi.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <vector>
#include <utility>

typedef struct commkey {
  std::vector<int> out, outw, in, inw;
} STI_Comminfo;

/* the keyval (global) */
static int gkeyval=MPI_KEYVAL_INVALID;

static int STI_Key_copy(MPI_Comm oldcomm, int keyval, void *extra_state, void *attribute_val_in, void *attribute_val_out, int *flag) {
    /* delete the attribute in the new comm  - it will be created at the
     *    * first usage */
    *flag = 0;

      return MPI_SUCCESS;
}

static int STI_Key_delete(MPI_Comm comm, int keyval, void *attribute_val, void *extra_state) {
  STI_Comminfo *comminfo;

  if(keyval == gkeyval) {
    comminfo=(STI_Comminfo*)attribute_val;
    free((void*)comminfo);
  } else {
    printf("Got wrong keyval!(%i)\n", keyval);
  }

  return MPI_SUCCESS;
}


int MPI_Dist_graph_create_adjacent(MPI_Comm comm_old, 
              int indegree, int sources[], int sourceweights[],
              int outdegree, int destinations[], int destweights[],
              MPI_Info info, int reorder, MPI_Comm *comm_dist_graph) {
  int res = MPI_Comm_dup(comm_old, comm_dist_graph);
  if((MPI_SUCCESS != res)) { printf("Error in MPI_Keyval_create() (%i)\n", res); return res; }

  if(MPI_KEYVAL_INVALID == gkeyval) {
    res = MPI_Keyval_create(STI_Key_copy, STI_Key_delete, &(gkeyval), NULL);
    if((MPI_SUCCESS != res)) { printf("Error in MPI_Keyval_create() (%i)\n", res); return res; }
  }

  STI_Comminfo *comminfo = new struct commkey(); 
  
  for(int i=0; i<indegree; ++i) {
    comminfo->in.push_back(sources[i]);
    comminfo->inw.push_back(sourceweights[i]);
  }
  for(int i=0; i<outdegree; ++i) {
    comminfo->out.push_back(destinations[i]);
    comminfo->outw.push_back(destweights[i]);
  }

  /* put the new attribute to the comm */
  res = MPI_Attr_put(*comm_dist_graph, gkeyval, comminfo);
  if((MPI_SUCCESS != res)) { printf("Error in MPI_Attr_put() (%i)\n", res); return NULL; }

  return MPI_SUCCESS;
}

int MPI_Dist_graph_create(MPI_Comm comm_old, int n, int nodes[], int degrees[], 
                          int targets[], int weights[], MPI_Info info,
                          int reorder, MPI_Comm *newcomm) {
  /* build arrays for flexible interface */
  int r, p;
  MPI_Comm_rank(comm_old, &r);
  MPI_Comm_size(comm_old, &p);

  int res = MPI_Comm_dup(comm_old, newcomm);
  if((MPI_SUCCESS != res)) { printf("Error in MPI_Keyval_create() (%i)\n", res); return res; }

  if(MPI_KEYVAL_INVALID == gkeyval) {
    res = MPI_Keyval_create(STI_Key_copy, STI_Key_delete, &(gkeyval), NULL);
    if((MPI_SUCCESS != res)) { printf("Error in MPI_Keyval_create() (%i)\n", res); return res; }
  }

  std::vector<std::vector<int> > rout(p), rin(p);
  int index=0;
  assert(n<p);
  for(int i=0; i<n; ++i) {
    assert(nodes[i] < p);
    for(int j=0; j<degrees[i]; ++j) {
      assert(targets[index] < p);

      rout[nodes[i]].push_back(targets[index]);
      rout[nodes[i]].push_back(weights[index]);

      rin[targets[index]].push_back(nodes[i]);
      rin[targets[index]].push_back(weights[i]);
      
      index++;
    }
  }

  std::vector<int> redscat(2*p);
  std::vector<int> redscatres(2);
  std::vector<int> cnts(p);
  for(int i=0; i<p; ++i) {
    if(rin[i].size()) redscat[2*i]=1; else redscat[2*i]=0;
    if(rout[i].size()) redscat[2*i+1]=1; else redscat[2*i+1]=0;
    cnts[i]=2;
  }

  MPI_Reduce_scatter(&redscat[0], &redscatres[0], &cnts[0], MPI_INT, MPI_SUM, comm_old);

  std::vector<MPI_Request> reqs;
  for(int i=0; i<p; ++i) {
    if(rin[i].size()) {
      reqs.resize(reqs.size()+1);
      MPI_Isend(&rin[i][0], rin[i].size(), MPI_INT, i, 99, comm_old, &reqs[reqs.size()-1]);
    }
    if(rout[i].size()) {
      reqs.resize(reqs.size()+1);
      MPI_Isend(&rout[i][0], rout[i].size(), MPI_INT, i, 98, comm_old, &reqs[reqs.size()-1]);
    }
  }

  STI_Comminfo *comminfo = new struct commkey(); 

  for(int i=0; i<redscatres[0]; ++i) {
    MPI_Status stat;
    /* receive incoming edges */
    MPI_Probe(MPI_ANY_SOURCE, 99, comm_old, &stat);
    int count;
    MPI_Get_count(&stat, MPI_INT, &count);
    int *buf = (int*)malloc(sizeof(int)*count);
    MPI_Recv(buf, count, MPI_INT, MPI_ANY_SOURCE, 99, comm_old, MPI_STATUS_IGNORE);
    for(int j=0; j<count/2; j++) {
      comminfo->in.push_back(buf[2*j]);
      comminfo->inw.push_back(buf[2*j+1]);
    }
    free(buf);
  }

  for(int i=0; i<redscatres[1]; ++i) {
    /* receive outgoung edges */
    MPI_Status stat;
    MPI_Probe(MPI_ANY_SOURCE, 98, comm_old, &stat);
    int count;
    MPI_Get_count(&stat, MPI_INT, &count);
    int *buf = (int*)malloc(sizeof(int)*count);
    MPI_Recv(buf, count, MPI_INT, MPI_ANY_SOURCE, 98, comm_old, MPI_STATUS_IGNORE);
    for(int j=0; j<count/2; j++) {
      comminfo->out.push_back(buf[2*j]);
      comminfo->outw.push_back(buf[2*j+1]);
    }
    free(buf);
  }

  MPI_Waitall(reqs.size(), &reqs[0], MPI_STATUSES_IGNORE);

  assert(comminfo->inw.size() == comminfo->in.size());
  assert(comminfo->outw.size() == comminfo->out.size());

  /* put the new attribute to the comm */
  res = MPI_Attr_put(*newcomm, gkeyval, comminfo);
  if((MPI_SUCCESS != res)) { printf("Error in MPI_Attr_put() (%i)\n", res); return NULL; }

  return MPI_SUCCESS;
}

int MPI_Dist_neighbors_count(MPI_Comm comm, int *inneighbors, int *outneighbors) {
  STI_Comminfo *comminfo = new struct commkey(); 

  int flag;
  int res = MPI_Attr_get(comm, gkeyval, &comminfo, &flag);
  if((MPI_SUCCESS != res)) { printf("Error in MPI_Attr_get() (%i)\n", res); return res; }

  *inneighbors = comminfo->in.size();
  *outneighbors = comminfo->out.size();

  return MPI_SUCCESS;
}

int MPI_Dist_neighbors(MPI_Comm comm, int maxindegree, int sources[], 
                       int sourceweights[], int maxoutdegree, 
                       int destinations[], int destweights[]) {
  
  int flag;
  STI_Comminfo *comminfo = new struct commkey(); 
  int res = MPI_Attr_get(comm, gkeyval, &comminfo, &flag);
  if((MPI_SUCCESS != res)) { printf("Error in MPI_Attr_get() (%i)\n", res); return res; }

  if(maxindegree < comminfo->in.size() || maxoutdegree < comminfo->out.size()) return MPI_ERR_COUNT;

  for(int i=0; i<comminfo->in.size(); ++i) {
    sources[i] = comminfo->in[i];
    sourceweights[i] = comminfo->inw[i];
  }
  for(int i=0; i<comminfo->out.size(); ++i) {
    destinations[i] = comminfo->out[i];
    destweights[i] = comminfo->outw[i];
  }
  
  return MPI_SUCCESS;
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

  weights = (int*)malloc(sizeof(int)*e);
  for(i=0;i<e;i++) weights[i] = 1;

  MPI_Dist_graph_create(MPI_COMM_WORLD, n, nodes, degrees, tgtnodes, 
                           weights, MPI_INFO_NULL, 0, &newcomm);

  MPI_Dist_neighbors_count(newcomm, &inneighbors, &outneighbors);
  printf("rank: %i has %i inneighbors and %i outneighbors\n", rank, inneighbors, outneighbors);

  inneigh =  (int*)malloc(inneighbors*sizeof(int));
  outneigh = (int*)malloc(outneighbors*sizeof(int));
  int *inneighw =  (int*)malloc(inneighbors*sizeof(int));
  int *outneighw = (int*)malloc(outneighbors*sizeof(int));
  MPI_Dist_neighbors(newcomm, inneighbors, inneigh, inneighw, outneighbors, outneigh, outneighw);

  printf("rank %i has outneighbors:", rank);
  for (i=0; i<outneighbors; i++) printf(" %i", outneigh[i]);
  printf(" and inneighbors:");
  for (i=0; i<inneighbors; i++) printf(" %i", inneigh[i]);
  printf("\n");

  MPI_Finalize();
}

#include "bfs_kernel.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

#ifdef _VEC

void tril_bfs_vec1(int mask, Node *h_graph_nodes, int *h_graph_edges, char *h_graph_mask, char *h_updating_graph_mask, \
                 char *h_graph_visited, int *h_cost, int max_edges, int start, int end, int ptid, int vtid)
{
  //this template uses separate scalar and vector code blocks but they can be interspersed as well as shown here
  //https://github.com/cucapra/gem5-mesh/wiki/Trilliasm-Language-Overview:-Vector-SIMD-in-C

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  //---------------------------------
  //scalar core code iterspersed with vissue
  ISSUE_VINST(init_label); //eg: this block will deal with initila stack manipulation and initilaization of variables
  //-----------------------------------

  //prefetch variables
  int spadRegion = 0;

  for (int i = start; i < end; i+=VEC_LEN) {
    ISSUE_VINST(check_frontier_label);

    for(int j=0; j<max_edges; j++){
      ISSUE_VINST(update_cost_label);
    }

    ISSUE_VINST(increment_loop_label);
  }



  //issue stack end portions of vector cores
  ISSUE_VINST(vector_stack_label);
  // devec with unique tag
  DEVEC(devec_0);

  //fence for all cores to ensure memory operations have completed
  asm volatile("fence\n\t");

  asm("trillium vissue_delim return scalar_return"); //return delimiter, delimiters can be of many types
  return;

  //all the vissue labels below:

  init_label: //this name matches with vissue label name
    asm("trillium glue_point init"); //name over here "init" matches with delimiter in vector code
  check_frontier_label:
    asm("trillium glue_point check_frontier");
  update_cost_label:
    asm("trillium glue_point update_cost");
  increment_loop_label:
    asm("trillium glue_point increment_loop");
  vector_stack_label: 
    asm("trillium glue_point vector_stack"); //name over here "vector_stack" matches with delimiter in vector code

  #elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next init"); //until_next delimiter used, name (init) over here same as in glue point above
  //vector core code

  volatile int bh1,bh2;

  int tid = start+vtid;

  do {

    // check_frontier:
    asm("trillium vissue_delim until_next check_frontier");
    int cond = (h_graph_mask[tid] == true);
    volatile int compiler_hack = 1;
    PRED_EQ(cond,1);
    if(compiler_hack){
      h_graph_mask[tid]=false;
      int i = h_graph_nodes[tid].starting;
      int edge_bound= h_graph_nodes[tid].starting + h_graph_nodes[tid].no_of_edges;
      do{
        asm("trillium vissue_delim until_next update_cost");
        int loop_cond = (i<edge_bound);
        loop_cond = cond & loop_cond;
        PRED_EQ(loop_cond,1);
        if(compiler_hack){
          int id = h_graph_edges[i];
          int cond2 = (!h_graph_visited[id]);
          cond2 = cond2 & loop_cond;
          PRED_EQ(cond2,1);
          if(compiler_hack){
            h_cost[id]=h_cost[tid]+1;
            h_updating_graph_mask[id]=true;
          }
          PRED_EQ(loop_cond,1);
        }
        PRED_EQ(cond,1);
        i++;
      } while(bh2);
    }
    asm("trillium vissue_delim until_next increment_loop");
    PRED_EQ(vtid,vtid);
    tid+=VEC_LEN;
  } while(bh1);

  asm("trillium vissue_delim return vector_stack"); //return delimiter
  return;
  #endif

}


void tril_bfs_vec2(int mask, char *h_graph_mask, char *h_updating_graph_mask, char *h_graph_visited, char *stop, \
                 int start, int end, int ptid, int vtid)
{

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);
  ISSUE_VINST(init_label);

  int spadRegion = 0;

  for (int i = start; i < end; i+=VEC_LEN) {
    ISSUE_VINST(update_graph_label);
  }

  ISSUE_VINST(vector_stack_label);
  // devec with unique tag
  DEVEC(devec_1);

  asm volatile("fence\n\t");

  asm("trillium vissue_delim return scalar_return"); //return delimiter, delimiters can be of many types
  return;

  init_label: //this name matches with vissue label name
    asm("trillium glue_point init"); //name over here "init" matches with delimiter in vector code
  update_graph_label:
    asm("trillium glue_point update_graph");
  vector_stack_label: 
    asm("trillium glue_point vector_stack");

  #elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next init");

  volatile int bh1;

  int tid = start+vtid;

  do {
    asm("trillium vissue_delim until_next update_graph");
    int cond = (h_updating_graph_mask[tid] == true);
    volatile int compiler_hack = 1;
    PRED_EQ(cond,1);
    if(compiler_hack){
      h_graph_mask[tid]=true;
      h_graph_visited[tid]=true;
      *stop=true;
      h_updating_graph_mask[tid]=false;
    }
    PRED_EQ(vtid,vtid);
    tid+=VEC_LEN;
  }while(bh1);

  asm("trillium vissue_delim return vector_stack"); //return delimiter
  return;
  #endif

}

#endif
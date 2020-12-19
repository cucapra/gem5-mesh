#include <stdio.h>
#include <stdlib.h>

#include "pthread_launch.h"
#include "bfs.h"
#include "spad.h"
#include "bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"
#include "util.h"

#include "bfs_kernel.h"


void __attribute__((optimize("-fno-inline")))
bfs_manycore1(Node *h_graph_nodes, int *h_graph_edges, char *h_graph_mask, char *h_updating_graph_mask, \
                 char *h_graph_visited, int *h_cost, int start, int end, int ptid, int max_edges)
{
  for (int tid = start; tid < end; tid++){
    int cond1 = (h_graph_mask[tid] == true);
    if (cond1!=0){ 
      h_graph_mask[tid]=false;

      int i=h_graph_nodes[tid].starting;
      int edge_bound = (h_graph_nodes[tid].no_of_edges + h_graph_nodes[tid].starting);
      // printf("i: %d, edge_bound: %d\n", i,edge_bound);
      
      for(int j=0; j<max_edges; j++){
        int cond2 = (i<edge_bound);
        cond2 = cond1 & cond2;
        if (cond2!=0){
          int id = h_graph_edges[i];
          int cond3 = (!h_graph_visited[id]);
          cond3 = cond3 & cond2;
          if(cond3!=0){
            h_cost[id]=h_cost[tid]+1;
            h_updating_graph_mask[id]=true;
          }
        }
        i++;
      }
      // for(int i=h_graph_nodes[tid].starting; i<(h_graph_nodes[tid].no_of_edges + h_graph_nodes[tid].starting); i++)
      // {
      //     int id = h_graph_edges[i];
      //     if(!h_graph_visited[id])
      //     {
      //         h_cost[id]=h_cost[tid]+1;
      //         h_updating_graph_mask[id]=true;
      //     }
      // }
    }
  }
  // if (ptid==0) printf("End of first kernel\n");
}

void __attribute__((optimize("-fno-inline")))
bfs_manycore2(char *h_graph_mask, char *h_updating_graph_mask, char *h_graph_visited, char *stop, int start, int end, int ptid)
{
  for (int tid = start; tid < end; tid++){
    if (h_updating_graph_mask[tid] == true) {
      h_graph_mask[tid]=true;
      h_graph_visited[tid]=true;
      *stop=true;
      h_updating_graph_mask[tid]=false;
    }
  }
  // if (ptid==0) printf("End of second kernel\n");
}

int find_max_edges(Node* h_graph_nodes, int total_nodes){
  int max=0;
  for (int i = 0; i < total_nodes; i++){
    if (h_graph_nodes[i].no_of_edges>max){
      max = h_graph_nodes[i].no_of_edges;
    }
  }
  return max;
}

void __attribute__((optimize("-fno-inline")))
bfs_main(core_config_info_t cinfo, int mask, Node* h_graph_nodes, char *h_graph_mask, char *h_updating_graph_mask, 
    char *h_graph_visited, int* h_graph_edges, int* h_cost, int no_of_nodes, int edge_list_size, char *stop,
    int start, int end, int ptid, int pdim, int pdim_x, template_info_t tinfo){

    // #ifdef _VEC
    int max_edges = find_max_edges(h_graph_nodes,no_of_nodes);
    // if (ptid==0) printf("Max edges in the graph: %d \n",max_edges);
    // #endif
    int k=0;
    // pthread_barrier_wait(&start_barrier);
    do
    {
      k++;
      //if no thread changes this value then the loop stops
      if (ptid==0) *stop=false;
      // pthread_barrier_wait(&start_barrier);
      // if (ptid==0) printf("Starting round: %d\n",k);

      #ifdef _VEC
      SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
      if (cinfo.used) {
        tril_bfs_vec1(mask, h_graph_nodes, h_graph_edges, h_graph_mask, h_updating_graph_mask,
                 h_graph_visited, h_cost, max_edges, start, end, cinfo.vtid);
      }
      #else
      bfs_manycore1(h_graph_nodes, h_graph_edges, h_graph_mask, h_updating_graph_mask, h_graph_visited, h_cost, start, end, ptid, max_edges);
      #endif

      pthread_barrier_wait(&start_barrier);
      // if (ptid==0){
      //   for (int i=0; i<no_of_nodes; i++){
      //     printf("%d ",h_cost[i]);
      //   }
      //   printf("\n");
      // }
      // pthread_barrier_wait(&start_barrier);

      #ifdef _VEC
      SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
      if (cinfo.used) {
        tril_bfs_vec2(mask, h_graph_mask, h_updating_graph_mask, h_graph_visited, stop, start, end, cinfo.vtid);
      }
      #else
      bfs_manycore2(h_graph_mask, h_updating_graph_mask, h_graph_visited, stop, start, end, ptid);
      #endif

      pthread_barrier_wait(&start_barrier);

    }while(*stop);
    // if (ptid==0) printf("Total rounds %d\n",k);

}


void kernel(Node* h_graph_nodes, char *h_graph_mask, char *h_updating_graph_mask, char *h_graph_visited, int* h_graph_edges,
    int* h_cost, int no_of_nodes, int edge_list_size, char *stop, int ptid_x, int ptid_y, int pdim_x, int pdim_y)
{

  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0) {
    stats_on();
  }

  // linearize tid and dim
  int ptid = ptid_x + ptid_y * pdim_x;
  int pdim = pdim_x * pdim_y;

  int start = 0;
  int end   = 0;

  template_info_t tinfo;

  #ifdef _VEC
  #if VEC_LEN==4
  tinfo = init_template_4x4_2x2();
  #elif VEC_LEN==16
  tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  if(cinfo.used){
    //do work division here
    int alignment = VEC_LEN; //each group should have elements of multiple of this number
    start = roundUp((cinfo.unique_id + 0) * no_of_nodes / cinfo.total_groups, alignment); 
    end = roundUp((cinfo.unique_id + 1) * no_of_nodes / cinfo.total_groups, alignment); 
    
    // if(cinfo.is_scalar==1) printf("ptid:%d, start=%d and end=%d\n",ptid,start,end);
  }

  #else
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);

  //do work division here
  start  = ( ( cinfo.unique_id + 0 ) * no_of_nodes ) / cinfo.total_groups;
  end    = ( ( cinfo.unique_id + 1 ) * no_of_nodes ) / cinfo.total_groups;
  #endif

  // get behavior of each core
  #ifdef _VEC
  int mask = getSIMDMask(&cinfo);
  // int mask = getDebugMask(&cinfo);
  #else
  int mask = 0;
  #endif

  // if (ptid==0) printf("Work division done, mask calculated \n");

  // move stack onto scratchpad for faster local access than default on DRAM
  // MOVE_STACK_ONTO_SCRATCHPAD();
  unsigned long long *spTop = getSpTop(ptid); 
  spTop -= 120;                                
  unsigned long long stackLoc;                
  unsigned long long temp;                    
  _Pragma("GCC unroll(120)")                   
  for(int i=0;i<120;i++){                      
    asm volatile("ld t0, %[id](sp)\n\t"       
                "sd t0, %[id](%[spad])\n\t"   
                : "=r"(temp)                  
                : [id] "i"(i*8), [spad] "r"(spTop) : "memory"); 
  }                                                   
  asm volatile (                                      
      "addi %[dest], sp, 0\n\t"                       
      "addi sp, %[spad], 0\n\t"                       
      : [ dest ] "=r"(stackLoc)                       
      : [ spad ] "r"(spTop));

  bfs_main(cinfo, mask, h_graph_nodes, h_graph_mask, h_updating_graph_mask, h_graph_visited, h_graph_edges, h_cost, 
    no_of_nodes, edge_list_size, stop, start, end, ptid, pdim, pdim_x, tinfo);

  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();
}

// helper functions
Kern_Args *construct_args(Node* h_graph_nodes, char *h_graph_mask, char *h_updating_graph_mask, char *h_graph_visited, int* h_graph_edges,
                          int* h_cost, int no_of_nodes, int edge_list_size, char *stop, int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->h_graph_nodes = h_graph_nodes;
  args->h_graph_mask = h_graph_mask;
  args->h_updating_graph_mask = h_updating_graph_mask;
  args->h_graph_visited = h_graph_visited;
  args->h_graph_edges = h_graph_edges;
  args->h_cost = h_cost;
  args->no_of_nodes = no_of_nodes;
  args->edge_list_size = edge_list_size;
  args->stop = stop;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;

  return args;
}

void *pthread_kernel(void *args)
{
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early

  pthread_barrier_wait(&start_barrier);

  // call the spmd kernel
  Kern_Args *a = (Kern_Args *)args;

  kernel(a->h_graph_nodes, a->h_graph_mask, a->h_updating_graph_mask, a->h_graph_visited, a->h_graph_edges,
         a->h_cost, a->no_of_nodes, a->edge_list_size, a->stop, a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}

#ifndef __BFS_H__
#define __BFS_H__

#include <stdbool.h> 


// #define VEC_LEN 4
#ifdef VEC_LEN
#define _VEC
#endif


#define REGION_SIZE 8 //configure using LCM of required frame/region sizes
#define NUM_REGIONS (256 / REGION_SIZE) // (0,512) in this case is the hardware region area 

//Structure to hold a node information
typedef struct Node
{
	int starting;
	int no_of_edges;
} Node;

// pthread argument for the kernel
typedef struct Kern_Args
{
  Node* h_graph_nodes;
  char *h_graph_mask, *h_updating_graph_mask, *h_graph_visited;
  int *h_graph_edges, *h_cost;
  int no_of_nodes,edge_list_size;
  char *stop;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    Node* h_graph_nodes, char *h_graph_mask, char *h_updating_graph_mask, char *h_graph_visited, int* h_graph_edges, int* h_cost,
    int no_of_nodes, int edge_list_size, char *stop, int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    Node* h_graph_nodes, char *h_graph_mask, char *h_updating_graph_mask, char *h_graph_visited, int* h_graph_edges, int* h_cost,
    int no_of_nodes, int edge_list_size, char *stop, int tid_x, int tid_y, int dim_x, int dim_y);

#endif

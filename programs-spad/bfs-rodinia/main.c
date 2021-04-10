#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "bfs.h"

void run_bfs_cpu(int no_of_nodes, Node *h_graph_nodes, int edge_list_size, \
                 int *h_graph_edges, char *h_graph_mask, char *h_updating_graph_mask, \
                 char *h_graph_visited, int *h_cost_ref)
{
    char stop;
    int k = 0;
    do {
        //if no thread changes this value then the loop stops
        stop=false;
        for(int tid = 0; tid < no_of_nodes; tid++ ) {
            if (h_graph_mask[tid] == true) {
                h_graph_mask[tid]=false;
                for(int i=h_graph_nodes[tid].starting; i<(h_graph_nodes[tid].no_of_edges + h_graph_nodes[tid].starting); i++) {
                    int id = h_graph_edges[i];	//--cambine: node id is connected with node tid
                    if(!h_graph_visited[id]) {	//--cambine: if node id has not been visited, enter the body below
                        h_cost_ref[id]=h_cost_ref[tid]+1;
                        h_updating_graph_mask[id]=true;
                    }
                }
            }
        }

        // for (int i=0; i<no_of_nodes; i++){
        //   printf("%d ",h_cost_ref[i]);
        // }
        // printf("\n");

        for(int tid=0; tid< no_of_nodes ; tid++ ) {
            if (h_updating_graph_mask[tid] == true) {
                h_graph_mask[tid]=true;
                h_graph_visited[tid]=true;
                stop=true;
                h_updating_graph_mask[tid]=false;
            }
        }
        k++;
    } while(stop);
    // printf("single core CPU: Total rounds %d\n",k);
}

int compare_results(int *h_cost_ref, int *h_cost, int n){
  // printf("Ref values\n");
  // for (int i=0; i<n; i++){
  //   printf("%d ",h_cost_ref[i]);
  // }
  // printf("\n");

  // printf("Manycore values\n");
  // for (int i=0; i<n; i++){
  //   printf("%d ",h_cost[i]);
  // }
  // printf("\n");

  for (int i=0; i<n; i++){
    if (h_cost_ref[i]!=h_cost[i]){
      printf("[[FAIL]] for bfs\n");
      return 1;
    }
  }
  return 0;
}


int main(int argc, char *argv[])
{

  /*--------------------------------------------------------------------
   * Setup scratchpads
   *------------------------------------------------------------------*/

  printf("starting\n");

  initScratchpads();

  /*--------------------------------------------------------------------
  * Get info about manycore
  *-------------------------------------------------------------------*/

  int cores_x, cores_y;
  int num_cores = get_dimensions(&cores_x, &cores_y);

  /*--------------------------------------------------------------------
  * Put the command line arguments into variables
  *-------------------------------------------------------------------*/

  // default values
  int no_of_nodes = 0;
  int edge_list_size = 0;
  char *input_f;

  // parse positional arguments
  if (argc > 1)
    input_f = argv[1];

  printf("Reading File\n");

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  //Read in Graph from a file
  FILE *fp;
	fp = fopen(input_f,"r");
	if(!fp)
	{
		printf("Error Reading graph file\n");
		return 0;
	}

	int source = 0;

	fscanf(fp,"%d",&no_of_nodes);
   
	// allocate host memory
	Node* h_graph_nodes = (Node*) malloc(sizeof(Node)*no_of_nodes);
	char *h_graph_mask = (char*) malloc(sizeof(char)*no_of_nodes);
	char *h_updating_graph_mask = (char*) malloc(sizeof(char)*no_of_nodes);
	char *h_graph_visited = (char*) malloc(sizeof(char)*no_of_nodes);

	int start, edgeno;   
	// initalize the memory
	for( unsigned int i = 0; i < no_of_nodes; i++) 
	{
		fscanf(fp,"%d %d",&start,&edgeno);
		h_graph_nodes[i].starting = start;
		h_graph_nodes[i].no_of_edges = edgeno;
		h_graph_mask[i]=false;
		h_updating_graph_mask[i]=false;
		h_graph_visited[i]=false;
	}

	//read the source node from the file
	fscanf(fp,"%d",&source);
	// source=0; //tesing code line

	//set the source node as true in the mask
	h_graph_mask[source]=true;
	h_graph_visited[source]=true;

	fscanf(fp,"%d",&edge_list_size);

	int id,cost;
	int* h_graph_edges = (int*) malloc(sizeof(int)*edge_list_size);
	for(int i=0; i < edge_list_size ; i++)
	{
		fscanf(fp,"%d",&id);
		fscanf(fp,"%d",&cost);
		h_graph_edges[i] = id;
	}

	if(fp)
		fclose(fp);    


	// allocate mem for the result on host side
	int* h_cost = (int*) malloc( sizeof(int)*no_of_nodes);
  int *h_cost_ref = (int*)malloc(sizeof(int)*no_of_nodes);
	for(int i=0;i<no_of_nodes;i++){
		h_cost[i]=-1;
    h_cost_ref[i] = -1;
  }
	h_cost[source]=0;
  h_cost_ref[source]=0;

  char stop;

  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args **)malloc(sizeof(Kern_Args *) * num_cores);

  for (int y = 0; y < cores_y; y++)
  {
    for (int x = 0; x < cores_x; x++)
    {
      int i = x + y * cores_x;
      kern_args[i] = construct_args(h_graph_nodes, h_graph_mask, h_updating_graph_mask, h_graph_visited,
                                    h_graph_edges, h_cost, no_of_nodes, edge_list_size, &stop, x, y, cores_x, cores_y);
    }
  }

  /*--------------------------------------------------------------------
  * Run the kernel
  *-------------------------------------------------------------------*/

  printf("Begin kernel on %d cores\n", num_cores);
  printf("Cores x:%d Cores y:%d\n", cores_x, cores_y);
  launch_kernel(pthread_kernel, (void **)kern_args, cores_x, cores_y);


  //Store the result into a file
	FILE *fpo = fopen("result.txt","w");
	for(int i=0;i<no_of_nodes;i++)
		fprintf(fpo,"%d) cost:%d\n",i,h_cost[i]);
	fclose(fpo);
	printf("Result stored in result.txt\n");
/*--------------------------------------------------------------------
  * Check result and cleanup data
  *-------------------------------------------------------------------*/
  
  // initalize the memory again
  for(int i = 0; i < no_of_nodes; i++) {
      h_graph_mask[i]=false;
      h_updating_graph_mask[i]=false;
      h_graph_visited[i]=false;
  }
  //set the source node as true in the mask
  h_graph_mask[source]=true;
  h_graph_visited[source]=true;
  
  run_bfs_cpu(no_of_nodes,h_graph_nodes,edge_list_size,h_graph_edges, h_graph_mask, h_updating_graph_mask, h_graph_visited, h_cost_ref);
  
  int fail;
  fail = compare_results(h_cost_ref, h_cost, no_of_nodes);
  if (fail)
    return 1;

  free( h_graph_nodes);
	free( h_graph_edges);
	free( h_graph_mask);
	free( h_updating_graph_mask);
	free( h_graph_visited);
	free( h_cost);
  printf("[[SUCCESS]]\n");
  return 0;
}

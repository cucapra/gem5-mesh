#ifndef __TEMP_KERNEL_H__
#define __TEMP_KERNEL_H__

#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "bfs.h"
#include "spad.h"
#include "bind_defs.h"

void tril_bfs_vec1(int mask, Node *h_graph_nodes, int *h_graph_edges, char *h_graph_mask, char *h_updating_graph_mask, \
                 char *h_graph_visited, int *h_cost, int max_edges, int start, int end, int vtid);

void tril_bfs_vec2(int mask, char *h_graph_mask, char *h_updating_graph_mask, char *h_graph_visited, char *stop, \
                 int start, int end, int vtid);

#endif

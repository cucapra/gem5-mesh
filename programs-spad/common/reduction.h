#ifndef __REDUCTION_H__
#define __REDUCTION_H__

#include "token_queue.h"
#include "group_templates.h"

void setup_reduction(token_queue_t *cons0, token_queue_t *cons1, token_queue_t *prod,
    int spmOffset, int bufSize, int ptid, int pdim_x, int pdim_y, int is_vec, core_config_info_t *cinfo, template_info_t *tinfo);

void reduce_vector_manycore(int* partialVec, int*c, int ptid, int activeTid, int dim, token_queue_t *cons0, 
    token_queue_t *cons1, token_queue_t *prod, int numElements, int token_len);

int get_reduction_dest_manycore(int src_id);

// a little more complicated to get when there's vector groups
int get_reduction_dest_vector(template_info_t *tinfo, int group_id, int vid_x, int vid_y, int virt_dim_x, int phys_dim_x, int *active_tid);




#endif
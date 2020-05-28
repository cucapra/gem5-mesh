#ifndef __GROUP_TEMPLATES_H__
#define __GROUP_TEMPLATES_H__

#include <stdio.h>

// ---------------------------------------------------------------------------------------------------
// structs to represent groups/set of gropus
// ---------------------------------------------------------------------------------------------------

// information required to define a rectangular group
typedef struct group_info_t {
  int scalar_x;
  int scalar_y;
  int vector_start_x;
  int vector_start_y;
  int vector_dim_x;
  int vector_dim_y;
} group_info_t;

// information about a template that can "stamped" multiple times over a larger mesh
#define MAX_TEMPLATE_GROUPS 8
typedef struct template_info_t {
  int num_groups_in_template;
  int template_dim_x;
  int template_dim_y;
  group_info_t groups[MAX_TEMPLATE_GROUPS];
} template_info_t;

// group constructor
group_info_t init_group_info(int scalar_x, int scalar_y, int vector_start_x, int vector_start_y, int vector_dim_x, int vector_dim_y) {
  group_info_t ginfo;
  ginfo.scalar_x = scalar_x;
  ginfo.scalar_y = scalar_y;
  ginfo.vector_start_x = vector_start_x;
  ginfo.vector_start_y = vector_start_y;
  ginfo.vector_dim_x = vector_dim_x;
  ginfo.vector_dim_y = vector_dim_y;
  return ginfo;
}

// based on the given template and group id return information about the groupo
group_info_t get_group_info(int group_id, template_info_t *tinfo) {
  int idx = group_id % tinfo->num_groups_in_template;
  return tinfo->groups[idx];
}


// ---------------------------------------------------------------------------------------------------
// specify the template here
// ---------------------------------------------------------------------------------------------------

// create a template for a 4x4 mesh area with 2x2 vector groups
// NOTE c copies struct by values so don't have to return pointer
template_info_t init_template_4x4_2x2() {
  template_info_t tinfo;
  tinfo.num_groups_in_template = 3;
  tinfo.template_dim_x = 4;
  tinfo.template_dim_y = 4;
  tinfo.groups[0] = init_group_info(0, 0, 1, 0, 2, 2);
  tinfo.groups[1] = init_group_info(0, 1, 0, 2, 2, 2);
  tinfo.groups[2] = init_group_info(3, 1, 2, 2, 2, 2);
  if (_N_SPS < tinfo.template_dim_x * tinfo.template_dim_y) printf("[[WARNING]] trying to instantiate template bigger than mesh\n");
  return tinfo;
}

template_info_t init_template_8x8_4x4() {
  template_info_t tinfo;
  tinfo.num_groups_in_template = 3;
  tinfo.template_dim_x = 8;
  tinfo.template_dim_y = 8;
  tinfo.groups[0] = init_group_info(0, 4, 0, 0, 4, 4);
  tinfo.groups[1] = init_group_info(7, 4, 4, 0, 4, 4);
  tinfo.groups[2] = init_group_info(1, 4, 2, 4, 4, 4);
  if (_N_SPS < tinfo.template_dim_x * tinfo.template_dim_y) printf("[[WARNING]] trying to instantiate template bigger than mesh\n");
  return tinfo;
}

// ---------------------------------------------------------------------------------------------------
// figuring out config based on group layout
// ---------------------------------------------------------------------------------------------------

// design a rectangular vector group with an attached scalar core
inline void rect_vector_group(
    int group_id, int scalar_x, int scalar_y, int vector_start_x, int vector_start_y, int vector_dim_x, int vector_dim_y, int id_x, int id_y,
    int template_offset,
    int *vtid, int *vtid_x, int *vtid_y, int *is_scalar, int *orig_x, int *orig_y, int *master_x, int *master_y, int *used, int *unique_id) {
  
  int vector_end_x = vector_start_x + vector_dim_x;
  int vector_end_y = vector_start_y + vector_dim_y;

  int is_vector_group = id_x >= vector_start_x && id_x < vector_end_x && 
    id_y >= vector_start_y && id_y < vector_end_y;

  int is_scalar_group = id_x == scalar_x && id_y == scalar_y;
  if (is_vector_group) {
    *vtid_x = id_x - vector_start_x;
    *vtid_y = id_y - vector_start_y;
    *vtid   = *vtid_x + *vtid_y * vector_dim_x;
  }
  if (is_scalar_group) {
    *is_scalar = 1;
  }
  if (is_vector_group || is_scalar_group) {
    // *start = roundUp((chunk_offset + group_num + 0) * n / vGroups, alignment);
    // *end   = roundUp((chunk_offset + group_num + 1) * n / vGroups, alignment); // make sure aligned to cacheline 
    *orig_x = vector_start_x;
    *orig_y = vector_start_y;
    *master_x = scalar_x;
    *master_y = scalar_y;
    *used = 1;
    *unique_id = template_offset + group_id;
  }
}

inline int vector_group_template(
    // inputs
    int ptid_x, int ptid_y, int pdim_x, int pdim_y, template_info_t *tinfo,
    // outputs
    int *vtid, int *vtid_x, int *vtid_y, int *is_scalar, int *orig_x, int *orig_y, int *master_x, int *master_y,
    int *unique_id, int *total_groups
  ) {

  // keep track of which cores will be used in this configuration
  // will want to terminate any cores not apart of a vector group
  int used = 0;

  // recover trivial fields
  int ptid = ptid_x + ptid_y * pdim_x;
  int pdim = pdim_x * pdim_y;

  // figure out how many times we need to stamp this template
  int template_dim_x = tinfo->template_dim_x;
  int template_dim_y = tinfo->template_dim_y;
  int template_dim = template_dim_x * template_dim_y;
  int template_id_x = ptid_x % template_dim_x;
  int template_id_y = ptid_y % template_dim_y;
  int template_id = template_id_x + template_id_y * template_dim_x;

  // which group it belongs to for absolute core coordinates
  int template_group_x = ptid_x / template_dim_x;
  int template_group_y = ptid_y / template_dim_y;
  int template_group_dim_x = pdim_x / template_dim_x;
  int template_group_dim_y = pdim_y / template_dim_y;
  int template_group_dim = template_group_dim_x * template_group_dim_y;
  int template_group = template_group_x + template_group_y * template_group_dim_x;

  // used to determine a unique group id
  int groups_per_template = tinfo->num_groups_in_template;
  *total_groups = groups_per_template * template_group_dim;

  int template_offset = template_group  * groups_per_template;

  // figure out the configurations for core within the groups
  for (int i = 0; i < *total_groups; i++) {
    group_info_t ginfo = get_group_info(i, tinfo);
    rect_vector_group(i, ginfo.scalar_x, ginfo.scalar_y, ginfo.vector_start_x, ginfo.vector_start_y,
      ginfo.vector_dim_x, ginfo.vector_dim_y, template_id_x, template_id_y, template_offset,
      vtid, vtid_x, vtid_y, is_scalar, orig_x, orig_y, master_x, master_y, &used, unique_id);
  }

  // need to shift the absolute coordinates based on which group this is for
  *orig_x = *orig_x + template_group_x * template_dim_x;
  *orig_y = *orig_y + template_group_y * template_dim_y;
  *master_x = *master_x + template_group_x * template_dim_x;
  *master_y = *master_y + template_group_y * template_dim_y;

  return used;
  
}


// if don't inline then need to copy stack pointer up to addr 88, which too lazy to do atm
// create a template for a vlen=4 config that can copy and paste multiple times on a large mesh
// ret whether core used in template
inline int vector_group_template_4(
    // inputs
    int ptid_x, int ptid_y, int pdim_x, int pdim_y,
    // outputs
    int *vtid, int *vtid_x, int *vtid_y, int *is_scalar, int *orig_x, int *orig_y, int *master_x, int *master_y,
    int *unique_id, int *total_groups
  ) {

  template_info_t tinfo = init_template_4x4_2x2();
  return vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo, 
            vtid, vtid_x, vtid_y, is_scalar, orig_x, orig_y, master_x, master_y, unique_id, total_groups);
  
}

inline int vector_group_template_16(
    // inputs
    int ptid_x, int ptid_y, int pdim_x, int pdim_y,
    // outputs
    int *vtid, int *vtid_x, int *vtid_y, int *is_scalar, int *orig_x, int *orig_y, int *master_x, int *master_y,
    int *unique_id, int *total_groups
  ) {

  template_info_t tinfo = init_template_8x8_4x4();
  return vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo, 
            vtid, vtid_x, vtid_y, is_scalar, orig_x, orig_y, master_x, master_y, unique_id, total_groups);
  
}

// ---------------------------------------------------------------------------------------------------
// helper to figure out where cores in other vector groups are
// ---------------------------------------------------------------------------------------------------

// TODO need to define this for every group size
void group_id_to_origin(int group_id, int *x, int *y) {
  #ifdef VECTOR_LEN
  #if VECTOR_LEN==4
  template_info_t tinfo = init_template_4x4_2x2(); // TODO use this for figuring out tid
  #endif
  group_info_t ginfo = get_group_info(group_id, &tinfo);
  *x = ginfo.vector_start_x;
  *y = ginfo.vector_start_y;
  #endif
}

int get_ptid_from_group(int group_id, int vid_x, int vid_y, int phys_dim_x) {
  int x,y;
  group_id_to_origin(group_id, &x, &y);
  x += vid_x;
  y += vid_y;
  return y * phys_dim_x + x;
}

#endif
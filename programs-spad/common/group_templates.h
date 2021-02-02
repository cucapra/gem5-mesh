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
group_info_t init_group_info(int scalar_x, int scalar_y, int vector_start_x, int vector_start_y, int vector_dim_x, int vector_dim_y);

// based on the given template and group id return information about the groupo
group_info_t get_group_info(int group_id, template_info_t *tinfo);

// get the offset of this due to being a stamp of template beyond the first
void get_template_offset(int group_id, template_info_t *tinfo, int *offset_x, int *offset_y);

// information about the configuration for a core in group
// this information is enough to create a core mask (and a little extra metadata)
typedef struct core_config_info_t {
  // whether this core is used or not
  int used;
  // virtual tid and dimensions within the gorup
  int vtid;
  int vtid_x;
  int vtid_y;
  int vdim_x;
  int vdim_y;
  // whether this is a scalar core
  int is_scalar;
  // pointers to scalar core and first vector core
  int orig_x;
  int orig_y;
  int master_x;
  int master_y;
  // info about which group this is relative to all vector groups
  int unique_id;
  int total_groups;
} core_config_info_t;

void reset_core_config_info(core_config_info_t *cinfo);


// ---------------------------------------------------------------------------------------------------
// specify the template here
// ---------------------------------------------------------------------------------------------------

// create a template for a 4x4 mesh area with 2x2 vector groups
// NOTE c copies struct by values so don't have to return pointer
template_info_t init_template_4x4_2x2();

template_info_t init_template_8x8_4x4();

template_info_t init_template_debug();

// ---------------------------------------------------------------------------------------------------
// figuring out config based on group layout
// ---------------------------------------------------------------------------------------------------

// design a rectangular vector group with an attached scalar core
inline void rect_vector_group(
    int group_id, int scalar_x, int scalar_y, int vector_start_x, int vector_start_y, int vector_dim_x, int vector_dim_y, int id_x, int id_y,
    core_config_info_t *info) {
  
  int vector_end_x = vector_start_x + vector_dim_x;
  int vector_end_y = vector_start_y + vector_dim_y;

  int is_vector_group = id_x >= vector_start_x && id_x < vector_end_x && 
    id_y >= vector_start_y && id_y < vector_end_y;

  int is_scalar_group = id_x == scalar_x && id_y == scalar_y;
  if (is_vector_group) {
    info->vtid_x = id_x - vector_start_x;
    info->vtid_y = id_y - vector_start_y;
    info->vtid   = info->vtid_x + info->vtid_y * vector_dim_x;
  }
  if (is_scalar_group) {
    info->is_scalar = 1;
  }
  if (is_vector_group || is_scalar_group) {
    // *start = roundUp((chunk_offset + group_num + 0) * n / vGroups, alignment);
    // *end   = roundUp((chunk_offset + group_num + 1) * n / vGroups, alignment); // make sure aligned to cacheline 
    info->orig_x = vector_start_x;
    info->orig_y = vector_start_y;
    info->master_x = scalar_x;
    info->master_y = scalar_y;
    info->used = 1;
    info->unique_id = group_id;
    info->vdim_x = vector_dim_x;
    info->vdim_y = vector_dim_y;
  }
}

// for parity make a manycore config as well
inline core_config_info_t manycore_template(
    // inputs
    int ptid_x, int ptid_y, int pdim_x, int pdim_y
  ) {

  core_config_info_t cinfo;
  reset_core_config_info(&cinfo);

  cinfo.is_scalar = 0;
  cinfo.vtid = 0;
  cinfo.vtid_x = 0;
  cinfo.vtid_y = 0;
  cinfo.vdim_x = 1;
  cinfo.vdim_y = 1;
  cinfo.used = 1;
  cinfo.orig_x = ptid_x;
  cinfo.orig_y = ptid_y;
  cinfo.unique_id = ptid_x + ptid_y * pdim_x;
  cinfo.total_groups = pdim_x * pdim_y;

  return cinfo;
}

inline core_config_info_t vector_group_template(
    // inputs
    int ptid_x, int ptid_y, int pdim_x, int pdim_y, template_info_t *tinfo
  ) {

  core_config_info_t cinfo;
  reset_core_config_info(&cinfo);

  // keep track of which cores will be used in this configuration
  // will want to terminate any cores not apart of a vector group
  cinfo.used = 0;

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
  cinfo.total_groups = groups_per_template * template_group_dim;

  int template_offset = template_group * groups_per_template;

  // figure out the configurations for core within the groups
  for (int i = 0; i < groups_per_template; i++) {
    group_info_t ginfo = get_group_info(i, tinfo);
    rect_vector_group(i, ginfo.scalar_x, ginfo.scalar_y, ginfo.vector_start_x, ginfo.vector_start_y,
      ginfo.vector_dim_x, ginfo.vector_dim_y, template_id_x, template_id_y,
      &cinfo);
  }

  cinfo.unique_id += template_offset;

  // need to shift the absolute coordinates based on which group this is for
  cinfo.orig_x   = cinfo.orig_x + template_group_x * template_dim_x;
  cinfo.orig_y   = cinfo.orig_y + template_group_y * template_dim_y;
  cinfo.master_x = cinfo.master_x + template_group_x * template_dim_x;
  cinfo.master_y = cinfo.master_y + template_group_y * template_dim_y;

  return cinfo;
  
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
  // printf("DEPRECATED use of vector_group_template_4() use vector_group_template()\n");
  template_info_t tinfo = init_template_4x4_2x2();
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  *vtid = cinfo.vtid;
  *vtid_x = cinfo.vtid_x;
  *vtid_y = cinfo.vtid_y;
  *is_scalar = cinfo.is_scalar;
  *orig_x = cinfo.orig_x;
  *orig_y = cinfo.orig_y;
  *master_x = cinfo.master_x;
  *master_y = cinfo.master_y;
  *unique_id = cinfo.unique_id;
  *total_groups = cinfo.total_groups;

  return cinfo.used;
  
}

inline int vector_group_template_16(
    // inputs
    int ptid_x, int ptid_y, int pdim_x, int pdim_y,
    // outputs
    int *vtid, int *vtid_x, int *vtid_y, int *is_scalar, int *orig_x, int *orig_y, int *master_x, int *master_y,
    int *unique_id, int *total_groups
  ) {
  // printf("DEPRECATED use of vector_group_template_16() use vector_group_template()\n");
  template_info_t tinfo = init_template_8x8_4x4();
  core_config_info_t cinfo =  vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);
  
  *vtid = cinfo.vtid;
  *vtid_x = cinfo.vtid_x;
  *vtid_y = cinfo.vtid_y;
  *is_scalar = cinfo.is_scalar;
  *orig_x = cinfo.orig_x;
  *orig_y = cinfo.orig_y;
  *master_x = cinfo.master_x;
  *master_y = cinfo.master_y;
  *unique_id = cinfo.unique_id;
  *total_groups = cinfo.total_groups;

  return cinfo.used;
}

// ---------------------------------------------------------------------------------------------------
// helper to figure out where cores in other vector groups are
// ---------------------------------------------------------------------------------------------------

// TODO need to define this for every group size
void group_id_to_origin(template_info_t *tinfo, int group_id, int *x, int *y);

void group_id_to_scalar(template_info_t *tinfo, int group_id, int *x, int *y);

int get_ptid_from_group(template_info_t *tinfo, int group_id, int vid_x, int vid_y, int phys_dim_x);


inline int get_vtid(core_config_info_t *cinfo) {
  return cinfo->vtid_x + cinfo->vtid_y * cinfo->vdim_x;
}

inline int get_vdim(core_config_info_t *cinfo) {
  return cinfo->vdim_x * cinfo->vdim_y;
}

#endif
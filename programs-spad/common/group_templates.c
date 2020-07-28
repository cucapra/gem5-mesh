#include "group_templates.h"
#include <math.h>

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

// get the offset of this due to being a stamp of template beyond the first
void get_template_offset(int group_id, template_info_t *tinfo, int *offset_x, int *offset_y) {
  int template_offset = group_id / tinfo->num_groups_in_template;
  int template_dim_x = tinfo->template_dim_x;
  int template_dim_y = tinfo->template_dim_y;

  int grid_x = sqrtf(_N_SPS);
  int grid_y = sqrtf(_N_SPS);

  int num_temp_x = grid_x/template_dim_x;
  int num_temp_y = grid_y/template_dim_y;
  
  int template_offset_x = template_offset % num_temp_x;
  int template_offset_y = template_offset / num_temp_x;
  *offset_x = template_offset_x * template_dim_x;
  *offset_y = template_offset_y * template_dim_y;
}

void reset_core_config_info(core_config_info_t *cinfo) {
  cinfo->used = 0;
  cinfo->vtid = 0;
  cinfo->vtid_x = 0;
  cinfo->vtid_y = 0;
  cinfo->vdim_x = 0;
  cinfo->vdim_y = 0;
  cinfo->is_scalar = 0;
  cinfo->orig_x = 0;
  cinfo->orig_y = 0;
  cinfo->master_x = 0;
  cinfo->master_y = 0;
  cinfo->unique_id = 0;
  cinfo->total_groups = 0;
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

// template used for debugging
// a single 4x4 group across the whole mesh
template_info_t init_template_debug() {
  template_info_t tinfo;
  tinfo.num_groups_in_template = 1;
  tinfo.template_dim_x = sqrtf(_N_SPS);
  tinfo.template_dim_y = sqrtf(_N_SPS);
  tinfo.groups[0] = init_group_info(0, 0, 1, 0, 2, 2);
  if (_N_SPS < tinfo.template_dim_x * tinfo.template_dim_y) printf("[[WARNING]] trying to instantiate template bigger than mesh\n");
  return tinfo;
}


// ---------------------------------------------------------------------------------------------------
// helper to figure out where cores in other vector groups are
// ---------------------------------------------------------------------------------------------------

// TODO need to define this for every group size
void group_id_to_origin(template_info_t *tinfo, int group_id, int *x, int *y) {
  group_info_t ginfo = get_group_info(group_id, tinfo);
  int offset_x, offset_y; // offset due to being in a template
  get_template_offset(group_id, tinfo, &offset_x, &offset_y);
  *x = ginfo.vector_start_x + offset_x;
  *y = ginfo.vector_start_y + offset_y;
}

int get_ptid_from_group(template_info_t *tinfo, int group_id, int vid_x, int vid_y, int phys_dim_x) {
  int x,y;
  group_id_to_origin(tinfo, group_id, &x, &y);
  x += vid_x;
  y += vid_y;
  return y * phys_dim_x + x;
}


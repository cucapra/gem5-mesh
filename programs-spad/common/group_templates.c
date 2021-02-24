#include "group_templates.h"
#include <math.h>

// group constructor
group_info_t init_group_info(int scalar_x, int scalar_y, int vector_start_x, int vector_start_y, int vector_dim_x, int vector_dim_y) {
  group_info_t ginfo;
  ginfo.scalar = init_core_coord(scalar_x, scalar_y);
  ginfo.vector_start = init_core_coord(vector_start_x, vector_start_y);
  ginfo.vector_dim = init_core_coord(vector_dim_x, vector_dim_y);
  return ginfo;
}

// extra core consturctor
extra_core_info_t init_extra_core_info(int x, int y, int affinity0, int affinity1, int affinity2) {
  extra_core_info_t einfo;
  einfo.coord = init_core_coord(x, y);
  einfo.group_affinity[0] = affinity0;
  einfo.group_affinity[1] = affinity1;
  einfo.group_affinity[2] = affinity2;
  return einfo;
}

// based on the given template and group id return information about the groupo
group_info_t get_group_info(int group_id, template_info_t *tinfo) {
  int idx = group_id % tinfo->num_groups_in_template;
  return tinfo->groups[idx];
}

// get the offset of this due to being a stamp of template beyond the first
void get_template_offset(int group_id, template_info_t *tinfo, int *offset_x, int *offset_y) {
  int template_offset = group_id / tinfo->num_groups_in_template;
  int template_dim_x = tinfo->template_dim.x;
  int template_dim_y = tinfo->template_dim.y;

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
  cinfo->vtid_flat = 0;
  cinfo->vtid = init_core_coord(0, 0);
  cinfo->vdim = init_core_coord(0, 0);
  cinfo->orig = init_core_coord(0, 0);
  cinfo->master = init_core_coord(0, 0);
  cinfo->is_scalar = 0;
  cinfo->unique_id = 0;
  cinfo->total_groups = 0;
  cinfo->num_next_cores = 0;
  cinfo->num_prev_cores = 0;
}

// ---------------------------------------------------------------------------------------------------
// specify the template here
// ---------------------------------------------------------------------------------------------------

// create a template for a 4x4 mesh area with 2x2 vector groups
// NOTE c copies struct by values so don't have to return pointer
template_info_t init_template_4x4_2x2() {
  template_info_t tinfo;
  tinfo.num_groups_in_template = 3;
  tinfo.template_dim = init_core_coord(4, 4);
  tinfo.groups[0] = init_group_info(0, 0, 1, 0, 2, 2);
  tinfo.groups[1] = init_group_info(0, 1, 0, 2, 2, 2);
  tinfo.groups[2] = init_group_info(3, 1, 2, 2, 2, 2);
  // set affinity (link id)
  tinfo.num_extra_in_template = 1;
  tinfo.extra[0] = init_extra_core_info(3, 0, 0, 1, 2);
  if (_N_SPS < tinfo.template_dim.x * tinfo.template_dim.y) printf("[[WARNING]] trying to instantiate template bigger than mesh\n");
  return tinfo;
}

template_info_t init_template_8x8_4x4() {
  template_info_t tinfo;
  tinfo.num_groups_in_template = 3;
  tinfo.template_dim = init_core_coord(8, 8);
  tinfo.groups[0] = init_group_info(0, 4, 0, 0, 4, 4);
  tinfo.groups[1] = init_group_info(7, 4, 4, 0, 4, 4);
  tinfo.groups[2] = init_group_info(1, 4, 2, 4, 4, 4);
  // declare extra core that can be used in software pipelining with the groups
  tinfo.num_extra_in_template = 3;
  tinfo.extra[0] = init_extra_core_info(0, 5, 0, -1, -1);
  tinfo.extra[1] = init_extra_core_info(1, 5, 1, -1, -1);
  tinfo.extra[2] = init_extra_core_info(6, 4, 2, -1, -1);
  if (_N_SPS < tinfo.template_dim.x * tinfo.template_dim.y) printf("[[WARNING]] trying to instantiate template bigger than mesh\n");
  return tinfo;
}

// template used for debugging
// a single 4x4 group across the whole mesh
template_info_t init_template_debug() {
  template_info_t tinfo;
  tinfo.num_groups_in_template = 1;
  tinfo.template_dim = init_core_coord(sqrtf(_N_SPS), sqrtf(_N_SPS));
  tinfo.groups[0] = init_group_info(0, 0, 1, 0, 2, 2);
  if (_N_SPS < tinfo.template_dim.x * tinfo.template_dim.y) printf("[[WARNING]] trying to instantiate template bigger than mesh\n");
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
  *x = ginfo.vector_start.x + offset_x;
  *y = ginfo.vector_start.y + offset_y;
}

void group_id_to_scalar(template_info_t *tinfo, int group_id, int *x, int *y) {
  group_info_t ginfo = get_group_info(group_id, tinfo);
  int offset_x, offset_y; // offset due to being in a template
  get_template_offset(group_id, tinfo, &offset_x, &offset_y);
  *x = ginfo.scalar.x + offset_x;
  *y = ginfo.scalar.y + offset_y;
}

int get_ptid_from_group(template_info_t *tinfo, int group_id, int vid_x, int vid_y, int phys_dim_x) {
  int x,y;
  group_id_to_origin(tinfo, group_id, &x, &y);
  x += vid_x;
  y += vid_y;
  return y * phys_dim_x + x;
}


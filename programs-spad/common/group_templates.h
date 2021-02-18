#ifndef __GROUP_TEMPLATES_H__
#define __GROUP_TEMPLATES_H__

#include <stdio.h>

// ---------------------------------------------------------------------------------------------------
// structs to represent groups/set of gropus
// ---------------------------------------------------------------------------------------------------
#define MAX_TEMPLATE_GROUPS 8
#define MAX_GROUP_AFFINITY 3

typedef struct core_coord_t {
  int x;
  int y;
} core_coord_t;

inline core_coord_t init_core_coord(int x, int y) {
  core_coord_t coords;
  coords.x = x;
  coords.y = y;
  return coords;
}

inline int coords_eq(core_coord_t a, core_coord_t b) {
  if (a.x == b.x && a.y == b.y)
    return 1;
  else
    return 0;
}

inline int get_flat_coord(core_coord_t a, int minor_dim) {
  return a.x + a.y * minor_dim;
}

inline core_coord_t coord_add(core_coord_t a, core_coord_t b) {
  core_coord_t c;
  c.x = a.x + b.x;
  c.y = a.y + b.y;
  return c;
}

inline core_coord_t coord_sub(core_coord_t a, core_coord_t b) {
  core_coord_t c;
  c.x = a.x - b.x;
  c.y = a.y - b.y;
  return c;
}

inline core_coord_t coord_mul(core_coord_t a, core_coord_t b) {
  core_coord_t c;
  c.x = a.x * b.x;
  c.y = a.y * b.y;
  return c ;
}

inline core_coord_t coord_div(core_coord_t a, core_coord_t b) {
  core_coord_t c;
  c.x = a.x / b.x;
  c.y = a.y / b.y;
  return c;
}

inline core_coord_t coord_mod(core_coord_t a, core_coord_t b) {
  core_coord_t c;
  c.x = a.x % b.x;
  c.y = a.y % b.y;
  return c;
}

inline int within_bounds(core_coord_t pt, core_coord_t lower, core_coord_t upper) {
  if (pt.x >= lower.x && pt.x < upper.x && 
    pt.y >= lower.y && pt.y < upper.y)
    return 1;
  else
    return 0;
}

// information required to define a rectangular group
typedef struct group_info_t {
  core_coord_t scalar;
  core_coord_t vector_start;
  core_coord_t vector_dim;
} group_info_t;

// information for extra core available to groups
typedef struct extra_core_info_t {
  core_coord_t coord;
  // preferred vector groups to work with
  int group_affinity[MAX_GROUP_AFFINITY];
} extra_core_info_t;

// information about a template that can "stamped" multiple times over a larger mesh
typedef struct template_info_t {
  int num_groups_in_template;
  int num_extra_in_template;
  core_coord_t template_dim;
  group_info_t groups[MAX_TEMPLATE_GROUPS];
  extra_core_info_t extra[MAX_TEMPLATE_GROUPS];
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
  // virtual tid and dimensions within the group
  int vtid_flat;
  core_coord_t vtid;
  core_coord_t vdim;
  // whether this is a scalar core
  int is_scalar;
  // pointers to scalar core and first vector core
  core_coord_t orig;
  core_coord_t master;
  // info about which group this is relative to all vector groups
  int unique_id;
  int total_groups;
  // pointers to core used for software pipelining
  int num_next_cores;
  int num_prev_cores;
  // which core in software link you are (may or may not be the same as unique id)
  int link_id[MAX_GROUP_AFFINITY];
  int next_cores[MAX_GROUP_AFFINITY];
  int prev_cores[MAX_GROUP_AFFINITY];
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
    int group_id, core_coord_t scalar, core_coord_t vector_start, core_coord_t vector_dim, core_coord_t id,
    core_config_info_t *info) {
  
  core_coord_t vector_end = coord_add(vector_start, vector_dim);

  int is_vector_group = within_bounds(id, vector_start, vector_end);

  int is_scalar_group = coords_eq(id, scalar);
  if (is_vector_group) {
    info->vtid      = coord_sub(id, vector_start);
    info->vtid_flat = get_flat_coord(info->vtid, vector_dim.x);
  }
  if (is_scalar_group) {
    info->is_scalar = 1;
  }
  if (is_vector_group || is_scalar_group) {
    // *start = roundUp((chunk_offset + group_num + 0) * n / vGroups, alignment);
    // *end   = roundUp((chunk_offset + group_num + 1) * n / vGroups, alignment); // make sure aligned to cacheline 
    info->orig = vector_start;
    info->master = scalar;
    info->used = 1;
    info->unique_id = group_id;
    info->vdim = vector_dim;
  }
}

inline void setup_software_pipeline(
    template_info_t *tinfo, core_coord_t template_id, core_coord_t template_shift, int template_offset, int meshdim_x,
    core_config_info_t *cinfo) {
  // determine which cores can be used for software pipelining
  int num_extra_in_template = tinfo->num_extra_in_template;
  int is_extra_core = 0;
  int extra_core_id = -1;
  for (int i = 0; i < num_extra_in_template; i++) {
    if (coords_eq(template_id, tinfo->extra[i].coord)) {
      is_extra_core = 1;
      extra_core_id = i;
    }
  }
  
  // if extra core find id that should wait for
  if (is_extra_core) {
    cinfo->num_prev_cores = 0;
    cinfo->num_next_cores = 0;
    // log all scalar cores with affinity
    for (int i = 0; i < MAX_GROUP_AFFINITY; i++) {
      int groupAffinity = tinfo->extra[extra_core_id].group_affinity[i];
      if (groupAffinity >= 0) {
        // add and shift to template offset (and flatten)
        core_coord_t abs_coord = coord_add(tinfo->groups[groupAffinity].scalar, template_shift);
        cinfo->prev_cores[cinfo->num_prev_cores] = get_flat_coord(abs_coord, meshdim_x);

        // set link id to group we're associate with
        cinfo->link_id[cinfo->num_prev_cores] = groupAffinity + template_offset;

        cinfo->num_prev_cores++;
      }
    }
  }
  // otherwise find scalar core that should send to
  else {
    cinfo->num_next_cores = 1;
    cinfo->num_prev_cores = 0;
    // based on group affinity assign link
    for (int i = 0; i < tinfo->num_extra_in_template; i++) {
      for (int j = 0; j < MAX_GROUP_AFFINITY; j++) {
        if (cinfo->unique_id == tinfo->extra[i].group_affinity[j]) {
          core_coord_t abs_coord = coord_add(tinfo->extra[i].coord, template_shift);
          cinfo->next_cores[0] = get_flat_coord(abs_coord, meshdim_x);
          cinfo->link_id[0] = j;
        }
      }
    }
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
  cinfo.vtid_flat = 0;
  cinfo.vtid = init_core_coord(0, 0);
  cinfo.vdim = init_core_coord(1, 1);
  cinfo.used = 1;
  cinfo.orig = init_core_coord(ptid_x, ptid_y);
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
  // int ptid_flat = ptid_x + ptid_y * pdim_x;
  // int pdim_flat = pdim_x * pdim_y;

  core_coord_t ptid = init_core_coord(ptid_x, ptid_y);
  core_coord_t pdim = init_core_coord(pdim_x, pdim_y);

  // figure out how many times we need to stamp this template
  core_coord_t template_dim = tinfo->template_dim;
  core_coord_t template_id = coord_mod(ptid, template_dim);

  // which group it belongs to for absolute core coordinates
  core_coord_t template_group = coord_div(ptid, template_dim);
  core_coord_t template_group_dim = coord_div(pdim, template_dim);
  int template_group_dim_flat = template_group_dim.x * template_group_dim.y;
  int template_group_flat = get_flat_coord(template_group, template_group_dim.x);
  core_coord_t template_shift = coord_mul(template_group, template_dim);

  // used to determine a unique group id
  int groups_per_template = tinfo->num_groups_in_template;
  cinfo.total_groups = groups_per_template * template_group_dim_flat;

  int template_offset = template_group_flat * groups_per_template;

  // figure out the configurations for core within the groups
  for (int i = 0; i < groups_per_template; i++) {
    group_info_t ginfo = get_group_info(i, tinfo);
    rect_vector_group(i, ginfo.scalar, ginfo.vector_start,
      ginfo.vector_dim, template_id, &cinfo);
  }

  // setup links for a potential software pipeline
  setup_software_pipeline(tinfo, template_id, template_shift, template_offset, pdim_x, &cinfo);

  cinfo.unique_id += template_offset;

  // need to shift the absolute coordinates based on which group this is for
  cinfo.orig = coord_add(cinfo.orig, template_shift);
  cinfo.master = coord_add(cinfo.master, template_shift);

  return cinfo;
  
}


// // if don't inline then need to copy stack pointer up to addr 88, which too lazy to do atm
// // create a template for a vlen=4 config that can copy and paste multiple times on a large mesh
// // ret whether core used in template
// inline int vector_group_template_4(
//     // inputs
//     int ptid_x, int ptid_y, int pdim_x, int pdim_y,
//     // outputs
//     int *vtid, int *vtid_x, int *vtid_y, int *is_scalar, int *orig_x, int *orig_y, int *master_x, int *master_y,
//     int *unique_id, int *total_groups
//   ) {
//   // printf("DEPRECATED use of vector_group_template_4() use vector_group_template()\n");
//   template_info_t tinfo = init_template_4x4_2x2();
//   core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

//   *vtid = cinfo.vtid;
//   *vtid_x = cinfo.vtid_x;
//   *vtid_y = cinfo.vtid_y;
//   *is_scalar = cinfo.is_scalar;
//   *orig_x = cinfo.orig_x;
//   *orig_y = cinfo.orig_y;
//   *master_x = cinfo.master_x;
//   *master_y = cinfo.master_y;
//   *unique_id = cinfo.unique_id;
//   *total_groups = cinfo.total_groups;

//   return cinfo.used;
  
// }

// inline int vector_group_template_16(
//     // inputs
//     int ptid_x, int ptid_y, int pdim_x, int pdim_y,
//     // outputs
//     int *vtid, int *vtid_x, int *vtid_y, int *is_scalar, int *orig_x, int *orig_y, int *master_x, int *master_y,
//     int *unique_id, int *total_groups
//   ) {
//   // printf("DEPRECATED use of vector_group_template_16() use vector_group_template()\n");
//   template_info_t tinfo = init_template_8x8_4x4();
//   core_config_info_t cinfo =  vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);
  
//   *vtid = cinfo.vtid;
//   *vtid_x = cinfo.vtid_x;
//   *vtid_y = cinfo.vtid_y;
//   *is_scalar = cinfo.is_scalar;
//   *orig_x = cinfo.orig_x;
//   *orig_y = cinfo.orig_y;
//   *master_x = cinfo.master_x;
//   *master_y = cinfo.master_y;
//   *unique_id = cinfo.unique_id;
//   *total_groups = cinfo.total_groups;

//   return cinfo.used;
// }

// ---------------------------------------------------------------------------------------------------
// helper to figure out where cores in other vector groups are
// ---------------------------------------------------------------------------------------------------

// TODO need to define this for every group size
void group_id_to_origin(template_info_t *tinfo, int group_id, int *x, int *y);

void group_id_to_scalar(template_info_t *tinfo, int group_id, int *x, int *y);

int get_ptid_from_group(template_info_t *tinfo, int group_id, int vid_x, int vid_y, int phys_dim_x);

inline int get_vtid(core_config_info_t *cinfo) {
  return get_flat_coord(cinfo->vtid, cinfo->vdim.x);
}

inline int get_vdim(core_config_info_t *cinfo) {
  return cinfo->vdim.x * cinfo->vdim.y;
}

#endif
#ifndef __VECTOR_UTIL_H__
#define __VECTOR_UTIL_H__

int roundUp(int numToRound, int multiple);
inline int min(int a, int b) {
  if (a > b) {
    return b;
  }
  else {
    return a;
  }
}
int float_compare(float a, float b, float eps);

// helpers for software sync

// scalar syncs with reduction core
#define SCALAR_SYNC_WITH_REDUCTION(sp_ptr, numCompleted)    \
    while (1) {                                             \
        int wait_val = sp_ptr[POST_FRAME_WORD];             \
        if (numCompleted + 1 < wait_val +                   \
            FRAMES_TO_SYNC_AFTER*ACCUM_GRANULARITY) break;  \
    }                                                       \
    numCompleted++

// reduction core syncs with scalar(s)
#define REDUCE_SYNC_WITH_SCALAR(group_start, spPtrs, flat_iter) \
  for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {               \
    if (group_start[g] < 0) {                                   \
      continue;                                                 \
    }                                                           \
    int *sp_scalar_ptr = spPtrs[g];                             \
    STORE_NOACK(flat_iter, &sp_scalar_ptr[POST_FRAME_WORD], 0); \
  }                                                             \
  flat_iter+=ACCUM_GRANULARITY

#define SETUP_REDUCTION_CORE(fwders, ptid)                      \
  int* spPtrs[NUM_GROUPS_PER_PIPE];                             \
  for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {               \
    spPtrs[g] = (int*)getSpAddr(fwders[g], 0);                  \
  }                                                             \
  int flat_iter = 0;                                            \
  int sp_self = 0;                                              \
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0)

// figure out how many valid elements we're expecting in the frame
#define SETUP_GROUP_ITERATION_CHUNKED(baseGroupId, numGroups, cnt)  \
    int group_start[MAX_GROUP_AFFINITY];                            \
    int expected_elements = 0;                                      \
    for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {                 \
      int gid = baseGroupId + g;                                    \
      if (cnt < get_group_len(gid, N, numGroups)) {                 \
        expected_elements+=PER_CORE_FULL_MAILER_FRAME*ACCUM_GRANULARITY; \
        group_start[g] = get_group_start(gid, N, numGroups) + cnt;  \
      }                                                             \
      else {                                                        \
        group_start[g] = -1;                                        \
      }                                                             \
    }                                                               \
    if (expected_elements == 0) return

#define SETUP_GROUP_ITERATION_STRIDED(baseGroupId, numGroups, cnt, maxCnt)  \
    int group_start[MAX_GROUP_AFFINITY];                            \
    int expected_elements = 0;                                      \
    for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {                 \
      int gstart = cnt * numGroups + baseGroupId + g;               \
      if (gstart < maxCnt) {                                        \
        expected_elements+=PER_CORE_FULL_MAILER_FRAME*ACCUM_GRANULARITY; \
        group_start[g] = gstart;                                    \
      }                                                             \
      else {                                                        \
        group_start[g] = -1;                                        \
      }                                                             \
    }                                                               \
    if (expected_elements == 0) return


// setup values for reduction
#define SETUP_REDUCE_CONFIG()                                       \
  int ptidFwders[MAX_GROUP_AFFINITY];                               \
  int linkId = cinfo.link_id[0];                                    \
  int isMailer = cinfo.num_prev_cores > 0;                          \
  if (isMailer)                                                     \
    unique_id = linkId;                                             \
  for (int i = 0; i < MAX_GROUP_AFFINITY; i++)                      \
    ptidFwders[i] = cinfo.prev_cores[i];                            \
  int ptidMailer = cinfo.next_cores[0]

// dont need values for reduction so initialize to watevs
#define SETUP_REDUCE_CONFIG_NULL()                                  \
  int ptidMailer = 0;                                               \
  int ptidFwders[MAX_GROUP_AFFINITY];                               \
  int linkId = 0;                                                   \
  int isMailer = 0

inline int get_group_start(int id, int N, int numGroups) {
  return (( id + 0 ) * N) / numGroups;
}

inline int get_group_end(int id, int N, int numGroups) {
  return (( id + 1 ) * N) / numGroups;
}

inline int get_group_len(int id, int N, int numGroups) {
  return get_group_end(id, N, numGroups) - get_group_start(id, N, numGroups);
}

inline int ceilToInt(float val) {
  int base = (int)val;
  return base + 1;// ceilf() not working so do cheap version
}

#define SET_USEFUL_VARIABLES_V4(ptid_x, ptid_y, pdim_x, pdim_y)                             \
  int ptid = ptid_x + ptid_y * pdim_x;                                                      \
  int pdim = pdim_x * pdim_y;                                                               \
  template_info_t tinfo = init_template_4x4_2x2();                                          \
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo); \
  int vtid = cinfo.vtid_flat;                                                               \
  int vtid_x = cinfo.vtid.x;                                                                \
  int vtid_y = cinfo.vtid.y;                                                                \
  int vdim_x = cinfo.vdim.x;                                                                \
  int vdim_y = cinfo.vdim.y;                                                                \
  int orig_x = cinfo.orig.x;                                                                \
  int orig_y = cinfo.orig.y;                                                                \
  int is_da  = cinfo.is_scalar;                                                             \
  int master_x = cinfo.master.x;                                                            \
  int master_y = cinfo.master.y;                                                            \
  int unique_id = cinfo.unique_id;                                                          \
  int total_groups = cinfo.total_groups;                                                    \
  int used = cinfo.used;                                                                    \
  int vdim = vdim_x * vdim_y;                                                               \
  int mask = getSIMDMask(&cinfo)

#define SET_USEFUL_VARIABLES_V16(ptid_x, ptid_y, pdim_x, pdim_y)                            \
  int ptid = ptid_x + ptid_y * pdim_x;                                                      \
  int pdim = pdim_x * pdim_y;                                                               \
  template_info_t tinfo = init_template_8x8_4x4();                                          \
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo); \
  int vtid = cinfo.vtid_flat;                                                               \
  int vtid_x = cinfo.vtid.x;                                                                \
  int vtid_y = cinfo.vtid.y;                                                                \
  int vdim_x = cinfo.vdim.x;                                                                \
  int vdim_y = cinfo.vdim.y;                                                                \
  int orig_x = cinfo.orig.x;                                                                \
  int orig_y = cinfo.orig.y;                                                                \
  int is_da  = cinfo.is_scalar;                                                             \
  int master_x = cinfo.master.x;                                                            \
  int master_y = cinfo.master.y;                                                            \
  int unique_id = cinfo.unique_id;                                                          \
  int total_groups = cinfo.total_groups;                                                    \
  int used = cinfo.used;                                                                    \
  int vdim = vdim_x * vdim_y;                                                               \
  int mask = getSIMDMask(&cinfo)

#define SET_USEFUL_VARIABLES_MANYCORE(ptid_x, ptid_y, pdim_x, pdim_y)                       \
  int ptid = ptid_x + ptid_y * pdim_x;                                                      \
  int pdim = pdim_x * pdim_y;                                                               \
  int vtid = 0;                                                                             \
  int vtid_x = 0;                                                                           \
  int vtid_y = 0;                                                                           \
  int vdim_x = 1;                                                                           \
  int vdim_y = 1;                                                                           \
  int orig_x = 0;                                                                           \
  int orig_y = 0;                                                                           \
  int is_da  = 0;                                                                           \
  int master_x = 0;                                                                         \
  int master_y = 0;                                                                         \
  int unique_id = 0;                                                                        \
  int total_groups = 0;                                                                     \
  int used = 1;                                                                             \
  int vdim = vdim_x * vdim_y;                                                               \
  int mask = 0                                                                              \

#endif
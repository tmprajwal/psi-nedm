#include <stdio.h>


#include "fasterac/farray.h"


//----  private  -----------------------------------//

size_t fsize (const char *filename) {
  FILE *f;
  size_t siz = 0L;
  f = fopen (filename, "rb");
  if (f != NULL) {
    fseek (f, 0L, SEEK_END);
    siz = ftell (f);
    fclose (f);
  }
  return siz;
}


//--------------------------------------------------//

void farray_reset (farray* far) {
  far->data_space = NULL;
  far->space_size = 0;
  far->data_array = NULL;
  far->nb_data    = 0;
  far->first_ns   = 0;
  far->last_ns    = 0;
}


//--------------------------------------------------//

void farray_set_data_space (farray* far, char* data_space, size_t space_size) {
  //
  faster_data_p          data;
  faster_buffer_reader_p reader;
  int i;
  //
  farray_reset (far);
  far->data_space = data_space;
  far->space_size = space_size;
  far->data_array = (faster_data_p*) malloc (sizeof (faster_data_p) * far->space_size / 12);
  reader = faster_buffer_reader_open (far->data_space, far->space_size);
  if ((data = faster_buffer_reader_next (reader)) != NULL) {
    far->data_array [0] = data;
    far->first_ns       = faster_data_clock_ns (data);
    far->nb_data        = 1;
  }
  while ((data = faster_buffer_reader_next (reader)) != NULL) {
    far->data_array [far->nb_data] = data;
    far->last_ns                   = faster_data_clock_ns (data);
    far->nb_data                   = far->nb_data + 1;
  }
  faster_buffer_reader_close (reader);
  far->data_array = (faster_data_p*) realloc (far->data_array, sizeof (faster_data_p) * far->nb_data);
}

//--------------------------------------------------//

int farray_data_file_to_memory (const char* filename, char** mem_space, size_t* mem_size) {
  //
  FILE* f;
  //
  *mem_size = fsize (filename);
  if (*mem_size == 0) {
    *mem_space = NULL;
    return 1;
  }
  *mem_space = (char*) malloc (*mem_size);
  if (*mem_space == NULL) {
    *mem_size = 0;
    return 2;
  }
  f = fopen  (filename, "r");
  *mem_size = fread (*mem_space, 1, *mem_size, f);
  fclose (f);
  return 0;
}

//--------------------------------------------------//

farray* farray_new (char* data_space, size_t space_size) {
  farray* far;
  far = (farray*) malloc (sizeof (farray));
  farray_set_data_space (far, data_space, space_size);
  return far;
}

//--------------------------------------------------//

void farray_free (farray* far) {
  free (far->data_array);
  free (far);
  far = NULL;
}

//--------------------------------------------------//

int farray_nearest_idx (const farray far, unsigned long long clock_ns) {
  unsigned long long t;
           long long dt1;
           long long dt2;
  int idx  = -1;
  int idx1 = 0;
  int idx2 = far.nb_data - 1;
  if (far.first_ns > clock_ns) return idx1;
  if (far.last_ns  < clock_ns) return idx2;
  while (idx2 - idx1 > 1) {
    idx = (idx2 + idx1) / 2;
    t   = faster_data_clock_ns (far.data_array [idx]);
    if (t > clock_ns) {
      idx2 = idx;
    } else {
      idx1 = idx;
    }
    dt1 = clock_ns - faster_data_clock_ns (far.data_array [idx1]);
    dt2 = clock_ns - faster_data_clock_ns (far.data_array [idx2]);
  }
  if (llabs (dt1) < llabs (dt2)) return idx1;
  else                           return idx2;
}


int farray_next_idx (const farray far, unsigned long long clock_ns) {
  unsigned long long t;
  int                idx;
  if (far.last_ns  <= clock_ns) return -1;
  if (far.first_ns >  clock_ns) return  0;
  idx = farray_nearest_idx (far, clock_ns);
  t   = faster_data_clock_ns (far.data_array [idx]);
  if (t > clock_ns) return idx;
  else              return idx + 1;
}


int farray_previous_idx (const farray far, unsigned long long clock_ns) {
  unsigned long long t;
  int                idx;
  if (far.first_ns >= clock_ns) return  -1;
  if (far.last_ns  <  clock_ns) return  far.nb_data - 1;
  idx = farray_nearest_idx (far, clock_ns);
  t   = faster_data_clock_ns (far.data_array [idx]);
  if (t < clock_ns) return idx;
  else              return idx - 1;
}

//--------------------------------------------------//




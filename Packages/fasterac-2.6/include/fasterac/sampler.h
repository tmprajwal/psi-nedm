
//
//  Sampler CARAS data definitions
//



#ifndef SAMPLER_H
#define SAMPLER_H 1

#ifdef __cplusplus
extern "C" {
#endif



//  CONST

typedef enum {
  SAMPLER_DATA_TYPE_ALIAS    =  22,
  SAMPLER_COUNTER_TYPE_ALIAS =  23
} sampler_const;



//  DATA DEFINITIONS

typedef short* sampler_data;


typedef struct sampler_counter {
  unsigned trig : 32;
  unsigned calc : 32;
  unsigned sent : 32;
} sampler_counter;



// CONVERSION ACCESSORS






#ifdef __cplusplus
}
#endif


#endif  // SAMPLER_H

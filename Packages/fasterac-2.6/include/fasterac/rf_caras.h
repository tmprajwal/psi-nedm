
//
//  RF CARAS data definitions
//



#ifndef RF_CARAS_H
#define RF_CARAS_H 1

#ifdef __cplusplus
extern "C" {
#endif


#include <math.h>



//  CONST


typedef enum {
  RF_DATA_TYPE_ALIAS    = 19,
  RF_COUNTER_TYPE_ALIAS = 20,
} rf_const;



//  DATA DEFINITIONS

typedef struct rf_data {
  unsigned period    : 31;
  unsigned saturated :  1;
    signed trig_dt   : 32;
    signed pll_dt    : 32;
} rf_data;


typedef struct rf_counter {
  unsigned trig : 32;
  unsigned sent : 32;
} rf_counter;



//  CONVERSION ACCESSORS

double rf_period_ns  (rf_data rf);
double rf_trig_dt_ns (rf_data rf);
double rf_pll_dt_ns  (rf_data rf);





#ifdef __cplusplus
}
#endif


#endif  // RF_CARAS_H


//
//  Scaler CARAS data definitions
//



#ifndef SCALER_H
#define SCALER_H 1

#ifdef __cplusplus
extern "C" {
#endif



//  CONST

typedef enum {
  SCALER_MEASUREMENT_TYPE_ALIAS =  82,
  SCALER_COUNTER_TYPE_ALIAS     =  83
} scaler_const;



//  DATA DEFINITIONS

typedef struct scaler_measurement {
  signed   max_ampl  : 20;    //  maximum amplitude
  unsigned nib       :  7;
  unsigned n_quanta  :  4;    //  number of quanta
  unsigned saturated :  1;    //  saturated signal
  unsigned fw_thres  : 16;    //  full width at threshold
  unsigned max_pos   : 16;    //  position of max
  signed   qtt       : 32;    //  charge auto thres2thres
} scaler_measurement;


typedef struct scaler_counter {
  unsigned quanta : 32;
  unsigned calc   : 32;
  unsigned sent   : 32;
} scaler_counter;



// CONVERSION ACCESSORS

double scaler_max_ampl_mV (scaler_measurement scaler);
double scaler_max_pos_ns  (scaler_measurement scaler);
double scaler_qtt_mVns    (scaler_measurement scaler);
double scaler_fw_thres_ns (scaler_measurement scaler);





#ifdef __cplusplus
}
#endif


#endif  // SCALER_H

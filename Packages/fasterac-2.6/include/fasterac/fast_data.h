
//
//  COMMON FASTER DATA definitions
//



#ifndef FAST_DATA_H
#define FAST_DATA_H 1

#ifdef __cplusplus
extern "C" {
#endif


typedef enum {
  GROUP_TYPE_ALIAS    =  10,
  TREF_TYPE_ALIAS     =  11,
  TREF_TDC_TYPE_ALIAS =  12,
  RF_TYPE_ALIAS       =  19,
  RF_COUNT_TYPE_ALIAS =  20,
  OSCILLO_TYPE_ALIAS  =  21,
  OSCILLO_NB_PTS      = 704,
  SAMPLING_TYPE_ALIAS = OSCILLO_TYPE_ALIAS,  // deprecated
  SAMPLING_NB_PTS     = OSCILLO_NB_PTS       // deprecated
} fast_data_const;


// OSCILLO
typedef struct oscillo {
  float    x0;                  //   1 * 32
  float  xlsb;                  //   1 * 32
  float  ylsb;                  //   1 * 32
  char   xcap[8];               //   8 *  8
  char   ycap[8];               //   8 *  8
  short  samp[OSCILLO_NB_PTS];  // 704 * 16
} oscillo;

// SAMPLING
typedef oscillo sampling;  // deprecated


//  TDC
typedef struct tref_tdc {
  signed tdc : 32;
} tref_tdc;


// DATA FIELD CONVERSIONS
double tref_conv_dt_ns  (int tdc);


double tref_get_tdc_sec (tref_tdc tref);


#ifdef __cplusplus
}
#endif


#endif  // FAST_DATA_H

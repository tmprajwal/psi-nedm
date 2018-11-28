
//
//  QDC CARAS data definitions
//



#ifndef QDC_CARAS_H
#define QDC_CARAS_H 1

#ifdef __cplusplus
extern "C" {
#endif


typedef enum {
  QDC_X1_TYPE_ALIAS     =  41,
  QDC_X2_TYPE_ALIAS     =  42,
  QDC_X3_TYPE_ALIAS     =  43,
  QDC_X4_TYPE_ALIAS     =  44,
  QDC_TDC_X1_TYPE_ALIAS = 141,
  QDC_TDC_X2_TYPE_ALIAS = 142,
  QDC_TDC_X3_TYPE_ALIAS = 143,
  QDC_TDC_X4_TYPE_ALIAS = 144,
  QDC_TOF_X1_TYPE_ALIAS = 241,
  QDC_TOF_X2_TYPE_ALIAS = 242,
  QDC_TOF_X3_TYPE_ALIAS = 243,
  QDC_TOF_X4_TYPE_ALIAS = 244,
  QDC_COUNTER_TYPE_ALIAS=  50
} qdc_const;


typedef struct qdc_x1 {
  signed   q1           : 31;
  unsigned q1_saturated :  1;
} qdc_x1;


typedef struct qdc_x2 {
  signed   q1           : 31;
  unsigned q1_saturated :  1;
  signed   q2           : 31;
  unsigned q2_saturated :  1;
} qdc_x2;


typedef struct qdc_x3 {
  signed   q1           : 31;
  unsigned q1_saturated :  1;
  signed   q2           : 31;
  unsigned q2_saturated :  1;
  signed   q3           : 31;
  unsigned q3_saturated :  1;
} qdc_x3;


typedef struct qdc_x4 {
  signed   q1           : 31;
  unsigned q1_saturated :  1;
  signed   q2           : 31;
  unsigned q2_saturated :  1;
  signed   q3           : 31;
  unsigned q3_saturated :  1;
  signed   q4           : 31;
  unsigned q4_saturated :  1;
} qdc_x4;


typedef struct qdc_t_x1 {
  signed   q1           : 31;
  unsigned q1_saturated :  1;
  signed   tdc          : 32;
} qdc_t_x1;


typedef struct qdc_t_x2 {
  signed   q1           : 31;
  unsigned q1_saturated :  1;
  signed   q2           : 31;
  unsigned q2_saturated :  1;
  signed   tdc          : 32;
} qdc_t_x2;


typedef struct qdc_t_x3 {
  signed   q1           : 31;
  unsigned q1_saturated :  1;
  signed   q2           : 31;
  unsigned q2_saturated :  1;
  signed   q3           : 31;
  unsigned q3_saturated :  1;
  signed   tdc          : 32;
} qdc_t_x3;


typedef struct qdc_t_x4 {
  signed   q1           : 31;
  unsigned q1_saturated :  1;
  signed   q2           : 31;
  unsigned q2_saturated :  1;
  signed   q3           : 31;
  unsigned q3_saturated :  1;
  signed   q4           : 31;
  unsigned q4_saturated :  1;
  signed   tdc          : 32;
} qdc_t_x4;


typedef struct qdc_counter {
  unsigned calc : 32;
  unsigned sent : 32;
} qdc_counter;


// DATA FIELD CONVERSIONS
double qdc_conv_q_mVns (int q);
double qdc_conv_dt_ns  (int tdc);






double qx1_get_tdc_sec (qdc_t_x1 qdc);
double qx2_get_tdc_sec (qdc_t_x2 qdc);
double qx3_get_tdc_sec (qdc_t_x3 qdc);
double qx4_get_tdc_sec (qdc_t_x4 qdc);



#ifdef __cplusplus
}
#endif


#endif  // QDC_CARAS_H

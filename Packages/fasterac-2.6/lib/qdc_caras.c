

#include "fasterac/qdc_caras.h"



double qdc_conv_q_mVns (int q) {
  return q * 0.036468506 * 2.0;        //   CARAS VOLTAGE  LSB : 2*2390 / 2^17 mV
}                                      //            SAMPLING  : 2.0 ns

double qdc_conv_dt_ns  (int tdc) {
  return tdc * 7.8125e-3;              //         HR CLOCK LSB : 2.0 / 256 ns
}








double qx1_get_tdc_sec (qdc_t_x1 qdc) {
  return qdc_conv_dt_ns (qdc.tdc) * 1.0e-9;
}

double qx2_get_tdc_sec (qdc_t_x2 qdc) {
  return qdc_conv_dt_ns (qdc.tdc) * 1.0e-9;
}

double qx3_get_tdc_sec (qdc_t_x3 qdc) {
  return qdc_conv_dt_ns (qdc.tdc) * 1.0e-9;
}

double qx4_get_tdc_sec (qdc_t_x4 qdc) {
  return qdc_conv_dt_ns (qdc.tdc) * 1.0e-9;
}


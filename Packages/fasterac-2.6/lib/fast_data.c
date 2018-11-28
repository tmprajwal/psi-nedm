
#include "fasterac/fast_data.h"


double tref_conv_dt_ns  (int tdc) {
  return tdc * 7.8125e-3;              //        HR CLOCK LSB : 2.0 / 256 ns
}




double tref_get_tdc_sec (tref_tdc tref) {
  return tref_conv_dt_ns (tref.tdc) * 1.0e-9;
}



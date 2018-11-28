

#include "fasterac/adc_caras.h"

//  lsb = 2*range / 2²²
#define MOSAHR_RANGE_2V_LSB_MV  0.001959801            //     2 *  2*2055 / 2^22   mV
#define MOSAHR_RANGE_10V_LSB_MV 0.009898186            //     2 * 2*10379 / 2^22   mV
#define MOSAHR_RANGE_1V_LSB_MV  0.001210213            //     2 *  2*1269 / 2^22   mV
#define MOSAHR_RANGE_5V_LSB_MV  0.005083084            //     2 *  2*5330 / 2^22   mV
#define CARAS_RANGE_LSB_MV      0.001139641            //     2 *    2390 / 2^22   mV
#define DELTA_T_LSB_NS          8.0


double adc_delta_t_ns (adc_data adc) {
  return adc.delta_t * DELTA_T_LSB_NS;
}


/*
double adc_measure_mV (adc_data adc);
  double lsb = 0.0;
  switch (adc.range) {
    case MOSAHR_RANGE_2V:
      lsb = MOSAHR_RANGE_2V_LSB_MV;
      break;
    case MOSAHR_RANGE_10V:
      lsb = MOSAHR_RANGE_10V_LSB_MV;
      break;
    case MOSAHR_RANGE_1V:
      lsb = MOSAHR_RANGE_1V_LSB_MV;
      break;
    case MOSAHR_RANGE_5V:
      lsb = MOSAHR_RANGE_5V_LSB_MV;
      break;
    case CARAS_RANGE:
      lsb = CARAS_RANGE_LSB_MV;
      break;
  }
  return adc.measure * lsb;
}
*/



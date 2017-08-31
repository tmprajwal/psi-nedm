
//
//  ADC CARAS & MOSAHR data definitions
//              ------
//



#ifndef ADC_CARAS_H
#define ADC_CARAS_H 1

#ifdef __cplusplus
extern "C" {
#endif



//  CONST

  typedef enum {
    ADC_DATA_TYPE_ALIAS    = 61,
    ADC_COUNTER_TYPE_ALIAS = 70,
    MOSAHR_RANGE_2V        = 0,
    MOSAHR_RANGE_10V       = 1,
    MOSAHR_RANGE_1V        = 2,
    MOSAHR_RANGE_5V        = 3,
    CARAS_RANGE            = 4
  } adc_const;



//  DATA DEFINITIONS

  typedef struct adc_data {
    unsigned pad1      : 10;
    signed   delta_t   : 16;
    unsigned pad2      :  6;
    signed   measure   : 22;
    unsigned range     :  3;
    unsigned pad3      :  5;
    unsigned saturated :  1;
    unsigned pileup    :  1;
  } adc_data;


  typedef struct adc_counter {
    unsigned calc : 32;
    unsigned sent : 32;
    unsigned trig : 32;
  } adc_counter;



//  CONVERSION ACCESSORS

  double adc_delta_t_ns (adc_data adc);
  //double adc_measure_mV  (adc_data adc);   waiting for 'range' in data (mosahr version)






#ifdef __cplusplus
}
#endif


#endif  // ADC_CARAS_H

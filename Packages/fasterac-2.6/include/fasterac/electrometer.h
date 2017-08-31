
//
//  ELECTROMETER data definitions
//



#ifndef ELECTROMETER_H
#define ELECTROMETER_H 1

#ifdef __cplusplus
extern "C" {
#endif


#include <math.h>



//  CONST

  typedef enum {
    ELECTROMETER_TYPE_ALIAS = 81
  } electrometer_const;



//  DATA DEFINITIONS

  typedef struct output_channel {
    unsigned charge    : 31;
    unsigned saturated :  1;
  } output_channel;


  typedef struct electrometer_data {
    unsigned int           channel_mask;
    struct output_channel  channel [32];
  } electrometer_data;



//  DATA PROC

  int    electrometer_nb_channels (struct electrometer_data elec);
         // return the number of channels present in the data (min 0, max 32)

  int    electrometer_channel_present (struct electrometer_data elec, int channel_num);
         // return 1 if channel present, else 0  (channel_num [1..32])

  int    electrometer_channel_charge_raw (struct electrometer_data elec, int channel_num);
         // return the charge of the channel in raw format,
         // or a negative value if this channel isn't present in the data  (channel_num [1..32])

  double electrometer_channel_charge_pC (struct electrometer_data elec, int channel_num);
         // return the charge of the channel in pC,
         // or a negative value if this channel isn't present in the data  (channel_num [1..32])

  int    electrometer_channel_saturated (struct electrometer_data elec, int channel_num);
         // return 1 measure is saturated, 0 not saturated, -1 channel not present. (channel_num [1..32])





#ifdef __cplusplus
}
#endif


#endif  // RF_CARAS_H

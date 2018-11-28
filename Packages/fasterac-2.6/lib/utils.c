#include <stdio.h>


#include "fasterac/utils.h"

#include "fasterac/fast_data.h"
#include "fasterac/qdc_caras.h"
#include "fasterac/adc_caras.h"
#include "fasterac/rf_caras.h"
#include "fasterac/electrometer.h"
#include "fasterac/scaler.h"
#include "fasterac/sampler.h"
#include "fasterac/qtdc.h"

//--------------------------------------------------//

void data_display (faster_data_p data, int n, int tab, int full) {
  relative_data_display (data, n, tab, full, 0, 0);
}

//--------------------------------------------------//

void relative_data_display (faster_data_p      data,
                            int                n,
                            int                tab,
                            int                full,
                            unsigned long long ref_ns,
                            time_unit          tunit) {
  //  div
  int i, j;
  //  faster data  (fasterac.h)
  unsigned char      alias;
  unsigned short     label;
  unsigned short     lsize;
  unsigned long long clock;           // time stamp ns
  long long          rel_clock;       // time / ref
  long double        hr_clock;        // time stamp + tdc
  //  tref tdc     (fast_data.h)
  tref_tdc    tref_t;
  //  rf           (rf_caras.h)
  rf_data     rf;
  rf_counter  rf_cnt;
  //  qdc          (qdc_caras.h)
  qdc_x1      q;
  qdc_x2      qq;
  qdc_x3      qqq;
  qdc_x4      qqqq;
  qdc_t_x1    q_tdc;
  qdc_t_x2    qq_tdc;
  qdc_t_x3    qqq_tdc;
  qdc_t_x4    qqqq_tdc;
  qdc_t_x1    q_tof;
  qdc_t_x2    qq_tof;
  qdc_t_x3    qqq_tof;
  qdc_t_x4    qqqq_tof;
  qdc_counter q_count;
  //  adc          (adc_caras.h)
  adc_data    a;
  adc_counter a_count;
  //  oscillo      (fast_data.h)
  oscillo     o;
  //  group        (fast_data.h)
  faster_buffer_reader_p group_reader;
  void*                  group_buffer;
  faster_data_p          group_data;
  int                    group_n = 0;
  //  electrometer
  electrometer_data      elec;
  int                    nb_out;
  double                 charge;
  int                    saturated;
  //  scaler
  scaler_measurement     scaler_meas;
  scaler_counter         scaler_cnt;
  //  sampler
  int                    width_ns;
  int                    nb_of_points;
  sampler_data           sampler_dta;
  sampler_counter        sampler_cnt;
  //  qtdc
  qtdc                   qt;
  qtdc_counter           qtdc_cnt;
  //
  char                   clk_str [256];
  //

  alias     = faster_data_type_alias (data);
  label     = faster_data_label      (data);
  lsize     = faster_data_load_size  (data);
  clock     = faster_data_clock_ns   (data);
  rel_clock = clock - ref_ns;
  if (tunit == TUNIT_NS) {
    sprintf (clk_str, "%15.0Lf%s", (long double) rel_clock / tunit_coef [tunit], tunit_str [tunit]);
  } else {
    sprintf (clk_str, "%15.2Lf%s", (long double) rel_clock / tunit_coef [tunit], tunit_str [tunit]);
  }
  for (i=0; i<tab; i++) printf ("   ");
  printf ("%10d  ", n);
  switch (alias) {
    case TREF_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      printf ("\n");
      break;
    case TREF_TDC_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &tref_t);
        hr_clock = (long double)clock + tref_conv_dt_ns (tref_t.tdc);   // long double
        printf ("  hr_clock=%.3Lfns", hr_clock);
      }
      printf ("\n");
      break;
    case GROUP_TYPE_ALIAS:
      printf ("%13s %5d  %s\n", type_name (alias), label, clk_str);
      group_buffer = faster_data_load_p (data);
      group_reader = faster_buffer_reader_open (group_buffer, lsize);
      for (i=0; i<tab; i++) printf ("   ");
      printf ("           -------------------------------------------\n");
      while ((group_data = faster_buffer_reader_next (group_reader)) != NULL) {
        group_n += 1;
        relative_data_display (group_data, group_n, tab+1, full, clock, tunit);
      }
      for (i=0; i<tab; i++) printf ("   ");
      printf ("           -------------------------------------------\n");
      faster_buffer_reader_close (group_reader);
      break;
    case OSCILLO_TYPE_ALIAS:
    //case SAMPLING_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &o);
        printf ("  x0=%f  %s  xlsb=%f  %s  ylsb=%f  %s", o.x0, o.xcap, o.xlsb, o.xcap, o.ylsb, o.ycap);
        /*
        printf ("   ");
        for (n=0; n<OSCILLO_NB_PTS; n++) {                                //   display points
          //printf (" (%f, %f)", n * o.xlsb + o.x0, o.samp[n] * o.ylsb);   //   (x1, y1) (x2, y2) ...
          printf (" %f", o.samp[n] * o.ylsb);                              //   y1 y2 ...
        }
        printf ("\n");
        */
      }
      printf ("\n");
      break;
    case RF_DATA_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &rf);
        printf ("  period=%.6lfns", rf_period_ns (rf));
        hr_clock =  (long double)clock + rf_trig_dt_ns (rf);   // long double
        printf ("  raw_trig=%.3Lfns", hr_clock);
        hr_clock =  (long double)clock + rf_pll_dt_ns (rf);
        printf ("  pll_trig=%.3Lfns", hr_clock);
        if (rf.saturated) {
          printf ("  saturated");
        }
      }
      printf ("\n");
      break;
    case RF_COUNTER_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &rf_cnt);
        printf ("  trig=%d  sent=%d", rf_cnt.trig, rf_cnt.sent);
      }
      printf ("\n");
      break;
    case QDC_X1_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &q);
        printf ("  q1=%.3lfmV.ns", qdc_conv_q_mVns (q.q1));
        if (q.q1_saturated) printf ("  saturated : q1");
      }
      printf ("\n");
      break;
    case QDC_X2_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &qq);
        printf ("  q1=%.3lfmV.ns  q2=%.3lfmV.ns", qdc_conv_q_mVns (qq.q1),
                                                  qdc_conv_q_mVns (qq.q2));
        if (qq.q1_saturated || qq.q2_saturated) {
          printf ("  saturated : ");
          if (qq.q1_saturated) printf ("q1 ");
          if (qq.q2_saturated) printf ("q2 ");
        }
      }
      printf ("\n");
      break;
    case QDC_X3_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &qqq);
        printf ("  q1=%.3lfmV.ns  q2=%.3lfmV.ns  q3=%.3lfmV.ns", qdc_conv_q_mVns (qqq.q1),
                                                                  qdc_conv_q_mVns (qqq.q2),
                                                                  qdc_conv_q_mVns (qqq.q3));
        if (qqq.q1_saturated || qqq.q2_saturated || qqq.q3_saturated) {
          printf ("  saturated : ");
          if (qqq.q1_saturated) printf ("q1 ");
          if (qqq.q2_saturated) printf ("q2 ");
          if (qqq.q3_saturated) printf ("q3 ");
        }
      }
      printf ("\n");
      break;
    case QDC_X4_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &qqqq);
        printf ("  q1=%.3lfmV.ns  q2=%.3lfmV.ns  q3=%.3lfmV.ns  q4=%.3lfmV.ns", qdc_conv_q_mVns (qqqq.q1),
                                                                                 qdc_conv_q_mVns (qqqq.q2),
                                                                                 qdc_conv_q_mVns (qqqq.q3),
                                                                                 qdc_conv_q_mVns (qqqq.q4));
        if (qqqq.q1_saturated || qqqq.q2_saturated || qqqq.q3_saturated || qqqq.q4_saturated) {
          printf ("  saturated : ");
          if (qqqq.q1_saturated) printf ("q1 ");
          if (qqqq.q2_saturated) printf ("q2 ");
          if (qqqq.q3_saturated) printf ("q3 ");
          if (qqqq.q4_saturated) printf ("q4 ");
        }
      }
      printf ("\n");
      break;
    case QDC_TDC_X1_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &q_tdc);
        hr_clock =  (long double)clock + qdc_conv_dt_ns (q_tdc.tdc);   // long double
        printf ("  q1=%.3lfmV.ns  hr_clock=%.3Lfns", qdc_conv_q_mVns (q_tdc.q1),
                                                     hr_clock);
        if (q_tdc.q1_saturated) printf ("  saturated : q1");
      }
      printf ("\n");
      break;
    case QDC_TDC_X2_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &qq_tdc);
        hr_clock =  (long double)clock + qdc_conv_dt_ns (qq_tdc.tdc);   // long double
        printf ("  q1=%.3lfmV.ns  q2=%.3lfmV.ns  hr_clock=%.3Lfns", qdc_conv_q_mVns (qq_tdc.q1),
                                                                    qdc_conv_q_mVns (qq_tdc.q2),
                                                                    hr_clock);
        if (qq_tdc.q1_saturated || qq_tdc.q2_saturated) {
          printf ("  saturated : ");
          if (qq_tdc.q1_saturated) printf ("q1 ");
          if (qq_tdc.q2_saturated) printf ("q2 ");
        }
      }
      printf ("\n");
      break;
    case QDC_TDC_X3_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &qqq_tdc);
        hr_clock =  (long double)clock + qdc_conv_dt_ns (qqq_tdc.tdc);   // long double
        printf ("  q1=%.3lfmV.ns  q2=%.3lfmV.ns  q3=%.3lfmV.ns  hr_clock=%.3Lfns", qdc_conv_q_mVns (qqq_tdc.q1),
                                                                                   qdc_conv_q_mVns (qqq_tdc.q2),
                                                                                   qdc_conv_q_mVns (qqq_tdc.q3),
                                                                                   hr_clock);
        if (qqq_tdc.q1_saturated || qqq_tdc.q2_saturated || qqq_tdc.q3_saturated) {
          printf ("  saturated : ");
          if (qqq_tdc.q1_saturated) printf ("q1 ");
          if (qqq_tdc.q2_saturated) printf ("q2 ");
          if (qqq_tdc.q3_saturated) printf ("q3 ");
        }
      }
      printf ("\n");
      break;
    case QDC_TDC_X4_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &qqqq_tdc);
        hr_clock =  (long double)clock + qdc_conv_dt_ns (qqqq_tdc.tdc);   // long double
        printf ("  q1=%.3lfmV.ns  q2=%.3lfmV.ns  q3=%.3lfmV.ns  q4=%.3lfmV.ns  hr_clock=%.3Lfns", qdc_conv_q_mVns (qqqq_tdc.q1),
                                                                                                  qdc_conv_q_mVns (qqqq_tdc.q2),
                                                                                                  qdc_conv_q_mVns (qqqq_tdc.q3),
                                                                                                  qdc_conv_q_mVns (qqqq_tdc.q4),
                                                                                                  hr_clock);
        if (qqqq_tdc.q1_saturated || qqqq_tdc.q2_saturated || qqqq_tdc.q3_saturated || qqqq_tdc.q4_saturated) {
          printf ("  saturated : ");
          if (qqqq_tdc.q1_saturated) printf ("q1 ");
          if (qqqq_tdc.q2_saturated) printf ("q2 ");
          if (qqqq_tdc.q3_saturated) printf ("q3 ");
          if (qqqq_tdc.q4_saturated) printf ("q4 ");
        }
      }
      printf ("\n");
      break;
    case QDC_TOF_X1_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &q_tof);
        printf ("  q1=%.3lfmV.ns  tof=%.3lfns", qdc_conv_q_mVns (q_tof.q1),
                                                qdc_conv_dt_ns  (q_tof.tdc));
        if (q_tof.q1_saturated) printf ("  saturated : q1");
      }
      printf ("\n");
      break;
    case QDC_TOF_X2_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      faster_data_load (data, &qq_tof);
      if (full) {
        printf ("  q1=%.3lfmV.ns  q2=%.3lfmV.ns  tof=%.3lfns", qdc_conv_q_mVns (qq_tof.q1),
                                                               qdc_conv_q_mVns (qq_tof.q2),
                                                               qdc_conv_dt_ns  (qq_tof.tdc));
        if (qq_tof.q1_saturated || qq_tof.q2_saturated) {
          printf ("  saturated : ");
          if (qq_tof.q1_saturated) printf ("q1 ");
          if (qq_tof.q2_saturated) printf ("q2 ");
        }
      }
      printf ("\n");
      break;
    case QDC_TOF_X3_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &qqq_tof);
        printf ("  q1=%.3lfmV.ns  q2=%.3lfmV.ns  q3=%.3lfmV.ns  tof=%.3lfns", qdc_conv_q_mVns (qqq_tof.q1),
                                                                              qdc_conv_q_mVns (qqq_tof.q2),
                                                                              qdc_conv_q_mVns (qqq_tof.q3),
                                                                              qdc_conv_dt_ns  (qqq_tof.tdc));
        if (qqq_tof.q1_saturated || qqq_tof.q2_saturated || qqq_tof.q3_saturated) {
          printf ("  saturated : ");
          if (qqq_tof.q1_saturated) printf ("q1 ");
          if (qqq_tof.q2_saturated) printf ("q2 ");
          if (qqq_tof.q3_saturated) printf ("q3 ");
        }
      }
      printf ("\n");
      break;
    case QDC_TOF_X4_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &qqqq_tof);
        printf ("  q1=%.3lfmV.ns  q2=%.3lfmV.ns  q3=%.3lfmV.ns  q3=%.3lfmV.ns  tof=%.3lfns", qdc_conv_q_mVns (qqqq_tof.q1),
                                                                                             qdc_conv_q_mVns (qqqq_tof.q2),
                                                                                             qdc_conv_q_mVns (qqqq_tof.q3),
                                                                                             qdc_conv_q_mVns (qqqq_tof.q4),
                                                                                             qdc_conv_dt_ns  (qqqq_tof.tdc));
        if (qqqq_tof.q1_saturated || qqqq_tof.q2_saturated || qqqq_tof.q3_saturated || qqqq_tof.q4_saturated) {
          printf ("  saturated : ");
          if (qqqq_tof.q1_saturated) printf ("q1 ");
          if (qqqq_tof.q2_saturated) printf ("q2 ");
          if (qqqq_tof.q3_saturated) printf ("q3 ");
          if (qqqq_tof.q4_saturated) printf ("q4 ");
        }
      }
      printf ("\n");
      break;
    case QDC_COUNTER_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &q_count);
        printf ("  calc=%d  sent=%d", q_count.calc, q_count.sent);
      }
      printf ("\n");
      break;
    case ADC_DATA_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &a);
        //printf ("  saturated=%d  pileup=%d  delta_t=%.0lfns  meas=%d", a.saturated, a.pileup, adc_delta_t_ns (a), a.measure);
        printf ("  delta_t=%.0lfns  meas=%d", adc_delta_t_ns (a), a.measure);
        if (a.saturated) printf ("  saturated");
        if (a.pileup)    printf ("  pileup");
      }
      printf ("\n");
      break;
    case ADC_COUNTER_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &a_count);
        printf ("  calc=%d  sent=%d  trig=%d", a_count.calc, a_count.sent, a_count.trig);
      }
      printf ("\n");
      break;
    case ELECTROMETER_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &elec);
        nb_out = electrometer_nb_channels (elec);               //  nb output channels
        printf ("   Nout=%d   ", nb_out);
        for (j=1;j<=32;j++) {                                   //  channels from 1 to 32
          saturated = electrometer_channel_saturated (elec, j);
          if (saturated != -1) {                                  //  -1  =>  channel 'j' not present in the data
            charge = electrometer_channel_charge_pC (elec, j);  //  get charge from channel 'j' (pico Coulomb)
            printf ("[%d:%fpC", j, charge);                     //  display [chan:charge:sat]
            if (saturated) printf (":sat] ");
            else         printf ("] ");
          }
        }
      }
      printf ("\n");
      break;
    case SCALER_MEASUREMENT_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &scaler_meas);
        printf ("  max_ampl=%fmV  max_pos=%fns  n_quanta=%d  fw_thres=%fns  qtt=%fmVns", scaler_max_ampl_mV (scaler_meas),
                                                                                         scaler_max_pos_ns  (scaler_meas),
                                                                                         scaler_meas.n_quanta,
                                                                                         scaler_fw_thres_ns (scaler_meas),
                                                                                         scaler_qtt_mVns    (scaler_meas));
        if (scaler_meas.saturated) printf ("  saturated");
      }
      printf ("\n");
      break;
    case SCALER_COUNTER_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &scaler_cnt);
        printf ("  quanta=%d  calc=%d  sent=%d", scaler_cnt.quanta, scaler_cnt.calc, scaler_cnt.sent);
      }
      printf ("\n");
      break;
    case SAMPLER_DATA_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        width_ns     = lsize;
        nb_of_points = lsize / 2;
        printf ("  width=%dns  nb_of_pts=%d   --   ", width_ns, nb_of_points);
        sampler_dta = faster_data_load_p (data);
        if (nb_of_points <= 10) {
          for (i=0; i<nb_of_points; i++) printf ("%d  ", sampler_dta [i]);
        } else {
          for (i=0; i<10; i++) printf ("%d  ", sampler_dta [i]);
          printf (" ...");
        }
      }
      printf ("\n");
      break;
    case SAMPLER_COUNTER_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &sampler_cnt);
        printf ("  trig=%d  calc=%d  sent=%d", sampler_cnt.trig, sampler_cnt.calc, sampler_cnt.sent);
      }
      printf ("\n");
      break;
    case QTDC_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &qt);
        printf ("  nb_q=%d", qtdc_nb_q (qt));
        if (qtdc_saturated (qt))  {
          printf ("  saturated");
        }
        if (qtdc_pileup    (qt)) {
          printf ("  pileup");
        }
        printf ("  width=%.2fns", qtdc_t2t_width_ns (qt));
        if (qtdc_t2t_max_ok (qt)) {
          printf ("  max=%.2fmV  max_pos=%.2fns", qtdc_t2t_max_mV (qt), qtdc_t2t_max_pos_ns (qt));
        }
        if (qtdc_t2t_charge_ok (qt)) {
          printf ("  t2t_q=%.2fmVns", qtdc_t2t_charge_mVns (qt));
        }
        for (j=1; j<=qtdc_nb_q (qt); j++) {
          printf ("  q%d=%.2fmVns", j, qtdc_charge_mVns (qt, j));
        }
      }
      printf ("\n");
      break;
    case QTDC_COUNTER_TYPE_ALIAS:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) {
        faster_data_load (data, &qtdc_cnt);
        printf ("  trig=%d  calc=%d  sent=%d", qtdc_cnt.trig, qtdc_cnt.calc, qtdc_cnt.sent);
      }
      printf ("\n");
      break;
    default:
      printf ("%13s %5d  %s", type_name (alias), label, clk_str);
      if (full) printf ("  load_size=%d", lsize);
      printf ("\n");
  }
}


//--------------------------------------------------//

const char* type_name (unsigned char type_alias) {
  switch (type_alias) {
    case TREF_TYPE_ALIAS:
      return "TREF";
      break;
    case TREF_TDC_TYPE_ALIAS:
      return "TREF_TDC";
      break;
    case GROUP_TYPE_ALIAS:
      return  "GROUP";
      break;
    case OSCILLO_TYPE_ALIAS:
      return "OSCILLO";
      break;
    case RF_DATA_TYPE_ALIAS:
      return "RF";
      break;
    case RF_COUNTER_TYPE_ALIAS:
      return "RF COUNTER";
      break;
    case QDC_X1_TYPE_ALIAS:
      return "QDC_X1";
      break;
    case QDC_X2_TYPE_ALIAS:
      return "QDC_X2";
      break;
    case QDC_X3_TYPE_ALIAS:
      return "QDC_X3";
      break;
    case QDC_X4_TYPE_ALIAS:
      return "QDC_X4";
      break;
    case QDC_TDC_X1_TYPE_ALIAS:
      return "QDC_TDC_X1";
      break;
    case QDC_TDC_X2_TYPE_ALIAS:
      return "QDC_TDC_X2";
      break;
    case QDC_TDC_X3_TYPE_ALIAS:
      return "QDC_TDC_X3";
      break;
    case QDC_TDC_X4_TYPE_ALIAS:
      return "QDC_TDC_X4";
      break;
    case QDC_TOF_X1_TYPE_ALIAS:
      return "QDC_TOF_X1";
      break;
    case QDC_TOF_X2_TYPE_ALIAS:
      return "QDC_TOF_X2";
      break;
    case QDC_TOF_X3_TYPE_ALIAS:
      return "QDC_TOF_X3";
      break;
    case QDC_TOF_X4_TYPE_ALIAS:
      return "QDC_TOF_X4";
      break;
    case QDC_COUNTER_TYPE_ALIAS:
      return "QDC_COUNTER";
      break;
    case ADC_DATA_TYPE_ALIAS:
      return "ADC";
      break;
    case ADC_COUNTER_TYPE_ALIAS:
      return "ADC_COUNTER";
      break;
    case ELECTROMETER_TYPE_ALIAS:
      return "ELECTROMETER";
      break;
    case SCALER_MEASUREMENT_TYPE_ALIAS:
      return "SCALER_MEAS";
      break;
    case SCALER_COUNTER_TYPE_ALIAS:
      return "SCALER_COUNT";
      break;
    case SAMPLER_DATA_TYPE_ALIAS:
      return "SAMPLER";
      break;
    case SAMPLER_COUNTER_TYPE_ALIAS:
      return "SAMPLER_COUNT";
      break;
    case QTDC_TYPE_ALIAS:
      return "QTDC_MEAS";
      break;
    case QTDC_COUNTER_TYPE_ALIAS:
      return "QTDC_COUNT";
      break;
    default:
      return "UNKNOWN DATA";
  }
}


//--------------------------------------------------//


long double conv_tdc_to_ns (int tdc_reg) {
  return tdc_reg * 7.8125e-3;              //   HR CLOCK LSB : 2.0 / 256 ns
}


long double tdc_ns (const faster_data_p data) {
  switch (faster_data_type_alias (data)) {
    case TREF_TDC_TYPE_ALIAS:
      return conv_tdc_to_ns (((tref_tdc*) faster_data_load_p (data))->tdc);
    case QDC_TDC_X1_TYPE_ALIAS:
      return conv_tdc_to_ns (((qdc_t_x1*) faster_data_load_p (data))->tdc);
    case QDC_TDC_X2_TYPE_ALIAS:
      return conv_tdc_to_ns (((qdc_t_x2*) faster_data_load_p (data))->tdc);
    case QDC_TDC_X3_TYPE_ALIAS:
      return conv_tdc_to_ns (((qdc_t_x3*) faster_data_load_p (data))->tdc);
    case QDC_TDC_X4_TYPE_ALIAS:
      return conv_tdc_to_ns (((qdc_t_x4*) faster_data_load_p (data))->tdc);
    case QTDC_TYPE_ALIAS:
      return qtdc_tdc_ns (*((qtdc*)faster_data_load_p (data)));
    case RF_DATA_TYPE_ALIAS:
      return conv_tdc_to_ns (((rf_data*)  faster_data_load_p (data))->trig_dt);
    default:
      return 0.0;
  }
}


long double faster_data_hr_clock_ns (const faster_data_p data) {
  long double t  = faster_data_clock_ns (data);
  long double dt = tdc_ns               (data);
  return t + dt;
}


long double faster_data_hr_clock_sec (const faster_data_p data) {
  return faster_data_hr_clock_ns (data) * 1e-9L;
}


long double faster_data_rf_time_ns (const faster_data_p data, const faster_data_p rf_ref) {
  rf_data     *rf;
  long double meas_data_time;
  long double meas_rf_time;
  long double rf_period;
  long double last_rf_time;

  if (faster_data_type_alias (rf_ref) != RF_DATA_TYPE_ALIAS) return -1;

  rf             = faster_data_load_p (rf_ref);
  rf_period      = rf_period_ns (*rf);
  meas_data_time = faster_data_hr_clock_ns (data);
  meas_rf_time   = (long double)faster_data_clock_ns (rf) + rf_pll_dt_ns (*rf);
  last_rf_time   = meas_rf_time + floorl ((meas_data_time - meas_rf_time) / rf_period) * rf_period;
  return meas_data_time - last_rf_time;
}

//--------------------------------------------------//

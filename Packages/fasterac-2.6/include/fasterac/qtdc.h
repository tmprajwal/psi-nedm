
//
//  QTDC CARAS data definitions
//



#ifndef QTDC_H
#define QTDC_H 1

#ifdef __cplusplus
extern "C" {
#endif


  typedef enum {
    QTDC_TYPE_ALIAS         = 45,
    QTDC_COUNTER_TYPE_ALIAS = 51
  } qtdc_const;


  typedef struct qtdc_counter {
    unsigned trig : 32;
    unsigned calc : 32;
    unsigned sent : 32;
  } qtdc_counter;


  typedef struct qtdc_header {
    unsigned t2t_width : 16;
    signed   tdc       :  9;
    unsigned pileup    :  1;
    unsigned saturated :  1;
    unsigned nb_q      :  3;   // nb_q -> 2bits -> nb_q max = 3  ==>  pour ajouter un bool barycentre
    unsigned t2t_q     :  1;
    unsigned t2t_max   :  1;
  } qtdc_header;


  typedef struct qtdc_max {
    unsigned t   : 16;
    signed   a   : 14;
    unsigned pad : 2;
  } qtdc_max;


  typedef struct qtdc {
    struct qtdc_header head;
    int                data [6];  // Q1 Q2 Q3 Q4 t2tQ t2tMax   (nb data compris entre 0 et 6 suivant le header)
  } qtdc;


  //  Data field getters
  double qtdc_tdc_sec         (qtdc q);
  double qtdc_tdc_ns          (qtdc q);
  int    qtdc_pileup          (qtdc q);
  int    qtdc_saturated       (qtdc q);
  double qtdc_t2t_width_ns    (qtdc q);
  int    qtdc_nb_q            (qtdc q);
  double qtdc_charge_mVns     (qtdc q, int n);
  int    qtdc_t2t_charge_ok   (qtdc q);
  double qtdc_t2t_charge_mVns (qtdc q);
  int    qtdc_t2t_max_ok      (qtdc q);
  double qtdc_t2t_max_mV      (qtdc q);
  double qtdc_t2t_max_pos_ns  (qtdc q);

  int    qtdc_charge_raw      (qtdc q, int n);
  int    qtdc_t2t_charge_raw  (qtdc q);
  int    qtdc_t2t_max_raw     (qtdc q);



#ifdef __cplusplus
}
#endif


#endif  // QDC_CARAS_H

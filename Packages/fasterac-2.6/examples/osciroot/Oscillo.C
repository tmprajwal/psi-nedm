#include "TMath.h"
#include "Riostream.h"


#include "Oscillo.h"


ClassImp (Oscillo)


// default values
Int_t 	Oscillo::nb_pts = SAMPLING_NB_PTS;
Float_t	Oscillo::x0_ns  = -352.0;
Float_t	Oscillo::n2ns   = 2.0;
Float_t	Oscillo::n2mV   = 0.072937;


void  Oscillo::Add (Oscillo* toAdd) {
  int n_max    = -1;
  int n_min    = -1;
  this->max_mV = INT_MIN;
  this->min_mV = INT_MAX;
  if (toAdd != NULL) {
    for (int i=0; i<nb_pts; i++) {
      this->y_mV [i] += toAdd->y_mV [i];
      if (this->y_mV [i] > this->max_mV) {
        this->max_mV = this->y_mV [i];
        n_max        = i;
      }
      if (this->y_mV [i] < this->min_mV) {
        this->min_mV = this->y_mV [i];
        n_min        = i;
      }
    }
    this->max_ns = num2ns (n_max);
    this->min_ns = num2ns (n_min);
  }
}


void Oscillo::Sub (Oscillo* toSub) {
  int n_max    = -1;
  int n_min    = -1;
  this->max_mV = INT_MIN;
  this->min_mV = INT_MAX;
  if (toSub != NULL) {
    for (int i=0; i<nb_pts; i++) {
      this->y_mV [i] -= toSub->y_mV [i];
      if (this->y_mV [i] > this->max_mV) {
        this->max_mV = this->y_mV [i];
        n_max  = i;
      }
      if (this->y_mV [i] < this->min_mV) {
        this->min_mV = this->y_mV [i];
        n_min  = i;
      }
    }
    this->max_ns = num2ns (n_max);
    this->min_ns = num2ns (n_min);
  }
}


void Oscillo::Mult (Float_t coef) {
  int n_max = -1;
  int n_min = -1;
  this->max_mV    = INT_MIN;
  this->min_mV    = INT_MAX;
  if (coef != 1.0) {
    for (int i=0; i<nb_pts; i++) {
      this->y_mV [i] *= coef;
      if (this->y_mV [i] > this->max_mV) {
        this->max_mV = this->y_mV [i];
        n_max  = i;
      }
      if (this->y_mV [i] < this->min_mV) {
        this->min_mV = this->y_mV [i];
        n_min  = i;
      }
    }
    this->max_ns = num2ns (n_max);
    this->min_ns = num2ns (n_min);
  }
}


void Oscillo::Init (short* pts) {
  int n_max    = -1;
  int n_min    = -1;
  this->max_mV = INT_MIN;
  this->min_mV = INT_MAX;
  if (pts == NULL) {
    for (int i=0; i<nb_pts; i++) this->y_mV [i] = 0.0;
    n_max = n_min = 0;
  } else {
    for (int i=0; i<nb_pts; i++) {
      this->y_mV [i] = pts [i] * n2mV;
      if (this->y_mV [i] > this->max_mV) {
        this->max_mV = this->y_mV [i];
        n_max  = i;
      }
      if (this->y_mV [i] < this->min_mV) {
        this->min_mV = this->y_mV [i];
        n_min  = i;
      }
    }
  }
  this->max_ns = num2ns (n_max);
  this->min_ns = num2ns (n_min);
}



Float_t Oscillo::Baseline (Float_t width_ns) {
  int     n  = ns2num (width_ns);
  Float_t bl = 0.0;
  for (int i=0; i<n; i++) {
    bl += this->y_mV [i];
  }
  return bl / n;
}


Float_t Oscillo::Charge_mVns (Float_t t1_ns, Float_t t2_ns) {
  int     n1 = ns2num (t1_ns);
  int     n2 = ns2num (t2_ns);
  Float_t q  = 0.0;
  for (int i=n1; i<n2; i++) {
    q += this->y_mV [i] * n2ns;
  }
  return q;
}


void Oscillo::Fill (TH1F* histo) {
  histo->Reset ();
  for (int i=0; i<nb_pts; i++) {
    histo->Fill (x0_ns + i * n2ns, this->y_mV [i]);
  }
  Float_t dy =      (this->max_mV - this->min_mV) / 10;
  histo->SetMinimum (this->min_mV - dy);
  histo->SetMaximum (this->max_mV + dy);
}


Float_t Oscillo::num2ns (int n) {
  return x0_ns + n * n2ns;
}


int Oscillo::ns2num (Float_t t_ns) {
  return (t_ns - x0_ns) / n2ns;
}



#ifndef Oscillo_h
#define Oscillo_h

#include "TNamed.h"
#include "TH1.h"

#include "fasterac/fast_data.h"


class Oscillo : public TNamed	{


	public:
		Float_t y_mV [SAMPLING_NB_PTS];
    Float_t max_mV;
    Float_t max_ns;
    Float_t min_mV;
    Float_t min_ns;

	public:
    //
                      Oscillo ()           : TNamed ("AnOscillo", "AnOscillo") {};
                      Oscillo (char* name) : TNamed (name, name)               {};
    //
    virtual void      Init         (short*   pts);
    //
    virtual void      Add          (Oscillo* toAdd);
    virtual void      Sub          (Oscillo* toSub);
    virtual void      Mult         (Float_t  coef);
    virtual void      Fill         (TH1F*    histo);
    virtual Float_t   Baseline     (Float_t  width_ns);
    virtual Float_t   Charge_mVns  (Float_t  t1_ns, Float_t t2_ns);

  protected:
    static Int_t   nb_pts;
    static Float_t x0_ns;
    static Float_t n2ns;
    static Float_t n2mV;

	private:
    Float_t num2ns (int n);
    int     ns2num (Float_t t_ns);


  public :
    ClassDef (Oscillo, 2)

};

#endif

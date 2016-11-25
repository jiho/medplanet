===================
  Use Constraints
===================

* Acknowledgement sentence that must be reproduced in any publication using the dataset:

"The authors acknowledge Météo-France for supplying the data and the HyMeX database teams (ESPRI/IPSL and SEDOO/Observatoire Midi-Pyrénées) for their help in accessing the data."


======================
  Format description
======================

* Columns:
num_sta date lat lon alt typesta t td u dd ff pres pmer vv ww n nbas hbas cl cm ch nnuage1 ctype1 hnuage1 nnuage2 ctype2 hnuage2 nnuage3 ctype3 hnuage3 nnuage4 ctype4 hnuage4 rr6 rr12 rr24 rr1 rr3 evapotrans instrum_evapo insolh insolj ray_net01 ray_net24 ray_glo01 ray_glo24 ray_dif01 ray_dif24 ray_hlo01 ray_hlo24 ray_llo01 ray_llo24 ray_bilan01 ray_bilan24 ray_dir01 ray_dir24

* Param description 
short_name     (bufr code) : description (unit)
 num_sta	           : WMO station number (see below)
 date		           : YYYYMMDDhhmmss
 lat           (005001)	   : Latitude (high accuracy) (Degree between -90/90)
 lon           (006001)    : Longitude (high accuracy) (Degree between -180/180)
 alt           (007001)    : Height of station (m)
 typesta       (002001)    : Type of station (Code table)
 t             (012004)    : Dry-bulb temperature at 2 m (K)
 td            (012006)    : Dew-point temperature at 2 m (K)
 u             (013003)    : Relative humidity (%)
 dd            (011011)    : Wind direction at 10 m (Degree true - clockwise - 
                             0 or 360 means northerly - 90 means easterly etc.) 
 ff            (011012)    : Wind speed at 10 m (m.s-1)
 pres          (010004)    : Pressure (Pa)
 pmer          (010051)    : Pressure reduced to mean sea level (Pa)
 
 rr24          (013023)    : Total precipitation past 24 hours (kg.m-2)
 ray_glo01     (014021)	   : Global solar radiation (J.m-2)

* Missing value: -999

Mediterranean Sea stations WMO:

NICE = 07690
CANNES = 07684
LE LUC = 07675
HYERES = 07667
CAP CEPET = 07661
MARIGNANE = 07650
SALON DE PROVENCE = 07648
ISTRES = 07647
MONTPELLIER = 07643
SETE = 07641
BEZIERS-VIAS = 07638
LEUCATE = 07666$
PERPIGNAN = 07747
CAP BEAR = 07749
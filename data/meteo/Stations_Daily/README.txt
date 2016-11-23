===================
  Use Constraints
===================

* Acknowledgement sentence that must be reproduced in any publication using the dataset:

"The authors acknowledge Météo-France for supplying the data and the HyMeX database teams (ESPRI/IPSL and SEDOO/Observatoire Midi-Pyrénées) for their help in accessing the data."

======================
  Format description
======================

* Columns
#num_poste date lat lon alti type_poste_actuel tn tx tm un ux um ffm ff2m rr inst glot

* Colums qxx contains quality code for param xx. See below for quality codes.

* Param description
short_name     		: description (unit)
 num_poste		: Station Number
 date (YYYYMMDDhhmmss)	: Climatological day (see below)
 lat			: Latitude (Degree between -90/90)
 lon			: Longitude (Degree between -180/180)
 alti			: Altitude (m)
 type_poste_actuel	: Type of station (Code table, see below)
 tn			: Minimum air temperature (°C)
 tx			: Maximum air temperature (°C)
 tm			: Mean air temperature (°C)
 un			: Minimum humidity (%)
 ux			: Maximum humidity (%)
 um			: Mean humidity (%)
 ffm			: Mean wind speed at 10 m (m.s-1)
 ff2m			: Mean wind speed at 2 m (m.s-1)
 rr			: Precipitation amount (mm)
 inst			: Total sunshine (Minute)
 glot			: Global solar radiation (J.m-2)
 

* Missing value: -999

======================
  Climatological day
======================

Its definition varies :

RR, TX 		: from D 6h to D+1 6h
TN 		: from D-1 18h to D 18h
Other params	: from D 0h to D+1 0h

(UTC time)

======================
  Geographic regions
======================

D5 (41N-46N / 2E-5E) from 1979 to 1999
D3 (41N-46N / 2E-11E) since 2000
D4 (47N-41N / 1W-11E) during SOP and EOP (sep-dec 2011, sep 2012 - mar 2013, sep-dec 2013, sep-dec 2014)


======================
  Code tables
======================

* Station codes
 0ddcccooo where
 dd 	is the number of the department
 ccc 	is the number of the town
 ooo	is the number of the station in the town

For the complete list, see file POSTES

* Station types (type_poste_actuel)
0 station synoptique, automatique ou avec personnel Meteo-France, temps reel en diffusion et expertise
1 station synoptique avec personnel non Meteo-France, temps reel en diffusion et expertise
2 station automatique, temps reel en diffusion et expertise
3 station automatique, expertise temps differe
4 station climatologique (benevole), expertise temps differe
5 station automatique ou poste a interrogation occasionnelle, donnees non expertisees
6 site meteorologique non point de mesure surface
7 point fictif servant a une prevision par adaptation statistique


======================
  Quality codes
======================

Coded on 2 digits : VA (Validation, Action)

Validation :
9: unchecked data
0: protected data
1: validated data
2: suspicious data (check in progress)
3: suspicious data

Action :
0: Original data
1: Modified data (Corrected, Estimated...)

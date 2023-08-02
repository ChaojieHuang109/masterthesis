/* Diagnoses */
data diagnoses;
set V_INTG01.ima_diagnoses_up_to_2015_h20_v01;
run;

/* Laboratory */
data laboratory;
set V_INTG01.ima_laboratory_up_to_2015_h20_v0;
run;

/* Measurments */
data measurements;
set V_INTG01.ima_measurement_upto_2015_h20_v0;
run;

/* Patients */
data patients;
set V_INTG01.ima_patients_up_to_2015_h20_v01;
run;

/* Nursing home */
data nursing_home;
set V_INTG01.ig_kost2011_h20_v01;
run;

/* 2011 data */
data data2011;
set V_INTG01.IG_GEZO_CONT2011_H20_V01;
run;

/* 2012 data */
data data2012;
set V_INTG01.IG_GEZO_CONT2012_H20_V01;
run;

/* 2013 data */
data data2013;
set V_INTG01.IG_GEZO_CONT2013_H20_V01;
run;

/* 2014 data */
data data2014;
set V_INTG01.IG_GEZO_CONT2014_H20_V01;
run;

/* 2015 data */
data data2015;
set V_INTG01.IG_GEZO_CONT2015_H20_V01;
run;

/* 2011 population */
data population2011;
set V_INTG01.ig_popu2011_h20_v01;
run;

/* 2012 population */
data population2012;
set V_INTG01.ig_popu2012_h20_v01;
run;

/* 2013 population */
data population2013;
set V_INTG01.ig_popu2013_h20_v01;
run;

/* 2014 population */
data population2014;
set V_INTG01.ig_popu2014_h20_v01;
run;

/* 2015 population */
data population2015;
set V_INTG01.ig_popu2015_h20_v01;
run;

proc datasets library=WORK noprint;
	run;
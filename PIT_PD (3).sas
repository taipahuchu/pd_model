options mlogic mprint symbolgen;

filename data '/home/u49308301/PhD/Lifetime_PD.xlsx';

proc import datafile=data dbms=xlsx out=mydata replace;
	getnames=yes;
RUN;


/* Getting the left and right censored yearly data  */

DATA mydata1;
	SET mydata;
	by cust_id year ;

	if last.cust_id=1 or last.year=1 then
		output;
RUN;

%Let inputs = default_flag repayment_type loan_term mob remaining_term bureau_score_orig income gdp uer cpi hpi ir gdp_lag uer_lag cpi_lag hpi_lag;
%put &inputs;


proc contents data = mydata1 order = varnum;
run;

DATA mydata2;
SET mydata1(keep = &inputs);
retain &inputs;
run;

proc means data = mydata2 n nmiss mean median mode min max;
var _numeric_;
run;

proc freq data = mydata2;
table _char_;
run;

proc univariate data = mydata2 plots ;
var default_flag loan_term mob remaining_term bureau_score_orig income gdp uer cpi hpi ir gdp_lag uer_lag cpi_lag hpi_lag;
run;

/* Oversampling */
proc freq data =  mydata1;
table default_flag;
run;

data sample; 
set mydata2;
where default_flag = 0 ;
run;

proc surveyselect data = sample method = srs  reps=1 sampsize = 1000 out = sampledata;
id _all_;
run;

Proc sql;
create table mydata3 as
select * from sampledata (drop= replicate)
union 
select * 
from mydata2 
where default_flag = 1;
quit;

proc print data=sampledata(obs=20);
run;


/* Splitting our dataset into development and validation datasets */

proc sort data = mydata3 out =mydata3;
by default_flag;
run;


title1 'Customer Satisfaction Survey';
title2 'Stratified Sampling';
proc surveyselect data=mydata3 method=srs  seed=1953 out=SampleStrata samprate= .75 outall  ;
strata default_flag;
RUN;

data train test;
set samplestrata;
if selected = 1 then output train;
else output test;
run;

/* Checking the splitting */

proc freq data = SampleStrata;
table default_flag*selected;
RUN;

DATA train;
set train;
drop selectionprob samplingweight selected;
run;

/* Calculating Information Number and Weight of Evidence for all variables */

/* Creating an empty table for the Information summary */

proc sql;
create table IV_summary
  (
   Variable char(30),
   IV num
  );
  run;
quit;


proc contents data = train out = cols noprint ;
run;

proc sql noprint;
select name into :name separated by " " from cols 
where upcase(name) ^= %upcase("default_flag");
quit;
%put &=name;

/* creating a macro for default rate aggregates and the calculation of the weight of evidence  */

%LET error = 1e-12;
%put &error;

%macro bivariate(var); 
proc sql noprint; 
create table &var as 
select &var , sum(Default_Flag) as default, 
count (*)as total, sum(Default_Flag)/count (*) as default_rate 
from train
group by &var; 

select sum(Default), sum(Total) into: default_total , :grand_total from &var; 
run;
create table &var as 
select *, log(per_good/(per_bad+&error)) as woe , (per_good - per_bad)*log(per_good/(per_bad+&error)) as IV from 
(
select  &var , default, Total , default / &default_total as per_bad ,
(total-default) /(&grand_total - &default_total+&error) as per_good
from &var
); 
Quit;


data &var;
set &var end=eof;
iv_sum  + iv;
if eof = 1 then  call symputx("sum",iv_sum);
run;

%put &=sum;

/* Creating the information number summary table */

proc sql;
insert into IV_summary values ("&var",&sum);
quit;

ods excel options (sheet_interval = "table" embedded_titles = 'yes' embedded_titles_onces= 'yes') ;

title "&var";
proc print data = &var ;
run; 
title;

%mend bivariate();

ods excel file = '/home/u49308301/PhD/IV.xlsx' ;

%macro runs();
%let i = 1;
%do %while(%scan(&name,&i) ne);
%let myvar = %scan(&name,&i);
%bivariate (&myvar); 
%let i = %eval(&i + 1);
%end;
%mend runs;
%runs ;
ods noproctitles;
proc print data = IV_summary; 
run; 

ods excel close;
/* dropping the WOE tables */

%macro mydel();
%let i = 1;
%do %while(%scan(&name,&i) ne);
%let myvar = %scan(&name,&i);
proc sql;
drop table &myvar;
quit; 
%let i = %eval(&i + 1);
%end;
%mend mydel;
%mydel ;


/* Coarse classing   */

data train1; 
set train; 

if  cpi < = 1.5 then cpi_band=1; 
else if  cpi < = 3 then cpi_band=2; 
else if cpi < = 4.5 then cpi_band=3; 
else cpi_band =4;  

if  cpi_lag < = 1.5 then cpi_lag_band=1; 
else if  cpi_lag < = 3 then cpi_lag_band=2; 
else if cpi_lag < = 4.5 then cpi_lag_band=3; 
else cpi_lag_band =4; 

if  bureau_score_orig < = 450 then bureau_band=1; 
else if  bureau_score_orig < = 550 then bureau_band=2; 
else bureau_band =3; 

if  gdp < = -3 then gdp_band=1; 
else if  gdp < = 0 then gdp_band=2; 
else if gdp < = 3 then gdp_band=3; 
else gdp_band =4; 

if  gdp_lag < = -3 then gdp_lag_band=1; 
else if  gdp_lag < = 0 then gdp_lag_band=2; 
else if gdp_lag < = 3 then gdp_lag_band=3; 
else gdp_lag_band =4; 

if  hpi < = -6 then hpi_band=1; 
else if  hpi < = 0 then hpi_band=2; 
else if hpi < = 6 then hpi_band=3; 
else hpi_band =4; 

if  hpi_lag < = -6 then hpi_lag_band=1; 
else if  hpi_lag < = 0 then hpi_lag_band=2; 
else if hpi_lag < = 6 then hpi_lag_band=3; 
else hpi_lag_band =4; 

if  income < = -.5 then income_band=1; 
else if  income < = 0 then income_band=2; 
else if income < = .5 then income_band=3; 
else income_band =4; 

if  ir < = 2 then ir_band=1; 
else if  ir < = 3 then ir_band=2; 
else if ir < = 4 then ir_band=3; 
else ir_band=4; 

if  loan_term < = 100 then loan_term_band=1; 
else if  loan_term < = 200 then loan_term_band=2; 
else if loan_term < = 300 then loan_term_band=3; 
else loan_term_band=4; 

if  mob < = 70 then mob_band=1; 
else if  mob < = 100 then mob_band=2; 
else if mob < = 220 then mob_band=3; 
else mob_band=4; 

if  remaining_term < = 70 then remaining_term_band=1; 
else if  remaining_term < = 150 then remaining_term_band=2; 
else if remaining_term < = 320 then remaining_term_band=3; 
else remaining_term_band=4; 


if  uer < = 5 then uer_band=1; 
else if  uer < = 6 then uer_band=2; 
else if uer < = 7 then uer_band=3; 
else uer_band=4; 


if  uer_lag < = 5 then uer_lag_band=1; 
else if  uer_lag < = 6 then uer_lag_band=2; 
else if uer_lag < = 7 then uer_lag_band=3; 
else uer_lag_band=4; 
drop bureau_score_orig cpi cpi_lag gdp gdp_lag hpi hpi_lag income ir loan_term mob remaining_term repayment_type uer uer_lag ;
run;

proc contents data= train1 out = col ;run;
/* Calculating Information Number and Weight of Evidence for all variables */

/* Creating an empty table for the Information summary */

proc sql;
create table IV_summary
  (
   Variable char(30),
   IV num
  );
  run;
quit;


proc contents data = train out = cols noprint ;
run;

proc sql noprint;
select name into :name separated by " " from cols 
where upcase(name) ^= %upcase("default_flag");
quit;
%put &=name;

/* creating a macro for default rate aggregates and the calculation of the weight of evidence  */

%LET error = 1e-12;
%put &error;

%macro bivariate(var); 
proc sql noprint; 
create table &var as 
select &var , sum(Default_Flag) as default, 
count (*)as total, sum(Default_Flag)/count (*) as default_rate 
from train1
group by &var; 

select sum(Default), sum(Total) into: default_total , :grand_total from &var; 
run;
create table &var as 
select *, log(per_good/(per_bad+&error)) as woe , (per_good - per_bad)*log(per_good/(per_bad+&error)) as IV from 
(
select  &var , default, Total , default / &default_total as per_bad ,
(total-default) /(&grand_total - &default_total+&error) as per_good
from &var
); 
Quit;


data &var;
set &var end=eof;
iv_sum  + iv;
if eof = 1 then  call symputx("sum",iv_sum);
run;

%put &=sum;

/* Creating the information number summary table */

proc sql;
insert into IV_summary values ("&var",&sum);
quit;

ods excel options (sheet_interval = "table" embedded_titles = 'yes' embedded_titles_onces= 'yes') ;

title "&var";
proc print data = &var ;
run; 
title;

%mend bivariate();

%let inputs = bureau_band cpi_band cpi_lag_band gdp_band gdp_lag_band hpi_band hpi_lag_band income_band ir_band loan_term_band mob_band remaining_term_band uer_band uer_lag_band;

ods excel file = '/home/u49308301/PhD/IV_band.xlsx' ;

%macro runs();
%let i = 1;
%do %while(%scan(&inputs,&i) ne);
%let inp = %scan(&inputs,&i);
%bivariate (&inp); 
%let i = %eval(&i + 1);
%end;
%mend runs;
%runs ;
ods noproctitles;
proc print data = IV_summary; 
run; 

ods excel close;


/* Creating WOE variable  */
data train2; 
set train1; 

 if  bureau_band =1 then bureau_band_woe = 0.52634;
 else if  bureau_band =2 then bureau_band_woe = 0.732153;
 else bureau_band_woe = 1.257207;


 if  cpi_band =1 then cpi_band_woe = 0.007475;
 else if  cpi_band =2 then cpi_band_woe = 0.015636;
 else if  cpi_band =3 then cpi_band_woe = 0.035004;
 else cpi_band_woe = 0.050416;

 if  cpi_lag_band =1 then cpi_lag_band_woe = 0.049433;
 else if  cpi_lag_band =2 then cpi_lag_band_woe = 0.068931;
 else if  cpi_lag_band =3 then cpi_lag_band_woe = 0.06965;
 else cpi_lag_band_woe = 0.069798;

 if  gdp_band =1 then gdp_band_woe = 0.150774;
 else if  gdp_band =2 then gdp_band_woe = 0.206133;
 else if  gdp_band =3 then gdp_band_woe = 0.25373;
 else gdp_band_woe = 0.269577;

 if  gdp_lag_band =1 then gdp_lag_band_woe = 0.00105;
 else if  gdp_lag_band =2 then gdp_lag_band_woe = 0.003755;
 else if  gdp_lag_band =3 then gdp_lag_band_woe = 0.003767;
 else gdp_lag_band_woe = 0.003883;

 if  hpi_band =1 then hpi_band_woe = 0.224709;
 else if  hpi_band =2 then hpi_band_woe = 0.225602;
 else if  hpi_band =3 then hpi_band_woe = 0.228048;
 else hpi_band_woe = 0.298897;

 if  hpi_lag_band =1 then hpi_lag_band_woe = 0.000777;
 else if  hpi_lag_band =2 then hpi_lag_band_woe = 0.001476;
 else if  hpi_lag_band =3 then hpi_lag_band_woe = 0.002321;
 else hpi_lag_band_woe = 0.002528;

 if  income_band =1 then income_band_woe = 0.001098;
 else if  income_band =2 then income_band_woe = 0.001449;
 else if  income_band =3 then income_band_woe = 0.033697;
 else income_band_woe = 0.044809;

 if  ir_band =1 then ir_band_woe = 0.000758;
 else if  ir_band =2 then ir_band_woe = 0.001889;
 else if  ir_band =3 then ir_band_woe = 0.012507;
 else ir_band_woe = 0.013688;

 if  loan_term_band =1 then loan_term_band_woe = 0.080659;
 else if  loan_term_band =2 then loan_term_band_woe = 0.080735;
 else if  loan_term_band =3 then loan_term_band_woe = 0.08981;
 else loan_term_band_woe = 0.119366;

 if  mob_band =1 then mob_band_woe = 0.00253;
 else if  mob_band =2 then mob_band_woe = 0.007594;
 else if  mob_band =3 then mob_band_woe = 0.008544;
 else mob_band_woe = 0.013002;

 if  remaining_term_band =1 then remaining_term_band_woe = 0.071665;
 else if  remaining_term_band =2 then remaining_term_band_woe = 0.106347;
 else if  remaining_term_band =3 then remaining_term_band_woe = 0.107785;
 else remaining_term_band_woe = 0.131812;

 if  uer_band =1 then uer_band_woe = 0.04727;
 else if  uer_band =2 then uer_band_woe = 0.052054;
 else if  uer_band =3 then uer_band_woe = 0.145755;
 else uer_band_woe = 0.146266;

 if  uer_lag_band =1 then uer_lag_band_woe = 0.103223;
 else if  uer_lag_band =2 then uer_lag_band_woe = 0.144062;
 else if  uer_lag_band =3 then uer_lag_band_woe = 0.146976;
 else uer_lag_band_woe = 0.154988;
 
 drop &inputs;
run; 

proc contents data = train2;
run;

/* Multicollinearity check  */
proc reg data= train2 ; 
model  Default_Flag=  bureau_band_woe cpi_band_woe cpi_lag_band_woe gdp_band_woe gdp_lag_band_woe  hpi_band_woe 
 hpi_lag_band_woe income_band_woe ir_band_woe loan_term_band_woe mob_band_woe remaining_term_band_woe uer_band_woe uer_lag_band_woe  /vif;
run; 
/* Logistic Regression Analysis  */
/* ods rtf ;  */


proc logistic data = train2 descending ; 
model  
Default_Flag =  remaining_term_band_woe loan_term_band_woe ir_band_woe hpi_lag_band_woe
 gdp_lag_band_woe gdp_band_woe bureau_band_woe /lackfit rsq ; 
output out = train3 p = p_hat; 
run; 

/*
log(p/1-p) = 2.2741 +  -21.2035 * remaining_term_band_ +  36.5747 * loan_term_band_woe +  -60.7317 * ir_band_woe 
+  509.8 * hpi_lag_band_woe +  -280.9 * gdp_lag_band_woe +  -15.9987 * gdp_band_woe +  -4.1325 * bureau_band_woe 
*/


proc freq data =  mydata1;
table default_flag;
run;


/* Creating Data for KS and Gini Coefficient (discriminatory test)  */


data train3;
set train3;
run;
 

proc sort data = train3 ;
 by descending Default_Flag p_hat; 
run;  
 
ods excel file= '/home/u49308301/PhD/probability.xlsx' ;
proc print data = train3;
run;
ods excel close;


/* divide data into 10 equal observation bins */ 
proc sql;
create table train3 as select * from  train3;
quit;
 
ods excel file='/home/u49308301/PhD/validate.xlsx'; 
%let Noofrecs = &sqlobs;
%let Noofbins = 10; 
data pred_default; 
set train3; 
count = 1; 
cumm_count +count; 
bin = round(cumm_count/(&Noofrecs/&Noofbins)-0.5)+1; 
if bin GT &Noofbins then Bin = &Noofbins; 
run; 
proc sql; 
create table gains_dev as 
select bin, count(*) as freq, sum(Default_Flag) as default, 
mean(p_hat)as exp_bad 
from pred_default 
group by bin; 
run; 
proc print data = gains_dev; 
run; 
ods excel close; 

/* Clustering  */

data train4;
set train3;
score = Round(p_hat*1000);
RUN ;

ods html file = '/home/u49308301/UDEMY/ods4.xls';
proc freq data = train4;
tables score;
RUN;
ODS HTML CLOSE;






/* Validation   */





proc means data= test n nmiss max min median mean p1 p5 p95 p99; 
run; 

data test1; 
set test; 

if  cpi < = 1.5 then cpi_band=1; 
else if  cpi < = 3 then cpi_band=2; 
else if cpi < = 4.5 then cpi_band=3; 
else cpi_band =4;  

if  cpi_lag < = 1.5 then cpi_lag_band=1; 
else if  cpi_lag < = 3 then cpi_lag_band=2; 
else if cpi_lag < = 4.5 then cpi_lag_band=3; 
else cpi_lag_band =4; 

if  bureau_score_orig < = 450 then bureau_band=1; 
else if  bureau_score_orig < = 550 then bureau_band=2; 
else bureau_band =3; 

if  gdp < = -3 then gdp_band=1; 
else if  gdp < = 0 then gdp_band=2; 
else if gdp < = 3 then gdp_band=3; 
else gdp_band =4; 

if  gdp_lag < = -3 then gdp_lag_band=1; 
else if  gdp_lag < = 0 then gdp_lag_band=2; 
else if gdp_lag < = 3 then gdp_lag_band=3; 
else gdp_lag_band =4; 

if  hpi < = -6 then hpi_band=1; 
else if  hpi < = 0 then hpi_band=2; 
else if hpi < = 6 then hpi_band=3; 
else hpi_band =4; 

if  hpi_lag < = -6 then hpi_lag_band=1; 
else if  hpi_lag < = 0 then hpi_lag_band=2; 
else if hpi_lag < = 6 then hpi_lag_band=3; 
else hpi_lag_band =4; 

if  income < = -.5 then income_band=1; 
else if  income < = 0 then income_band=2; 
else if income < = .5 then income_band=3; 
else income_band =4; 

if  ir < = 2 then ir_band=1; 
else if  ir < = 3 then ir_band=2; 
else if ir < = 4 then ir_band=3; 
else ir_band=4; 

if  loan_term < = 100 then loan_term_band=1; 
else if  loan_term < = 200 then loan_term_band=2; 
else if loan_term < = 300 then loan_term_band=3; 
else loan_term_band=4; 

if  mob < = 70 then mob_band=1; 
else if  mob < = 100 then mob_band=2; 
else if mob < = 220 then mob_band=3; 
else mob_band=4; 

if  remaining_term < = 70 then remaining_term_band=1; 
else if  remaining_term < = 150 then remaining_term_band=2; 
else if remaining_term < = 320 then remaining_term_band=3; 
else remaining_term_band=4; 


if  uer < = 5 then uer_band=1; 
else if  uer < = 6 then uer_band=2; 
else if uer < = 7 then uer_band=3; 
else uer_band=4; 


if  uer_lag < = 5 then uer_lag_band=1; 
else if  uer_lag < = 6 then uer_lag_band=2; 
else if uer_lag < = 7 then uer_lag_band=3; 
else uer_lag_band=4; 
drop bureau_score_orig cpi cpi_lag gdp gdp_lag hpi hpi_lag income ir loan_term mob remaining_term repayment_type uer uer_lag ;
run;



/* Creating woe variables for validation sample  */
data test2; 
set test1; 

 if  bureau_band =1 then bureau_band_woe = 0.52634;
 else if  bureau_band =2 then bureau_band_woe = 0.732153;
 else bureau_band_woe = 1.257207;


 if  cpi_band =1 then cpi_band_woe = 0.007475;
 else if  cpi_band =2 then cpi_band_woe = 0.015636;
 else if  cpi_band =3 then cpi_band_woe = 0.035004;
 else cpi_band_woe = 0.050416;

 if  cpi_lag_band =1 then cpi_lag_band_woe = 0.049433;
 else if  cpi_lag_band =2 then cpi_lag_band_woe = 0.068931;
 else if  cpi_lag_band =3 then cpi_lag_band_woe = 0.06965;
 else cpi_lag_band_woe = 0.069798;

 if  gdp_band =1 then gdp_band_woe = 0.150774;
 else if  gdp_band =2 then gdp_band_woe = 0.206133;
 else if  gdp_band =3 then gdp_band_woe = 0.25373;
 else gdp_band_woe = 0.269577;

 if  gdp_lag_band =1 then gdp_lag_band_woe = 0.00105;
 else if  gdp_lag_band =2 then gdp_lag_band_woe = 0.003755;
 else if  gdp_lag_band =3 then gdp_lag_band_woe = 0.003767;
 else gdp_lag_band_woe = 0.003883;

 if  hpi_band =1 then hpi_band_woe = 0.224709;
 else if  hpi_band =2 then hpi_band_woe = 0.225602;
 else if  hpi_band =3 then hpi_band_woe = 0.228048;
 else hpi_band_woe = 0.298897;

 if  hpi_lag_band =1 then hpi_lag_band_woe = 0.000777;
 else if  hpi_lag_band =2 then hpi_lag_band_woe = 0.001476;
 else if  hpi_lag_band =3 then hpi_lag_band_woe = 0.002321;
 else hpi_lag_band_woe = 0.002528;

 if  income_band =1 then income_band_woe = 0.001098;
 else if  income_band =2 then income_band_woe = 0.001449;
 else if  income_band =3 then income_band_woe = 0.033697;
 else income_band_woe = 0.044809;

 if  ir_band =1 then ir_band_woe = 0.000758;
 else if  ir_band =2 then ir_band_woe = 0.001889;
 else if  ir_band =3 then ir_band_woe = 0.012507;
 else ir_band_woe = 0.013688;

 if  loan_term_band =1 then loan_term_band_woe = 0.080659;
 else if  loan_term_band =2 then loan_term_band_woe = 0.080735;
 else if  loan_term_band =3 then loan_term_band_woe = 0.08981;
 else loan_term_band_woe = 0.119366;

 if  mob_band =1 then mob_band_woe = 0.00253;
 else if  mob_band =2 then mob_band_woe = 0.007594;
 else if  mob_band =3 then mob_band_woe = 0.008544;
 else mob_band_woe = 0.013002;

 if  remaining_term_band =1 then remaining_term_band_woe = 0.071665;
 else if  remaining_term_band =2 then remaining_term_band_woe = 0.106347;
 else if  remaining_term_band =3 then remaining_term_band_woe = 0.107785;
 else remaining_term_band_woe = 0.131812;

 if  uer_band =1 then uer_band_woe = 0.04727;
 else if  uer_band =2 then uer_band_woe = 0.052054;
 else if  uer_band =3 then uer_band_woe = 0.145755;
 else uer_band_woe = 0.146266;

 if  uer_lag_band =1 then uer_lag_band_woe = 0.103223;
 else if  uer_lag_band =2 then uer_lag_band_woe = 0.144062;
 else if  uer_lag_band =3 then uer_lag_band_woe = 0.146976;
 else uer_lag_band_woe = 0.154988;
 
logit = 2.2741 +  -21.2035 * remaining_term_band_woe +  36.5747 * loan_term_band_woe +  -60.7317 * ir_band_woe 
+  509.8 * hpi_lag_band_woe +  -280.9 * gdp_lag_band_woe +  -15.9987 * gdp_band_woe +  -4.1325 * bureau_band_woe ; 

p_hat = 1 / (1 + exp(-logit)); 

run; 

/*  The final logistic equation is log (P/1-P)=  */
/* 1.6879+income_woe*1.6038+housing_woe*2.2502+age_woe*1.0382+Occupartner_w */
/*  oe*2.3120+Occutype_woe*2.2050;  */
/* Creating data for KS and Gini   */
PROC SQL; create table test2 as select * from test2; QUIT;

proc sort data = test2; by descending p_hat; 
run; 

ods html file= '/home/u49308301/UDEMY/ods_test1.xls'; 
%let Noofrecs = &sqlobs; 
%let Noofbins = 10; 
data test3; 
set test2; 
retain cumm_count; 
count = 1; 
cumm_count = sum(cumm_count,count); 
bin = round(cumm_count/(&Noofrecs/&Noofbins)-0.5)+1; 
if bin GT &Noofbins then Bin = &Noofbins; 
run; 
proc sql; 
create table gains_val as 
select bin, count(*) as freq, sum(Default_Flag) as default 
from test3 
group by bin; 
run; 
proc print data = gains_val; 
run; 
ods html close; 


proc logistic data = test2 ;
model default_flag = remaining_term_band_woe loan_term_band_woe ir_band_woe hpi_lag_band_woe gdp_lag_band_woe
gdp_band_woe bureau_band_woe ; 
score out = testscore;
run;
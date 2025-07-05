options mprint mlogic;
libname score base '/home/u49308301/UDEMY';
filename data '/home/u49308301/UDEMY/Data.csv';

PROC IMPORT DATAFILE=data OUT=train DBMS=csv replace;
	getnames=yes;
	guessingrows=20;
RUN;

proc freq data=train;
	table _char_;
RUN;

/* Univariate analysis  */
proc means data=train n nmiss min max median mean;
run;

/* MISSING INDICATORS   */
data train;
set train;
array m{*} MDPD_30_6M MDPD_60_6M;
array x{*} DPD_30_6M DPD_60_6M;
do i = 1 to dim(m);
m(i) = (x(i)=.);
mmiss + m(i);
drop i;
end;
run;


/* Filling missing values  */
proc stdize data=train reponly method=median out=train_impute;
run;

/* Collapsing categories by thresholding  */
data train_impute;
	set train_impute;

	if Occupation_type in ("Self-Em", "Self-em", "Unemplo") then
		Occupation_type="Other";
	else IF Occupation_type=Occupation_type;
run;

proc means data=work.train_impute noprint nway;
	class Occupation_Partner;
	var default_flag;
	OUTPUT OUT=WORK.LEVEL mean=prop;
RUN;

title "Proportion of the event by level";

proc print data=work.level;
run;

/* Use the ods output the Clusterhistory output object into a dataset named "cluster" */
ods output clusterhistory=work.cluster;

proc cluster data=work.level method=ward outtree=work.fortree 
		plots=(dendrogram (vertical height=rsq));
	freq _freq_;
	var prop;
	id Occupation_Partner;
run;

/* Use the freq procedure to get the Pearson Chi ^2 statistic of the full Occupation_partner and default_flag table */
proc freq data=work.train_impute noprint;
	tables Occupation_partner*default_flag / chisq;
	output out=work.chi(keep=_pchi_) chisq;
run;

/* use the one - to _many merge to put the chi^2 statistic on the clustering results. calculate a (log) p_value for each level of clustering  */
data cutoff;
	if _n_=1 then
		set work.chi;
	set work.cluster;
	chisquare=_pchi_*rsquared;
	degfree=numberofclusters - 1;
	logpvalue=logsdf("CHISQ", chisquare, degfree);
run;

/* Plot the log values against number of the clusters */
proc sgplot data=work.cutoff;
	scatter y=logpvalue x=numberofclusters / markerattrs=(color=blue 
		symbol=circlefilled);
	xaxis label="Number of clusters";
	yaxis label='Log pvalue';
run;

/* create a macro variable (&ncl) that contains the number of clusters  */
/* associated with the minimum log p-value  */
proc sql;
	select NumberofClusters into :ncl from work.cutoff having 
		logpvalue=min(logpvalue);
quit;

data fortrees;
	set fortree;
run;

proc tree data=work.fortree nclusters=&ncl out=work.clus noprint;
	id occupation_partner;
run;

proc sort data=work.clus;
	by clusname;
run;

/* The data step creates the scoring code to assign the occupation_partner to a cluster; */
filename brclus "/home/u49308301/UDEMY/Branchclus.sas";

data _null_;
	file brclus;
	set work.clus end=last;

	if _n_=1 then
		put "select (Occupation_partner);";
	put " when ('" Occupation_partner +(-1) "') OccupationP_clus = '" 
		cluster +(-1) "';";

	if last then
		do;
			put " otherwise OccupationP_clus = 'U';"/"end;";
		end;
run;


data work.train_impute;
set work.train_impute;
%include brclus/ source2;
run; 


proc freq data=train_impute;
	table OccupationP_clus Occupation_partner;
RUN;

/* Getting columns names  */
proc contents data=train_impute out=cols noprint;
run;

proc sql noprint;
	select name into :inputs separated by " " from cols where upcase(name) not in 
		%upcase(("Cust_id", "Credit_Amount"));
quit;

%put &inputs;

/* Bivariate Analysis (Fine Classing)  */
/* Calculating Information Number and Weight of Evidence for all variables */
/* Creating an empty table for the Information summary gettting the predictive power of a varible */
proc sql;
	create table IV_summary
  (Variable char(30), IV num);
	run;
quit;

%put &=inputs;

/* creating a macro for default rate aggregates and the calculation of the weight of evidence  */
%macro bivariate(var);
	proc sql noprint;
		create table bi_&var as select &var , sum(Default_Flag) as default, 
			count (*)as total, sum(Default_Flag)/count (*) as default_rate from 
			train_impute group by &var;
		select sum(Default), sum(Total) into: default_total , :grand_total from 
			bi_&var;
		create table bi_&var as select *, log(per_good/(per_bad+.00000000001)) as 
			woe , (per_good - per_bad)*log(per_good/(per_bad+.00000000001)) as IV from 
(select  &var , default, Total , default_rate, default / &default_total as 
			per_bad , (total-default) /(&grand_total - &default_total+.00000000000001) 
			as per_good from bi_&var);
	Quit;

	data bi_&var;
		set bi_&var end=eof;
		iv_sum  + iv;

		if eof=1 then
			call symputx("sum", iv_sum);
	run;

	/* Creating the information number summary table */
	proc sql noprint;
		insert into IV_summary values ("&var", &sum);
		ods excel options (sheet_name="Summary");
	quit;

	title1 "bi_&var";

	proc print data=bi_&var;
		ods excel options (sheet_name="&var");
	run;

	title1;
%mend bivariate();

filename outfile "/home/u49308301/UDEMY/ODS1.xlsx";
ods excel file=outfile options(sheet_interval="table" embedded_titles='yes');

%macro runs();
	%let i = 1;

	%do %while(%scan(&inputs, &i) ne);
		%let myvar = %scan(&inputs, &i);
		%bivariate (&myvar);
		%let i = %eval(&i + 1);
	%end;
%mend runs;

%runs;
title1 'Variable Information Number summary';

proc print data=iv_summary;
run;

ods excel options (sheet_name="Summary" embedded_titles='Yes');
title1;
ods excel close;

/* Coarse classing   */
data train_impute_coarse;
	set train_impute;

	if Occupation_type="Govt" then
		occupation_type_band=1;
	else if Occupation_type="Private" then
		occupation_type_band=2;
	else
		occupation_type_band=3;

	if Employed_since_yrs <=7 then
		employed_band=1;
	else if 7 < Employed_since_yrs < 12 then
		employed_band=2;
	else
		employed_band=3;

	if age <=27 then
		age_band=1;
	else if 27 < age < 43 then
		age_band=2;
	else
		age_band=3;

	if Marital_Status="Married" then
		Marital_band=1;
	else
		Marital_band=2;

	if Occupation_type="Govt" then
		Occutype_band=1;
	else if Occupation_type="Private" then
		Occutype_band=2;
	else
		Occutype_band=3;

	if DPD_30_6M=0 then
		DPD306M=1;
	else
		DPD306M=2;

	if DPD_60_6M=0 then
		DPD606M=1;
	else
		DPD606M=2;

	if Income_group="< = 15000" then
		Income_band=1;
	else if Income_group="< = 25000" then
		Income_band=1;
	else if Income_group="< = 35000" then
		Income_band=2;
	else if Income_group="< = 50000" then
		Income_band=2;
	else if Income_group="<=100000" then
		Income_band=2;
	else
		Income_band=3;
run;

/* Bivariate Analysis (coarse Classing)  */
/* Calculating Information Number and Weight of Evidence for all variables */
/* Creating an empty table for the Information summary gettting the predictive power of a varible */

%let myinputs = age_band Marital_band occupation_type_band OccupationP_clus DPD306M DPD606M Income_band employed_band MDPD_30_6M MDPD_60_6M;

proc sql;
	create table IV_summ
  (Variable char(30), IV num);
	run;
quit;

%put &=inputs;

/* creating a macro for default rate aggregates and the calculation of the weight of evidence  */
%macro bivariate(var);
	proc sql noprint;
		create table bi_&var as select &var , sum(Default_Flag) as default, 
			count (*)as total, sum(Default_Flag)/count (*) as default_rate from 
			train_impute_coarse group by &var;
		select sum(Default), sum(Total) into: default_total , :grand_total from 
			bi_&var;
		create table bi_&var as select *, log(per_good/(per_bad+.00000000001)) as 
			woe , (per_good - per_bad)*log(per_good/(per_bad+.00000000001)) as IV from 
(select  &var , default, Total , default_rate, default / &default_total as 
			per_bad , (total-default) /(&grand_total - &default_total+.00000000000001) 
			as per_good from bi_&var);
	Quit;

	data bi_&var;
		set bi_&var end=eof;
		iv_sum  + iv;

		if eof=1 then
			call symputx("sum", iv_sum);
	run;

	/* Creating the information number summary table */
	proc sql noprint;
		insert into IV_summ values ("&var", &sum);
		ods excel options (sheet_name="Summary");
	quit;

	title1 "bi_&var";

	proc print data=bi_&var;
		ods excel options (sheet_name="&var");
	run;

	title1;
%mend bivariate();

filename outfile "/home/u49308301/UDEMY/ODS2.xlsx";
ods excel file=outfile options(sheet_interval="table" embedded_titles='yes');

%macro runs();
	%let i = 1;

	%do %while(%scan(&myinputs, &i) ne);
		%let myvar = %scan(&myinputs, &i);
		%bivariate (&myvar);
		%let i = %eval(&i + 1);
	%end;
%mend runs;

%runs;
title1 'Variable Information Number summary';

proc print data=IV_summ;
run;

ods excel options (sheet_name="My Summary" embedded_titles='Yes');
title1;
ods excel close;

/* Smooth weight of eveidence to marital status SWOE*/

/* ===================================================== */

/* Rho1 is the proportion of events in the training data set. */
%global rho1;
proc sql noprint;
   select mean(default_flag) into :rho1
   from work.train_impute_coarse;
run; 
%put &rho1;
/* The output data set from PROC MEANS will have the number of
   observations and events for each level of branch. */

proc means data=work.train_impute_coarse sum nway noprint;
   class marital_status;
   var default_flag;
   output out=work.counts sum=events;
run;

/* The DATA Step creates the scoring code that assigns each branch to
   a value of the smoothed weight of evidence. */


filename mrswoe "/home/u49308301/EPMLR51/swoe_branch.sas";

data _null_;
   file mrswoe;
   set work.counts end=last;
   logit=log((events + &rho1*24)/(_FREQ_ - events + (1-&rho1)*24));
   if _n_=1 then put "select (marital_status);" ;
   put "  when ('" marital_status +(-1) "') marital_status_swoe = " logit ";" ;
   if last then do;
   logit=log(&rho1/(1-&rho1));
   put "  otherwise marital_status_swoe = " logit ";" / "end;";
   end;
run;

data work.train_impute_coarse;
   set work.train_impute_coarse;
   %include mrswoe / source2;
run;


/* Creating WOE variable  */
data train_impute_coarse1;
	set train_impute_coarse;

	if age_band=1 then
		age_band_woe=-0.79605;
	else if age_band=2 then
		age_band_woe=-0.21258;
	else
		age_band_woe=0.49186;

	if occupation_type_band=1 then
		occupation_type_band_woe=-0.39361;
	else if occupation_type_band=2 then
		occupation_type_band_woe=0.62011;
	else
		occupation_type_band_woe=-1.43191;

	if OccupationP_clus=1 then
		OccupationP_clus_woe=0.29425;
	else if OccupationP_clus=2 then
	OccupationP_clus_woe =1.20779;
	else OccupationP_clus_woe = -0.41826;	 

	if Occutype_band=1 then
		Occutype_band_woe=-0.39361;
	else if Occutype_band=2 then
		Occutype_band_woe=0.62011;
	else
		Occutype_band_woe=-1.43191;

	if Income_band=1 then
		Income_band_woe=-0.56317;
	else if Income_band=2 then
		Income_band_woe=-0.20362;
	else
		Income_band_woe=0.89939;

	if employed_band=1 then
		employed_band_woe=-0.36819;
	else if employed_band=2 then
		employed_band_woe=-0.19734;
	else
		employed_band_woe=0.77219;

	if Housing_1_Own_2_Rent=1 then
		housing_woe=-0.20757;
	else
		housing_woe=0.137949;
run;

/* Multicollinearity check  */
proc reg data=train_impute_coarse1;
	model Default_Flag=age_band_woe  occupation_type_band_woe 
		OccupationP_clus_woe Income_band_woe employed_band_woe housing_woe marital_status_swoe
		/vif;
	run;

/* VARIBLE CLUSTERING  */

/* ===================================================== 

   Demonstration: Reducing Redundancy by Clustering Variables
/* ===================================================== */

/* Use the ODS OUTPUT statement to generate data sets based on the variable 
   clustering results and the clustering summary. */
%let inputs = age_band_woe  occupation_type_band_woe 
		OccupationP_clus_woe Income_band_woe employed_band_woe housing_woe marital_status_swoe;
		
ods select none;
ods output clusterquality=work.summary
           rsquare=work.clusters;

proc varclus data=work.train_impute_coarse1 maxeigen=.7 hi;
   var &inputs;
run;
ods select all;

/* Use the CALL SYMPUT function to create a macro variable:&NVAR = 
   the number of clusters. This is also the number of variables 
   in the analysis, going forward. */

%global nvar;
data _null_;
   set work.summary;
   call symput('nvar',compress(NumberOfClusters));
run;

title1 "Variables by Cluster";
proc print data=work.clusters noobs label split='*';
   where NumberOfClusters=&nvar;
   var Cluster Variable RSquareRatio ;
   label RSquareRatio="1 - RSquare*Ratio";
run;
title1 ;

title1 "Variation Explained by Clusters";
proc print data=work.summary label;
run;

/* Choose a representative from each cluster.  */
%global reduced;
%let reduced=age_band_woe  occupation_type_band_woe 
		OccupationP_clus_woe Income_band_woe employed_band_woe housing_woe marital_status_swoe;


	/* Logistic Regression Analysis  */
proc logistic data=train_impute_coarse1 descending;
	model Default_Flag=age_band_woe  occupation_type_band_woe 
		OccupationP_clus_woe income_band_woe employed_band_woe  housing_woe marital_status_swoe;
	output out=score.train5 p=phat;
run;

/* ===================================================== *
   Demonstration: Performing Variable Screening
                                                            */
/* ===================================================== */

ods select none;
ods output spearmancorr=work.spearman
           hoeffdingcorr=work.hoeffding;

proc corr data=work.train_impute_coarse1 spearman hoeffding;
   var default_flag;
   with &reduced;
run;

ods select all;

proc sort data=work.spearman;
    by variable;
run;

proc sort data=work.hoeffding;
    by variable;
run;

data work.correlations;
   merge work.spearman(rename=(default_flag=scorr pdefault_flag=spvalue))
         work.hoeffding(rename=(default_flag=hcorr pdefault_flag=hpvalue));
   by variable;
   scorr_abs=abs(scorr);
   hcorr_abs=abs(hcorr);
run;

proc rank data=work.correlations out=work.correlations1 descending;
    var scorr_abs hcorr_abs;
    ranks ranksp rankho;
run;

proc sort data=work.correlations1;
   by ranksp;
run;

title1 "Rank of Spearman Correlations and Hoeffding Correlations";
proc print data=work.correlations1 label split='*';
   var variable ranksp rankho scorr spvalue hcorr hpvalue;
   label ranksp ='Spearman rank*of variables'
         scorr  ='Spearman Correlation'
         spvalue='Spearman p-value'
         rankho ='Hoeffding rank*of variables'
         hcorr  ='Hoeffding Correlation'
         hpvalue='Hoeffding p-value';  
run;

/* ===================================================== */
/*    Demonstration: Performing Variable Screening, Part 2
              
/* ===================================================== */

/* Plot variable names, Hoeffding ranks, and Spearman ranks. */

title1 "Scatter Plot of the Ranks of Spearman vs. Hoeffding";
proc sgplot data=work.correlations1;
    refline 5.5 / axis=y; 
    refline 5.5 / axis=x; 
   scatter y=ranksp x=rankho / datalabel=variable;
   yaxis label="Rank of Spearman";
   xaxis label="Rank of Hoeffding";
run;
title1 ;

/* kept all variables since all are significant  */

%global screened;
%let screened= age_band_woe  occupation_type_band_woe OccupationP_clus_woe income_band_woe employed_band_woe
  housing_woe marital_status_swoe;

proc logistic data=train_impute_coarse1 descending;
	model Default_Flag=&screened;
	output out=score.train5 p=phat;
run;

/* ===================================================== */
/* Lesson 3, Section 4: l3d6.sas
   Demonstration: Creating Empirical Logit Plots
   [m643_4_i; derived from pmlr03d06.sas]               */
/* ===================================================== */

%global var;
%let var=Age;

/* Group the data by the variable of interest in order to create 
   empirical logit plots.   */

proc rank data=work.train_impute groups=100 out=work.ranks;
   var &var;
   ranks bin;
run;

title1 "Checking Account Balance by Bin";
proc print data=work.ranks(obs=10);
   var &var bin;
run;

/* The data set BINS will contain:INS=the count of successes in each bin,
   _FREQ_=the count of trials in each bin, DDABAL=the avg DDABAL in each bin. */

proc means data=work.ranks noprint nway;
   class bin;
   var default_flag &var;
   output out=work.bins sum(default_flag)=default_flag mean(&var)=&var;
run;

title1 "Number of Observations, Events, and Average Checking Account Balance by Bin";
proc print data=work.bins(obs=10);
run;

/* Calculate the empirical logit */ 

data work.bins;
   set work.bins;
   elogit=log((default_flag+(sqrt(_FREQ_ )/2))/
          ( _FREQ_ -default_flag+(sqrt(_FREQ_ )/2)));
run;

title1 "Empirical Logit against &var";
proc sgplot data=work.bins;
   reg y=elogit x=&var /
       curvelabel="Linear Relationship?"
       curvelabelloc=outside
       lineattrs=(color=ligr);
   series y=elogit x=&var;
run;

title1 "Empirical Logit against Binned &var";
proc sgplot data=work.bins;
   reg y=elogit x=bin /
       curvelabel="Linear Relationship?"
       curvelabelloc=outside
       lineattrs=(color=ligr);
   series y=elogit x=bin; 
run;














































/* Creating Data for KS and Gini Coefficient (discriminatory test)  */
proc sort data=score.train5;
	by descending phat;
run;













/* divide data into 10 equal observation bins */
ods html file='/home/u49308301/UDEMY/ODS3.xls';
%let Noofrecs = 18248;
%let Noofbins = 10;

data score.pred_default;
	set score.train5;
	retain cumm_count;
	count=1;
	cumm_count=sum(cumm_count, count);
	bin=round(cumm_count/(&Noofrecs/&Noofbins)-0.5)+1;

	if bin GT &Noofbins then
		Bin=&Noofbins;
run;

proc sql;
	create table score.gains_dev as select bin, count(*) as freq, 
		sum(Default_Flag) as default, mean(phat)as exp_bad from score.pred_default 
		group by bin;
	run;

proc print data=score.gains_dev;
run;

ods html close;

/* Validation   */
proc means data=score.test n nmiss max min median mean p1 p5 p95 p99;
run;

/* Creating woe variables for validation sample  */
data score.test1;
	set score.test;

	if age <=27 then
		age_band=1;
	else if 27 < age < 43 then
		age_band=2;
	else
		age_band=3;

	if Income_group="< = 15000" then
		Income_band=1;
	else if Income_group="< = 25000" then
		Income_band=1;
	else if Income_group="< = 35000" then
		Income_band=2;
	else if Income_group="< = 50000" then
		Income_band=2;
	else if Income_group="<=100000" then
		Income_band=2;
	else
		Income_band=3;

	if Occupation_Partner="fixed" then
		Occupartner_band=1;
	else if Occupation_Partner="freelance" then
		Occupartner_band=2;
	else
		Occupartner_band=3;

	if Occupation_type="Govt" then
		Occutype_band=1;
	else if Occupation_type="Private" then
		Occutype_band=2;
	else
		Occutype_band=3;
run;

data score.test2;
	set score.test1;

	if Income_band=1 then
		income_woe=0.244591;
	else if Income_band=2 then
		income_woe=0.088829;
	else
		income_woe=-0.39331;

	if Occutype_band=1 then
		Occutype_woe=0.170621;
	else if Occutype_band=2 then
		Occutype_woe=-0.2689;
	else
		Occutype_woe=0.621877;

	if Occupartner_band=1 then
		Occupartner_woe=0.181674;
	else if Occupartner_band=2 then
		Occupartner_woe=-0.10572;
	else
		Occupartner_woe=-0.4476;

	/*if Marital_band=1 then Marital_woe= -0.10049;
	else  Marital_woe=0.092989;
	*/
	if age_band=1 then
		age_woe=0.345728;
	else if age_band=2 then
		age_woe=0.091898;
	else
		age_woe=-0.21301;

	if Housing_1_Own_2_Rent=1 then
		housing_woe=-0.20757;
	else
		housing_woe=0.137949;
	logit=- 1.6879+income_woe*1.6038+housing_woe*2.2502+age_woe*1.0382+Occupartner_woe*2.3120+Occutype_woe*2.2050;
	phat=1 / (1 + exp(-logit));
run;

/*
 The final logistic equation is log (P/1-P)=
1.6879+income_woe*1.6038+housing_woe*2.2502+age_woe*1.0382+Occupartner_woe*2.3120+Occutype_woe*2.2050;
*/
/* Creating data for KS and Gini   */
proc sort data=score.test2;
	by descending phat;
run;

ods html file='/home/u49308301/UDEMY/ODS4.xls';
%let Noofrecs = 4567;
%let Noofbins = 10;

data score.test3;
	set score.test2;
	retain cumm_count;
	count=1;
	cumm_count=sum(cumm_count, count);
	bin=round(cumm_count/(&Noofrecs/&Noofbins)-0.5)+1;

	if bin GT &Noofbins then
		Bin=&Noofbins;
run;

proc sql;
	create table score.gains_val as select bin, count(*) as freq, 
		sum(Default_Flag) as default from score.test3 group by bin;
	run;

proc print data=score.gains_val;
run;
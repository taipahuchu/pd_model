filename life "/home/u49308301/Lifetime_PD.xlsx";

proc import datafile=life out=work.life dbms=xlsx replace replace;
	getnames=yes;
run;

proc freq data=life;
	table cust_id;
RUN;

proc sort data = life out = life noduprecs;
by cust_id report_date;
RUN;


data life2 (keep=cust_id default_flag  yob gdp income cpi);
set life;
by cust_id;
yob = round(mob/12,0.1);
if last.cust_id =1 then OUTPUT;
RUN;



proc phreg data = life2 ;
 ods output parameterestimates=parms;
class default_flag (ref ="1") ;
model yob*default_flag(0) = gdp income cpi;
baseline covariates=life out=baseline cumhaz=cumhaz logsurv=logsurv timelist=5 to 10 by 1 xbeta=xbeta;
output out = survival survival = survival;
RUN;

data default;
set survival;
PD_lifetime = 1 - survival;
format PD_lifetime comma4.2;
run;
**importing data;
proc import datafile='/folders/myfolders/xlsx datasets/Vehicle Data.xlsx'
			dbms=xlsx
			out=vehicle_data
			replace;
			getnames=yes;
			
**imputation of missing values;
data vehicle_data;
	set vehicle_data;

	if MPG___City=' ' and Vehicle_Type='Sedan' then MPG___City=21.76695; 
	if MPG___City=' ' and Vehicle_Type='Sports Car' then MPG___City=18.59574; 
	if MPG___City=' ' and Vehicle_Type='SUV' then MPG___City=16.20339; 
	if MPG___City=' ' and Vehicle_Type='Wagon' then MPG___City=20.96552; 
	if MPG___City=' ' and Vehicle_Type='Minivan' then MPG___City=17.9; 
	if MPG___City=' ' and Vehicle_Type='Pickup' then MPG___City=16.69565;

	if MPG___Hwy=' ' and Vehicle_Type='Sedan' then MPG___Hwy=29.36864; 
	if MPG___Hwy=' ' and Vehicle_Type='Sports Car' then MPG___Hwy=25.7234; 
	if MPG___Hwy=' ' and Vehicle_Type='SUV' then MPG___Hwy=20.62712; 
	if MPG___Hwy=' ' and Vehicle_Type='Wagon' then MPG___Hwy=27.7931; 
	if MPG___Hwy=' ' and Vehicle_Type='Pickup' then MPG___Hwy=21.17391;

	if Weight=' ' and Vehicle_Type='Sedan' then Weight=3319.687;

	if Wheelbase=' ' and Vehicle_Type='Sedan' then Wheelbase=107.177;

	if Length=' ' and Vehicle_Type='Sedan' then Length=186.1152;
	if Length=' ' and Vehicle_Type='Pickup' then Length=208.4737;

	if Width=' ' and Vehicle_Type='Sedan' then Width=70.42387;
	if Width=' ' and Vehicle_Type='Sports Car' then Width=70.87234;
	if Width=' ' and Vehicle_Type='Pickup' then Width=74.26316;

proc print data=vehicle_data;
run;


**Explore quantitative data;

*perform preliminary analysis;
proc sgplot data=vehicle_data;
    vbox MSRP / category=Drive_Wheels 
                     connect=mean;
    title "MSRP Differences across Drive_Wheels";
run;

proc sgplot data=vehicle_data;
    vbox MSRP / category=Vehicle_type 
                     connect=mean;
    title "MSRP Differences across Vehicle_type";
run;

options nolabel;
proc sgscatter data= vehicle_data;
plot MSRP*(Engine_Size Number_of_Cylinders Horsepower MPG___City MPG___Hwy Weight Wheelbase Length Width) / reg;
    	title "Associations of Interval Variables with MSRP";
run;

*check collinearity;
proc reg data=vehicle_data;
model MSRP= Engine_Size Number_of_Cylinders Horsepower MPG___City MPG___Hwy Weight Wheelbase Length Width/VIF;
run;

proc corr data=vehicle_data
	nosimple 
	best=4;
	var Engine_Size Number_of_Cylinders Horsepower MPG___City MPG___Hwy Weight 	Wheelbase Length Width;
	title "Correlations and Scatter Plot Matrix of Predictors";
run;

/*candidates for removal: MPG___City: VIF= 10.89756 and MPG___Hwy: VIF= 12.55100 */

*try log transformation for msrp;
data log_vehicle_data;
	set vehicle_data;
	log_msrp = log(MSRP);
run;

*detect outliers;
proc reg data=log_vehicle_data plots=all;
	model log_msrp= Engine_Size Number_of_Cylinders Horsepower MPG___City MPG___Hwy 	Weight Wheelbase Length Width/r;
	output out=log_vehicle_data predicted=predicted residual=resid student=studresid;
run;

proc sort data=log_vehicle_data; by studresid;
run;

*remove outliers;
	data log_vehicle_data;
	set log_vehicle_data;
	if studresid > 3 then delete;
run;

proc print data=log_vehicle_data;
run;

*find infulential points;
ods graphics on;
ods output RSTUDENTBYPREDICTED=Rstud 
           COOKSDPLOT=Cook
           DFFITSPLOT=Dffits 
           DFBETASPANEL=Dfbs;
proc reg data=log_vehicle_data
         plots(only label)=
              (RSTUDENTBYPREDICTED 
               COOKSD 
               DFFITS 
               DFBETAS);
SigLimit: model log_msrp = Engine_Size Number_of_Cylinders Horsepower  MPG___Hwy Weight Wheelbase  Width; 
title 'SigLimit Model - Plots of Diagnostic Statistics';
run;
quit;

proc print data=Rstud;
run;

proc print data=Cook;
run;

proc print data=Dffits;
run;

proc print data=Dfbs;
run;

data Dfbs01;
	set Dfbs (obs=424);
run;

data Dfbs02;
	set Dfbs (firstobs=425);
run;

data Dfbs2;
update Dfbs01 Dfbs02;
	by Observation;
run;

data influential; *merge datasets from above;
	merge Rstud
      	  Cook 
      	  Dffits
	  	  Dfbs2;
	by observation;

	if (ABS(Rstudent)>3) or (Cooksdlabel ne ' ') or Dffitsout then flag=1; 
*flag observations that have exceeded at least one cutpoint;
	
array dfbetas{*} _dfbetasout: ;
	do i=2 to dim(dfbetas);
   		if dfbetas{i} then flag=1;
	end;

	if ABS(Rstudent)<=3 then RStudent=.; 
*set to missing values of influence statistics for those that have not exceeded cutpoints;
	if Cooksdlabel eq ' ' then CooksD=.;

	if flag=1; *subset only observations that have been flagged;
	drop i flag;
run;

title;
proc print data=influential;
	id observation;
	var Rstudent CooksD Dffitsout _dfbetasout:; 
run;

*remove influential points;
data vehicle_data_inf;
	set log_vehicle_data;
	if _n_ in (70,161,219,229,247,274,300,323,326) then delete;
run;

proc print data=vehicle_data_inf;
run;

**Model selection;

*backward;
proc glmselect data=vehicle_data_inf plots=all;
	class Drive_Wheels Vehicle_type; /* generates dummy variables internally */
model log_msrp= Engine_Size Number_of_Cylinders Horsepower MPG___Hwy Weight Wheelbase Length Width Drive_Wheels Vehicle_type/ selection=backward;
run;
quit;


*forward;
proc glmselect data=vehicle_data_inf plots=all;
	class Drive_Wheels Vehicle_type; /* generates dummy variables internally */
model log_msrp= Engine_Size Number_of_Cylinders Horsepower MPG___Hwy Weight Wheelbase Length Width Drive_Wheels Vehicle_type/ selection=forward;
run;
quit;

*stepwise;
proc glmselect data=vehicle_data_inf plots=all;
	class Drive_Wheels Vehicle_type; /* generates dummy variables internally */
model log_msrp= Engine_Size Number_of_Cylinders Horsepower MPG___Hwy Weight Wheelbase Length Width Drive_Wheels Vehicle_type/ selection=stepwise;
run;
quit;


**visualize plots for selected model;
proc glm data=vehicle_data_inf plots=all;
	class Drive_Wheels Vehicle_type;
model log_msrp= Engine_Size Number_of_Cylinders Horsepower Weight Width Drive_Wheels Vehicle_type;
	output out=vehicle_data predicted=predicted residual=resid student=studresid;
run;
quit;

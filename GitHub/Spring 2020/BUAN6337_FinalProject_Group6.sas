* BUAN 6337.007 - Predictive Analytics with SAS
*               Project 
*               Group 6
* Nishanth Damarancha | nxd180021
* Aneesa Noorani | axn180021
* Prem Kumar Pulluri | pxp190001
* Rohith Selvarajan | rds130030
* April Wu | cxw180004
*
*           April 23, 2020
*************************************************;

dm 'clear log'; dm 'clear output';  /* clear log and output before running anything */

libname project "C:\Users\axn180021\Desktop\Project";

title; /*this will clean out all titles */

ods graphics on;

/* Import dataset */
PROC IMPORT out = work.vehicles
		datafile = "C:\Users\axn180021\Desktop\Project\vehicles.csv"
		dbms=csv replace;
	getnames = yes;
	datarow=2;
	run;

/* Print data */
PROC PRINT data=vehicles(obs=5);
	run;

/* Print contents */
PROC CONTENTS data=vehicles varnum;
	run;

/* Create a format to group missing and nonmissing */
PROC FORMAT;
	value $missfmt ' '='Missing' other='Not Missing';
	value  missfmt  . ='Missing' other='Not Missing';
	title 'PROC Format';
	run;

/* Drop unnecessary columns */
DATA vehicles2;
	set vehicles;
    drop url region_url image_url id description vin county size region lat long;
	if condition = '' then condition = "unspecified";
	if drive = '' then drive = "unspecified";
	if fuel = '' then fuel = "unspecified";
	if paint_color = '' then paint_color = "unspecified";
	if title_status = '' then title_status = "unspecified";
	if transmission = '' then transmission = "unspecified";
	if type = '' then type = "unspecified";
	if cylinders = '' then cylinders = "unspecified";
	if manufacturer = '' then manufacturer = "unspecified";
	if model = '' then model = "unspecified";
    run;

/* Delete outlier observstions
	Convert CA to ca */
DATA vehicles3;
	set vehicles2;
    IF price <= 750 THEN delete;
	IF price >= 100000 THEN delete;
	IF odometer <= 50 THEN delete;
	IF odometer >= 300000 THEN delete;
	IF year <= 1990 THEN delete;
	IF year >= 2021 THEN delete;
	IF state = 'CA' THEN state = 'ca';
	run;

/* Print missing, non-missing values in our new data set */
PROC FREQ data=vehicles3;
	format _CHAR_ $missfmt.; /* apply format for the duration of this PROC */
	tables _CHAR_ / missing missprint nocum;
	format _NUMERIC_ missfmt.;
  	tables _NUMERIC_ / missing missprint nocum;
	title 'PROC FREQ: missing vs non-missing values';
   	run;

/* Print contents */
PROC CONTENTS data=vehicles3 varnum;
	title 'PROC Contents: vehicles3';
	run;

********************** EXPLORATORY DATA ANALYSIS *********************;
/* I. Continuous Variables */
/* 1. Histogram of 'price' variable */

ods graphics on;
PROC UNIVARIATE data=vehicles3 normal noprint;
	var price;
	histogram price / normal kernel
	endpoints = 0 to 100000 by 2000;
	inset n min mean median max std / position = ne;
	probplot price / normal;
	qqplot;
	title "Distribution Analysis - Price Variable";
	run;

/* 2. Histogram of 'odometer' variable */
PROC UNIVARIATE data=vehicles3 normal noprint;
	var odometer;
	histogram odometer / normal kernel
	endpoints = 50 to 300000 by 10000;
	inset n min mean median max std / position = ne;
	probplot odometer / normal;
	qqplot;
	title "Distribution Analysis - Odometer Variable";
	run;

/* 3. Correlation among price, year & odometer */
PROC CORR data=vehicles3;
	var price year odometer;
	title "Correlation among numerical variables";
	run;

PROC SGSCATTER data=vehicles3;
	matrix price odometer year;
 	title 'Correlation among numerical variables';
 	run;

/* II. Categorical Variables */
/* 1. Histogram of 'manufacturer' variable */

PROC FREQ data=vehicles3 order=freq;
	tables manufacturer / plots= freqplot;
	title "Histogram: Manufacturer";
	run;

/* 2. Histogram of 'condition' variable */
PROC FREQ data=vehicles3 order=freq;
	tables condition / plots= freqplot;
	title "Histogram: Condition";
	run;

/* 3. Median price by condition */
PROC SGPLOT data=vehicles3;
	VBOX price / CATEGORY = condition;
	title 'Price by Condition';
	run;

/* 4. Median odometer reading by condition */
PROC SGPLOT data=vehicles3;
	VBOX odometer / CATEGORY = condition;
	title 'Odometer Reading by Condition';
	run;

PROC FREQ data=vehicles3;
	table condition;
	title 'Count by Condition' ;
	run;

/* 5. Median price by condition & type */
PROC TABULATE data=vehicles3;
	VAR price;
	CLASS condition type;
	TABLE condition ALL, MEDIAN*(price*FORMAT=DOLLAR9.0)*(type ALL)
	/BOX='Median Price' MISSTEXT='none';
	title 'PROC Tabulate: Median Price by Condition & Type';
	run;

/* 6. Median price by paint color */
PROC SGPLOT data=vehicles3;
	VBOX price / CATEGORY = paint_color;
	title 'Price by Paint_color';
	run;

/* 7. Median price by transmission */
PROC SGPLOT data=vehicles3;
	VBOX price / CATEGORY = transmission;
	title 'Price by Transmission';
	run;

******************** PRE-MODELING DATA CLEANING *********************;
/* 1. Drop cars with condition of new 
	also drop 'model' variable, bc it's too much for server
	create log_price */

DATA vehicles4;
	set vehicles3;
	drop model;
	log_price = log(price);
	IF condition='new' THEN delete;
	run;

/* 2. Check if log_price is normally dsitributed now */
PROC UNIVARIATE data=vehicles4 normal;
	var log_price;
	histogram log_price / normal kernel
	endpoints = 6 to 12 by 0.5;
	inset n min mean median max std / position = ne;
	qqplot log_price / normal grid;
	title "Distribution Analysis - Price Variable";
	run;

******************** STATISTICAL ANALYSES *********************;
/* 1. Correlation */

ods graphics on;
PROC CORR data=vehicles4;
	var log_price odometer;
	title 'Correlation: Odometer vs. Log_Price';
	run;

/* 2. ANOVA for paint_color */
PROC GLM data=vehicles4;
	class paint_color;
	model log_price = paint_color;
	title "ANOVA by paint_color";
	MEANS paint_color/ hovtest welch;
	run;

/* 3. ANOVA for title_status */
PROC GLM data=vehicles4;
	class title_status;
	model log_price = title_status;
	title "ANOVA by title_status";
	MEANS title_status/ hovtest welch;
	run;

/* 4. ANOVA for type */
PROC GLM data=vehicles4;
	class type;
	model log_price = type;
	title "ANOVA by type";
	MEANS drive;
	run;

/* 5. Comparing type & condition versus type*condition */
/* condition*type interaction variable is 
	more significant than the 2 variables by themselves
	R-squared: 25.5% if interaction, 24.1% if no interaction
	*/
PROC GLM data=vehicles4;
	class condition type;
	model log_price = condition*type;
	title "ANOVA: type*condition";
	run;

PROC GLM data=vehicles4;
	class condition type;
	model log_price = condition type;
	title "ANOVA: type & condition";
	run;

********************************** MODELING *****************************;

/* Dummy variables */
DATA vehicles5;
	SET vehicles4;
	/*Condition. dropped category: 'salvage' */
		length condition_excellent 3;
		length condition_fair 3;
		length condition_good 3;
		length condition_likenew 3;
		length condition_unspec 3;
	/* Drive. dropped category: 'rwd' */
		length drive_4wd 3;
		length drive_fwd 3;
		length drive_unspec 3;
	/* Fuel. dropped category: 'hybrid' */
		length fuel_diesel 3;
		length fuel_elec 3;
		length fuel_gas 3;
		length fuel_other 3;
		length fuel_unspec 3;
	/* Paint. dropped category: 'purple'  */
		length paint_yellow 3;
		length paint_white 3;
		length paint_black 3;
		length paint_brown 3;
		length paint_blue 3;
		length paint_silver 3;
		length paint_green 3;
		length paint_grey 3;
		length paint_custom 3;
		length paint_red 3;
		length paint_orange 3;
		length paint_unspec 3;
	/* Title. dropped category: 'salvage' */
		length title_clean 3;
		length title_missi 3;
		length title_lien 3;
		length title_rebui 3;
		length title_parts 3;
		length title_unspec 3;
	/* Transmission. dropped category: 'other'  */
		length transmission_auto 3;
		length transmission_manual 3;
		length transmission_unspec 3;
	/* Type. dropped category: 'offroad' */
		length type_SUV 3;
		length type_wagon 3;
		length type_sedan 3;
		length type_van 3;
		length type_truck 3;
		length type_pickup 3;
		length type_bus  3;
		length type_hatch 3;
		length type_coupe  3;
		length type_convertible 3;
		length type_minivan 3;
		length type_unspec 3;
	/* Manufacturer. dropped category: 'POLARIS'  */
		length menu_porsche 3;
		length manu_bmw 3;
		length manu_buick 3;
		length manu_cadillac 3;
		length manu_chevy 3;
		length manu_chrysler 3;
		length manu_dodge 3;
		length manu_ford 3;
		length manu_gmc 3;
		length manu_honda 3;
		length manu_hyundai 3;
		length manu_infiniti 3;
		length manu_jeep 3;
		length manu_kia 3;
		length manu_lincoln 3;
		length manu_mercedes 3;
		length manu_nissan 3;
		length manu_subaru 3;
		length manu_toyota 3;
		length manu_isuzu 3;
		length manu_audi 3;
		length manu_mitsubishi 3;
		length manu_ram 3;
		length manu_rover 3;
		length manu_volkswagen 3;
		length manu_fiat 3;
		length manu_tesla 3;
		length manu_bentley 3;
		length manu_am 3;
		length manu_maserati 3;
		length manu_plymouth 3;
		length manu_scion 3;
		length manu_suzuki 3;
		length manu_acura 3;
		length manu_jaguar 3;
		length manu_lexus 3;
		length manu_mazda 3;
		length manu_mercury 3;
		length manu_mini 3;
		length manu_pontiac 3;
		length manu_saturn 3;
		length manu_volvo 3;
		length manu_hummer 3;
		length manu_aston 3;
		length manu_harley 3;
		length manu_ferrari 3;
		length manu_alfa 3;
		length manu_oldsmobile 3;
		length manu_smart 3;
		length manu_landrover 3;
		length manu_unspec 3;
	/* Cylinder. dropped category: 12 */
		length cyl_3 3;
		length cyl_4 3;
		length cyl_5 3;
		length cyl_6 3;
		length cyl_8 3;
		length cyl_10 3;
		length cyl_unspec 3;
	run;


DATA vehicles6;
	SET vehicles5;
	********************************* IF / ELSE STATEMENTS ********************************;
		/* Condition. dropped category: 'salvage' */
	if condition = 'excellent' then condition_excellent=1;
		else condition_excellent=0;
	if condition = 'fair' then condition_fair=1;
		else condition_fair=0;
	if condition = 'good' then condition_good=1;
		else condition_good=0;
	if condition = 'like new ' then condition_likenew=1;
		else condition_likenew=0;
	if condition = 'unspecified' then condition_unspec=1;
		else condition_unspec=0;
		/* Drive. dropped category: 'rwd' */
	if drive = '4wd' then drive_4wd=1;
		else drive_4wd=0;
	if drive = 'fwd' then drive_fwd=1;
		else drive_fwd=0;
	if drive = 'unspecified' then drive_unspec=1;
		else drive_unspec=0;

		/* Fuel. dropped category: 'hybrid'  */
	if fuel = 'diesel' then fuel_diesel=1;
		else fuel_diesel=0;
	if fuel = 'electr' then fuel_elec=1;
		else fuel_elec=0;
	if fuel = 'gas' then fuel_gas=1;
		else fuel_gas=0;
	if fuel = 'other' then fuel_other=1;
		else fuel_other=0;
	if fuel = 'unspecified' then fuel_unspec=1;
		else fuel_unspec=0;

		/* Paint. dropped category: 'purple' */
	if paint_color = 'yellow' then paint_yellow=1;
		else paint_yellow=0;
	if paint_color = 'white' then paint_white=1;
		else paint_white=0;
	if paint_color = 'black' then paint_black=1;
		else paint_black=0;
	if paint_color = 'brown' then paint_brown=1;
		else paint_brown=0;
	if paint_color = 'blue' then paint_blue=1;
		else paint_blue=0;
	if paint_color = 'silver' then paint_silver=1;
		else paint_silver=0;
	if paint_color = 'green' then paint_green=1;
		else paint_green=0;
	if paint_color = 'grey' then paint_grey=1;
		else paint_grey=0;
	if paint_color = 'custom' then paint_custom=1;
		else paint_custom=0;
	if paint_color = 'red' then paint_red=1;
		else paint_red=0;
	if paint_color = 'orange' then paint_orange=1;
		else paint_orange=0;
	if paint_color = 'unspecified' then paint_unspec=1;
		else paint_unspec=0;

		/* Title. dropped category: 'salva' */
	if title_status = 'clean' then title_clean=1;
		else title_clean=0;
	if title_status = 'missi' then title_missi=1;
		else title_missi=0;
	if title_status = 'lien' then title_lien=1;
		else title_lien=0;
	if title_status = 'rebui' then title_rebui=1;
		else title_rebui=0;
	if title_status = 'unspecified' then title_unspec=1;
		else title_unspec=0;

		/* Transmission. dropped category: 'other' */
	if transmission = 'manual' then transmission_manual=1;
		else transmission_manual=0;
	if transmission = 'auto' then transmission_auto=1;
		else transmission_auto=0;
	if transmission = 'unspecified' then transmission_unspec=1;
		else transmission_unspec=0;

		/* Type. dropped category: 'offroad' */
	if type = "SUV" then type_SUV = 1;
		else type_SUV = 0;
	if type = "wagon" then type_wagon = 1;
		else type_wagon = 0;
	if type = "sedan" then type_sedan = 1;
		else type_sedan = 0;
	if type = "van" then type_van = 1;
		else type_sedan = 0;
	if type = "truck" then type_truck = 1;
		else type_truck = 0;
	if type = "pickup" then type_pickup = 1;
		else type_pickup = 0;
	if type = "bus" then type_bus = 1;
		else type_bus = 0;
	if type = "hatchback" then type_hatch = 1;
		else type_hatch = 0;
	if type = "coupe" then type_coupe = 1;
		else type_coupe = 0;
	if type = "convertib" then type_convertible = 1;
		else type_convertible = 0;
	if type = "mini-van" then type_minivan = 1;
		else type_minivan = 0;
	if type = "unspecified" then type_unspec = 1;
		else type_unspec = 0;

		/* Manufacturer. dropped category: 'POLARIS' */
	if manufacturer = "PORSCHE" then manu_porsche = 1;
		else manu_porsche = 0;
	if manufacturer = "bmw" then manu_bmw = 1;
		else manu_bmw = 0;
	if manufacturer = "buick" then manu_buick = 1;
	  else manu_buick = 0;
	if manufacturer = "cadillac" then manu_cadillac = 1;
	  else manu_cadillac = 0;
	if manufacturer = "chevrolet" then manu_chevy = 1;
	  else manu_chevy = 0;
	if manufacturer = "chrysler" then manu_chrysler = 1;
	  else manu_chrysler = 0;
	if manufacturer = "dodge" then manu_dodge = 1;
	  else manu_dodge = 0;
	if manufacturer = "ford" then manu_ford = 1;
	  else manu_ford = 0;
	if manufacturer = "gmc" then manu_gmc = 1;
	  else manu_gmc = 0;
	if manufacturer = "honda" then manu_honda = 1;
	  else manu_honda = 0;
	if manufacturer = "hyundai" then manu_hyundai = 1;
	  else manu_hyundai = 0;
	if manufacturer = "infiniti" then manu_infiniti = 1;
	  else manu_infiniti = 0;
	if manufacturer = "jeep" then manu_jeep = 1;
	  else manu_jeep = 0;
	if manufacturer = "kia" then manu_kia = 1;
	  else manu_jeep = 0;
	if manufacturer = "lincoln" then manu_lincoln = 1;
	  else manu_lincoln = 0;
	if manufacturer = "mercedes-benz" then manu_mercedes = 1;
	  else manu_mercedes = 0;
	if manufacturer = "nissan" then manu_nissan = 1;
	  else manu_nissan = 0;
	if manufacturer = "subaru" then manu_subaru = 1;
	  else manu_subaru = 0;
	if manufacturer = "toyota" then manu_toyota = 1;
	  else manu_toyota = 0;
	if manufacturer = "ISUZU" then manu_isuzu = 1;
	  else manu_isuzu = 0;
	if manufacturer = "audi" then manu_audi = 1;
	  else manu_audi = 0;
	if manufacturer = "mitsubishi" then manu_mitsubishi = 1;
	  else manu_mitsubishi = 0;
	if manufacturer = "ram" then manu_ram = 1;
	  else manu_ram = 0;
	if manufacturer = "rover" then manu_rover = 1;
	  else manu_rover = 0;
	if manufacturer = "volkswagen" then manu_volkswagen = 1;
	  else manu_volkswagen = 0;
	if manufacturer = "fiat" then manu_fiat = 1;
	  else manu_fiat = 0;
	if manufacturer = "tesla" then manu_tesla = 1;
	  else manu_tesla = 0;
	if manufacturer = "BENTLEY" then manu_bentley = 1;
	  else manu_bentley = 0;
	if manufacturer = "MERCEDES" then manu_mercedes = 1;
	  else manu_mercedes = 0;
	if manufacturer = "AM" then manu_am = 1;
	  else manu_am = 0;
	if manufacturer = "MASERATI" then manu_maserati = 1;
	  else manu_maserati = 0;
	if manufacturer = "PLYMOUTh" then manu_plymouth = 1;
	  else manu_plymouth = 0;
	if manufacturer = "SCION" then manu_scion = 1;
	  else manu_scion = 0;
	if manufacturer = "SUZUKI" then manu_suzuki = 1;
	  else manu_suzuki = 0;
	if manufacturer = "acura" then manu_acura = 1;
	  else manu_acura = 0;
	if manufacturer = "jaguar" then manu_jaguar = 1;
	  else manu_jaguar = 0;
	if manufacturer = "lexus" then manu_lexus = 1;
	  else manu_lexus = 0;
	if manufacturer = "mazda" then manu_mazda = 1;
	  else manu_mazda = 0;
	if manufacturer = "mercury" then manu_mercury = 1;
	  else manu_mercury = 0;
	if manufacturer = "mini" then manu_mini = 1;
	  else manu_mini = 0;
	if manufacturer = "pontiac" then manu_pontiac = 1;
	  else manu_pontiac = 0;
	if manufacturer = "saturn" then manu_saturn = 1;
	  else manu_saturn = 0;
	if manufacturer = "volvo" then manu_volvo = 1;
	  else manu_volvo = 0;
	if manufacturer = "HUMMER" then manu_hummer = 1;
	  else manu_hummer = 0;
	if manufacturer = "aston-martin" then manu_aston = 1;
	  else manu_aston = 0;
	if manufacturer = "harley-davids" then manu_harley = 1;
	  else manu_harley = 0;
	if manufacturer = "ferrari" then manu_ferrari = 1;
	  else manu_ferrari = 0;
	if manufacturer = "alfa-romeo" then manu_alfa = 1;
	  else manu_alfa = 0;
	if manufacturer = "OLDSMOBILE" then manu_oldsmobile = 1;
	  else manu_oldsmobile = 0;
	if manufacturer = "SMART" then manu_smart = 1;
	  else manu_smart = 0;
	if manufacturer = "landrover" then manu_landrover = 1;
	  else manu_landrover = 0;
	if manufacturer = "unspecified" then manu_unspec= 1;
	  else manu_unspec = 0;

	 		/* Cylinder. dropped category: 12  */
	if cylinders = "3 cylinders" then cyl_3 = 1;
	  else cyl_3 = 0;
	if cylinders = "4 cylinders" then cyl_4 = 1;
	  else cyl_4 = 0;
	if cylinders = "5 cylinders" then cyl_5 = 1;
	  else cyl_5 = 0;
	if cylinders = "6 cylinders" then cyl_6 = 1;
	  else cyl_6 = 0;
	if cylinders = "8 cylinders" then cyl_8 = 1;
	  else cyl_8 = 0;
	if cylinders = "10 cylinder" then cyl_10 = 1;
	  else cyl_10 = 0;
	if cylinders = "unspecified" then cyl_unspec = 1;
	  else cyl_unspec = 0;
	run;

************************************************* MODELING ***************************************************;

/* 1. PROC REG - using dummified variables */
PROC REG data=vehicles6 plots(maxpoints=none);
	model log_price = odometer year
	condition_excellent condition_fair condition_good condition_likenew condition_unspec
	cyl_3 cyl_4 cyl_5 cyl_6 cyl_8 cyl_10 cyl_unspec;
	title "Model 1: Initial PROC REG";
run;
	
/* 2. PROC GLM - going back to non-dummified dataset */
/* No seletion */
PROC GLMSELECT data=vehicles4 outdesign=glm_model1;
	class condition cylinders drive fuel manufacturer paint_color state title_status transmission type / REF=First; 
	model log_price = odometer year condition cylinders drive fuel manufacturer
	paint_color state title_status transmission type / selection = none
	showpvalues;
	title "Model 2: PROC GLMSELECT Model 1 - No selection";
	run;

/* 3. PROC GLMSELECT - stepwise selection */
PROC GLMSELECT data=vehicles4 outdesign=glm_model2;
	class condition cylinders drive fuel manufacturer paint_color state title_status transmission type / REF = First; 
	Stepwise_Selection: model log_price = odometer year condition type cylinders drive fuel manufacturer paint_color
	paint_color state title_status transmission / selection = stepwise(select = adjrsq)
	showpvalues;
	title "Model 3: PROC GLMSELECT Model 2 - Stepwise Selection";
	run;

/* 4. LASSO REGRESSION */
PROC GLMSELECT data=vehicles4;
	class condition cylinders drive fuel manufacturer paint_color state title_status transmission type / REF = First;
	model log_price = odometer year condition type cylinders drive fuel manufacturer paint_color
		 paint_color state title_status transmission / selection=lasso(choose=adjrsq stop=adjrsq);
	score data=vehicles4 p out=lasso;
	title "Model 4: PROC GLMSELECT Model 3 - Lasso Regression";
	run;

/* 5. POLYNOMIAL REGRESSION */
PROC GLMSELECT data=vehicles4 outdesign=Polynomial;
	class condition cylinders drive fuel manufacturer paint_color state title_status transmission type / REF = First;
	/* effect - creates polynomial, rather than just linear effect */
	effect p_odometer =polynomial(odometer / degree = 4);
	FourthDegree_GLM: model log_price = p_odometer year condition type cylinders drive fuel manufacturer paint_color
	paint_color state title_status transmission / selection = none
	showpvalues;
	title "Model 5: PROC GLMSELECT Model 4: 4th Degree Polynomial";
	run;

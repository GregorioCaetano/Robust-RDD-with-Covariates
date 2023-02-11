**************************************************************************************************************
**************************************************************************************************************
**************************************************************************************************************
* This data replicate the application section from Caetano, Caetano and Escanciano (2023), "Robust Identification in Regression Discontinuity Designs with Covariates".
**************************************************************************************************************
**************************************************************************************************************
**************************************************************************************************************
local path = "add your path here" // Add your path here
local datpath = "`path'/"
local progpath = "`path'/"
local outpath = "`path'/"

cap log close

cap clear mata
clear matrix
clear
set more 1
set mem 50g
set matsize 11000
set maxvar 12000
cap program drop _all

**************************************************************************************************************
**************************************************************************************************************
**************************************************************************************************************
* CLEANING THE DATA
* DATA AND CODE FROM Amarante, Manacorda, Miguel, and Vigorito (2016)
* Note that you need to first download the data "peso4_anonymized.dta" in the website of this paper. See readme file for more information.
**************************************************************************************************************
**************************************************************************************************************
**************************************************************************************************************
u if newdate_pa !=. & newind!=. using "`datpath'peso4_anonymized.dta", replace

*drop if q_tot==.
drop if bajo25==.

drop if edad>=18
tab edad, g(dage)
collapse dag*, by(nro_i) fast
merge 1:m nro_i using "`datpath'peso4_anonymized.dta", keep(matched using) generate(merge10)
#delimit ;
for  any 
depa localidad tipo_v artef_calefon artef_lluveiro artef_cocina artef_horno_m artef_heladera artef_freezer artef_lavarro 
artef_lavavaj artef_estufa artef_calefac artef_televis artef_video artef_tv_cable artef_computa artef_automovil 
artef_telefono casa_ute casa_ose casa_sa cuadra_ute cuadra_ose cuadra_sa cuadra_ba cuadra_pa cuadra_alu 
material_te material_pi material_pa prop_viv servicio_sanitario serv_sanit_u serv_sanit_c serv_sanit_e habitaciones_h 
habitaciones_d: replace X=-99 if X==.;
#delimit cr
keep if newdate_pa !=. & newind!=.
for any 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 : replace dageX =0 if dageX==.
drop if bajo25==.


cap drop orden* esm*
sort cedulam date_pa
qui by cedulam date_pa: g orden=_n
egen esmult=max(orden), by(date_pa cedulam)


cap drop maxc
sort cedula date_pa 
qui by cedula date_pa : g n=_n
egen maxc=count(cedulam) if date_pa<date_vis & n==1, by(cedula)
egen maxchildren=max(maxc), by(cedula)
drop n maxc
replace maxc=0 if maxc==.
replace maxc=2 if maxc>2 & maxc<.

egen gr1=group(indice mv)
cap drop childtime
egen childtime=group(date_pa orden)
cap drop count1
egen count1=sd(childtime), by(cedulam)
replace count1=count1>0 & count1<.

cap drop mand
g mandatory=(prim>=1)*(seg>=3)*(ter>=3) if semg<. & prim<. & seg<. & ter<. 
cap drop posprim
g posprim=prim>0 if prim<.

cap drop cohort
g cohort=2005-edad
cap drop tmpi
egen tmpi=mean(instm), by(cedulam)
cap drop gr2
egen gr2=group( cod_d cod_loc cohort newdate_vis newdate_ins newdate_prim dage* tmpi ind_ree)
g priv_insurance=seguro==2 | seguro==3 if seguro<.



tab instm, g(dinstm)
g newage_m=edad if edad>0 & edad<99
rename dinstm1 incomplete_prim
rename dinstm2 complete_prim
rename dinstm3 complete_sec
rename dinstm4 complete_tertiary
g sexo1=sexo==1 if sexo<. & sexo>0 & sexo<99
g house=tipo_v==1 if tipo_v>0

for any calefon lluveiro cocina horno_m heladera freezer lavarro lavavaj estufa: g X=artef_X==1 if artef_X>0 & artef_X<=2 
for any calefac televis video tv_cable computa automovi telefono: g X=artef_X==1 if artef_X>0 & artef_X<=2
for any  ute ose saneamiento basurero paviment alumbrado: g X=cuadra_X==1 if  cuadra_X>=0 & cuadra_X<=2
g piso=material_piso==1 if material_piso>0 & material_piso<.
g pared=material_pared==1 if material_pared>0 & material_pared<.
g home_owned=prop_viv==1 if prop_v>0 & prop_viv<.
g sanitario=serv_sanit_uso==1 if serv_sanit_u>0 & serv_sanit_u<.
g rooms=habitaciones_hog if habitaciones_ho>0
g rooms_sleep=habitaciones_d if habitaciones_d>0

replace local=-999 if local==.
cap drop tmp
egen tmp=group(codep local)
replace local=tmp
drop tmp

replace loc=-999 if loc==.
egen tmp=group(depar loc)
replace loc=tmp
drop tmp
tab loc, g(dloc)

cap drop elig*
g eligible=newind<0 if newind<.
cap drop treat*
g treat_post12=ing_ciud_txu_hh1>0 | ing_ciud_txu_hh2>0 if ing_ciud_txu_hh1<. & ing_ciud_txu_hh2<.
g treat_post13=ing_ciud_txu_hh1>0 | ing_ciud_txu_hh2>0 | ing_ciud_txu_hh3>0 if ing_ciud_txu_hh1<. & ing_ciud_txu_hh2<. & ing_ciud_txu_hh3<.
	
g treat_tarjeta_post13=ing_tarjeta_hh1>0 | ing_tarjeta_hh2>0 | ing_tarjeta_hh3>0 if ing_tarjeta_hh1<. & ing_tarjeta_hh2<. & ing_tarjeta_hh3<.



cap program drop nocontrols
program define nocontrols
	global add=""
	global covm=""
	global cov=""
	global cov1=""
	global cov2=""
	global cov3=""
	global cov4=""
	global cov5=""
	global title2="no controls"
end

cap program drop controls
program define controls
	global covm="i.edadm i.newdate_vis i.newdate_ins i.instm i.depar" /*dloc"*/
	global cov="i.sexo i.numemb i.newesmul"
	global cov1="i.depa i.tipo_v i.artef_calefon i.artef_lluveiro i.artef_cocina i.artef_horno_m i.artef_heladera i.artef_freezer i.artef_lavarro"
	global cov2="i.artef_lavavaj i.artef_estufa i.artef_calefac i.artef_televis i.artef_video i.artef_tv_cable i.artef_computa i.artef_automovil"
	global cov3="i.artef_telefono i.casa_ute i.casa_ose i.casa_sa i.cuadra_ute i.cuadra_ose i.cuadra_sa i.cuadra_ba i.cuadra_pa i.cuadra_alu i.material_te" 
	global cov4="i.material_pi i.material_pa i.prop_viv i.servicio_sanitario i.serv_sanit_u i.serv_sanit_c i.serv_sanit_e i.habitaciones_h i.habitaciones_d"
	global cov5=""
	global title2="controls"
end


*1. treatment defined using min by date of vis
cap drop day_post1
cap drop q_post1
cap drop day_post
cap drop q_post

cap drop tmp

egen tmp=min(date_prim) , by(newdate_vis)
g olddate=date_prim 
replace date_prim=tmp 
g newdate_prim1=mofd(date_prim)
replace newdate_prim1=-999 if newdate_prim1==.
g period1=newdate_prim1<=newdate_co+6 
g eligible_post1=eligible*period1
g missnewdate_prim1=newdate_prim1<0
replace date_prim=olddate
drop olddate


*1a. tarjeta defined using min by date of vis
cap drop tmp
egen tmp=min(date_tar), by(date_vis)
g olddate_tar=date_tar 
replace date_tar=	tmp 
g newdate_tar1=mofd(date_tar)
replace newdate_tar1=-999 if newdate_tar1==.
g period_tarjeta1=newdate_tar1<=newdate_co+6 
g eligible_tarjeta_post1=eligible*period_t
g missnewdate_tar1=newdate_tar1<0
replace date_tar=olddate_tar
drop olddate_tar


*2. treatment based on fixed date (the mode )
cap drop day_post2
cap drop q_post2
cap drop tmp
cap drop olddate
g olddate=date_prim 
replace date_prim=dofm(543)
g newdate_prim2=mofd(date_prim)
replace newdate_prim2=-999 if newdate_prim2==.
g day_post2=date_prim-date_co if semg<999
replace day_post2=0 if day_post2<0
g q_post2=floor(day_post2/(365/4))+1 if day_post2>0 & day_post2<.
replace q_post2=4 if q_post2>4 & q_post2<.
replace q_post2=0 if  q_post2==. & semg<. & newind<.
cap drop new
g new=4 if date_prim-date_co<=0
replace new=3 if q_post2==1
replace new=2 if q_post2==2
replace new=1 if q_post2==3
replace q_post2=new	
replace q_post2=0 if q_post2==. & date_co<.
g period2=(q_post2>0) if q_post2<. & newind<.
g eligible_post2=eligible*(q_post2>0) if q_post2<. & newind<.
g b2=q_post2
replace b2=3 if b2==4
g a2=b2*eligible
replace date_prim=olddate
drop olddate



*4. treatment defined using actual on date of prim+ 
cap drop day_post4
cap drop q_post4
cap drop day_post
cap drop q_post
g newdate_prim4=mofd(date_prim)
replace newdate_prim4=-999 if newdate_prim4==.
g day_post4=date_prim-date_co if semg<999
replace day_post4=0 if day_post4<0
g q_post4=floor(day_post4/(365/4))+1 if day_post4>0 & day_post4<.
replace q_post4=4 if q_post4>4 & q_post4<.
replace q_post4=0 if  q_post4==. & semg<. & newind<.
cap drop new
g new=4 if date_prim-date_co<=0
replace new=3 if q_post4==1
replace new=2 if q_post4==2
replace new=1 if q_post4==3
replace q_post4=new	
replace q_post4=0 if q_post4==. & date_co<.
g period4=(q_post4>0) if q_post4<. & newind<.
g eligible_post4=eligible*(q_post4>0) if q_post4<. & newind<.
g b4=q_post4
replace b4=3 if b4==4
g a4=b4*eligible
**************************************************************************************************************
**************************************************************************************************************
**************************************************************************************************************
* ANALYSIS IN Caetano, Caetano and Escanciano (2023) BEGINS HERE
**************************************************************************************************************
**************************************************************************************************************
**************************************************************************************************************
keep if period1==1 // during program only, no diff-in-diff
gen Y = bajo25
gen X = treat_post13
gen D = eligible
gen Z = newind
gen DZ = D*Z
gen agem=edadm
replace agem=16 if edadm<=16
replace agem=46 if edadm>=46
tab agem
gen visit=newdate_vis
replace visit = 1 if newdate_vis<543
replace visit = 2 if newdate_vis>=543 & newdate_vis<=545
replace visit = 3 if newdate_vis>=546 & newdate_vis<=548
replace visit = 4 if newdate_vis>=549 & newdate_vis<=551
replace visit = 5 if newdate_vis>551

gen educm=.
replace educm=1 if instm==1
replace educm=2 if instm==2
replace educm=3 if instm==3 | instm==4
replace educm=4 if instm==5

gen educf=.
replace educf=1 if instp==1
replace educf=2 if instp==2
replace educf=3 if instp==3 | instm==4
replace educf=4 if instp==5

gen ageM=.
replace ageM=1 if agem>=31
replace ageM=2 if agem>=26 & agem<=30
replace ageM=3 if agem>=21 & agem<=25
replace ageM=4 if agem>=16 & agem<=20


* Create the rest of the variables necessary for figures
sum Z
egen run=cut(Z), at(-.1(.002).1)
replace run=run+.001
tempfile temp
save `temp', replace

egen Xm=mean(X), by(run)
egen XmO=mean(X) if agem>=26 & agem<=30, by(run)
egen XmY=mean(X) if agem>=16 & agem<=20, by(run)
egen tag=tag(run)
egen tagO=tag(run) if agem>=26 & agem<=30
egen tagY=tag(run) if agem>=16 & agem<=20

* First stage plot (Figure 4)
scatter Xm run if run>-.1 & run<=.1 & tag==1, xline(0) scheme(s1mono) symbol(Oh) xtitle("Predicted income score (Z)") ///
		ytitle("Received cash transfer while pregnant (X)") xlabel(-.1(.05).1) ylabel(0(.2)1) yscale(range(0 1))
graph export "`outpath'first_stage.pdf", replace

* First stage plot by age (Figure 5)
scatter XmY run if run>-.1 & run<=.1 & tagY==1, xline(0) scheme(s1mono) symbol(Oh) title("Age 16-20") xtitle("Predicted income score (Z)") ///
		ytitle("Received cash transfer while pregnant (X)") xlabel(-.1(.05).1) ylabel(0(.2)1) yscale(range(0 1))
graph export "`outpath'first_stage_byW_younger.pdf", replace

scatter XmO run if run>-.1 & run<=.1 & tagO==1, xline(0) scheme(s1mono) symbol(Oh) title("Age 26-30") xtitle("Predicted income score (Z)") ///
		ytitle("Received cash transfer while pregnant (X)") xlabel(-.1(.05).1) ylabel(0(.2)1) yscale(range(0 1))
graph export "`outpath'first_stage_byW_older.pdf", replace

* Table 4
gen first=(numemb==0)
local WlistD = "visit agem instp" // list of W covariates.
local Clist = "" 				// list of additional covariates, if any.
local bwlist="4 5 6 7 8 9 10" // 
egen g = group(`WlistD')
tab g, gen(W_)
foreach var of varlist W_* {
	gen D`var'=D*`var'
	gen Z`var'=Z*`var'
	gen DZ`var'=DZ*`var'	
}
keep if period1==1
local bw_base = "0.01" 
foreach bw in `bwlist' {
	gen u = (Z/(`bw'*`bw_base'))
	gen  weightT`bw' = 1 - abs(u)
	replace weightT`bw' = 0 if u<-1 | u>1
	drop u
}


log using "`outpath'main_results_`bwlist'.log", text replace
* Obtain information about optimal bandwidth based on CCT. Also get estimates for comparison.
rdrobust Y Z, fuzzy(X)
rdrobust Y Z, fuzzy(X) covs(W_* `Clist')

** RDD without covariates
foreach bw in `bwlist' {
	qui ivregress 2sls Y (X=D) Z DZ [w=weightT`bw'], vce(robust)
	local RDD`bw' = round(_b[X],0.0001)
	local RDD_se`bw' = round(_se[X],0.0001)
	display "bw=`bw', `RDD`bw''(`RDD_se`bw'')"
}
** RDD with covariates
foreach bw in `bwlist' {
	qui xi: ivregress 2sls Y (X=D) Z DZ W_* `Clist' [w=weightT`bw'], vce(robust)
	local RDDcov`bw' = round(_b[X],0.0001)
	local RDDcov_se`bw' = round(_se[X],0.0001)	
	display "bw=`bw', `RDDcov`bw''(`RDDcov_se`bw'')"	
}
** RATE
foreach bw in `bwlist' {
	qui xi: ivregress 2sls Y (X=D DW_*) Z DZ W_* ZW_* DZW_* `Clist' [w=weightT`bw'], vce(robust)
	local CCE`bw' = round(_b[X],0.0001)
	local CCE_se`bw' = round(_se[X],0.0001)	
	display "bw=`bw', `CCE`bw''(`CCE_se`bw'')"	
}
log close
** Organize estimates into a table.
drop _all
local count=0
foreach bw in `bwlist' {
	local count=`count'+1
}
set obs `count'
gen band=.
gen RDD=.
gen RDD_se=.
gen RDDcov=.
gen RDDcov_se=.
gen CCE=.
gen CCE_se=.
local count=0
foreach bw in `bwlist' {
	local count=`count'+1
	local t_RDD_se`bw'=abs(`RDD`bw''/`RDD_se`bw'')
	local t_RDDcov_se`bw'=abs(`RDDcov`bw''/`RDDcov_se`bw'')
	local t_CCE_se`bw'=abs(`CCE`bw''/`CCE_se`bw'')
	replace band=`bw'*`bw_base' if _n==`count'
	replace RDD=`RDD`bw'' if _n==`count'
	replace RDD_se=`RDD_se`bw'' if _n==`count'	
	replace RDDcov=`RDDcov`bw'' if _n==`count'
	replace RDDcov_se=`RDDcov_se`bw'' if _n==`count'	
	replace CCE=`CCE`bw'' if _n==`count'	
	replace CCE_se=`CCE_se`bw'' if _n==`count'		
}

local list="RDD RDD_se RDDcov RDDcov_se CCE CCE_se"
local list_se="RDD_se RDDcov_se CCE_se"
replace band = round(band,0.01)
tostring band, replace force format(%04.2f)
foreach var in `list' {
	replace `var' = round(`var',0.0001)
	tostring `var', replace force format(%05.4f)
}
local count=0
foreach bw in `bwlist' {
	local count=`count'+1	
	foreach var in `list_se' {
		if `t_`var'`bw'' > 1.96 {
			replace `var'=`var'+"**" if _n==`count'
		}
		else if `t_`var'`bw'' > 1.645 {
			replace `var'=`var'+"*"	 if _n==`count'	
		}
		replace `var'="("+`var'+")" if _n==`count'
	}
}
save "`outpath'/MainTable.dta", replace


order band RDD RDD_se RDDcov RDDcov_se CCE CCE_se
texsave band RDD RDD_se RDDcov RDDcov_se CCE CCE_se using "`outpath'/MainTable.tex", replace ///
varlabels size(5) align(c|ll|ll|ll) rowsep(.05in) ///
headerlines(" & \multicolumn{4}{c}{LATE} & \multicolumn{2}{c}{RATE} \\ \midrule \vspace{-.2in} Bandwidth & \multicolumn{2}{c}{Standard RDD} & \multicolumn{2}{c}{RDD w/ Covariates} & \multicolumn{2}{c}{}") //frag







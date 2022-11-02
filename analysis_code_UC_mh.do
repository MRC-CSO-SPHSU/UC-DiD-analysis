

//Attaching new UC data to US dataframe



// setting the level 2 weight as the weight in the first wave  respondent entered - this is inital sampling weight - then stage 2 weights are further logitudinal weights 
/// I have set the PSU to household - in reponse to reviewer. 

bysort pid (wave):gen level1wt=lwt[1]

// set weights using surveyset
svyset hid, weight(level1wt) || _n, weight(lwt)

/// gen square of age - relationship with age is nonlinear
gen agesq=age_dv^2

capture drop emp
gen emp=jbstat<3
replace emp=. if jbstat==.
bysort pid:gen numwave=_N

replace highq=. if ghq_likert ==.

/// gen lagged variables // accounting for BHPS sample not being in USPS wave 1 - use lagged data from wave 18 BHPS

gen lemp=l.emp
///replace lemp=l2.emp if wave==20 & lemp==.
gen lhighq=l.highq
//// replace lhighq=l2.lhighq if wave==20 & lhighq==.
drop if wave<19
save temp,replace

/////////ANALYSIS


use temp,clear
  
  // parallel trends analysis- Test whether the trends were parallel prior to the intervention

asdoc svy,subpop(if (elig==1 |jbstat !=8)  & country !=4  & year_uc<0 ): logistic highq  c.age_dv agesq  i.country i.married i.sex i.educ1 i.elig##c.year_uc 
asdoc margins r.elig, dydx(year_uc) contrast(effects) post



//MAIN ANALYSIS - logistics regression using GHQ cut scores (DID)

asdoc svy,subpop(if (elig==1 |jbstat !=8)  & country !=4   ): logistic highq year_uc c.age_dv agesq  i.country i.married i.sex i.educ1 i.elig##i.uc_treat,
asdoc margins r.1.elig#r.1.uc_treat,contrast(effects) post

//REPEATED WITH TWO OTHER OUTCOMES
//using linear regression on continuous outcome
asdoc svy,subpop(if (elig==1 |jbstat !=8) & country !=4 ): meglm hlghq1 year_uc c.age_dv agesq i.country i.married i.sex i.educ1 i.elig_crit##i.uc_treat


// using the SF12 - Mental component summary 
asdoc svy,subpop(if (elig==1 |jbstat !=8) & country !=4 ): meglm sf12mcs_dv year_uc c.age_dv agesq i.country i.married i.sex i.educ1  i.elig_crit##i.uc_treat
// using the SF12 - Physical component summary
xtset
svy,subpop(if (elig==1 |jbstat !=8) & country !=4 ): meglm sf12pcs_dv year_uc c.age_dv agesq i.country i.married i.sex i.educ1  i.elig_crit##i.uc_treat


 // interaction with age, sex and education

  use temp,clear
 egen age_group=cut(age_dv), at(0,25,45,55,70)
 
xtset 
svy,subpop(if (elig==1 |jbstat !=8) & country !=4 ): logistic highq year_uc i.country i.married i.sex i.educ1  i.age_group##i.elig_crit##i.uc_treat
xtset 
svy,subpop(if (elig==1 |jbstat !=8) & country !=4 ):logistic highq year_uc age_dv  i.country i.married  i.educ1  i.sex##i.elig_crit##i.uc_treat
xtset 
svy,subpop(if (elig==1 |jbstat !=8) & country !=4 ):logistic highq year_uc age_dv  i.country i.married i.sex  i.educ1##i.elig_crit##i.uc_treat



//SUPLEMENTARY ANALYSIS
//does the introduction of UC lead to greater employment?

// does introduction of UC increases emplyment - i.e if uc_treat  associated with employment of people who were elligible in previous wave
 use temp,clear
  capture drop emp
  gen emp=jbstat<3
 
xtset 
svy,subpop(if (elig==1 |jbstat !=8) & country !=4 ): logistic emp year highq c.age_dv  i.country i.married i.sex i.educ1 l.uc_treat if l.elig_crit==1

 
  

//ROBUST ANALYSIS

 use temp,clear

//taking out those that identified as self-employed/employed from main analysis

//pta

svy,subpop(if (elig==1 |jbstat !=8)  & country !=4 & jb !=1 & year_uc<0 ): logistic highq  c.age_dv agesq  i.country i.married i.sex i.educ1 i.elig##c.year_uc 
margins r.elig, dydx(year_uc) contrast(effects) post

svy,subpop(if (elig==1  |jbstat !=8)  & country !=4 & jb !=1): logistic highq year_uc age_dv  i.country i.married i.sex i.educ1 i.elig_crit##i.uc_treat
margins r.1.elig#r.1.uc_treat,contrast(effects) post



/// we can estimate a linear regression model with binary outcome - with a random effect
svy: meglm highq year_uc age_dv  i.country i.married i.sex i.educ1  sanc_rate i.elig_crit##i.uc_treat,fam(gauss) link(identity) || pid:
margins r.1.elig#r.1.uc_treat,contrast(effects) post


/// model conditioning on prior employment and mental health

svy,subpop(if (elig==1 |jbstat !=8)  & country !=4  &l.emp==1 & l.highq==0): logistic highq year_uc c.age_dv agesq  i.country i.married i.sex i.educ1 i.elig##i.uc_treat
margins r.1.elig#r.1.uc_treat,contrast(effects) post



//balanced panel
use temp.dta,replace

bysort pid:gen numave=_N
  
svy,subpop(if (jbstat !=8) & country !=4 & jb !=1): logistic highq year_uc age_dv  i.country i.married i.sex i.educ1  i.elig_crit##i.uc_treat if numave==8
 margins r.1.elig#r.1.uc_treat,contrast(effects) post

 
 
 
 
 
////FIGURES

 
/// PLOT TO SHOW DIFFERENT EMPLOYMENT PATTERNS AND MENTAL HEALTH _ SENSITIVITY ANALYSIS
parmby "svy,subpop(if country !=4 ):tab jb highq,  row ci", by(year_uc)  saving(highq_b,replace)

*NEED TO SET THE SCHEME. PLOTPLAINBLIND WORKS WELL HERE
*ssc install blindschemes
set scheme plotplainblind


use highq_b,clear
drop if substr(parm,3,1)=="1"
gen jbb=real(substr(parm,2,1))-1
label define  jb  0 "employed" 1 "unemployed" 2 "long term sick" 3"mat leave/carer" 4 "retired" 5 "student/unpaid"
label values  jb  jb

drop if year_uc >4
drop if year_uc <-4
foreach var in estimate min95 max95 {
replace `var'=`var'*100
}


twoway  ////
		(connected estimate year_uc if jb==0, mcolor(blue) lcolor(blue)  msymbol(square) lpattern(dash) msize(small)) ///
		(rcap min95 max95 year_uc if jb==0, lcolor(blue)) ////
		(connected estimate year_uc if jb==1, mcolor(cranberry) lcolor(cranberry) msymbol(square) lpattern(dash) msize(small)) ///
		(rcap min95 max95 year_uc if jb==1, lcolor(cranberry)) ////, ///
		(connected estimate year_uc if jb==2, mcolor(red) lcolor(red) msymbol(square) lpattern(dash) msize(small)) ///
		(rcap min95 max95 year_uc if jb==2, lcolor(red)) ////, ///
		(connected estimate year_uc if jb==3, mcolor(green) lcolor(green) msymbol(square) lpattern(dash) msize(small)) ///
		(rcap min95 max95 year_uc if jb==3, lcolor(green)) ////, ///
		(connected estimate year_uc if jb==4, mcolor(black) lcolor(black) msymbol(square) lpattern(dash) msize(small)) ///
		(rcap min95 max95 year_uc if jb==4, lcolor(black)) ////, ///
		(connected estimate year_uc if jb==5, mcolor(orange) lcolor(orange) msymbol(square) lpattern(dash) msize(small)) ///
		(rcap min95 max95 year_uc if jb==5, lcolor(orange)) ////, ///
		, ylabel(#5,labsize(medium) angle(horizontal)) ///
xlabel(#10  ,  labsize(medium)) ///
yscale(range(. .))  xscale(range(. .)) ///
legend(order(2 "employed" 4 "unemployed" 6 "long-term sick" 8 "mat leave/carer" 10 "retired" 12 "student" ) pos(6) rows(1) size(medium)  region(fcolor(white) margin(tiny) lcolor(white)))  ///
ytitle("% psychological distress",size(large)) xtitle("years exposed to Universal Credit" , size(large))  ///
 scale(0.8) ysize(4) xsize(6) ///
 graphregion(color(white) margin(medium))  plotregion(fcolor(white) lstyle(solid) lcolor(white)) ///
  note("") xline(0) subtitle(, size(medium) nobox pos(11))
  
  
/// PLOT TO SHOW MENTAL HEALTH PROBLEMS BY YEAR
use temp.dta,replace
 
parmby "svy,subpop(if (elig==1 |jbstat !=8) & country !=4 ):tab elig_crit highq,  row ci", by(year)  saving(highq,replace)


*NEED TO SET THE SCHEME. PLOTPLAINBLIND WORKS WELL HERE
* ssc install blindschemes
set scheme plotplainblind


use highq,clear
drop if substr(parm,3,1)=="1"
gen elig=real(substr(parm,2,1))-1
label define  elig  1 "Unemployed" 0 "Not-unemployed"  
label values  elig  elig

foreach var in estimate min95 max95 {
replace `var'=`var'*100
}


twoway  ////
		(connected estimate year if elig==1, mcolor(blue) lcolor(blue)  msymbol(square) lpattern(dash) msize(small)) ///
		(rcap min95 max95 year if elig==1, lcolor(blue)) ////
		(connected estimate year if elig==0, mcolor(cranberry) lcolor(cranberry) msymbol(square) lpattern(dash) msize(small)) ///
		(rcap min95 max95 year if elig==0, lcolor(cranberry)) ////, ///
		, ylabel(#5,labsize(medium) angle(horizontal)) ///
xlabel(#10  ,  labsize(medium)) ///
yscale(range(. .))  xscale(range(2009 2016)) ///
legend(order(1 "Intervention group" 3 "Comparison"  ) pos(6) rows(1) size(medium)  region(fcolor(white) margin(tiny) lcolor(white)))  ///
ytitle("% psycholgical distress",size(large)) xtitle("year" , size(large))  ///
 scale(0.8) ysize(4) xsize(6) ///
 graphregion(color(white) margin(medium))  plotregion(fcolor(white) lstyle(solid) lcolor(white)) ///
  note("") xline(2013) subtitle(, size(medium) nobox pos(11))



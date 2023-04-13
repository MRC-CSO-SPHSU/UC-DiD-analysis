use "data\UKDA-8861-stata\stata\stata13\apsh_jd20_eul_phhwta22.dta", clear

gen employ = 0

replace employ = 1 if ILODEFR == 1

logit employ AGE SEX MARDY6 ETHUK14

predict double ps
gen double ipw0 = .
gen double ipw1 = .

replace ipw0 = 0.employ/(1-ps)
replace ipw1 = 1.employ/ps

gen double ipw=. 
replace ipw = ipw1 if employ==1 // Sampling weights for the treated group
replace ipw = ipw0 if employ==0 // Sampling weights for the non - treated group

sum ipw

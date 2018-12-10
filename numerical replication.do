**********************************************************************************************************
*Putting a Number on Preferences: How Numerical Attitudes Are Shaped by Ideology and Equivalency Framing.*
**********************************************************************************************************
*Authors: Rasmus T. Pedersen and Martin V. Larsen*

*FILE PURPOSE: Reproduces all figures, tables and other results.
*VERSION: STATA 13.1
*REQUIRED PACKAGES: plotplain, esttab



************************************************************************************************************************************************
***Recoding/Creating variables******************************************************************************************************************
************************************************************************************************************************************************

****************
***Treatments***
****************


*Treatment groups*
gen treat=.
replace treat=1 if q4a!=.
replace treat=2 if q4b!=.
replace treat=3 if q4c!=.
replace treat=4 if q4d!=.

label define treat 1 "Gain, per month" 2 "Loss, per month" 3 "Gain, per hour" 4 "Loss, per hour"
label values treat treat

*Win or loss*
gen win=.
replace win=1 if treat ==1 |treat==3
replace win=0 if treat ==2 |treat==4

*Small or large numerator*
gen smallenumerator=.
replace smallenumerator=0 if treat < 3
replace smallenumerator=1 if treat > 2


*************************
***Dependent variables***
*************************

*Preferred difference (gain/loss)*
gen difference=q4a if treat==1
replace difference=q4b if treat==2
replace difference=q4c if treat==3
replace difference=q4d if treat==4

*adjusting for ratiodifference in treatment question
replace difference=difference*4.33*37 if treat > 2 //


*creating variable that measures whether you pick a value above the atatus quo
gen sq=0
replace sq=1 if q4a==1000 | q4b==1000
replace sq=1 if q4c==6 | q4d==6
gen lsq=0
replace lsq=1 if q4a<1000 &treat==1 | q4b<1000 &treat==2
replace lsq=1 if q4c<6 &treat==3 | q4d<6 &treat==4
gen defect=1-lsq-sq

***Dealing with outliers in dependent variable***

*Taking the log
gen logdifference=log(difference)

*Excluding cases where DV is above p95 (within each treatment group)
ta treat, nol
gen trim=difference
foreach x in 1 2 3 4 {
su difference if treat==`x', d 
replace trim=. if treat==`x' & difference > r(p95)
}

****************
***Covariates***
****************

*Subjective numeracy*
recode q1_1 (7=.), gen(num_fraction)
recode q1_2 (7=.), gen(num_percentages)
recode q1_3 (7=.), gen(num_currency)
recode q1_4 (7=.), gen(num_discount) 

*creating index
alpha num_fraction num_percentages num_currency num_discount, item gen(numeracy_unstandardized) min(2)
tab numeracy_unstandardized
gen numeracy=(numeracy_unstandardized-1)/5
tab numeracy

*splitting numeracy into two groups
egen highnumeracy=cut(numeracy), group(2)
label define highnumeracy 0 "Low Numeracy" 1 "High Numeracy"
label values highnumeracy highnumeracy

*ideology
gen rightwing=.
replace rightwing=(q3-1) if q3!=12

*standardizing ideology
su rightwing
gen rightSD=right/r(sd)

*Education
recode education_wm (1 2 3=0) (4 5 6=1), gen(college)

*Generating index of unemployment attitudes.
recode q5_* (6=.)
gen unem=(q5_1+q5_2+q5_4-3)/12
alpha q5_1 q5_2 q5_4

*Recoding age groups.
recode age (18/29=1 "18-29") (30/44=2 "30-44") (45/59=3 "45-59") (60/100=4 "60+"), gen(agegrp)
tab agegrp, gen(agegrp1)
tab region, gen(region1)

*gender
ta gender
gen man=gender-1

***label variables
la var gender "Gender (male)"
la var agegrp11 "18-29"
la var agegrp12 "30-44"
la var agegrp13 "30-44"
la var agegrp14 "60+"
la var region11 "Capital area"
la var region12 "Zealand"
la var region13 "Southern Denmark"
la var region14 "Middle Jutland"
la var region15 "Northern Jutland"
label var man "Man (ref: Woman)"

**generating attrition variable
gen attr=0
replace attr=1 if disposition==0 & q3!=.


*labeling continued
label var difference "Welfare-Work Gap"
label var trim "Trimmed Welfare-Work Gap"
label var defect "Larger Welfare-Work Gap than Status Quo (dummy)"
label var highnumeracy "High numeracy (dummy)"
label var rightw "Ideological orientation"
label var small "Small denominator (dummy)"
label var win  "Win frame (dummy)"
label var unem  "Unemployment belief scale"
label var rightSD  "Ideology (standardized)"

***********************************************************************************************************************************************
***Analysis************************************************************************************************************************************
***********************************************************************************************************************************************


*testing for variation in attrition across treatment
tab attr treat, chi


**analysis of variance - unemployment
anova  unem  i.treat 

pwcorr unem rightw, sig obs


**Descriptive Statistics**
tabstat gender agegrp1* region1*, stats(mean sd) columns(stat) 



*H1
tempfile temp1

*regr analysis

*correlation
 pwcorr trim rightw, sig 

reg  trim  i.rightw 
margins, at(rightw=(0(1)10)) saving (`temp1', replace)

*graph
preserve
use `temp1', clear
replace _at=_at-1
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin


twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) || ///
scatter  _margin _at,  msym(O) msize(vlarge) mfcolor(black*0.5) mlwidth(medthick) mlcolor(black)  ///
ylabel(1000(1000)4500, angle(0) labsize(medlarge)) ytitle(" ") ymtick(1000(125)4500) ytick(1000(250)4500) ///
xlabel(0 "Left" 1(1)9 10 "Right", nogrid labsize(medlarge)) xtitle(" ") title(" ") legend(off)  /// 
ytitle("Difference between work and welfare (DKK)", size(medlarge)) 
graph export hyp1.eps, replace 
restore


*H2
*regr analysis
reg  trim  i.treat 
*significance testing
test 1.treat==2.treat
test 1.treat==3.treat
test 1.treat==4.treat
test 2.treat==3.treat
test 2.treat==4.treat
test 3.treat==4.treat
margins, at(treat=(1(1)4)) saving (`temp1', replace)

*graph
preserve
use `temp1', clear
replace _at=_at+0.5 if _at >2 

gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
ta _margin
twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) || ///
scatter  _margin _at if _at == 1 | _at==3.5,  msym(O) msize(vlarge) mfcolor(white) mlwidth(medthick) mlcolor(black) || ///
scatter  _margin _at if _at == 2 | _at==4.5,  msym(O) msize(vlarge) mfcolor(black*0.5) mlwidth(medthick) mlcolor(black)  ///
ylabel(1000(1000)4500, angle(0) labsize(medlarge)) ytitle(" ") ymtick(1000(125)4500) ytick(1000(250)4500) ///
xtitle(" ") title(" ") xlabel(0.75 " " 1.5 "Per Month" 4 "Per Hour"  4.75 " ", nogrid labsize(medlarge) notick) ///
 ytitle("Difference between work and welfare (DKK)", size(medlarge)) xtick(0.75 2.75 4.75) ///
 legend(order (3 4) label(3 "Gain") label(4 "Loss") ring(0) pos(1) margin(0)) xsize(5)
graph export hyp2.eps, replace  
restore

*anova
 an trim small##win


*H3 - third hypothesis

*regression analysis
reg  trim  c.rightSD i.small i.win ,r
margins, dydx(right win small)  saving (`temp1', replace)

*effect size estimates
reg trim small win rightSD
estat esize, omega

*graph
preserve
use `temp1', clear
replace _at=_n
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) horizontal || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) horizontal || ///
scatter   _at _margin ,  msym(O) msize(vlarge) mfcolor(black*0.5) mlwidth(medthick) mlcolor(black)  ///
xlabel(0(500)1800, labsize(medlarge) angle(0)) xtitle(" ") xmtick(0(50)2000)  xtick(0(100)2000) ///
ytitle(" ")  yscale(reverse) title(" ")  legend(off) ///
ylabel(0.75 " " 1 "Effect of ideology (1 SD)  " 2 "Small denominator   " 3 "Loss frame  " 3.25 " ", nogrid ///
labsize(medlarge) notick angle(0) )  xtitle("Effect in DKK", size(medlarge)) ytick(0.5 1.5 2.5 3.5)
graph export hyp3.eps, replace  
restore

*examples of super rightw/leftw person with opposing treatment effects
margins, at(rightSD=(0 3.846154) small=(0 1) win=(0 1))  post
test 5._at==4._at


*regr comparing high and low numeracy
reg  trim  (c.rightSD small win)##c.highnumeracy , r
margins, dydx(right win small) over(highnumeracy) saving(`temp1', replace)

*graph
preserve
use `temp1', clear
replace _at=_n
replace _at=_at-0.6 if _by1==1
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
ta _at
twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) horizontal || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) horizontal || ///
scatter _at _margin if _by1==1, msym(O) msize(vlarge) mcolor(white) mlcolor(black) mlwidth(medthick) ||   ///
scatter  _at  _margin if _by1==0, msym(O) msize(vlarge) mcolor(black*0.5) mlcolor(black) mlwidth(medthick) ///
xlabel(0(500)1800, labsize(medlarge) angle(0)) xtitle(" ") xmtick(0(50)2000) ///
yscale(reverse) xtitle("Effect in DKK",  size(medlarge)) ytitle(" ")ylabel(0.75 " " 1 "Effect of ideology (1 SD)  " 3 "Small denominator   " 5 "Loss frame  " 5.25 " ", nogrid ///
labsize(medlarge) notick angle(0) )  xtitle("Effect in DKK", size(medlarge)) ytick(0 2 4 6) ///
legend(style(background) size(medlarge) ring(0) cols(1) position(1) bmargin(tiny) region(lwidth(none)) order(4 3) label(3 "High numeracy") label(4 "Low numeracy"))
graph export hyp3a.eps, replace 
restore 

**table of descriptive statistics
eststo clear
estpost sum difference trim defect highnumeracy rightw small win unem, d
esttab using descriptives.rtf, cell("mean(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.0fc)) p50(fmt(%9.0fc)) max(fmt(%9.0fc)) count(fmt(%9.0fc))") nonumber label  replace 


***********************************************************************************************************************************************
***Appendix************************************************************************************************************************************
***********************************************************************************************************************************************

*APPENDIX E: Placebo for numeracy

preserve
gen eductwo=0 if education!=.
replace eductwo=1 if education > 4
tempfile temp1
reg  trim  (c.rightSD small win)##(c.eductwo) , r
margins, dydx(right win small) over(eductwo) saving(`temp1', replace)
use `temp1', clear
replace _at=_n
replace _at=_at-0.6 if _by1==1
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
ta _at
twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) horizontal || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) horizontal || ///
scatter _at _margin if _by1==1, msym(O) msize(vlarge) mcolor(white) mlcolor(black) mlwidth(medthick) ||   ///
scatter  _at  _margin if _by1==0, msym(O) msize(vlarge) mcolor(black*0.5) mlcolor(black) mlwidth(medthick) ///
xlabel(0(500)1800, labsize(medlarge) angle(0)) xtitle(" ") xmtick(0(50)2000) ///
yscale(reverse) xtitle("Effect in DKK",  size(medlarge)) ytitle(" ")ylabel(0.75 " " 1 "Effect of ideology (1 SD)  " 3 "Small denominator   " 5 "Loss frame  " 5.25 " ", nogrid ///
labsize(medlarge) notick angle(0) )  xtitle("Effect in DKK", size(medlarge)) ytick(0 2 4 6) ///
legend(style(background) size(medlarge) ring(0) cols(1) position(1) bmargin(tiny) region(lwidth(none)) order(4 3) label(3 "More Education") label(4 "Less education"))

graph export apdxplacebo.png, replace width(1000) 
restore


***********************************************************************************************************************************************
***Analysis************************************************************************************************************************************
***********************************************************************************************************************************************

*APPENDIX C: DIFFERENT DV's

*Defection from status quo

tempfile temp1
logit  defect  i.rightw 
margins, at(rightw=(0(1)10)) saving(`temp1')
preserve
use `temp1', clear
replace _at=_at-1
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) || ///
scatter  _margin _at,  msym(O) msize(vlarge) mfcolor(black*0.5) mlwidth(medthick) mlcolor(black)  ///
ylabel(0.3(0.1)1, angle(0) labsize(medlarge)) ytitle(" ") ymtick(0.3(0.025)1) ytick(0.3(0.05)1) ///
xlabel(0 "Left" 1(1)9 10 "Right", nogrid labsize(medlarge)) xtitle(" ") title(" ") legend(off) title(Larger than Status Quo)  /// 
ytitle("Proportion preferring larger gap than status quo", size(medlarge)) saving(apdx1, replace) nodraw
restore

logit  defect  i.treat 
margins, at(treat=(1(1)4)) saving(`temp1', replace)
preserve
use `temp1', clear
replace _at=_at+0.5 if _at >2 
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
ta _margin
twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) || ///
scatter  _margin _at if _at == 1 | _at==3.5,  msym(O) msize(vlarge) mfcolor(white) mlwidth(medthick) mlcolor(black) || ///
scatter  _margin _at if _at == 2 | _at==4.5,  msym(O) msize(vlarge) mfcolor(black*0.5) mlwidth(medthick) mlcolor(black)  ///
ylabel(0.3(0.1)1, angle(0) labsize(medlarge)) ytitle(" ") ymtick(0.3(0.025)1) ytick(0.3(0.05)1) ///
xtitle(" ") title(" ") xlabel(0.75 " " 1.5 "Per Month" 4 "Per Hour"  4.75 " ", nogrid labsize(medlarge) notick) ///
ytitle("Proportion preferring larger gap than status quo", size(medlarge)) xtick(0.75 2.75 4.75) title(Larger than Status Quo) ///
legend(order (3 4) label(3 "Gain") label(4 "Loss") ring(0) pos(1) margin(0)) xsize(5) saving(apdx2, replace) nodraw
restore

tempfile temp1
eststo c: logit  defect  c.rightSD i.small i.win ,r
margins, dydx(right win small) saving(`temp1', replace)
preserve
use `temp1', clear
replace _at=_n
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) horizontal || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) horizontal || ///
scatter   _at _margin ,  msym(O) msize(vlarge) mfcolor(black*0.5) mlwidth(medthick) mlcolor(black)  ///
xlabel(0(0.05)0.35, angle(0) labsize(medlarge)) xtitle(" ") xmtick(0(0.01)0.35)   ///
ytitle(" ")  yscale(reverse) title(" ")  legend(off)  title(Larger than Status Quo)  ///
ylabel(0.75 " " 1 "Effect of ideology (1 SD)  " 2 "Small denominator   " 3 "Loss frame  " 3.25 " ", nogrid ///
labsize(medlarge) notick angle(0) )  xtitle("Effect in percentage points", size(medlarge)) ytick(0.5 1.5 2.5 3.5) saving(apdx3.gph, replace)
restore

eststo d: logit  defect  (c.rightSD i.small i.win)##c.highnumeracy 
margins, dydx(right win small) over(highnumeracy) saving(`temp1', replace)
preserve
use `temp1', clear
replace _at=_n
replace _at=_at-0.6 if _by1==1
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
twoway rspike _ci_lb _ci_ub _at, lwidth(medium) lcolor(black) horizontal || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) horizontal || ///
||scatter  _at _margin if _by1==1, msym(O) msize(large) mcolor(white) mlcolor(black) mlwidth(medthick)   ///
||scatter  _at _margin if _by1==0, msym(O) msize(large) mlcolor(black)  mlwidth(medthick) mfcolor(black*0.5) ///
 scheme(plotplain) title(Larger than Status Quo)  xlabel(0(0.05)0.35, angle(0) labsize(medlarge)) xtitle(" ") xmtick(0(0.01)0.35)   ///
 yscale(reverse) xtitle("Effect in percentage points", size(medlarge)) ytitle(" ") ylabel(0.5 " " 1 ///
 "Effect of ideology (1 SD)  " 3 "Small denominator   " 5 "Loss frame  " 5.5 " ", notick angle(0) labsize(medlarge)) ///
 legend(style(background) size(medlarge) ring(0) cols(1) position(1) bmargin(tiny) region(lwidth(none)) order(4 3) label(3 "High") label(4 "Low"))  saving(apdx4.gph, replace)
restore


*Median responses

tempfile temp1

qreg  difference  i.rightw 
margins, at(rightw=(0(1)10)) saving(`temp1')
preserve
use `temp1', clear
replace _at=_at-1
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) || ///
scatter  _margin _at,  msym(O) msize(vlarge) mfcolor(black*0.5) mlwidth(medthick) mlcolor(black)  ///
ylabel(1000(1000)4500, angle(0) labsize(medlarge)) ytitle(" ") ymtick(1000(125)4500) ytick(1000(250)4500) ///
xlabel(0 "Left" 1(1)9 10 "Right", nogrid labsize(medlarge)) xtitle(" ") title(" ") legend(off) title(Median)  /// 
ytitle("Difference between work and welfare (DKK)", size(medlarge))  saving(apdx1a.gph, replace) nodraw
restore


qreg  difference  i.treat 
margins, at(treat=(1(1)4)) saving(`temp1', replace)
preserve
use `temp1', clear
replace _at=_at+0.5 if _at >2 
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
ta _margin
twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) || ///
scatter  _margin _at if _at == 1 | _at==3.5,  msym(O) msize(vlarge) mfcolor(white) mlwidth(medthick) mlcolor(black) || ///
scatter  _margin _at if _at == 2 | _at==4.5,  msym(O) msize(vlarge) mfcolor(black*0.5) mlwidth(medthick) mlcolor(black)  ///
ylabel(1000(1000)4500, angle(0) labsize(medlarge)) ytitle(" ") ymtick(1000(125)4500) ytick(1000(250)4500) ///
xtitle(" ") title(" ") xlabel(0.75 " " 1.5 "Per Month" 4 "Per Hour"  4.75 " ", nogrid labsize(medlarge) notick) ///
ytitle("Difference between work and welfare (DKK)", size(medlarge)) xtick(0.75 2.75 4.75) title(Median) ///
legend(order (3 4) label(3 "Gain") label(4 "Loss") ring(0) pos(1) margin(0)) xsize(5) saving(apdx2a.gph, replace) nodraw
restore

eststo e: qreg  difference  c.rightSD i.small i.win 
margins, dydx(right win small)  saving(`temp1', replace)
preserve
use `temp1', clear
replace _at=_n
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) horizontal || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) horizontal || ///
scatter   _at _margin ,  msym(O) msize(vlarge) mfcolor(black*0.5) mlwidth(medthick) mlcolor(black)  ///
xlabel(0(500)1500, labsize(medlarge) angle(0)) xtitle(" ") xmtick(0(50)1500)  xtick(0(100)1500) ///
ytitle(" ")  yscale(reverse) title(" ")  legend(off) title(Median) ///
ylabel(0.75 " " 1 "Effect of ideology (1 SD)  " 2 "Small denominator   " 3 "Loss frame  " 3.25 " ", nogrid ///
labsize(medlarge) notick angle(0) )  xtitle("Effect in DKK", size(medlarge)) ytick(0.5 1.5 2.5 3.5) saving(apdx3a.gph, replace)
restore

 eststo f: qreg  difference  (c.rightSD i.small i.win)##c.highnumeracy 
margins, dydx(right win small) over(highnumeracy) saving(`temp1', replace)
preserve
use `temp1', clear
replace _at=_n
replace _at=_at-0.6 if _by1==1
gen _ci_lb1=-1.64*_se+_margin
gen _ci_ub1=1.64*_se+_margin
ta _at
twoway rspike _ci_lb _ci_ub _at, scheme(plotplain) lwidth(medium) lcolor(black) horizontal || ///
rspike _ci_lb1 _ci_ub1 _at, lwidth(thick) lcolor(black) horizontal || ///
scatter _at _margin if _by1==1, msym(O) msize(vlarge) mcolor(white) mlcolor(black) mlwidth(medthick) ||   ///
scatter  _at  _margin if _by1==0, msym(O) msize(vlarge) mcolor(black*0.5) mlcolor(black) mlwidth(medthick) ///
xlabel(0(500)1500, labsize(medlarge) angle(0)) xtitle(" ") xmtick(0(50)1500) ///
yscale(reverse) xtitle("Effect in DKK",  size(medlarge)) ytitle(" ")ylabel(0.75 " " 1 "Effect of ideology (1 SD)  " 3 "Small denominator   " 5 "Loss frame  " 5.25 " ", nogrid ///
labsize(medlarge) notick angle(0) )  xtitle("Effect in DKK", size(medlarge)) ytick(0 2 4 6) title(Median) ///
legend(style(background) size(medlarge) ring(0) cols(1) position(1) bmargin(tiny) region(lwidth(none)) order(4 3) label(3 "High") label(4 "Low")) saving(apdx4a.gph, replace)
restore

graph combine apdx1.gph apdx1a.gph,  xsize(9) scheme(plotplain)
graph export apdx1.png, replace width(1000) 

graph combine apdx2.gph apdx2a.gph,  xsize(9) scheme(plotplain)
graph export apdx2.png, replace width(1000) 

graph combine apdx3.gph apdx3a.gph, xsize(9) scheme(plotplain)
graph export apdx3.png, replace width(1000) 

graph combine apdx4.gph apdx4a.gph, xsize(9) scheme(plotplain)
graph export apdx4.png, replace width(1000) 



*Appendix D: FULL TABLES

eststo a: reg  trim  c.rightSD i.small i.win ,r
eststo b: reg  trim  (c.rightSD small win)##c.highnumeracy , r

esttab a b c d e f using fullmodel.rtf, replace se label stat(N, label("N") fmt(%9.0f)) b(%9.2f) star("*" 0.05) ///
drop(0.win 0.smallenumerator 0.win#c.highnumeracy 0.smallenumerator#c.highnumeracy)

 

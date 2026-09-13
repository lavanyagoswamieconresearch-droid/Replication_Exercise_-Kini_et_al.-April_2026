
*******************************************
**# Econometrics Project EC2C1 2025-2026 **
** BY: LAVANYA GOSWAMI
*******************************************
** Replication of Kini et al (2022)

clear all
**# KSSS Firm Panel Data (Loading Panel dataset)************
use "E:\LSE\First_Year\Econometrics_Project\KSSS_firm_panel.dta", clear

**# Data Preparation
** Generate non-string panel id variable
destring firm_id, generate (firm)
xtset firm year // declaring panel nature (panel id, time variable)

** Winsorise control variables to limit influence of outliers
* in the regressors (same as original paper)
winsor2 leverage_book_ibdebtw rdintensityw ///
        hhi_sale_ccmsic_3digw tfp1_2digw, cuts(1 99) replace
		
** Constructed control variable 
gen size_2 = logmew*logmew
label var size_2 "Size square"

label var hhi_sale_ccmsic_3digw "Herfindahl-Hirschman Index"
label var rdintensityw "Research and Development"
label var nsupps1 "Number of suppliers"
label var vifvepct_sglvl "Vertical Integration"
label var tfp1_2digw "Total Factor Productivity"
*************************************************		
**#** Recall dummies with t+1,2,3
* recall_t`i' takes value 1 if cumulative recall>0 in t=t+i, 
* 0 if no recalls, preserves missing values

forvalues t = 1/3 {
    gen recall_t`t' = (cumrcl_mo_`t' > 0) if !missing(cumrcl_mo_`t')
	label var recall_t`t' "CumRecall(t+`t')"
}

*************************************************		
**# Panel Regressions 1 *****************

** Probability of recall <-- Probability of Unionisation

eststo clear // clear previous estimates

foreach t in 1 3  {

reghdfe recall_t`t' ///
    ifunion_len3 leverage_book_ibdebtw rdintensityw hhi_sale_ccmsic_3digw nsupps1 tfp1_2digw ///
    vifvepct_sglvl logmew size_2,  vce(cluster firm) absorb(i.year##i.ind)
eststo model_`t'
estadd local year_fe "No"
estadd local ind_fe "Yes"

reghdfe recall_t`t' ///
    ifunion_len3 nsupps1 vifvepct_sglvl hhi_sale_ccmsic_3digw logmew size_2, vce(cluster firm) absorb(i.year##i.ind)
eststo model2_`t'
estadd local year_fe "No"
estadd local ind_fe "Yes"

reghdfe recall_t`t' ///
    ifunion_len3 leverage_book_ibdebtw rdintensityw hhi_sale_ccmsic_3digw nsupps1 tfp1_2digw ///
    vifvepct_sglvl logmew size_2, vce(cluster firm) absorb(i.year)
eststo model3_`t'
estadd local year_fe "Yes"
estadd local ind_fe "No"
}

esttab model* ///
    using "Reg_Table1.tex", replace ///
    keep(ifunion_len3 leverage_book_ibdebtw hhi_sale_ccmsic_3digw rdintensityw nsupps1 tfp1_2digw ///
         vifvepct_sglvl logmew size_2) ///
    label se star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Prob(CumRecall t+1)""t+1""t+1""t+3""t+3""t+3") ///
stats(year_fe ind_fe N r2 r2_a, ///
    labels("Year Fixed Effects" "IndustryxYear Fixed Effects" "Observations" "R-squared" "Adj. R-squared")) ///
    title("Effect of any unionization on probability of any cumulative recalls") ///
    booktabs
	
*************************************************		
**# Panel Regressions 2 *****************

** Frequency of positive recall <-- Percentage of Unionisation

eststo clear // clear previous estimates

foreach t in 1 3  {

reghdfe cumrcl_mo_`t' ///
    wpctl_union_len3 leverage_book_ibdebtw rdintensityw hhi_sale_ccmsic_3digw nsupps1 tfp1_2digw ///
    vifvepct_sglvl logmew size_2 if cumrcl_mo_`t'>0 , vce(cluster firm) absorb(i.year##i.ind)
eststo model1_`t'
estadd local year_fe "No"
estadd local ind_fe "Yes"

reghdfe cumrcl_mo_`t' ///
    wpctl_union_len3 nsupps1 hhi_sale_ccmsic_3digw vifvepct_sglvl logmew size_2 if cumrcl_mo_`t'>0 , vce(cluster firm) absorb(i.year##i.ind)
eststo model2_`t'
estadd local year_fe "No"
estadd local ind_fe "Yes"

reghdfe cumrcl_mo_`t' ///
    wpctl_union_len3 leverage_book_ibdebtw hhi_sale_ccmsic_3digw rdintensityw nsupps1 tfp1_2digw ///
    vifvepct_sglvl logmew size_2 if cumrcl_mo_`t'>0 , vce(cluster firm) absorb(i.year)
eststo model3_`t'
estadd local year_fe "Yes"
estadd local ind_fe "No"
}

esttab model* ///
    using "Reg_Table2.tex", replace ///
    keep(wpctl_union_len3 leverage_book_ibdebtw hhi_sale_ccmsic_3digw rdintensityw nsupps1 tfp1_2digw ///
         vifvepct_sglvl logmew size_2) ///
    label se star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("CumRecalls(t+1)""t+1""t+1""t+3""t+3""t+3") ///
stats(year_fe ind_fe N r2 r2_a, ///
    labels("Year Fixed Effects" "IndustryxYear Fixed Effects" "Observations" "R-squared" "Adj. R-squared")) ///
    title("Effect of Unionization perc. on number of positive cumulative recalls") ///
    booktabs
	
**# Robustness section: Panel **************************************************

**#1. Check if leads and lags of unionisation are significant

gen ifunion_len3_lead = F.ifunion_len3
gen ifunion_len3_lag = L.ifunion_len3
gen ifunion_len3_lead2 = F2.ifunion_len3
gen ifunion_len3_lag2 = L2.ifunion_len3

gen wpctl_union_len3_lead = F.wpctl_union_len3
gen wpctl_union_len3_lag = L.wpctl_union_len3
gen wpctl_union_len3_lead2 = F2.wpctl_union_len3
gen wpctl_union_len3_lag2 = L2.wpctl_union_len3


eststo clear

reghdfe cumrcl_mo_1 ///
    ifunion_len3_lag ifunion_len3 ifunion_len3_lead ///
    leverage_book_ibdebtw rdintensityw ///
    hhi_sale_ccmsic_3digw nsupps1 tfp1_2digw vifvepct_sglvl ///
    logmew size_2, ///
    absorb(i.year##i.ind) ///
    cluster(firm)
estadd local year_fe "No"
estadd local ind_fe "Yes"
eststo t1
	
reghdfe recall_t1 ///
    ifunion_len3_lag ifunion_len3 ifunion_len3_lead  ///
    leverage_book_ibdebtw rdintensityw ///
    hhi_sale_ccmsic_3digw nsupps1 tfp1_2digw vifvepct_sglvl ///
    logmew size_2, ///
    absorb(i.year##i.ind) ///
    cluster(firm)
estadd local year_fe "No"
estadd local ind_fe "Yes"
eststo t2
	
reghdfe cumrcl_mo_1 ///
    wpctl_union_len3_lead  wpctl_union_len3 wpctl_union_len3_lag ///
    leverage_book_ibdebtw rdintensityw ///
    hhi_sale_ccmsic_3digw nsupps1 tfp1_2digw vifvepct_sglvl ///
    logmew size_2, ///
    absorb(i.year##i.ind) ///
    cluster(firm)
estadd local year_fe "No"
estadd local ind_fe "Yes"
eststo t3
	
reghdfe recall_t1 ///
    wpctl_union_len3_lead  wpctl_union_len3 wpctl_union_len3_lag  ///
	leverage_book_ibdebtw rdintensityw ///
    hhi_sale_ccmsic_3digw nsupps1 tfp1_2digw vifvepct_sglvl ///
    logmew size_2, ///
    absorb(i.year##i.ind) ///
    cluster(firm)
estadd local year_fe "No"
estadd local ind_fe "Yes"
eststo t4
	
esttab t* ///
    using "Reg_Table3.tex", replace ///
    keep( ifunion_len3_lag ifunion_len3 ifunion_len3_lead wpctl_union_len3_lag  wpctl_union_len3 wpctl_union_len3_lead  ///  
	leverage_book_ibdebtw hhi_sale_ccmsic_3digw rdintensityw nsupps1 tfp1_2digw ///
         vifvepct_sglvl logmew size_2) ///
    label se star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Freq Recall (t+1)" "Prob Recall (t+1)" "Freq Recall (t+1)" "Prob Recall (t+1)") ///
stats(year_fe ind_fe N r2 r2_a, ///
    labels("Year Fixed Effects" "IndustryxYear Fixed Effects" "Observations" "R-squared" "Adj. R-squared")) ///
    title("Effect of unionisation  (leads and lags) on recalls") ///
    booktabs
	
	
**#2. Plot by year
* Identify first year of unionisation
bysort firm (year): gen first_union_year = year if ifunion_len3 == 1
bysort firm: egen event_year = min(first_union_year)

* Event time (relative year)
gen rel_time = year - event_year
gen rel_time_shift = rel_time + 5
keep if rel_time >= 0 & rel_time <= 10

fvset base 4 rel_time_shift

eststo clear 
reghdfe recall_t1 ib4.rel_time ///
    leverage_book_ibdebtw rdintensityw hhi_sale_ccmsic_3digw ///
    nsupps1 hhi_sale_ccmsic_3digw vifvepct_sglvl ///
    logmew size_2, ///
    absorb(i.ind##i.year) ///
    vce(cluster firm)

eststo t1

reghdfe recall_t1 ib4.rel_time ///
    leverage_book_ibdebtw rdintensityw hhi_sale_ccmsic_3digw ///
    nsupps1 hhi_sale_ccmsic_3digw vifvepct_sglvl ///
    logmew size_2, ///
    absorb(i.ind##i.year) ///
    vce(cluster firm)
	
eststo t2

reghdfe recall_t3 ib4.rel_time ///
    leverage_book_ibdebtw rdintensityw hhi_sale_ccmsic_3digw ///
    nsupps1 hhi_sale_ccmsic_3digw vifvepct_sglvl ///
    logmew size_2, ///
    absorb(i.ind##i.year) ///
    vce(cluster firm)

eststo t3

coefplot ///
    (t1, label("T+1") ///
        mcolor(blue) ///
        ciopts(recast(rcap) lcolor(blue))) ///
    (t2, label("T+2") ///
        mcolor(green) ///
        ciopts(recast(rcap) lcolor(green))) ///
    (t3, label("T+3") ///
        mcolor(red) ///
        ciopts(recast(rcap) lcolor(red))), ///
    keep(*.rel_time) ///
    vertical ///
    xline(5, lpattern(dash)) ///
    yline(0, lpattern(dash)) ///
    xlabel(0 "-5" 1 "-4" 2 "-3" 3 "-2" 4 "-1" ///
           5 "0" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5", angle(45)) ///
    ylabel(, labsize(small)) ///
    msymbol(circle) msize(small) ///
    legend(order(1 "CI T+1" 2 "T+1" 3 "CI T+2" 4 "T+2" 5 " CI T+3" 6 "T+3")) ///
    title("Effect of Unionisation on Recall Probability") ///
    xtitle("Years Relative to Unionisation") ///
    ytitle("Effect on Recall Probability")
	
	
**********************************************************************
**#*******************************************************************
**********************************************************************

**# KSS Union Election Data ********************
clear all
use "E:\LSE\First_Year\Econometrics_Project\KSSS_union_elections.dta", clear

**# Data Preparation
** Generate firm identifier for controls
destring firm_id, generate (firm)

gen main = . // label for agency type
replace main=1 if main_cpsc ==1
replace main=2 if main_fda ==1 
replace main=3 if main_nh ==1
label define main 1 "CPSC" 2 "FDA" 3 "NHTSA"
label values main main

label define year 1 "2003" 2 "2004" 3 "2005" 4 "2006" /// year labels
5 "2007" 6 "2008" 7 "2009" 8 "2010" 9 "2011" 10 "2012" 11 "2013"
label values year year

label define rtw 1 "RTW" 0 "Non - RTW" // Right to Work state label
label values rtw rtw

** Centered running variable
gen election_dist = pctl_for - 0.5
** This variable is to measure how much difference the election win/loss 
** was from the 50% election win cut-off 
gen election_dist2=election_dist*election_dist // square of election distance

**#** Probability of Recall Dummies
foreach t in 1 3{
gen any_rcl`t' = (cumrcl_`t' > 0) if !missing(cumrcl_`t')
label var any_rcl`t' "Prob(Recall) t+`t'"
}

**# Descriptive Statistics Table***********
** to check whether sample covariates are balanced around cut-off
dtable i.year i.rtw main_fda main_nh main_cpsc ///
	if election_dist>-0.15 & election_dist<0.15, ///
	by(union_win, tests) export("RDDdtable", as(tex) replace)
	
dtable i.year i.rtw main_fda main_nh main_cpsc ///
	if election_dist>-0.2 & election_dist<0.2, ///
	by(union_win, tests) export("RDDdtable", as(tex) append)
* notice no significance among observed covariates. 

**# RDD regressions 1: probability of recall
eststo clear
foreach t in 1 3 {
	reg any_rcl`t' i.union_win##c.election_dist ///
    if abs(election_dist)<=0.15, ///
    vce(robust)
eststo model1_`t'
estadd local firm_fe "No"
estadd local year_fe "No"

	reg any_rcl`t' i.union_win##c.election_dist rtw ///
    if abs(election_dist)<=0.2, ///
    vce(robust)
eststo model2_`t'
estadd local firm_fe "No"
estadd local year_fe "No"

	areg any_rcl`t' i.union_win##c.election_dist ///
    if abs(election_dist)<=0.2, ///
    absorb(year ind) vce(robust)
eststo model3_`t'
estadd local firm_fe "Yes"
estadd local year_fe "Yes"
}

esttab model* ///
    using "RDD_Table1.tex", replace ///
    label se star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("15%" "20%" "20%" "15%" "20%" "20%") ///
	stats(firm_fe year_fe N, ///
    labels("Firm Fixed Effects" "Year Fixed Effects" "Observations")) ///
    title("RDD Estimates: Effect of Unionization on probability of recall") ///
    booktabs

	

**# RDD Regressions 2: Frequency of recall
eststo clear
foreach t in 1 3 {
reg cumrcl_`t' i.union_win##c.election_dist ///
    if abs(election_dist)<=0.15 & cumrcl_`t'>0, vce(robust)
eststo model1_`t'
estadd local firm_fe "No"
estadd local year_fe "No"

reg cumrcl_`t' i.union_win##c.election_dist ///
    if abs(election_dist)<=0.2 & cumrcl_`t'>0, vce(robust)
eststo model2_`t'
estadd local firm_fe "No"
estadd local year_fe "No"

areg cumrcl_`t' i.union_win##c.election_dist ///
    if abs(election_dist)<=0.2 & cumrcl_`t'>0,  absorb(year ind) vce(robust)
eststo model3_`t'
estadd local firm_fe "Yes"
estadd local year_fe "Yes"
}

esttab model* ///
    using "RDD_Table2.tex", replace ///
    label se star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("15%" "20%" "20%" "15%" "20%" "20%") ///
	stats(firm_fe year_fe N, ///
    labels("Firm Fixed Effects" "Year Fixed Effects" "Observations")) ///
    title("RDD Estimates: Effect of Unionization on cumulative recalls") ///
    booktabs
	
**************************************************************
**************************************************************
**************************************************************
	
**# Robustness section: RD setting

eststo clear

local varlist cogs_sale kld_adj6 capxint_l1at oplev_lvl4_r

    foreach v of local varlist {

        reg `v'_1 ///
            i.union_win##c.election_dist ///
            if abs(election_dist)<=0.15, ///
            vce(robust)

        eststo model_`v'_1
    }

    * store each group into a separate table
    esttab model_*_1 using table1_1.tex, replace ///
        title("Results for suffix 1") ///
        se star(* 0.10 ** 0.05 *** 0.01)
        
** Project by: Lavanya Goswami



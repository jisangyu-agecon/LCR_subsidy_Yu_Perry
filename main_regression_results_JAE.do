clear all
set more off

*Data are available at "https://www.dropbox.com/sh/0xy7g9mfugb108k/AABdUXyZ22z9IzvDNgB7T_Sxa?dl=0"

cd "/Users/jisangyu/Dropbox/Crop_insurance_demand_selection/selection_risk_pool_paper/data/JAE_Yu_Perry"
use all_sob_1989_2018.dta, clear

global results "/Users/jisangyu/Dropbox/Crop_insurance_demand_selection/selection_risk_pool_paper/manuscript/JAE_submission"

*keep corn and soybeans
replace cropcode=41 if cropcode==441
replace cropcode=81 if cropcode==481
keep if cropcode==41 | cropcode==81 

*YP
gen YP=0
replace YP=1 if plancode==1 | plancode==90

*RP
gen RP=0
replace RP=1 if plancode==2 | plancode==3 | plancode==25 | plancode==42 | plancode==44

*drop other plans
drop if YP==0 & RP==0

*cov group
encode coveragecategory, gen(covtype)
drop if coverage==0 | coverage==.
gen covlevel=2 if coverage<.55
replace covlevel=1 if covtype==2 //cat

replace covlevel=3 if coverage>.54 & coverage<.56
replace covlevel=4 if coverage>.59 & coverage<.61
replace covlevel=5 if coverage>.64 & coverage<.66
replace covlevel=6 if coverage>.69 & coverage<.71
replace covlevel=7 if coverage>.74 & coverage<.76
replace covlevel=8 if coverage>.79 & coverage<.81
replace covlevel=9 if coverage>.84 & coverage<.86

label define cov_lab 1 "CAT 50" 2 "Buy-up 50" 3 "Buy-up 55" 4 "Buy-up 60" 5 "Buy-up 65" 6 "Buy-up 70" 7 "Buy-up 75" 8 "Buy-up 80" 9 "Buy-up 85"
label values covlevel cov_lab
label var covlevel "Coverage Levels"
drop if covlevel==.

*County-by-coverage-by-crop
collapse (sum) indemnity total_premium subsidy quantity liability, by(statecode countycode cropcode covlevel year YP RP)

*fips and weather variables
gen fips=statecode*1000+countycode
merge m:1 fips year using PRISM_1989_2017_apr_sep.dta
keep if _merge==3
drop _merge

gen gdd=dday10C-dday29C
gen prec2=prec_apr_sep^2

*Legislatively-set subsidy rates
gen subrate=.
replace subrate=1 if covlevel==1
replace subrate=0.30 if year<1995 & covlevel==2
replace subrate=0.30 if year<1995 & covlevel==5
replace subrate=0.17 if year<1995 & covlevel==7

replace subrate=0.55 if year>1994 & year<2001 & covlevel==2
replace subrate=0.46 if year>1994 & year<2001 & covlevel==3
replace subrate=0.38 if year>1994 & year<2001 & covlevel==4
replace subrate=0.42 if year>1994 & year<2001 & covlevel==5
replace subrate=0.32 if year>1994 & year<2001 & covlevel==6
replace subrate=0.24 if year>1994 & year<2001 & covlevel==7
replace subrate=0.17 if year>1994 & year<2001 & covlevel==8
replace subrate=0.13 if year>1994 & year<2001 & covlevel==9

replace subrate=0.67 if year>2000 & covlevel==2
replace subrate=0.64 if year>2000 & covlevel==3
replace subrate=0.64 if year>2000 & covlevel==4
replace subrate=0.59 if year>2000 & covlevel==5
replace subrate=0.59 if year>2000 & covlevel==6
replace subrate=0.55 if year>2000 & covlevel==7
replace subrate=0.48 if year>2000 & covlevel==8
replace subrate=0.38 if year>2000 & covlevel==9

*average subsidy rate in a crop-by-county combination
bys fips year cropcode: egen total_liability=sum(liability)
gen weighted_subrate=subrate*(liability/total_liability)
by fips year cropcode: egen avg_subrate=sum(weighted_subrate)
replace avg_subrate=(avg_subrate-weighted_subrate)*(total_liability/(total_liability-liability))

*gen Buy-up dummy
gen bup=1
replace bup=0 if covlevel==1

*post 2008 dummy (EU subsidy increase)
gen post2008=0
replace post2008=1 if year>2008

*EU subsidy rate increase post2008 (for bup only)
gen eu_subrate=subrate
replace eu_subrate=0.8 if year>2008
replace eu_subrate=0.77 if year>2008 & covlevel==7
replace eu_subrate=0.68 if year>2008 & covlevel==8 
replace eu_subrate=0.53 if year>2008 & covlevel==8 

*gen EU subrate x post 2008 x bup
gen subratexpost2008=eu_subrate*post2008*bup

*trend
gen trend=year-1988
gen trend2=trend^2

*lcr
gen lcr=indemnity/liability
drop if lcr==.

*lcr outliers
replace lcr=0 if lcr<0
replace lcr=1 if lcr>1

*farm-paid prem per lia
gen farm_prelia=(total_premium-subsidy)/liability

*LCR - avg prem rate
gen dev_prelia=lcr-(total_premium)/liability


*average farm-paid prem per lia in a crop-by-county combination
gen weighted_farm_prelia=farm_prelia*(liability/total_liability)
by fips year cropcode: egen avg_farm_prelia=sum(weighted_farm_prelia)
replace avg_farm_prelia=(avg_farm_prelia-weighted_farm_prelia)*(total_liability/(total_liability-liability))

*variable label
label var lcr "Loss-cost ratio"

label var subrate "Subsidy Rate$ {_{j}}$"
label var avg_subrate "Avg. Subsidy Rate$ {_{j'}}$"
label var farm_prelia "Farm-paid Premium$ {_{ijkt}}$"
label var avg_farm_prelia "Avg. Farm-paid Premium$ {_{ij'k't}}$"

label var RP "Revenue Products"
label var gdd "GDD"
label var dday29C "HDD"
label var prec_apr_sep "Precipitation"
label var prec2 "Precipitation$^2$"

*FIG 1: CROP-SPECIFIC LCR AND 29C DDAYS
preserve 
gen corn=0
replace corn=1 if cropcode==41
gen soy=0
replace soy=1 if cropcode==81

bysort year: egen dday29_mean = mean(dday29C)
foreach x of varlist quantity liability total_premium subsidy indemnity dday29C {
	bysort year: egen `x'_corn = sum(corn*`x')
	bysort year: egen `x'_soy = sum(soy*`x')
}
foreach x of varlist corn soy {
	ge lcr_`x' = indemnity_`x'/liability_`x'
	ge prem_`x' = (total_premium_`x'-subsidy_`x')/liability_`x'
}
duplicates drop year, force
*GRAPH
twoway (line lcr_corn lcr_soy year, sort yaxis(1)) ///
(line dday29_mean year,  lwidth(vthin) lpattern(shortdash) yaxis(2)), ///
xlabel(1989(2)2017, angle(ninety)) legend(label(1 "Corn") label(2 "Soybeans") ///
label(3 "Mean 29C DDays") rows(1) pos(6)) xtitle("Year") title() ///
ytitle("LCR", axis(1)) ytitle("29C Degree Days", axis(2)) graphregion(color(white))
gr export "$results/LCR_DDays.eps", replace
restore

*summary table
preserve
reghdfe  lcr subrate i.covlevel RP if cropcode==41, absorb(fips year) cluster(statecode year)
keep if e(sample)==1
keep lcr subrate farm_prelia RP gdd dday29C prec_apr_sep prec2
order lcr subrate farm_prelia RP gdd dday29C prec_apr_sep prec2
outreg2 using "$results/summary_stat.tex", replace tex(frag) cttop("Corn") sum(log) eqkeep(mean sd) label
restore
preserve
reghdfe  lcr subrate i.covlevel RP if cropcode==81, absorb(fips year) cluster(statecode year)
keep if e(sample)==1
keep lcr subrate farm_prelia RP gdd dday29C prec_apr_sep prec2
order lcr subrate farm_prelia RP gdd dday29C prec_apr_sep prec2
outreg2 using "$results/summary_stat.tex", append tex(frag) cttop("Soybeans") sum(log) eqkeep(mean sd) label
restore

*Map of avg. LCR
preserve
keep if cropcode==41
collapse (sum) indemnity liability, by(fips)
merge 1:1 fips using "us_county_shape/cb_2017_us_county_20m.dta"
gen lcr=indemnity/liability
format lcr %12.2f 
spmap lcr using "us_county_shape/cb_2017_us_county_20m_shp.dta", id(fips)  clm(quantile) cln(5)  ///
 fcolor(Heat) legend(pos(4)) ///
ndf(white) ndl("no data") title("Average LCR (1989-2017), Corn", size(small))
gr save "corn_lcr.gph", replace
restore
preserve
keep if cropcode==81
collapse (sum) indemnity liability, by(fips)
merge 1:1 fips using "us_county_shape/cb_2017_us_county_20m.dta"
gen lcr=indemnity/liability
format lcr %12.2f 
spmap lcr using "us_county_shape/cb_2017_us_county_20m_shp.dta", id(fips)  clm(quantile) cln(5)  ///
 fcolor(Heat) legend(pos(4)) ///
ndf(white) ndl("no data") title("Average LCR (1989-2017), Soybeans", size(small))
gr save "soy_lcr.gph", replace
restore

graph combine corn_lcr.gph soy_lcr.gph, col(1) graphregion(color(white))
graph export "$results/LCR_map.eps", replace


*farm-paid premium per liability as the treatment var (table 3)
preserve
keep if cropcode==41
reghdfe  lcr farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_corn.tex",   nocon keep(farm_prelia RP gdd dday29C prec_apr_sep prec2) cttop("FE") addtext(First stage F, NA, Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) replace
reghdfe  lcr farm_prelia avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_corn.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("FE") addtext(First stage F, NA, Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_corn.tex",   nocon keep(farm_prelia RP gdd dday29C prec_apr_sep prec2) cttop("FE-IV") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_corn.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("FE-IV") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
restore

preserve
keep if cropcode==81
reghdfe  lcr farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_soy.tex",   nocon keep(farm_prelia RP gdd dday29C prec_apr_sep prec2) cttop("FE") addtext(First stage F, NA, Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) replace
reghdfe  lcr farm_prelia avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_soy.tex",   nocon keep(farm_prelia avg_farm_prelia RP gdd dday29C prec_apr_sep prec2) cttop("FE") addtext(First stage F, NA, Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_soy.tex",   nocon keep(farm_prelia RP gdd dday29C prec_apr_sep prec2) cttop("FE-IV") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_soy.tex",   nocon keep(farm_prelia avg_farm_prelia RP gdd dday29C prec_apr_sep prec2) cttop("FE-IV") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
restore

*LCR vs subsidy and demand vs subsidy (table 4)
reghdfe  lcr subrate i.covlevel RP gdd dday29C prec_apr_sep prec2 if cropcode==41, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_rf.tex",   nocon keep(subrate RP gdd dday29C prec_apr_sep prec2) cttop("FE: Corn") addtext(Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) replace
reghdfe  lcr subrate avg_subrate i.covlevel RP gdd dday29C prec_apr_sep prec2 if cropcode==41, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_rf.tex",   nocon keep(subrate RP avg_subrate gdd dday29C prec_apr_sep prec2) cttop("FE: Corn") addtext(Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
reghdfe  lcr subrate i.covlevel RP gdd dday29C prec_apr_sep prec2 if cropcode==81, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_rf.tex",   nocon keep(subrate RP gdd dday29C prec_apr_sep prec2) cttop("FE: Soybeans") addtext(Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
reghdfe  lcr subrate avg_subrate i.covlevel RP gdd dday29C prec_apr_sep prec2 if cropcode==81, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_rf.tex",   nocon keep(subrate RP avg_subrate gdd dday29C prec_apr_sep prec2) cttop("FE: Soybeans") addtext(Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append

*LCR - Premium rate vs subsidy and demand vs subsidy (table 4a)
label var dev_prelia "(LCR - Prem. Rate)"
reghdfe  dev_prelia subrate i.covlevel RP gdd dday29C prec_apr_sep prec2 if cropcode==41, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_rf_dev_prelia.tex",   nocon keep(subrate RP gdd dday29C prec_apr_sep prec2) cttop("FE: Corn") addtext(Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) replace
reghdfe  dev_prelia subrate avg_subrate i.covlevel RP gdd dday29C prec_apr_sep prec2 if cropcode==41, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_rf_dev_prelia.tex",   nocon keep(subrate RP avg_subrate gdd dday29C prec_apr_sep prec2) cttop("FE: Corn") addtext(Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
reghdfe  dev_prelia subrate i.covlevel RP gdd dday29C prec_apr_sep prec2 if cropcode==81, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_rf_dev_prelia.tex",   nocon keep(subrate RP gdd dday29C prec_apr_sep prec2) cttop("FE: Soybeans") addtext(Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
reghdfe  dev_prelia subrate avg_subrate i.covlevel RP gdd dday29C prec_apr_sep prec2 if cropcode==81, absorb(fips year) cluster(statecode year)
outreg2 using "$results/preliminary_rf_dev_prelia.tex",   nocon keep(subrate RP avg_subrate gdd dday29C prec_apr_sep prec2) cttop("FE: Soybeans") addtext(Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append


*Long Difference: lcr CIRA (table 5)
preserve
keep if cropcode==41
egen fips_cov_rp=group(fips covlevel RP)
xtset fips_cov_rp year
foreach x in lcr subrate avg_subrate gdd dday29C prec_apr_sep prec2{
tssmooth ma avg5_`x'=`x', window(4 1 0)
}
keep if year==2017 | year==1993
gen new_t=1
replace new_t=2 if year==1993

xtset fips_cov_rp new_t
foreach x in lcr subrate avg_subrate gdd dday29C prec_apr_sep prec2{
gen d_`x'=d.avg5_`x'
}

*variable label
label var d_lcr "$\Delta$ Loss-cost ratio"
label var d_subrate "$\Delta$ Subsidy Rate$ {_j}$"
label var d_avg_subrate "$\Delta$ Avg. Subsidy Rate$ {_j'}$"

label var d_gdd "$\Delta$ GDD"
label var d_dday29C "$\Delta$ HDD"
label var d_prec_apr_sep "$\Delta$ Precipitation"
label var d_prec2 "$\Delta$ Precipitation$^2$"

keep if new_t==2
reghdfe  d_lcr d_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2, absorb(statecode) cluster(statecode)
outreg2 using "$results/preliminary_rf_ld_0.tex",   nocon keep(d_subrate RP d_gdd d_dday29C d_prec_apr_sep d_prec2) cttop("LD: Corn") addtext(State FE, Yes) nonotes auto(2) label tex(frag) replace
reghdfe  d_lcr d_subrate d_avg_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2, absorb(statecode) cluster(statecode)
outreg2 using "$results/preliminary_rf_ld_0.tex",   nocon keep(d_subrate RP d_avg_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2) cttop("LD: Corn") addtext(State FE, Yes) nonotes auto(2) label tex(frag) append
restore

preserve
keep if cropcode==81
egen fips_cov_rp=group(fips covlevel RP)
xtset fips_cov_rp year
foreach x in lcr subrate avg_subrate gdd dday29C prec_apr_sep prec2{
tssmooth ma avg5_`x'=`x', window(4 1 0)
}
keep if year==2017 | year==1993
gen new_t=1
replace new_t=2 if year==1993

xtset fips_cov_rp new_t
foreach x in lcr subrate avg_subrate gdd dday29C prec_apr_sep prec2{
gen d_`x'=d.avg5_`x'
}

*variable label
label var d_lcr "$\Delta$ Loss-cost ratio"
label var d_subrate "$\Delta$ Subsidy Rate$ {_j}$"
label var d_avg_subrate "$\Delta$ Avg. Subsidy Rate$ {_j'}$"

label var d_gdd "$\Delta$ GDD"
label var d_dday29C "$\Delta$ HDD"
label var d_prec_apr_sep "$\Delta$ Precipitation"
label var d_prec2 "$\Delta$ Precipitation$^2$"

keep if new_t==2
reghdfe  d_lcr d_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2, absorb(statecode) cluster(statecode)
outreg2 using "$results/preliminary_rf_ld_0.tex",   nocon keep(d_subrate RP d_gdd d_dday29C d_prec_apr_sep d_prec2) cttop("LD: Soybeans") addtext(State FE, Yes) nonotes auto(2) label tex(frag) append
reghdfe  d_lcr d_subrate d_avg_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2, absorb(statecode) cluster(statecode)
outreg2 using "$results/preliminary_rf_ld_0.tex",   nocon keep(d_subrate RP d_avg_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2) cttop("LD: Soybeans") addtext(State FE, Yes) nonotes auto(2) label tex(frag) append
restore

*Long Difference: lcr ARPA (table 5)
preserve
keep if cropcode==41
egen fips_cov_rp=group(fips covlevel RP)
xtset fips_cov_rp year
foreach x in lcr subrate avg_subrate gdd dday29C prec_apr_sep prec2{
tssmooth ma avg5_`x'=`x', window(4 1 0)
}
keep if year==2017 | year==1999
gen new_t=1
replace new_t=2 if year==2017

xtset fips_cov_rp new_t
foreach x in lcr subrate gdd avg_subrate dday29C prec_apr_sep prec2{
gen d_`x'=d.avg5_`x'
}

*variable label
label var d_lcr "$\Delta$ Loss-cost ratio"
label var d_subrate "$\Delta$ Subsidy Rate$ {_j}$"
label var d_avg_subrate "$\Delta$ Avg. Subsidy Rate$ {_j'}$"

label var d_gdd "$\Delta$ GDD"
label var d_dday29C "$\Delta$ HDD"
label var d_prec_apr_sep "$\Delta$ Precipitation"
label var d_prec2 "$\Delta$ Precipitation$^2$"

keep if new_t==2
reghdfe  d_lcr d_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2, absorb(statecode) cluster(statecode)
outreg2 using "$results/preliminary_rf_ld.tex",   nocon keep(d_subrate RP d_gdd d_dday29C d_prec_apr_sep d_prec2) cttop("LD: Corn") addtext(State FE, Yes) nonotes auto(2) label tex(frag) replace
reghdfe  d_lcr d_subrate d_avg_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2, absorb(statecode) cluster(statecode)
outreg2 using "$results/preliminary_rf_ld.tex",   nocon keep(d_subrate RP d_avg_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2) cttop("LD: Corn") addtext(State FE, Yes) nonotes auto(2) label tex(frag) append
restore

preserve
keep if cropcode==81
egen fips_cov_rp=group(fips covlevel RP)
xtset fips_cov_rp year
foreach x in lcr subrate avg_subrate gdd dday29C prec_apr_sep prec2{
tssmooth ma avg5_`x'=`x', window(4 1 0)
}
keep if year==2017 | year==1999
gen new_t=1
replace new_t=2 if year==2017

xtset fips_cov_rp new_t
foreach x in lcr subrate gdd avg_subrate dday29C prec_apr_sep prec2{
gen d_`x'=d.avg5_`x'
}

*variable label
label var d_lcr "$\Delta$ Loss-cost ratio"
label var d_subrate "$\Delta$ Subsidy Rate$ {_j}$"
label var d_avg_subrate "$\Delta$ Avg. Subsidy Rate$ {_j'}$"

label var d_gdd "$\Delta$ GDD"
label var d_dday29C "$\Delta$ HDD"
label var d_prec_apr_sep "$\Delta$ Precipitation"
label var d_prec2 "$\Delta$ Precipitation$^2$"

keep if new_t==2
reghdfe  d_lcr d_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2, absorb(statecode) cluster(statecode)
outreg2 using "$results/preliminary_rf_ld.tex",   nocon keep(d_subrate RP d_gdd d_dday29C d_prec_apr_sep d_prec2) cttop("LD: Soybeans") addtext(State FE, Yes) nonotes auto(2) label tex(frag) append
reghdfe  d_lcr d_subrate d_avg_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2, absorb(fips) cluster(statecode)
outreg2 using "$results/preliminary_rf_ld.tex",   nocon keep(d_subrate RP d_avg_subrate d_gdd d_dday29C d_prec_apr_sep d_prec2) cttop("LD: Soybeans") addtext(State FE, Yes) nonotes auto(2) label tex(frag) append
restore


***APPENDIX***
*RP liability share over time
preserve
collapse (sum) liability, by(RP year)
reshape wide liability, i(year) j(RP)
gen tot_l=liability0+liability1
gen share=liability1/tot_l
twoway line share year, sort xlabel(1989(2)2017, angle(ninety)) ///
ytitle("RP liability Share") xtitle("Year") graphregion(color(white))
gr export "$results/RPShare.eps", replace
restore


*table 3 col 4: Exclude 1998 and 1999
preserve
keep if cropcode==41
drop if year==1998 | year==1999
ivreghdfe lcr avg_farm_prelia i.covlevel gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if RP==0, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/appendix_exclude9899.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Corn") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) replace
restore

preserve
keep if cropcode==81
drop if year==1998 | year==1999
ivreghdfe lcr avg_farm_prelia i.covlevel gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if RP==0, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/appendix_exclude9899.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Soybeans") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
restore


*different fixed effects for table 3 col 4
preserve
keep if cropcode==41
ivreghdfe lcr avg_farm_prelia RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/alt_fe.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Corn") addtext(First stage F, `fstat', Coverage FE, No, Year FE, No, County FE, No) nonotes auto(2) label tex(frag) replace
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/alt_fe.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Corn") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, No, County FE, No) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), absorb(year)  cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/alt_fe.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Corn") addtext(First stage F, `fstat', Coverage FE, No, Year FE, Yes, County FE, No) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), absorb(year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/alt_fe.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Corn") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, No) nonotes auto(2) label tex(frag) append
restore

preserve
keep if cropcode==81
ivreghdfe lcr avg_farm_prelia RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/alt_fe.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Soybeans") addtext(First stage F, `fstat', Coverage FE, No, Year FE, No, County FE, No) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/alt_fe.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Soybeans") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, No, County FE, No) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), absorb(year)  cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/alt_fe.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Soybeans") addtext(First stage F, `fstat', Coverage FE, No, Year FE, Yes, County FE, No) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate), absorb(year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/alt_fe.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Soybeans") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, No) nonotes auto(2) label tex(frag) append
restore

*Subset analyses by YP and RP for table 3 col 4
preserve
keep if cropcode==41
ivreghdfe lcr avg_farm_prelia i.covlevel gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if RP==0, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/appendix_yp_rp.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("YP only: Corn") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) replace
ivreghdfe lcr avg_farm_prelia i.covlevel gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if RP==1, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/appendix_yp_rp.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("RP only: Corn") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
restore
preserve
keep if cropcode==81
ivreghdfe lcr avg_farm_prelia i.covlevel gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if RP==0, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/appendix_yp_rp.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("YP only: Soybeans") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if RP==1, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/appendix_yp_rp.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("RP only: Soybeans") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
restore

*Subset analyses by region for table 3 col 4
*Regional codes
gen region_code=0
*Northeast
replace region_code=1 if statecode==9 |statecode==10 | statecode==23 | statecode==24 | statecode==25 | statecode==33 | statecode==34 | statecode==36 | statecode==42 | statecode==44 | statecode==50
*Southeast
replace region_code=2 if statecode==21 |statecode==37 | statecode==47 | statecode==51 | statecode==54 | statecode==1 | statecode==13 | statecode==12 | statecode==45 
*North Central
replace region_code=3 if statecode==26 |statecode==18 | statecode==39 | statecode==19 | statecode==27 | statecode==55 | statecode==29 | statecode==17 
*Delta
replace region_code=4 if statecode==5 |statecode==22 | statecode==28 
*Central and Northern Plain
replace region_code=5 if statecode==31 |statecode==20 | statecode==38 | statecode==46 | statecode==8 | statecode==30 | statecode==56 
*Southern Plains
replace region_code=6 if statecode==35 | statecode==40 | statecode==48
*Far West
replace region_code=7 if statecode==2 | statecode==4 | statecode==6 | statecode==15 | statecode==16 | statecode==32 | statecode==41 | statecode==49 | statecode==53
preserve
keep if cropcode==41
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==1, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_corn_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Northeast") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) replace
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==2, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_corn_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Southeast") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==3, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_corn_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("North Central") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==4, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_corn_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Delta") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==5, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_corn_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Central and Northern Plains") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==6, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_corn_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Southern Plains") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
restore
preserve
keep if cropcode==81
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==1, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_soy_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Northeast") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) replace
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==2, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_soy_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Southeast") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==3, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_soy_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("North Central") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==4, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_soy_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Delta") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==5, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_soy_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Central and Northern Plains") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
ivreghdfe lcr avg_farm_prelia i.covlevel RP gdd dday29C prec_apr_sep prec2 (farm_prelia = subrate) if region_code==6, absorb(fips year) cluster(statecode year) endog(farm_prelia) first
dis e(widstat)
local fstat: di %6.2f e(widstat)
outreg2 using "$results/preliminary_soy_regional.tex",   nocon keep(farm_prelia RP avg_farm_prelia gdd dday29C prec_apr_sep prec2) cttop("Southern Plains") addtext(First stage F, `fstat', Coverage FE, Yes, Year FE, Yes, County FE, Yes) nonotes auto(2) label tex(frag) append
restore




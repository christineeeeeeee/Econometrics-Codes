import excel "/Users/fengwang/Documents/Academic/Master Thesis/Data revise versions/Master Thesis Data copy Final copy.xlsx", sheet("Data") firstrow

* 1. define labels
label variable UnemploymentRate "% of total labor force ages 15-24) (modeled ILO estimate)"
label variable GDP "current US$"
label variable Inflation "consumer prices (annual %)"
label variable Population "total"
label variable ForeignDirectInvestment "net inflows (BoP, current US$)"
label variable ExchangeRate "LCU per US$, period average"
label variable RealInterestRate "LCU per US$, period average"
label variable GovernmentExpenditureonEducat "total (% of GDP)"


* 2. Standardize the four variables
egen FDI_z = std(ForeignDirectInvestment)
egen ExchangeRate_z = std(ExchangeRate)
egen RealInterestRate_z = std(RealInterestRate)
egen GovEduExp_z = std(GovernmentExpenditureonEducat)

* 3. Generate two growth rate difference variables
gen lnGDP = ln(GDP)
encode CountryName, gen(CountryName_id)
encode Region, gen(Region_id)
xtset CountryName_id year
gen GDPGrowth= (lnGDP - L.lnGDP) * 100
gen lnPopulation = ln(Population)
gen PopGrowth= (lnPopulation - L.lnPopulation) * 100

* 4. Missing data interpolation

* Using MI multiple imputation

* a. First tell Stata to do MI → declare the data structure: xtset+ declare the data is for MI multiple imputation

 mi set mlong

* b. Register missing and other variables
mi register imputed RealInterestRate GovernmentExpenditureonEducat 
mi register regular UnemploymentRate GDPGrowth Inflation FDI_z ExchangeRate_z PopGrowth 

* c. Perform multiple chain interpolation

mi impute chained (regress) RealInterestRate GovernmentExpenditureonEducat = UnemploymentRate GDPGrowth Inflation FDI_z ExchangeRate_z PopGrowthi.CountryName_id i.year, add(20)

* Conditional models:
*    RealInterest~e: regress RealInterestRate GovernmentExpenditureonEducat UnemploymentRate GDPGrowth Inflation FDI_z
*                     ExchangeRate_z PopGrowth PopGrowth i.CountryName_id i.year
 *   GovernmentEx~t: regress GovernmentExpenditureonEducat RealInterestRate UnemploymentRate GDPGrowth Inflation FDI_z
  *                   ExchangeRate_z PopGrowth PopGrowth i.CountryName_id i.year


* Performing chained iterations ...
*GovernmentExpenditureonEducat: missing imputed values produced
*   This may occur when imputation variables are used as independent variables or when independent variables contain
*    missing values. You can specify option force if you wish to proceed anyway.


 * ⚠️This code means that there are missing values ​​in the independent variable, and the difference variable needs to be replaced with the original variable.
 mi impute chained (regress) RealInterestRate GovernmentExpenditureonEducat = UnemploymentRate GDP Inflation FDI_z ExchangeRate_z Population i.CountryName_id i.year, add(20)

 * d.After interpolation, standardize the two variables.
 egen RealInterestRate_MI_z = std(RealInterestRate)
 egen GovEduExp_MI_z = std(GovernmentExpenditureonEducat)
 
 
* 5. Add dummy variables and interaction terms
gen CrisisDummy = inrange(year, 2008, 2009)
gen CovidDummy = inrange(year, 2020, 2022)
gen CrisisGDPInter = GDPGrowth * CrisisDummy 
gen CovidGDPinter = GDPGrowth * CovidDummy 


*6. Start modeling analysis

* a. Descriptive statistical analysis
sum UnemploymentRate GDP Inflation Population ForeignDirectInvestment ExchangeRate RealInterestRate GovernmentExpenditureonEducat
estpost summarize UnemploymentRate GDP Inflation Population ForeignDirectInvestment ExchangeRate RealInterestRate GovernmentExpenditureonEducat

* b.Correlation analysis
pwcorr UnemploymentRate GDP Inflation Population ForeignDirectInvestment ExchangeRate RealInterestRate GovernmentExpenditureonEducat
pwcorr UnemploymentRate GDP Inflation Population ForeignDirectInvestment ExchangeRate RealInterestRate GovernmentExpenditureonEducat, sig star(0.05)
ssc install asdoc, replace
asdoc pwcorr UnemploymentRate GDP Inflation Population ForeignDirectInvestment ExchangeRate RealInterestRate GovernmentExpenditureonEducat, starlevels(* 0.10 ** 0.05 *** 0.01) save(Table2_Correlation.doc) replace

* c.OLS regression
mi estimate: reg UnemploymentRate GDPGrowth Inflation PopGrowth FDI_z ExchangeRate_z RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPinter
asdoc mi estimate: reg UnemploymentRate GDPGrowth Inflation PopGrowth FDI_z ExchangeRate_z RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPinter, save(Table3_OLS.doc) replace
 
 * d. Fixed effects test
 mi estimate: xtreg UnemploymentRate GDPGrowth Inflation PopGrowth FDI_z ExchangeRate_z RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPinter i.CountryName_id i.year, fe robust
asdoc mi estimate: xtreg UnemploymentRate GDPGrowth Inflation PopGrowth FDI_z ExchangeRate_z RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPinter i.CountryName_id i.year, fe robust save(Table4_FE.doc) replace

mi estimate: xtreg UnemploymentRate GDPGrowth Inflation PopGrowth FDI_z ExchangeRate_z RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPinter i.CountryName_id i.year if Region_id==2, fe robust
asdoc mi estimate: xtreg UnemploymentRate GDPGrowth Inflation PopGrowth FDI_z ExchangeRate_z RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPinter i.CountryName_id i.year if Region_id==2, fe robust save(Table5_FE_Europe.doc) replace

mi estimate: xtreg UnemploymentRate GDPGrowth Inflation PopGrowth FDI_z ExchangeRate_z RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPinter i.CountryName_id i.year if Region_id==1, fe robust
asdoc mi estimate: xtreg UnemploymentRate GDPGrowth Inflation PopGrowth FDI_z ExchangeRate_z RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPinter i.CountryName_id i.year if Region_id==1, fe robust save(Table6_FE_Asia.doc) replace

 * e. GMM

* Note: The interaction term is GDP × dummy. It does not need to be written separately into gmm(). Put it into GDPGrowth_ln. The interaction term is automatically processed.

* When MI multiple imputation + xtabond2 are used together, there is a common "omitted terms vary" problem. MI has m=20 imputation data sets, and each set has some factor variables (such as i.Region_id, i.year). In some sets, some dummy categories (year, region) are omitted, which causes xtabond2 to omit variables inconsistently between different MI sets, which violates the MI estimate requirements.


* Method 1 The problem may be that there are many duplicate rows
* sort CountryName_id year
* mi xeq: xtabond2 UnemploymentRate L(1/2).UnemploymentRate GDPGrowth CrisisGDPInter CovidGDPinter FDI_z Inflation ExchangeRate_z RealInterestRate_MI_z GovEduExp_MI_z PopGrowth CrisisDummy CovidDummy i.CountryName_id i.year, gmm(L(1/2).UnemploymentRate GDPGrowth FDI_z ExchangeRate_z RealInterestRate_MI_z) iv(Inflation PopGrowth GovEduExp_MI_z CrisisDummy CovidDummy i.CountryName_id i.year) twostep robust small
*duplicates report CountryName_id year
* skip this method

* Method 2
mi rename GovernmentExpenditureonEducat GovEduExp
mi rename RealInterestRate_MI_z RI_M_z

 * 1. Save mi data (named clearly)
* save thesis_MI_renamed, replace

* 2. Convert to passive data
* mi convert wide //⚠️There is a problem with this step

* save thesis_MI_passive, replace

* 3. Read passive dataset (for xtabond2)
* use thesis_MI_passive, clear

* 4.Generate fixed dummy variables (if necessary)
* gen flag = 1
* label variable flag "Passive dummy flag"
* No need for mi passive: gen, normal gen will do

* 5. Confirm panel data format
* xtset CountryName_id year

* 6. xtabond2 Dynamic panel regression
* xtabond2 UnemploymentRate L(1/2).UnemploymentRate GDPGrowth Inflation PopGrowth FDI_z ExchangeRate_z RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPInter i.Region_id i.year, gmm(L(1/2).UnemploymentRate GDPGrowth FDI_z ExchangeRate_z, collapse) iv(Inflation PopGrowth RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPInter) twostep robust small

* 7. Save xtabond2 results (with esttab or asdoc)


***** 💡 There is a problem when running to step 5. Here is the solution1：
* First clear the current data
* clear

* Reread passive data
* use thesis_MI_passive, clear

* Then set xtset
*xtset CountryName_id year

* Then run the xtabond2 main model
* xtabond2 UnemploymentRate L(1/2).UnemploymentRate GDPGrowth Inflation PopGrowth FDI_z ExchangeRate_z RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPInter i.Region_id i.year, gmm(L(1/2).UnemploymentRate GDPGrowth FDI_z ExchangeRate_z, collapse) iv(Inflation PopGrowth RealInterestRate_MI_z GovEduExp_MI_z CrisisDummy CovidDummy CrisisGDPInter CovidGDPInter) twostep robust small

	
***** 💡 The previous solution1 still has problems. Here is the solution2:
* Step 1: extract m=0 to export passive data
mi extract 0

* Step 2: Save a new passive data
save thesis_passive_ready.dta, replace

* Step 3: Clear and reload
clear
use thesis_passive_ready.dta, clear

* Step 4, xtset is OK
xtset CountryName_id year

* Step 5: GMM main model
xtabond2 UnemploymentRate L(1/2).UnemploymentRate GDPGrowth Inflation PopGrowth FDI_z ExchangeRate_z RI_M_z GovEduExp_MI_z  CrisisDummy CovidDummy CrisisGDPInter CovidGDPinter i.Region_id i.year, gmm(L(1/2).UnemploymentRate GDPGrowth FDI_z ExchangeRate_z, collapse) iv(Inflation PopGrowth RI_M_z GovEduExp_MI_z CrisisDummy CovidDummy CountryName_id year) twostep robust small
	
**The second solution succeeded,
*mi extract 0 → Extract m=0 data
*save thesis_passive_ready.dta → Manually save a copy of the passive data for xtabond2
*Next time just use this thesis_passive_ready to run GMM



* Scatter plot
* 1.GDPGrowth vs UnemploymentRate
twoway (scatter UnemploymentRate GDPGrowth, msize(medlarge) mcolor(blue))(lfit UnemploymentRate GDPGrowth, lcolor(red) lwidth(medthick)), title("Scatter Plot: GDP Growth vs Youth Unemployment Rate") xtitle("GDP Growth (%)") ytitle("Youth Unemployment Rate (%)")legend(off) note("Source: World Bank, author's calculation") name(GDPGrowth_scatter, replace)
graph export "GDPGrowth_UnemploymentRate.png", replace

* Europe
twoway (scatter UnemploymentRate GDPGrowth if Region_id==2, msize(medlarge) mcolor(blue))(lfit UnemploymentRate GDPGrowth if Region_id==2, lcolor(red) lwidth(medthick)), title("Europe: GDP Growth vs Youth Unemployment Rate") xtitle("GDP Growth (%)") ytitle("Youth Unemployment Rate (%)") legend(off) note("Source: World Bank, author's calculation") name(GDP_scatter_Europe, replace)
graph export "Europe_GDPGrowth_UnemploymentRate.png", replace
* Asia
twoway (scatter UnemploymentRate GDPGrowth if Region_id==1, msize(medlarge) mcolor(blue))(lfit UnemploymentRate GDPGrowth if Region_id==1, lcolor(red) lwidth(medthick)) , title("Asia: GDP Growth vs Youth Unemployment Rate") xtitle("GDP Growth (%)") ytitle("Youth Unemployment Rate (%)") legend(off) note("Source: World Bank, author's calculation") name(GDP_scatter_Asia, replace)
graph export "Asia_GDPGrowth_UnemploymentRate.png", replace

* 2.Inflation vs UnemploymentRate
twoway (scatter UnemploymentRate Inflation, msize(medlarge) mcolor(green)) (lfit UnemploymentRate Inflation, lcolor(red) lwidth(medthick)) , title("Scatter Plot: Inflation vs Youth Unemployment Rate") xtitle("Inflation Rate (%)") ytitle("Youth Unemployment Rate (%)") legend(off) note("Source: World Bank, author's calculation") name(Inflation_scatter, replace)
graph export "Inflation_UnemploymentRate.png", replace
* Europe
twoway (scatter UnemploymentRate Inflation if Region_id==2, msize(medlarge) mcolor(green)) (lfit UnemploymentRate Inflation if Region_id==2, lcolor(red) lwidth(medthick)), title("Europe: Inflation vs Youth Unemployment Rate") xtitle("Inflation Rate (%)") ytitle("Youth Unemployment Rate (%)")  legend(off)  note("Source: World Bank ")  name(Inflation_scatter_Europe, replace)
graph export "Europe_Inflation_UnemploymentRate.png", replace
* Asia
twoway (scatter UnemploymentRate Inflation if Region_id==1, msize(medlarge) mcolor(green)) (lfit UnemploymentRate Inflation if Region_id==1, lcolor(red) lwidth(medthick)) , title("Asia: Inflation vs Youth Unemployment Rate") xtitle("Inflation Rate (%)") ytitle("Youth Unemployment Rate (%)") legend(off) note("Source: World Bank, author's calculation") name(Inflation_scatter_Asia, replace)
graph export "Asia_Inflation_UnemploymentRate.png", replace






 











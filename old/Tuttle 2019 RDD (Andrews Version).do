/*	PROGRAM: Replication of Snapping Back: Food Stamp Bans and Criminal Recidivism - Tuttle (2019)
	DATE: Nov. 17, 2021
	
	BACKGROUND
	In 1996, Flordia adapted a national welfare reform to impose a lifetime ban on food stamps
	for drug trafficers who committed their crime on or after August 23, 1996. This sharp
	temporal cutoff creates a prime opportunity to examine the effects of welfare bans on a 
	host of possible outcomes. Tuttle (2019) finds that individuals subject to the ban are 
	more likely to commit another crime than those who are not subject to the ban and that
	these effects are largely driven by financially-motivated crimes.
	
	We'll try to recreate their results here in the following steps:
	(1) Load data, do some initial exploration, and prepare it for analysis
	(2) Test for gaming around the cutoff, heaping, and statistical power
	(3) Estimation via OLS and interpretation
	(4) Common robustness checks
	(5) Fuzzy RDDs
	
	*/

// ---------------------------------------------------- (0) Installing packages and graph schemes

local install = 0 // Change this to 1 to download packages/schemes for this project

if `install' == 1 {
	ssc install rdrobust, replace // Includes many useful commands for RDD
	ssc install estout, replace // For storing and outputting results
	net install rddensity, from("https://raw.githubusercontent.com/rdpackages/rddensity/master/stata") replace
	net install lpdensity, from("https://raw.githubusercontent.com/nppackages/lpdensity/master/stata") replace
	net install rdpower, from("https://raw.githubusercontent.com/rdpackages/rdpower/master/stata") replace


		/* Note on Schemes: The default graphing options in STATA are ugly.
			A user-developed package called schemepack includes many more
			visually appealing schemes. The code below installs that package
			and sets the scheme to one I like, but you should check out the
			github page and pick one for yourself
			
			https://github.com/asjadnaqvi/Stata-schemes */
			
	ssc install schemepack, replace // Appealing graph schemes (optional)
	set scheme white_tableau // Add ", perm" after this command to lock change in
}

	
	
// ----------------------------------------------------- (1) Loading data and initial exploration
use "https://github.com/andrewmcamp/MSER/raw/main/data/florida_offenders.dta", clear

* First thing to do is get a sense of our variables
summarize _all, sep(0)

* Next, we might make a simple graph to motivate our analysis
gen reldate = date - date("23Aug1996", "DMY") // Recentering running variable at cutoff
label variable reldate "Relative Date"
gen bin = floor(reldate/60) // Creating bins of 60 days before/after cutoff
replace bin = bin + 1 if bin >= 0 // Personal preference not to have a 0 bin
bysort bin: egen avg_finrecidany = mean(finrecidany) // Average recid. rate by bin
twoway scatter avg_finrecidany bin, xline(0) // Make a simple scatterplot



// ---------------------------------------------------- (2) Testing for gaming, heaping and power
* We need to check for a few issues that might undermine the continuity assumption.
* Firstly, do we see evidence that people "gamed" the cutoff to try to get on one side?
bysort bin: egen count = count(bin) // Creating a count of obs. per  bin
twoway bar count bin, xline(0) // What does the graph tell us?

* The next issue is heaping -- patterns of data that arrise due to how data is recorded
* We might see evidence of counts of bins tended to end in the same numbers
gen heap_test = mod(count,10)
tab heap_test

* Testing for gaming of the cutoff
rddensity reldate, c(0) h(60) plot // McCrary Test bandwidth of 60 and cutoff of 0

* Testing for statistical power (benchmark of 0.8)
rdpow finrecidany reldate, c(0) h(240) // What effect size are we powered to detect?



// --------------------------------------------------- (3a) Estimation via OLS and interpretation
gen treat = reldate > 0 // Define a treatment variable
label variable treat "Treatment"

reg finrecidany treat reldate if abs(reldate) <= 60, robust
	eststo simple

* Add in interaction term to allow different slopes on either side of the cutoff
reg finrecidany treat reldate treat#c.reldate if abs(reldate) <= 60, robust
	eststo bw_60
reg finrecidany treat reldate treat#c.reldate if abs(reldate) <= 120, robust
	eststo bw_120
reg finrecidany treat reldate treat#c.reldate if abs(reldate) <= 180, robust
	eststo bw_180
reg finrecidany treat reldate treat#c.reldate if abs(reldate) <= 240, robust
	eststo bw_240

* Compare these naive models
esttab simple bw*, p ar2 scalars(rmse) noomit label ///
	mtitles("Simple" "X BW=60" "X BW=120" "X BW=180" "X BW=240")

	
	
// ---------------------------------------------------- (3b) Estimation via OLS with Kernel Weighting
* Kernel weights work by giving the most weight to values close to the cutoff. 
* Here, we'll estimate the same as above using kernel weights and a forvalues loop

forvalues h = 60(60)240 {
	* generating triangular kernel weights for bandwidth h
	gen weight = 1-abs(reldate)/`h'
	replace weight = 0 if weight < 0
	
	* running the regression with the weights included.
	* note that we don't need to use an if statement anymore!
	reg finrecidany treat reldate treat#c.reldate [aw=weight], robust
		eststo kernel_`h'

	* dropping the weight variable for the next iteration
	drop weight
}

* Compare these kernel-weighted models
esttab kernel_*, p ar2 scalars(rmse) noomit label ///
	mtitles("Kernel 60" "Kernel 120" "Kernel 180" "Kernel 240")


	
// ---------------------------------------------------------------------------- (4) Robustness Checks
* Another issue that we might have with RDDs is that the effect we're finding is spurious.
* A common robustness check is to try to find an effect using an outcome that should not have changed
* Here, I'll estimate our bandwidth models using a for loop and have age as the outcome variable

forvalues h = 60(60)240 {
	* No need to create weights since we're just using the bandwidth restriction
	reg age treat reldate treat#c.reldate if abs(reldate) <= `h', robust
	eststo age_`h'
}

* Compare these age falsification models
esttab age_*, p ar2 scalars(rmse) noomit label ///
	mtitles("Age 60" "Age 120" "Age 180" "Age 240")

* Another robustness check is to arbitrarily change the cutoff and see if we can find an effect
* Again, I'll use a forvalues loop here to save some space and effort. This time, the values 
* that change will represent the cutoff, not the bandwidth.
forvalues c = 0(500)2000 {
	* Need to redefine our treatment variable since the cutoff has changed
	replace treat = 0
	replace treat = 1 if reldate > `c'

	reg finrecidany treat reldate treat#c.reldate if abs(reldate-`c') <= 120, robust
		eststo placebo_`c'
}
* Compare these placebo test models
esttab placebo_*, p ar2 scalars(rmse) noomit label ///
	mtitles("C=0" "C=500" "C=1000" "C=1500" "C=2000")

* returning treatment variable to original definition
drop treat
gen treat = (reldate > 0)



// ----------------------------------------------------------------------------------- (5) Fuzzy RDDs
/* 	In this scenario, we have a very sharp RD, but that might not always be the case.
	Imagine that the policy was written in such a way that people could somehow avoid the ban if they
	fulfilled certain requirements. First we'll do some data manipulation to simulate this. */

* The first thing to do is randomly create a variable taking values from 0 -> 1
gen randVar = runiform(0,1)
gen fuzzy_treat = treat
replace fuzzy_treat = 0 if randVar >= 0.8 & treat == 1 // Simulating the opt-out
replace fuzzy_treat = 1 if randVar <= 0.1 & treat == 0 // Simulating the opt-in
drop randVar

* Okay! Now that our data is setup, let's make a graph showing the noncompliance
bysort bin: egen pr_treat = mean(fuzzy_treat)
twoway scatter pr_treat bin if reldate <= 1000, xline(0) ///
	ytitle("Pr(SNAP Ban)") yscale(r(0(0.2)1))  

* Cool. So the Fuzzy RD is really just an IV where our instrument is being beyond the cutoff.
ivregress 2sls finrecidany reldate ///
	(fuzzy_treat fuzzy_treat#c.reldate = treat treat#c.reldate) ///
	if abs(reldate) <= 120, robust first perfect

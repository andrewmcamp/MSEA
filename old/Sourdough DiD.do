/*	
	PROGRAM: 	Simple DiD Coding Example using Google Search Trends
	DATE:		November 28th, 2021
	
	BACKGROUND:	During the first few months of COVID-19, it seemed like 
	making sourdough bread became quite popular. This could be for a lot of
	different reasons, including that sourdough does not require yeast which
	was difficult to find at the time. In this example, borrowed from Nick
	Huntington-Klein's The Effect book, we'll use differences-in-differences 
	to see if there was a statistically significant increase in searches for
	the term "sourdough" as compared to some other foods.
*/
	

// ---- (1) Loading data and inspecting
	use "https://github.com/andrewmcamp/MSEA/raw/main/data/sourdough.dta", clear // Import from github
	sum _all, sep(0) // Look at variables
	tab keyword

	
	
// ---- (2) Motivating graphs and initial data exploration
	* First see what the start of the pandemic date value is stored as in Stata
	di date("17March2020", "DMY") // We'll use this in specifying our graph

	#delimit ; // Changing the delimiter to make the graph a little easier to specify
	twoway (line hits date if keyword == 1) || (line hits date if keyword == 2) ||
		(line hits date if keyword == 3)  || (line hits date if keyword == 4, xline(21989) 
		legend(lab(1 "1 Cereal") lab(2 "2 Sandwich") lab(3 "3 Soup") 
		lab(4 "4 Sourdough") pos(6) row(1)));
	#delimit cr // Changing the delimiter back to a new line

	* This graph loks quite "choppy." Let's go ahead and drop this from daily to weekly.
	gen week = week(date)
	bysort week keyword: egen week_hits = total(hits)
	keep if dow(date) == 1
	
	* Note that March 16th is in week 11 and March 17th is in week 12...
	#delimit ; 
	twoway (line hits week if keyword == 1) || (line hits week if keyword == 2) ||
		(line hits week if keyword == 3)  || (line hits week if keyword == 4, xline(12) 
		legend(lab(1 "1 Cereal") lab(2 "2 Sandwich") lab(3 "3 Soup") 
		lab(4 "4 Sourdough") pos(6) row(1)));
	#delimit cr 


	
// ----- (3) DiD specifications
	* Before anything else, we need an indicator for "treated"
	gen treated = (keyword == 4)
	gen post = (week >= 12)

	* Let's take a look at the simple, static regression
	reg week_hits treated##post, vce(cluster keyword)

	* Now what if we allow for a bit more variation by week of year? 
	reg week_hits treated##b11.week, vce(cluster keyword)
	
	
	
// ----- (4) Event Study
	
	/*	While there are some packages that can help us make nice event study
		graphs, we can make them ourselves fairly easily. Manually making these
		graphs also allows us to have better control over them. A package that's
		helpful, but has a steep learning curve, is event_plot which can be 
		installed by typing ssc install event_plot into the command line.
	*/
	
	* We're going to store the coefficients and standard errors from a regression
	gen coef = . // Empty coefficient variable
	gen se = . // Empty standard error variable

	* Now let's repeat our regression, 
	* Add the coeflegend option to see how results are labeled by Stata.
	reg week_hits treated##b11.week, vce(cluster keyword) coeflegend

	* Let's use a for loop to populate those empty coef and se variables
	levelsof week, l(times)
	foreach t in `times' {
		replace coef = _b[1.treated#`t'.week] if week == `t' // coefficients
		replace se = _se[1.treated#`t'.week] if week == `t' // standard errors
	}
	
	* A little bit more cleaning before graphing
	keep week coef se // Keep just the variables we need
	duplicates drop week, force // And one obs per week
	sort week // Sorting by week

	* Make some 95% confidence intervals
	gen ci_top = coef+(1.96*se)
	gen ci_bottom = coef-(1.96*se)

	* Time for out nice graph!
	#delimit ;
	twoway (rcap ci_top ci_bottom week, lcolor(red%70) xline(12) yline(0)) ||
		(scatter coef week, connect(line) lcolor(black) mcolor(black)
		ytitle("Searches - Sourdough vs. Comparison") xtitle("Week of 2020")
		legend(lab(1 "95% Confidence Interval") lab(2 "Point Estimate") pos(6) row(1)));
	#delimit cr

	* What can our event study plot tell us that the static specification cannot?



// ----- (5) Placebo Test Study

	/* 	One way we can check for pre-trends/validity is to change the date of treatment 
		to some time before true treatment and see if we can still detect an effect.
		
	*/
	
	* First, I'll load our data again and clean it a bit. 
	use "https://github.com/andrewmcamp/MSEA/raw/main/data/sourdough.dta", clear // Import from github
	gen week = week(date)
	bysort week keyword: egen week_hits = total(hits)
	keep if dow(date) == 1
	
	* Next, I'm going to define the treatment and post variables, but change time of treatment.
	gen treated = (keyword == 4)
	gen post = (week >= 8)
	
	* Now, we can perform our regression and see if there was a significant effect in week 8
	reg week_hits treated##post, vce(cluster keyword)

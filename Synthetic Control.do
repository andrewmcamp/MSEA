local install = 0 // Change to one to install packages
if `install' == 1 {
	ssc install synth // Does the synthetic control
	ssc install mat2txt // Exports a Stata matrix to a text file
}

// ----------------------------------------------------------------------------------------------
	*Import data from Abadie (2003)
	import delimited "https://raw.githubusercontent.com/andrewmcamp/MSEA/main/data/basque.csv", clear

	local covariates = "sec_agriculture sec_energy sec_industry sec_construction " + ///
		"sec_services_venta sec_services_nonventa school_illit school_prim " + ///
		"school_med school_high school_post_high invest"

	* recode NAs to missing values
	foreach var in `covariates' {
			replace `var' = "" if `var' == "NA"
			destring `var', replace
		}

	* Labeling variables to make things nice
	label variable year "Year"
	label variable gdpcap "GDP per Capita"
		
	* Need to declare as time-series data for the synth command
	tsset regionno year, yearly

	* Save that partially cleaned data file
	save "synth_raw.dta", replace 

		
// ---------------------------------------------------------------------------------------------------- (2) Estimation
/*	The first step is to call the synth command. Like a regression, the dependent variable goes first. 
	Following this are two 'groups' of covariates. The first are our normal covariates defined in the
	local above. The second is our outcome variable. The command will then automatically construct a 
	matched sampled based on pre-treatment covariates AND outcomes. It will not consider anything past
	the date specified in trperiod(). If you want to restrict covariates to specific dates, you can do
	so using the syntax covariate(2000), covariate(2000,2005) or covariate(2000(1)2005).
	
	The command will also make a new data file (called synth1.dta) here that contains the real and
	synthetic control outcome data. It can make a graph using the "figure" option, but I prefer to
	make my own using the new data file so I can control the appearence a bit better. */

	synth gdpcap `covariates' gdpcap, trunit(17) trperiod(1969) unitnames(regionname) ///
		mspeperiod(1955(1)1969) keep("synth1.dta") replace
	
	* Loading this new data set, cleaning, then making comparisons graphs
	use "synth1.dta", clear
	keep _Y_treated _Y_synthetic _time
	drop if _time == .
	rename _time year
	rename _Y_treated y_observed
	rename _Y_synthetic y_synth
	label variable year "Year"
	label variable y_observed "Observed"
	label variable y_synth "Synthetic"

	* Gap between real and synthetic. Positive values indicate that Y_real > Y_synth.
	gen gap = y_observed - y_synth
	label variable gap "Gap Between Observed and Synthetic Control"
	
	gen per_gap = (y_observed - y_synth)/y_observed
	label variable gap "% Gap Between Observed and Synthetic Control"

	* Making the graphs. Changing the delimiter becasuse maybe that makes the code easier to read?
	#delimit ;
	twoway (line y_observed year) || (line y_synth year,
		lpattern(dash) xline(1969, lpattern(longdash))
		ytitle("GPD per Capita (Thousands)")
		note("NOTE: GDP is in constant 1986 dollars.", span)
		legend(pos(6) col(2)));
	graph export "output/GDP_synthetic_real.pdf", replace;
	#delimit cr

	* And this is the graph of the gap over time. 
	* Should be 0 before treatment and not zero afterwards.
	twoway line gap year, xline(1969, lpattern(longdash)) yline(0, lpattern(longdash))
	graph export "output/gap_synthetic_real.pdf", replace

erase "synth1.dta"	

// ----------------------------------------------------------------------------------------------------- (3) Inference
/*	The values estimated above are nice, but we have no way to determine if they're meaningful 
	or simply the result of random noise or modeling decisions. To test if the difference we 
	find is real, we can build on a very old idea - randomization inference. The idea is that
	we can construct a distribution of gaps by systematically assigning treatment to untreated
	units and then seeing what gaps emerge.
	
	Following Abadie, Diamond, and Hainmueller (2010), we'll quantify the "goodness" of these
	gaps using root mean squared prediction error (RMSPE). If our true-treatment model has 
	significantly higher RMSPE than other models, it indicates that the difference is not
	due to modeling decisions or random noise.
	
	In plain language, the RMSPE is the sum of squared differences between Y_observed and 
	Y_synth for all pre/post-treatment periods, multiplied by 1/(# of pre/post-treatment 
	periods), then raises to the 1/2 power (square rooting it). Since differences for 
	individual periods are squared, periods with especially high differences between 
	Y_synth and Y_observed have a greater effect on the estimate RMSPE. 
	
	This next code estimates RMSPE for all of the true- and false-treatment models and
	creates one file  containing that data. */
	
	* first, we need to create an empty data file that results will be appended into.
	clear all
	gen y_observed = .
	gen y_synth = .
	gen gap = .
	gen year = .
	gen treatedUnit = .
	gen rmspe_pre = .
	gen rmspe_post = .
	gen rmspe_ratio = .
	save "synth_random.dta", replace
	
	
	forvalues i = 1/18 {
		* Loading raw data
		use "synth_raw.dta", clear
		
		local covariates = "sec_agriculture sec_energy sec_industry sec_construction " + ///
		"sec_services_venta sec_services_nonventa school_illit school_prim " + ///
		"school_med school_high school_post_high invest"
				
		* Estimating with unit i as the treated unit
		synth gdpcap `covariates' gdpcap, trunit(`i') trperiod(1969) unitnames(regionname) ///
			mspeperiod(1955(1)1969) keep("synth_`i'.dta") replace;
			
		* Loading newly created data file, cleaning, then appending to the sythn_random file
		use "synth_`i'.dta", clear
		drop _Co_Number _W_Weight
		rename _Y_treated y_observed
		rename _Y_synthetic y_synth
		gen gap = y_observed - y_synth
		rename _time year
		gen treatedUnit = `i'
		sort year
		
		*calculating RMSPE for pre/post and the ratio between them
		egen rmspe_pre = mean(gap*gap) if year <= 1968
		replace rmspe_pre = sqrt(rmspe_pre)
		
		egen rmspe_post = mean(gap*gap) if year > 1968
		replace rmspe_post = sqrt(rmspe_post)
		
		gen temp_ratio = rmspe_post/rmspe_pre[_n-1]
		egen rmspe_ratio = max(temp_ratio)
		drop temp_ratio 
		
		append using "synth_random.dta"
		save "synth_random.dta", replace
		erase "synth_`i'.dta"
	}
	
	* calculating the rank/total p-value, then creating a histogram
	egen rank = rank(rmspe_ratio) if year == 1970
	gen p_val = (19-rank)/18
	
	* Now making some nice visuals to go along with these figures
	#delimit ;
		twoway (line gap year if treatedUnit == 1, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 2, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 3, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 4, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 5, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 6, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 7, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 8, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 9, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 10, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 11, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 12, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 13, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 14, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 15, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 16, lwidth(vthin) lcolor(gray%70))
				(line gap year if treatedUnit == 17, 
					lcolor(blue) xline(1969, lpattern(longdash))
					xtitle("Year") yline(0, lpattern(longdash))
					legend(pos(6) order(1 17) col(2) 
					lab(1 "Placebo") lab(17 "True Treatment"))
					ytitle("Gap Between Observed and Synthetic Control")
					note("NOTE: GDP is in constant 1986 dollars.", span))
				(line gap year if treatedUnit == 18, lwidth(vthin) lcolor(gray%70));
		graph export "output/true_false_gap.pdf", replace;
	#delimit cr
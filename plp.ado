program plp, eclass
version 14.0:

	syntax varlist (fv ts) [if] [in], [TRansf(string) Shock(varlist fv ts) YLags(integer 0) SLags(integer 0) Controls(varlist fv ts) Met(string) Hor(numlist integer) * ]

// */SAVEirf IRFName(string)  NOIsily STats /*
// */nograph 

// tr("cmlt") s(crisis) yl(4) sl(4) m(xtreg) h(1 10) fe
// loc varlist f0lngdp
// loc transf "level"
// loc shock crisis
// loc ylags 4
// loc slags 4
// loc met xtreg
// loc hor 0 10
** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
**  required option
** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

if "`shock'"=="" {
	di as error "Error: option shock() is required."
	exit 198
}

if "`met'" == "" {
    display as error "Error: option met() is required."
    exit 198
}


** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
**  shock and control variables
** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

loc s `shock'
loc ls L(1/`slags').`shock'

loc c `controls'

** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
**  horizon
** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

loc hor : subinstr local hor "," " ", all
loc nh = wordcount("`hor'")

if `nh' > 1 {
    * Case 1: User entered two numbers (e.g., "0 5" or "1 10")
    tokenize "`hor'"
    loc hs `1'   // Extract the first number as the start point
    loc hor `2'  // Extract the second number as the end point

    * Check: No more than 2 arguments allowed
    if `nh' > 2 {
        di as error "Error: Too many arguments in hor(). Please enter 'H' or 'Start End'."
        exit 198
    }

    * Core Constraint: Start horizon must be 0 or 1
    if `hs'!=0 & `hs'!=1 {
        di as error "Error: Start Horizon must be 0 or 1."
        exit 198
    }

    * Check: End horizon must be greater than start horizon
    if `hor' <= `hs' {
        di as error "Error: End horizon must be greater than start horizon."
        exit 198
    }

    loc hran `hs'/`hor'
}
else if `nh' == 0 {
    * Case 2: User did not provide input (Default settings)
    * Default set to 0 to 5
    loc hs = 0
    loc hor = 5
    loc hran `hs'/`hor'
}
else if `nh' == 1 {
    * Case 3: User entered only one number (e.g., "8")
    * Default start point set to 0 (i.e., 0 to 8)
    * Note: If you want single-digit input to start from 1 by default, change 0 to 1 below
    loc hs = 0 
    loc hran `hs'/`hor'
}

** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
**  panel data
** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

capture tsset
if _rc>0 {
	di as err "Error: data not set. Please use 'xtset panelvar timevar' first."
	exit 459
}
loc panvar=r(panelvar)
loc timevar=r(timevar)
if "`panvar'" == "." | "`panvar'" == "" {
    di as err "Error: data not set. Please use 'xtset panelvar timevar' first."
    exit 459
}

** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
**  dep variables and ylags
** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

loc y `varlist'

* levels
if "`transf'"==""|"`transf'"=="level" {
	forvalues h = `hran' {
		loc hstr = `h' - `hs'
		loc m = `h'
		tempvar y_h`hstr'
		qui gen `y_h`hstr'' = f`h'.`y'		
		loc trn`hstr' "`y'_h(`m')"
	}
	if `ylags'>0 loc ly L(1/`ylags').`y'
	loc y y_h
}

* differences
else if "`transf'"=="diff" {
	tempvar dy
	qui gen `dy' = `y' - l.`y'
	fvexpand `dy' 
	loc ltr=r(varlist)
	loc ltrn "D.`y'"
	forvalues h = `hran' {
		loc hstr = `h' - `hs'
		loc m = `h'
		tempvar dy_h`hstr'
		qui gen `dy_h`hstr'' = f`h'.`y' - l.f`h'.`y' 
		loc trn`hstr' "D.`y'_h(`m')"
	}
	if `ylags'>0 loc ly L(1/`ylags').`dy'
	loc y dy_h
}

* Cumulative
else if "`transf'"=="cmlt" {
	tempvar dy
	qui gen `dy' = `y' - l.`y'
	fvexpand `dy' 
	loc ltr=r(varlist)
	loc ltrn "D.`y'"
	forvalues h = `hran' {
		loc hstr = `h' - `hs'
		loc m = `h'
		tempvar cy_h`hstr'
		qui gen `cy_h`hstr'' = f`h'.`y' - l.`y' 
		loc trn`hstr' "cml_`y'_h(`m')"
	}
	if `ylags'>0 loc ly L(1/`ylags').`dy'
	loc y cy_h
}
* logs
if "`transf'"=="logs" {
	tempvar lny
	qui gen `lny' = ln(`y')
	fvexpand `lny' 
	loc ltr=r(varlist)
	loc ltrn "ln`y'"
	forvalues h = `hran' {
		loc hstr = `h' - `hs'
		loc m = `h'
		tempvar lny_h`hstr'
		qui gen `lny_h`hstr'' = ln(f`h'.`y')
		loc trn`hstr' "ln`y'_h(`m')"
	}
	if `ylags'>0 loc ly L(1/`ylags').`lny'
	loc y lny_h
}

* log differences
else if "`transf'"=="logs diff" {
	tempvar dlny
	qui gen `dlny' = ln(`y') - ln(l.`y')
	fvexpand `dlny' 
	loc ltr=r(varlist)
	loc ltrn "D.ln`y'"
	forvalues h = `hran' {
		loc hstr = `h' - `hs'
		loc m = `h'
		tempvar dlny_h`hstr'
		qui gen `dlny_h`hstr'' = ln(f`h'.`y') - ln(l.f`h'.`y')
		loc trn`hstr' "D.ln`y'_h(`m')"
	}
	if `ylags'>0 loc ly L(1/`ylags').`dlny'
	loc y dlny_h
}

* Cumulative logs
else if "`transf'"=="logs cmlt" {
	tempvar dlny
	qui gen `dlny' = ln(`y') - ln(l.`y')
	fvexpand `dlny' 
	loc ltr=r(varlist)
	loc ltrn "D.ln`y'"
	forvalues h = `hran' {
		loc hstr = `h' - `hs'
		loc m = `h'
		tempvar clny_h`hstr'
		qui gen `clny_h`hstr'' = ln(f`h'.`y') - ln(l.`y')
		loc trn`hstr' "cml_ln`y'_h(`m')"
	}
	if `ylags'>0 loc ly L(1/`ylags').`dlny'
	loc y clny_h
}

** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
**  slags
** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

if `slags'>0 {
	fvexpand `ls'
	loc varls=r(varlist)
	fvrevar `ls'
	loc lsl=r(varlist)
}

** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
**  plot data
** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
cap drop _birf _seirf _birf_lo _birf_up	
cap drop _birf_lo2 _birf_up2
tempvar birf seirf _t birf_up birf_lo birf_up2 birf_lo2 _zero

if `hs'<=0 loc h1 = `hor'+ 1 -`hs'
else loc h1 = `hor'

qui gen `birf' = 0 if _n<=`h1'
qui gen `seirf' = 0 if _n<=`h1'
qui gen `birf_up' = 0 if _n<=`h1'
qui gen `birf_lo' = 0 if _n<=`h1'

if `hs'<=0 qui gen `_t' =_n-1+`hs'
else  qui gen `_t' =_n

qui gen `_zero' = 0

** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
**  estimation
** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
matrix stats = J(`hor'-`hs'+1,6,.)

qui {
	forval h=`hran' {
		if `hs'<=0 loc k=`h'+ 1 - `hs'
		else loc k=`h'
		loc hstr = `h' - `hs'
		
		tempname b`hstr' V`hstr'
		
		`met' ``y'`hstr'' `s' `ls' `ly' `c' `if' `in', `options'
		
		matrix `b`hstr'' = e(b)
		matrix `V`hstr'' = e(V)
		loc cfn : colfullnames e(b)
		
		matrix stats[`hstr'+1,1]=e(N)
		if e(r2)!=. matrix stats[`hstr'+1,2]=e(r2)
		else matrix stats[`hstr'+1,2]=1-e(rss)/(e(rss)+e(mss))
		matrix stats[`hstr'+1,3]=e(r2_p)
		matrix stats[`hstr'+1,4]=e(F)
		matrix stats[`hstr'+1,5]=e(chi2)		
		if e(p)!=.  matrix stats[`hstr'+1,6]=e(p)
		else if e(F)!=. matrix stats[`hstr'+1,6]=1-F(e(df_m)+1,e(df_r),e(F))
		else matrix stats[`hstr'+1,6]=1-chi2(e(df_m)+1,e(chi2))

		tokenize `varls'
		loc vln=1
		foreach x of local lsl {
			local cfn=regexr("`cfn'","`x'","``vln''")
			loc vln = `vln'+1
		}
		local cfn=regexr("`cfn'","`ltr'","`ltrn'")
		if `ylags'>1 {
			forval p=2/`ylags' {
				local cfn=regexr("`cfn'","`p'\.`ltr'","`p'.`ltrn'")
			}	
		}
        matrix colnames `b`hstr'' = `cfn'
        matrix rownames `V`hstr'' = `cfn'
        matrix colnames `V`hstr'' = `cfn'
		
		
		lincom `s', level(95)
		loc lb = r(lb)
		loc ub = r(ub)
		if `lb'==. loc lb = r(estimate)
		if `ub'==. loc ub = r(estimate)
		
		replace `birf' = r(estimate) if _n==`k'
		replace `seirf' = r(se) if _n==`k'
		replace `birf_up' = `ub' if _n==`k'
		replace `birf_lo' = `lb' if _n==`k'
		
		
		ereturn post `b`hstr'' `V`hstr''
        `noisily' di "`trn`hstr''"
		`noisily' _coef_table
	}
}

loc h1 = `h1'
loc hran `hs'/`hor'


mkmat `birf'    if _n<=`h1', mat(BIRF)
mkmat `seirf'   if _n<=`h1', mat(SEIRF)
mkmat `birf_lo' if _n<=`h1', mat(SEIRF_LO)
mkmat `birf_up' if _n<=`h1', mat(SEIRF_UP)

mat IRF = BIRF , SEIRF , SEIRF_LO , SEIRF_UP
matrix colnames IRF = "IRF" "Std.Err." "IRF LOW" "IRF UP"

loc rows ""
loc lines ""
forval i=`hran' {
	loc rows `rows' `i'
	loc lines `lines'&
}
matrix rownames IRF = `rows'

matrix colnames stats = " N " "R2" "psR2" "F" "Chi2" "Prob"
matrix rownames stats = `rows'

if "`stats'"=="stats" matlist stats, cspec(&o4 %9.0f w2 R|o1 %9.0f &o1 %9.3f &o1 %9.3f &o1 %9.2f &o1 %9.1f &o1 %9.3f &) rspec(&-`lines') title("Statistics by step") 
matlist IRF, noheader format(%9.5f) title("Impulse Response Function") lines(oneline)


** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
**  graph
** ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
loc mod = mod(`hor'-`hs',2)
if `hor'-`hs'>12 & `mod'==0 loc p 2
else if `hor'-`hs'>12 & `mod'==1 loc p 3
else loc p 1

if "`graph'"!="nograph" {
	loc lcolor blue
	qui twoway (rarea `birf_up' `birf_lo' `_t', fcolor(`lcolor'%15) lc(`lcolor'%7)) ///
	(line `_zero' `_t', lcolor(gs5) lpattern(dash)) ///
	(line `birf' `_t', lcolor(`lcolor') lpattern(solid)) if _n<=`h1', ///
	legend(`off' order(3) position(6)) tlabel(`hs'(`p')`hor') xtitle("Horizon") ///
	name("IRF", replace)
}

*********************************************************************************************************************************************
*********************************************************************************************************************************************

// if "`saveirf'"=="saveirf" {
// 	if "`irfname'"=="" {
// 		qui gen _birf = `birf'
// 		qui gen _seirf = `seirf'
// 		qui gen _birf_up = `birf_up'
// 		qui gen _birf_lo = `birf_lo'
// 		if `nconf'>1 {
// 			qui gen _birf_up2 = `birf_up2'
// 			qui gen _birf_lo2 = `birf_lo2'
// 		}
// 		label var _birf "`label'"
// 	}
// 	else {
// 		cap drop `irfname' `irfname'_se `irfname'_up `irfname'_lo
// 		cap drop `irfname'_up2 `irfname'_lo2
// 		qui gen `irfname' = `birf'
// 		qui gen `irfname'_se = `seirf'
// 		qui gen `irfname'_up = `birf_up'
// 		qui gen `irfname'_lo = `birf_lo'
// 		if `nconf'>1 {
// 			qui gen `irfname'_up2 = `birf_up2'
// 			qui gen `irfname'_lo2 = `birf_lo2'
// 		}
// 		label var `irfname' "`label'"		
// 	}
// }
*********************************************************************************************************************************************
*********************************************************************************************************************************************

ereturn matrix irf IRF

end

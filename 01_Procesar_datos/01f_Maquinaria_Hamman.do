******************************
*** Medicion de productividad agricola BM
*** Ultima fecha de modificacion: 23 noviembre, 2021
*** Procesamiento de datos de Maquinaria siguiendo a Hamman et al. (2018)
******************************

*-------------
*---  Configuracion y paths
*-------------
  
clear all
set more off

glo pc "C:\Users\anapi\OneDrive - Universidad EAFIT\2021_WB_Productividad\01_Analisis\BM_productividad"
glo grl "C:\Users\anapi\OneDrive - Universidad EAFIT\2021_WB_Productividad\01_Analisis\BM_productividad\01_Datos_originales\BR"
glo data "$grl\datos"
glo input "$data\input"
glo output "$data\output"

******************************
*--- 01. Datos de precios maquinaria
**** El siguiente codigo proviene del paper de Hamman et al. (2018)
******************************

*FIX MACHINARY CODES
	import excel "$input\diccionario_CNA.xlsx", ///
		sheet("Maquinaria") cellrange(A1:B50) firstrow clear
	rename *, lower
	set obs 50
	replace p_s9p118 = 0 in 50
	replace maquinaria = "not reported" in 50
	tempfile mach_codes
	save `mach_codes'

* IMPORT MACHINARY PRICES
	import excel "$input\precios_maquinaria.xlsx", sheet("final") ///
		cellrange(A1:H51) firstrow clear 
	drop B
	destring pmean, replace
	tempfile mach_prices
	save `mach_prices'
	
* ESTIMATE CAPITAL VALUATION
	use "$pc\02_Datos\CNA\base_maquinaria_agro.dta", clear
		
	foreach var in p_s9p119 p_s9p120 {
		recode  `var' (99999 9999 999=0)
	}
	
	merge m:1 p_s9p118 using `mach_codes', nogen
	merge m:1 p_s9p118 using `mach_prices', nogen keep(1 3)
	destring pm*, replace
	
	egen total_maq=rowtotal(p_s9p119 p_s9p120) if maq_agro==1, missing 
	recode total_maq (0=.)
			
	foreach v of varlist pm* {
	replace `v'=`v'/1000
	}
	
	*kmin kmean kmax son las que tienen todas las maquinarias
	*kmin_agro es el capital con el precio minimo y solo maquinaria agrícola
	gen kmin=total_maq*pmin
	gen kmax=total_maq*pmax
	gen kmean=total_maq*pmean
	gen kmin_agro=total_maq*pmin_agro
	
	bys uc_uo encuesta cod_vereda kmean: gen allmissing = mi(kmean[1])
	bys uc_uo encuesta cod_vereda kmin_agro: gen allmissing2 = mi(kmin_agro[1])
		
	collapse (sum) total_maq k* (min) allmissing*, by(p_depto p_munic encuesta uc_uo)
	
	foreach v of varlist kmin kmax kmean {
		replace `v'=. if allmissing
	}
	replace kmin_agro=. if allmissing2
	
	destring p_depto p_munic encuesta uc_uo, replace
	save  "$output\capital_valuation.dta", replace
******************************
*** Medicion de productividad agricola BM
*** Ultima fecha de modificacion: 24 noviembre, 2021
*** Estimacion de productividad siguiendo a Hamman et al. (2018)
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
*--- 00. UPAS de interes
******************************

* Abrimos informacion de unidad productora, identificamos UPA de interes, y guardamos sus nombres en minuscula
import delim "$pc\01_Datos_originales\CNA\Total_nacional(csv)\S01_15(Unidad_productora).csv", stringcols(5 6) clear
rename *, lower

merge m:1 p_depto p_munic cod_vereda encuesta using "$pc\02_Datos\CNA\limpieza_censo\lista_codigos_upa_cna.dta"
keep if _merge == 3
drop _merge

compress
save "$input\CNA_nacional_unidad_productora.dta", replace

******************************
*--- 01. Ingresos agricolas (nivel UPA)
******************************

import delim "$pc\01_Datos_originales\CNA\Total_nacional(csv)\S06A(Cultivos).csv", stringcols(5 6 10) clear 

// use "$input\CNA_nacional_cultivos.dta", clear
rename *, lower
rename p_s6p57a quantity_ton
rename p_s6p45b plot_number

* Identificamos UPAs de interes (es normal que no todas peguen, hay UPAs sin informacion de cultivos en esta lista)
merge m:1 p_depto p_munic cod_vereda encuesta using "$pc\02_Datos\CNA\limpieza_censo\lista_codigos_upa_cna.dta"

keep if _merge == 3
drop _merge
compress

* Pegamos nombres de cultivos
gen cod_cultivo = p_s6p46

merge m:1 cod_cultivo using "$pc\02_Datos\CNA\limpieza_censo\lista_codigos_cultivos_cna.dta"
drop _merge
	
gen cropname_ed = nombre_cultivo
label variable cropname_ed "Original CNA cropname may have been edited" 
replace cropname_ed = "CAFE" if regexm(cropname_ed, "CAFE")
replace cropname_ed = "MORA" if cropname_ed == "MORAANDINA"
replace cropname_ed = "BANANO" if cropname_ed == "BANANOTIPOEXPORTACION"
	
// paste_prices_revenue_var
// merge m:1 cropname_ed using "$output\prices_CNA.dta", nogen keep(1 3) assert(1 2 3) keepusing(crop_price)	

merge m:1 cod_cultivo using "$pc\02_Datos\CNA\precios\lista_precios_cultivos_cna.dta"
drop cod_cultivo _merge cultivo tipo_cul tipo_fuente fuente unidad
rename precio crop_price
destring crop_price, replace

replace crop_price = (514 + 611)/2 if p_s6p46 == "00149102001"

//Average of 2013 reference price per semester
//http://web.fedepalma.org/precios-de-referencia-del-fondo-de-fomento-palmero
replace crop_price = (5045000 / 1000) if p_s6p46 == "00192101001"

//Data from Fondo de Fomento Algodonero, minimum price paid to producers.
*CONVERT QUANTITIES USING CONVERSION FACTORS FOR CROP SPECIFICATIONS
gen quantity_or=quantity_ton
replace quantity_ton = quantity_ton * (1/.187) if p_s6p46 == "00149102001"
replace quantity_ton = quantity_ton * (1/.1) if p_s6p46 == "00180201002"
replace quantity_ton = quantity_ton * (1/0.115) if p_s6p46 == "00180201001" 
replace quantity_ton = quantity_ton * (1/.28) if p_s6p46 == "00112201002" | p_s6p46 == "00112201001"
replace quantity_ton = quantity_ton * (1/0.909) if p_s6p46 == "00113202001"

*GENERATE REVENUE VAR
gen revenue = crop_price * 1000 * quantity_ton
label variable revenue "COP Revenue from selling output"

snapshot save, label("Copia temporal 1")

*to solve ties- important crop in the country 
collapse (sum) revenue, by(cropname_ed)
gsort - revenue 
gen n=_n
tempfile rank
save `rank'

* to get crop_maxarea for regression
snapshot restore 1
collapse (sum) area_sembrada revenue, by(p_depto p_munic uc_uo encuesta cropname_ed)
merge m:1 cropname_ed using  `rank'
egen double max_area_crop = max(area_sembrada), by(p_depto p_munic uc_uo encuesta)

gen crop_maxarea=cropname_ed if area_sembrada == max_area_crop
keep if area_sembrada == max_area_crop
egen id=group(p_depto p_munic uc_uo encuesta)
bys id: egen x=min(n)
keep if x==n
keep p_depto p_munic uc_uo encuesta crop_maxarea

tempfile maxcrop
save `maxcrop'

*to get plot_area for regression
snapshot restore 1
collapse (max) area_sembrada, by(p_depto p_munic uc_uo encuesta plot_number)
collapse (sum)area_sembrada, by(p_depto p_munic uc_uo encuesta   )
rename area_sembrada Tplot_area 
lab var Tplot_area "Total Plot Area"

destring uc_uo encuesta, replace
tempfile plot_area
save `plot_area'

******************************
*--- 02. TFP
******************************

// Start with crop level data 
snapshot restore 1
merge m:1 p_depto p_munic uc_uo encuesta using `maxcrop', nogen

// Sum over more general names of crops in the all plots 
collapse (sum) revenue area_sembrada quantity_ton,	 ///
	by(p_depto p_munic uc_uo encuesta cropname_ed crop_maxarea)

//identify farms with 80% of area un a single crop 
bys p_depto p_munic uc_uo encuesta: egen den=sum(area_sembrada) 	
gen p=area_sembrada/den
gen imono=(p>=0.8)
drop den 

// collapse at the farm level 
collapse (sum) revenue* area_sembrada* quantity_ton* ///
(max) imono, by(p_depto p_munic uc_uo encuesta crop_maxarea)

* BRING VARIABLES FROM FARM TABLE
// merge 1:1 p_depto p_munic uc_uo encuesta using "$input\CNA_nacional_unidad_productora.dta", nogen assert(2 3) keep(3) keepusing(p_s5pautos p_s* pred_etnica cod_vereda)

merge 1:1 p_depto p_munic uc_uo encuesta using "$input\CNA_nacional_unidad_productora.dta", nogen keep(3) keepusing(p_s5pautos p_s* pred_etnica cod_vereda)
	 
*Change varnames
rename p_s4p16 nomanagers
replace p_s5pautos = p_s5pautos/10000
rename p_s5pautos farm_area_ha
la var farm_area_ha "Farm area in ha"

rename p_s9p117 machine_dummy
rename p_s11p138 perm_workers
rename p_s11p139 perm_workers_hh
rename p_s11p140 extra_jornales

*hectares
foreach v of varlist p_s12p142 p_s6p66 p_s6p68 p_s12p143 p_s12p145 p_s12p146 p_s12p148 p_s12p149 {
    replace `v' = `v'/10000
}

rename p_s12p142 agro_area_ha
rename p_s6p66 natgrass_area_ha
rename p_s6p68 pgrass_area_ha
rename p_s12p143 fallow_area_ha
rename p_s12p145 scrub_area_ha 
rename p_s12p146 forest_area_ha
rename p_s12p148 infraest_ha
rename p_s12p149 otheruse_ha

//Livestock 
recode p_s7p78 p_s7p86 p_s7p90 p_s7p94 p_s8p107 (2=0)	
rename p_s7p78 cattle
rename p_s7p86 pigs
rename p_s7p90 poulty
rename p_s7p94 fish
rename p_s8p107 fishing

*this results from intersecting maps of potential yields for several crops from FAO with farms coordinates in CNA
destring encuesta uc_uo, replace
merge 1:1 p_depto p_munic encuesta uc_uo using ///
		"$input/potential_yield_farm.dta", nogen keep(1 3)
merge 1:1 p_depto p_munic uc_uo encuesta using `plot_area', nogen
		
/// * GENERATE ADJUSTED PRODUCTIVITY
gen areaprodtiva_ag = farm_area_ha - ///
	(natgrass_area_ha + pgrass_area_ha + infraest_ha + otheruse_ha)
replace areaprodtiva_ag = 0 if areaprodtiva_ag<0

tempvar jornalperm jornaladic
gen jornalperm = perm_workers*6*4*12
gen jornaladic = extra_jornales*12
egen jornales 	= rowtotal(jornalperm jornaladic), missing
gen workers = perm_workers + extra_jornales/21

// Make capital valuation 
merge 1:1 p_depto p_munic encuesta uc_uo using ///
 "$output\capital_valuation.dta", nogen keep(1 3)
gen larea=log(farm_area_ha)
gen lkmin_agro=log(kmin_agro)
xi: reg lkmin_agro larea imono jornalperm jornaladic cattle pigs poulty fish fishing i.p_depto i.crop_maxarea 
predict y, xb	
gen kminbar=exp(y) 
replace kminbar=kmin_agro if kmin_agro!=.
 
//Generate variables per workers 
foreach var in areaprodtiva_ag kmin_agro kminbar revenue farm_area_ha mpoty area_sembrada Tplot_area {
	gen `var'_j = `var'/jornales
}
	 

* Parametrization from Restuccia & Santaeulaia (2015) 
local thetal = 0.36
local thetak = 0.18
local y "revenue_j"
local k "kmin_agro_j"
local ki "kminbar_j"
local q "mpoty"

*adjusted productivity: with total agricultrual area 
local l "areaprodtiva_ag_j" 
gen s1=`y'/((`ki'^`thetak')*(`q'*`l')^`thetal') if `y'>0 & areaprodtiva_ag>=0.01
gen yield1=revenue/areaprodtiva_ag if revenue>0 & areaprodtiva_ag>=0.01 
 *adjusted productivity with total planted area 
local l "area_sembrada_j" 	
gen s2=`y'/((`ki'^`thetak')*(`q'*`l')^`thetal') if `y'>0 & areaprodtiva_ag>=0.01
gen yield2=revenue/area_sembrada if revenue>0 & areaprodtiva_ag>=0.01 
 *adjusted productivity with sum of plot areas 
 local l "Tplot_area_j" 	
gen s3=`y'/((`ki'^`thetak')*(`q'*`l')^`thetal') if `y'>0 & areaprodtiva_ag>=0.01
gen yield3=revenue/Tplot_area if revenue>0 & areaprodtiva_ag>=0.01 
 
forvalue i=1/3 {
gen ls`i'=log(s`i')
}

 
* Organizar base y exportar
save "$output\farm_level.dta", replace

keep p_depto p_munic cod_vereda encuesta revenue* kmin* mpoty* area_sembrada* areaprodtiva_ag* Tplot_area* jornales s1 yield1 s2 yield2 s3 yield3 ls*

save "$pc\02_Datos\Productividad\base_productividad_hamman.dta", replace

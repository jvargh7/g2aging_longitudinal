


From: Chihua Li <chihuali@umich.edu> 
Sent: 03 February 2024 10:10
To: Varghese, Jithin Sam <jithin.sam.varghese@emory.edu>
Cc: peiyilu <peiyilu@hku.hk>
Subject: Re: [External] FW: Media Inquiry- Time Sensitive

For ELSI-Brizal, the height, weight, BMI, and blood pressure are available.

For CHARLS, please see the code on how I corrected SBP and DBP in 2015 in the STATA code (line 41-49).

*Correct sbp and dbp by using communityID; 1146043 has systematic swap
g temp_sbp = bpsys if bpsys <= bpdia & communityID == "1146043"
g temp_dbp = bpdia if bpsys <= bpdia & communityID == "1146043"

replace bpsys = temp_dbp if bpsys <= bpdia & communityID == "1146043"
replace bpdia = temp_sbp if bpsys <= bpdia & communityID == "1146043" 

replace bpsys = . if bpsys <= bpdia & !missing(bpsys) & !missing(bpdia)
replace bpdia = . if bpsys <= bpdia & !missing(bpsys) & !missing(bpdia) 
 
I found that in this community, SBP and DBP were systematically swapped, which has to be corrected. Another thought is that in the future, we need to check SBP and DBP systematically across studies to make sure SBP is larger than DBP.



**=========================================**
** Proj: biomarker and social stratification
** File: charls01- data creation 
** Version: June 2023
**-----------------------------------------**

** Setup directory
clear all 
set maxvar 32000

cd "C:\Users\chihu\Dropbox\NIA R01 - HCAP Harmonization\POSTED\ANALYSIS\CmrSocialStratification"

global dir "/Users/chihu/Dropbox/NIA R01 - HCAP Harmonization"
global source "$dir/POSTED/DATA/SOURCE"
global derived "$dir/POSTED/DATA/DERIVED"

*CHARLS survey data 2015 and 2018
use "$source/CharlsHCAP/H_CHARLS.dta", clear

g bmi = .
  replace bmi = r3mbmi
  replace bmi = . if bmi >=50
  
g waist = .
  replace waist = r3mwaist
  
g ht =.
  replace ht = r3mheight
  replace ht = r2mheight if missing(ht)
  replace ht = r1mheight if missing(ht)
  
g bpsys = .
  replace bpsys = r3systo

g bpdia = .
  replace bpdia = r3diasto
  
g bppuls = .
  replace bppuls = r3pulse
  
*Correct sbp and dbp by using communityID; 1146043 has systematic swap
g temp_sbp = bpsys if bpsys <= bpdia & communityID == "1146043"
g temp_dbp = bpdia if bpsys <= bpdia & communityID == "1146043"

replace bpsys = temp_dbp if bpsys <= bpdia & communityID == "1146043"
replace bpdia = temp_sbp if bpsys <= bpdia & communityID == "1146043" 

replace bpsys = . if bpsys <= bpdia & !missing(bpsys) & !missing(bpdia)
replace bpdia = . if bpsys <= bpdia & !missing(bpsys) & !missing(bpdia) 
  
sort ID

save CHARLS_questionnaire, replace

*CHARLS blood data 2015
use "$source/CHARLS 2015/CHARLS2015r/Blood2015.dta", clear

keep ID Blood_weight bl_glu bl_glu bl_hbalc bl_cho bl_hdl bl_ldl bl_tg bl_crp

g a1c = .
  replace a1c = bl_hbalc
  
g cho = . 
  replace cho = bl_cho
  
g hdl = .
  replace hdl = bl_hdl
  
g ldl = .
  replace ldl = bl_ldl
  
g tchol = .
  replace tchol = bl_cho
  
g crp = .
  replace crp = bl_crp
  
g tg = .
  replace tg = bl_tg  
  
g glu = .
  replace glu = bl_glu 
  
sort ID  

merge 1:1 ID using CHARLS_questionnaire
drop _merge

**Check distributions of biomarkers
summarize bpsys bpdia bppuls bmi crp a1c hdl ldl tchol glu tg

**** generate distributions of biomakers and identify 1% and 99% (gender specific)
foreach var in bpsys bpdia bppuls bmi ht crp a1c hdl ldl tchol glu tg {
	summ `var' if ragender == 2, d
	g p1_`var'_female = r(p1)
	g p99_`var'_female = r(p99)
	
	summ `var' if ragender == 1, d
	g p1_`var'_male = r(p1)
	g p99_`var'_male = r(p99)	
}

foreach var in bpsys bpdia bppuls bmi ht crp a1c hdl ldl tchol glu tg {
	replace `var' = p1_`var'_female if `var' < p1_`var'_female  & ragender == 2
	replace `var' = p99_`var'_female if `var' > p99_`var'_female & !mi(`var') & ragender == 2
	
	replace `var' = p1_`var'_male if `var' < p1_`var'_male  & ragender == 1
	replace `var' = p99_`var'_male if `var' > p99_`var'_male & !mi(`var') & ragender == 1
}

**Age
g age = r3agey
summarize age

g agegrp = 0 if age >= 50 & age < 65
replace agegrp = 1 if age >= 65 & age < 75
replace agegrp = 2 if age >= 75 & age < 85
replace agegrp = 3 if age >=85 & !missing(age)
tab agegrp

**Sex
g female = 1 if ragender == 2
replace female = 0 if ragender == 1
tab female

**Education: follow HRS category 1 to 4
g educattain_resp =.
replace educattain_resp = 0 if raeduc_c >= 1 & raeduc_c <= 4
replace educattain_resp = 1 if raeduc_c == 5
replace educattain_resp = 2 if raeduc_c >= 6 & raeduc_c < 12
tab educattain_resp
label define educattain_resp ///
   0  "Primary education and less" ///
   1  "Lower secondary education" ///
   2  "Upper secondary education and above" 
   
g edu_high = 0 if educattain_resp == 0 | educattain_resp == 1
replace edu_high = 01 if educattain_resp == 2
tab edu_high
   
**Parental education
g f_educattain_resp = .
replace f_educattain_resp = 0 if rafeduc_c <= 4
replace f_educattain_resp = 1 if rafeduc_c == 5
replace f_educattain_resp = 2 if rafeduc_c >= 6 & rafeduc_c < 12
table f_educattain_resp

g m_educattain_resp = .
replace m_educattain_resp = 0 if rameduc_c <= 4
replace m_educattain_resp = 1 if rameduc_c == 5
replace m_educattain_resp = 2 if rameduc_c >= 6 & rameduc_c < 12
table m_educattain_resp

table f_educattain_resp m_educattain_resp

g p_educattain_resp = .
replace p_educattain_resp = 0 if f_educattain_resp == 0 & m_educattain_resp == 0
replace p_educattain_resp = 1 if f_educattain_resp == 1 & m_educattain_resp <= 1
replace p_educattain_resp = 1 if f_educattain_resp <= 1 & m_educattain_resp == 1
replace p_educattain_resp = 2 if f_educattain_resp == 2 & m_educattain_resp <= 2
replace p_educattain_resp = 2 if f_educattain_resp <= 2 & m_educattain_resp == 2
table p_educattain_resp

g p_edu_high = .
replace p_edu_high = 0 if p_educattain_resp == 0 | p_educattain_resp == 1
replace p_edu_high = 1 if p_educattain_resp == 2
table p_edu_high
   
**Rural
g rural = 1 if h3rural == 1
replace  rural = 0 if h3rural == 0

**Hukou
g hukou_rural = 1 if r3rural2 == 1
replace hukou_rural = 0 if r3rural2 == 0
replace hukou_rural = r2rural2 if missing(hukou_rural)
replace hukou_rural = r1rural2 if missing(hukou_rural)
table hukou_rural
   
**Check numbers for flow chart
*Whether interviewed in 2016

tab inw3

tab inw3 if age >= 50  & !missing(age)

tab rural edu_high if inw3 == 1 & age >= 50 & !missing(age)

gen any_biomaker = 1 if !missing(bmi) | !missing(bpsys) | !missing(a1c) | !missing(tchol) 
table any_biomaker if inw3 == 1 & !missing(edu_high) & !missing(rural) & age >= 50  & !missing(age)

mvpatterns hukou_rural p_edu_high if inw3 == 1 & !missing(edu_high) & !missing(rural) & age >= 50 & !missing(age)

keep if any_biomaker == 1 & inw3 == 1 & !missing(edu_high) & !missing(rural) & age >= 50  & !missing(age)

summarize bmi bpsys a1c tchol

save "charls-analysis-unimputed.dta", replace

mvpatterns age female edu_high p_edu_high hukou_rural 

**Imputation
ice age female edu_high p_edu_high hukou_rural h3atotf, m(1) seed(1) persist saving(charls-analysis-imputed, replace)

use charls-analysis-imputed, clear

keep if _mj == 1

keep ID inw3 bpsys bpdia bppuls bmi ht crp glu a1c hdl ldl tchol tg age agegrp female educattain_resp f_educattain_resp m_educattain_resp p_educattain_resp rural hukou_rural edu_high p_edu_high raeduc_c

save "charls-analysis.dta", replace
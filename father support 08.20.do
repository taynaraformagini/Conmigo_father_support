******************* CONMIGO DATASET: FATHER SUPPORT FOR DAUGHTER PA N=60  ******************* 

***** Variables included 

*** exposure variable: father support
* Father support was measured used 5 variables (mfathsup1 mfathsup2 mfathsup3 mfathsup4 mfathsup5) I used factor analysis to create a composite score 

*** Fator analysis 
factor mfathsup1 mfathsup2 mfathsup3 mfathsup4 mfathsup5
gen Factor1_Score =  0.7229  * mfathsup1 + 0.7669    * mfathsup2 + 0.6833   * mfathsup3 + 0.7489   * mfathsup4 + 0.5648    * mfathsup5

*exposure variable: 
Factor1_Score 

*** Outcome variables: light PA and MVPA, already calculated in the dataset 
*light PA
dli

*MVPA
dtotmvpa

**covariates
*daughter age: already calculated in dataset
dage

*income: 7-level in dataset - need to create a 3-level income variable
gen income3 =. 
replace income3 = 1 if mincome == 1 |  mincome == 2  |  mincome == 3 |  mincome == 4
replace income3 = 2 if mincome == 5 |  mincome == 6  
replace income3 = 3 if mincome == 7 |  mincome == 8 
replace income3 = 4 if mincome == -888 
 
*creatre a 3-level income variable including missing for the regression
gen incomemis =. 
replace incomemis = 1 if mincome == 1 |  mincome == 2  |  mincome == 3 |  mincome == 4
replace incomemis = 2 if mincome == 5 |  mincome == 6  
replace incomemis = 3 if mincome == 7 |  mincome == 8 
replace incomemis = 4 if mincome == -888 
replace incomemis = 5 if mincome == . 

*daughter acculturation; already calculated in dataset
dacctot

*mother support for PA 
*Mother acculturation level combinining the two sub-scales: Non-directive support (dnondirtot) and  Autonomy support  (dautontot)
gen combined_mothersupport  = dnondirtot + dautontot
gen combined_mothersupport_rescaled = (combined_mothersupport / 8) * 4

 
** BMI category based on CDC percentile accounting for age
*Step 1: Generate a new variable to store the dichotomized BMI categories
gen bmi_category = .

// Step 2: Calculate the 85th percentile of BMI based on age for valid data
egen pct_85 = pctile(mdbmi) if !missing(mdbmi) & !missing(maged), by(maged)

// Step 3: Dichotomize the BMI variable for valid data using numeric codes
replace bmi_category = 1 if !missing(mdbmi) & !missing(maged) & mdbmi < pct_85
replace bmi_category = 2 if !missing(mdbmi) & !missing(maged) & mdbmi >= pct_85

*Create a BMI variable including missing for the regression
gen bmimis =.
replace bmimis =1 if bmi_category==1
replace bmimis =2 if bmi_category==2
replace bmimis =3 if bmi_category==.

*create a second BMI variable including missing but combining 1 and 2 to compare those with and without missing BMI dataset
gen missingbmi =.
replace missingbmi =1 if bmimis==1 | bmimis==2
replace missingbmi =2 if bmimis==3 


* Finally, generate variable Sample which only includes girls with completed data on light PA, MVPA and father support for PA  n= 60
gen sample =.
replace sample = 1 if mfather==1 & dli !=. & dtotmvpa !=.

**brief descriptive analysis comparing those with missing BMI data with those without missing 
mean dage if sample==1, over(missingbmi)
mean dacctot if sample==1, over(missingbmi)
mean combined_mothersupport_rescaled if sample==1, over(missingbmi)
mean dli if sample==1, over(missingbmi)
mean dtotmvpa if sample==1, over(missingbmi)


 
******************* DESCRIPTIVE STATISTICS ******************* 
****sample characteristics

**covariates
summarize dage if sample==1, detail
tab income3 if sample==1 
summarize dacctot if sample==1, detail
summarize combined_mothersupport_rescaled if sample==1, detail
tab bmi_category if sample==1

*Other characteristics
tab mdcountry if sample==1
tab dethnic if sample==1
tab dlang if sample==1
summarize dstep if sample==1, detail

*exposure variable
summarize mfathsuptot if sample==1, detail

*outcome variables
summarize dli if sample==1, detail
summarize dtotmvpa if sample==1, detail


******************* INFERENTIAL STATISTICS ******************* 

*regression light PA subsequent models  

*unadjusted
regress Factor1_Score dli
regress Factor1_Score dli, beta

*age
regress Factor1_Score dli dage 
regress Factor1_Score dli dage , beta

*age + income 
regress Factor1_Score dli dage i.incomemis
regress Factor1_Score dli dage i.incomemis, beta

*age + income + acculturation
regress Factor1_Score dli dage i.incomemis dacctot
regress Factor1_Score dli dage incomemis dacctot, beta

*age + income + acculturation + mother acculturation 
regress Factor1_Score dli dage i.incomemis dacctot combined_mothersupport
regress Factor1_Score dli dage incomemis dacctot dnondirtot, beta

**age + income + acculturation + mother acculturation + BMI
regress Factor1_Score dli dage incomemis dacctot dnondirtot i.bmimis
regress Factor1_Score dli dage incomemis dacctot dnondirtot i.bmimis, beta


*regression MVPA  subsequent models  

*unadjusted
regress Factor1_Score dtotmvpa
regress Factor1_Score dtotmvpa, beta

*age
regress Factor1_Score dtotmvpa dage
regress Factor1_Score dtotmvpa dage , beta

*age + income 
regress Factor1_Score dtotmvpa dage i.incomemis
regress Factor1_Score dtotmvpa dage i.incomemis, beta

*age + income + acculturation
regress Factor1_Score dtotmvpa dage i.incomemis dacctot
regress Factor1_Score dtotmvpa dage i.incomemis dacctot, beta

*age + income + acculturation + mother acculturation 
regress Factor1_Score dtotmvpa dage i.incomemis dacctot combined_mothersupport
regress Factor1_Score dtotmvpa dage i.incomemis dacctot combined_mothersupport, beta

**age + income + acculturation + mother acculturation + BMI
regress Factor1_Score dtotmvpa dage i.incomemis dacctot combined_mothersupport i.bmimis
regress Factor1_Score dtotmvpa dage i.incomemis dacctot combined_mothersupport i.bmimis, beta



** interaction support#BMI

** create a interaction term variable
gen BMI_father_support = Factor1_Score * bmimis

*assess the significance of the interaction term on Light PA and MVPA
*light PA 
reg BMI_father_support dli
reg BMI_father_support dtotmvpa

*assess the significance of both variables 
reg Factor1_Score i.bmi_category

*Logistic regression models Light PA
reg dli c.Factor1_Score#bmi_category
reg dli c.Factor1_Score#bmi_category, beta

reg dli c.Factor1_Score#bmi_category dage
reg dli c.Factor1_Score#bmi_category dage, beta

reg dli c.Factor1_Score#bmi_category  dage i.incomemis 
reg dli c.Factor1_Score#bmi_category  dage i.incomemis, beta

reg dli c.Factor1_Score#bmi_category  dacctot dage i.incomemis
reg dli c.Factor1_Score#bmi_category  dacctot dage i.incomemis, beta

reg dli c.Factor1_Score#bmi_category  combined_mothersupport dacctot dage i.incomemis
reg dli c.Factor1_Score#bmi_category  combined_mothersupport dacctot dage i.incomemis, beta


*Logistic regression models MVPA
reg dtotmvpa c.Factor1_Score#bmi_category
reg dtotmvpa c.Factor1_Score#bmi_category, beta

reg dtotmvpa c.Factor1_Score#bmi_category dage
reg dtotmvpa c.Factor1_Score#bmi_category dage, beta

reg dtotmvpa c.Factor1_Score#bmi_category  dage i.incomemis 
reg dtotmvpa c.Factor1_Score#bmi_category  dage i.incomemis, beta

reg dtotmvpa c.Factor1_Score#bmi_category  dacctot dage i.incomemis
reg dtotmvpa c.Factor1_Score#bmi_category  dacctot dage i.incomemis, beta

reg dtotmvpa c.Factor1_Score#bmi_category  combined_mothersupport dacctot dage i.incomemis
reg dtotmvpa c.Factor1_Score#bmi_category  combined_mothersupport dacctot dage i.incomemis, beta


























************** NOT USED COMMANDS ***********
* linear regressions type of support and total self-report PA

* need to combine total PA of those attending remote schol and those with in-person school 
gen totalpa= dflashatot
replace totalpa= dflashbtot if dflashatot==.

* linear regressions type of support and totalpa (self-reported)
regress mfathsup1 totalpa
regress mfathsup2 totalpa
regress mfathsup3 totalpa
regress mfathsup4 totalpa 
regress mfathsup5 totalpa 
regress mfathsuptot totalpa 

*adjusted by daughters age and BMI 
regress mfathsup1 totalpa mdbmi dage
regress mfathsup2 totalpa mdbmi dage
regress mfathsup3 totalpa mdbmi dage
regress mfathsup4 totalpa mdbmi dage
regress mfathsup5 totalpa mdbmi dage
regress mfathsuptot totalpa mdbmi dage


mfathsup1 mfathsup2 mfathsup3 mfathsup4 mfathsup5

* linear regressions type of support and light PA accelerometer
regress mfathsup1 dli
regress mfathsup2 dli
regress mfathsup3 dli
regress mfathsup4 dli 
regress mfathsup5 dli 
regress mfathsuptot dli 

*adjusted by daughters age and BMI 
regress mfathsup1 dli mdbmi dage
regress mfathsup2 dli mdbmi dage
regress mfathsup3 dli mdbmi dage
regress mfathsup4 dli mdbmi dage
regress mfathsup5 dli mdbmi dage
regress mfathsuptot dli mdbmi dage


* linear regressions type of support and MVPA accelerometer

*unadjusted
regress mfathsup1 dtotmvpa
regress mfathsup2 dtotmvpa
regress mfathsup3 dtotmvpa
regress mfathsup4 dtotmvpa 
regress mfathsup5 dtotmvpa 
regress mfathsuptot dtotmvpa 



*adjusted by daughters age and BMI 
regress mfathsup1 dtotmvpa mdbmi dage
regress mfathsup2 dtotmvpa mdbmi dage
regress mfathsup3 dtotmvpa mdbmi dage
regress mfathsup4 dtotmvpa mdbmi dage
regress mfathsup5 dtotmvpa mdbmi dage
regress mfathsuptot dtotmvpa mdbmi dage





*regression light PA 
regress Factor1_Score dli 
regress Factor1_Score dli mdbmi
regress Factor1_Score dli dage 
regress Factor1_Score dli  dnondirtot
regress Factor1_Score dli dacctot
regress Factor1_Score dli incomemis

regress Factor1_Score dli mdbmi dage dnondirtot dacctot mincome








*OTHER 
**scatterplot
twoway scatter dtotmvpa Factor1_Score || scatter dtotmvpa Factor1_Score if bmi_category == 2, legend(label(1 "Underweight/Normal") label(2 "Overweight/Obese"))



*father dummy 
gen father_dummy =.
replace father_dummy = 0 if Factor1_Score  < 7.953571
replace father_dummy = 1 if Factor1_Score  >= 7.953571 & Factor1_Score !=.


** 2 x 2 table 
mean  dtotmvpa if father_dummy ==0 & bmi_category==1
mean  dtotmvpa if father_dummy ==0 & bmi_category==2
mean  dtotmvpa if father_dummy ==1 & bmi_category==1
mean  dtotmvpa if father_dummy ==1 & bmi_category==2


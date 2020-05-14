#######################
#  ROC   - https://www.youtube.com/watch?v=TZwI0XgcphM
######################

#ND - this is working and generating ROC (need to make sure preds are probabilities... they are little high)
library("ROSE")
require(ROSE)
DDlog<-glm(patientDF.GSTATUS_KI_T ~ Candidate_age.Linear , family = binomial(link="logit"), data=DDdata)
DDlog<-glm(patientDF.GSTATUS_KI_T ~ Candidate_age.Linear+Candidate_age.Missing+Candidate_diabetes_type.Type_1+
             Candidate_diabetes_type.Type_2+Candidate_diabetes_type.Unknown+Candidate_diabetes_type.Missing+
             Candidate_age.50.right+Candidate_ethnicity.Latino+Candidate_race.Asian+Candidate_PVD.Yes+tx_Center_REGION.6+
             Candidate_albumin.4.left+Weight_at_listing.80.right+Candidate_working_for_income.Yes+tx_PREV_TX_ANY.Y+
             Candidate_age.30.left+Recip_dialdur.400.left+Recip_dialdur.1000.right+Candidate_primary_diagnosis.Other+
             tx_Center_REGION.9+Candidate_primary_insurance.Medicare+Candidate_Functional_Status.20+tx_Center_REGION.2+
             Candidate_Functional_Status.10+Height_at_listing.165.right+Candidate_Functional_Status.Missing+Candidate_primary_diagnosis.Diabetes+
             Weight_at_listing.Missing+tx_Center_REGION.3+tx_Center_REGION.10+Recip_dialdur.5000.left+Recip_dialdur.200.left+
             Candidate_albumin.2.8.left+Candidate_Functional_Status.100+Candidate_primary_insurance.Missing+tx_PREV_TX_KI.Y+
             Candidate_primary_diagnosis.ReTX+Candidate_Functional_Status.90+Candidate_previous_malignancy.Missing+tx_Center_REGION.11+
             Candidate_albumin.3.left+Recip_dialdur.6500.left+Recip_dialdur.4500.left+Recip_dialdur.1200.left+Recip_dialdur.1800.left+
             Recip_dialdur.1400.left+Height_at_listing.170.right+Height_at_listing.160.right+Weight_at_listing.125.right+
             Weight_at_listing.115.right+Height_at_listing.175.right+Height_at_listing.180.right+Weight_at_listing.65.left+
             Weight_at_listing.55.left+Candidate_BMI.26.left+Candidate_Functional_Status.50+Candidate_race.Other+Recip_dialdur.2000.left,
             
             family = binomial(link="logit"), data=DDdata)
summary(DDlog)
exp(DDlog$coef)

pred<-predict(DDlog,newdata=DDdata)
Log.predict<-exp(pred)/(1+exp(pred)) #convert to probabilities
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)

Log.predict<-predict(DDlog,newdata=DDdata, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)




DDlog2011<-glm(patientDF.GSTATUS_KI_T ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
                 Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
                 Candidate_diabetes_type.Missing + Recip_dialdur.Linear + 
                 Candidate_ethnicity.Latino + Candidate_race.Asian + Candidate_working_for_income.Yes + 
                 Candidate_PVD.Yes + patientDF.PREMPTIVE + tx_Center_REGION.6 + 
                 tx_PREV_TX_ANY.Y + Candidate_primary_insurance.Medicare + 
                 tx_Center_REGION.2 + Weight_at_listing.Linear + Candidate_primary_diagnosis.Other + 
                 tx_Center_REGION.9 + tx_Center_REGION.10 + Candidate_BMI.Linear + 
                 Candidate_Functional_Status.20 + Candidate_Functional_Status.10 + 
                 Candidate_albumin.Linear + Candidate_albumin.Missing + 
                 tx_Center_REGION.3 + Candidate_previous_malignancy.No + 
                 Candidate_Functional_Status.80 + Candidate_Functional_Status.50 + 
                 Weight_at_listing.Missing + tx_PREV_TX_KI.Y + Candidate_race.Other + 
                 Candidate_primary_diagnosis.Hypertension + Candidate_primary_insurance.Missing + 
                 Candidate_Functional_Status.Missing + Candidate_primary_diagnosis.Glomerulonephritis + 
                 tx_Center_REGION.11 + Candidate_primary_insurance.Medicaid + 
                 Candidate_Functional_Status.30 + Candidate_ABO_AB + Candidate_primary_diagnosis.ReTX + 
                 Candidate_END_cPRA.Linear,
           family = binomial(link="logit"), data=DDdata)
summary(DDlog2011)
Log.predict<-predict(DDlog2011,newdata=DDdata, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)



DDlog2012<-glm(patientDF.GSTATUS_KI_T ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
                 Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
                 Candidate_diabetes_type.Missing + Recip_dialdur.Linear + 
                 Candidate_ethnicity.Latino + Candidate_race.Asian + Candidate_PVD.Yes + 
                 Candidate_working_for_income.Yes + tx_Center_REGION.6 + 
                 tx_PREV_TX_ANY.Y + patientDF.PREMPTIVE + Candidate_primary_insurance.Medicare + 
                 Weight_at_listing.Linear + Candidate_primary_diagnosis.Other + 
                 tx_Center_REGION.2 + tx_Center_REGION.9 + Candidate_Functional_Status.20 + 
                 Candidate_Functional_Status.10 + Candidate_previous_malignancy.No + 
                 Candidate_BMI.Linear + tx_Center_REGION.10 + tx_Center_REGION.3 + 
                 Candidate_albumin.Linear + Candidate_albumin.Missing + 
                 Height_at_listing.Missing + tx_PREV_TX_KI.Y + Candidate_Functional_Status.80 + 
                 Candidate_Functional_Status.50 + Candidate_primary_insurance.Missing + 
                 Candidate_primary_diagnosis.ReTX + Candidate_Functional_Status.Missing + 
                 Weight_at_listing.Missing + Candidate_primary_diagnosis.Diabetes + 
                 Candidate_primary_insurance.Medicaid + Candidate_working_for_income.Missing + 
                 Candidate_PVD.No + Recip_dialdur.Missing + tx_Center_REGION.11 + 
                 Candidate_race.White,
               family = binomial(link="logit"), data=DDdata)
summary(DDlog2012)
Log.predict<-predict(DDlog2012,newdata=DDdata, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)

DDlog2012_365<-glm(patientDF.GSTATUS_KI_T1y ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
                     Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
                     Candidate_diabetes_type.Missing + Recip_dialdur.Linear + 
                     Candidate_PVD.Yes + tx_PREV_TX_ANY.Y + tx_Center_REGION.6 + 
                     Candidate_ethnicity.Latino + Candidate_primary_insurance.Private + 
                     Candidate_Functional_Status.10 + Candidate_race.Asian + 
                     tx_Center_REGION.9 + patientDF.PREMPTIVE + Candidate_albumin.Linear + 
                     Candidate_albumin.Missing + Candidate_Functional_Status.20 + 
                     tx_Center_REGION.7 + Candidate_race.White + Weight_at_listing.Linear + 
                     tx_Center_REGION.8 + Candidate_primary_diagnosis.Other + 
                     Candidate_primary_insurance.Missing + Weight_at_listing.Missing + 
                     tx_Center_REGION.4 + Candidate_END_cPRA.Linear + Candidate_primary_diagnosis.Diabetes + 
                     tx_Center_REGION.2 + Height_at_listing.Linear,
               family = binomial(link="logit"), data=DDdata)
summary(DDlog2012_365)
Log.predict<-predict(DDlog2012_365,newdata=DDdata, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)




DDlog2012_365a<-glm(patientDF.GSTATUS_KI_T1y ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
                      Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
                      Candidate_diabetes_type.Missing + Recip_dialdur.Linear + 
                      Candidate_PVD.Yes + tx_PREV_TX_ANY.Y + Candidate_ethnicity.Latino + 
                      Candidate_race.Asian + Candidate_Functional_Status.10 + 
                      Candidate_primary_insurance.Private + patientDF.PREMPTIVE + 
                      Candidate_albumin.Linear + Candidate_albumin.Missing + 
                      Candidate_Functional_Status.20 + Candidate_END_cPRA.Linear + 
                      Candidate_primary_insurance.Missing + Candidate_race.White + 
                      Candidate_primary_diagnosis.Other + Candidate_BMI.Linear + 
                      Candidate_BMI.Missing + Candidate_primary_diagnosis.Diabetes + 
                      Candidate_working_for_income.Yes,
                   family = binomial(link="logit"), data=DDdata)
summary(DDlog2012_365a)
Log.predict<-predict(DDlog2012_365a,newdata=DDdata, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)



DDlog2012<-glm(patientDF.GSTATUS_KI_T ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
                 Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
                 Candidate_diabetes_type.Missing + Recip_dialdur.Linear + 
                 Candidate_ethnicity.Latino + Candidate_race.Asian + Candidate_PVD.Yes + 
                 Candidate_working_for_income.Yes + tx_Center_REGION.6 + 
                 tx_PREV_TX_ANY.Y + patientDF.PREMPTIVE + Candidate_primary_insurance.Medicare + 
                 Weight_at_listing.Linear + Candidate_primary_diagnosis.Other + 
                 tx_Center_REGION.2 + tx_Center_REGION.9 + Candidate_Functional_Status.20 + 
                 Candidate_Functional_Status.10 + Candidate_previous_malignancy.No + 
                 Candidate_BMI.Linear + tx_Center_REGION.10 + tx_Center_REGION.3 + 
                 Candidate_albumin.Linear + Candidate_albumin.Missing + 
                 Height_at_listing.Missing + tx_PREV_TX_KI.Y + Candidate_Functional_Status.80 + 
                 Candidate_Functional_Status.50 + Candidate_primary_insurance.Missing + 
                 Candidate_primary_diagnosis.ReTX + Candidate_Functional_Status.Missing + 
                 Weight_at_listing.Missing + Candidate_primary_diagnosis.Diabetes + 
                 Candidate_primary_insurance.Medicaid + Candidate_working_for_income.Missing + 
                 Candidate_PVD.No + Recip_dialdur.Missing + tx_Center_REGION.11 + 
                 Candidate_race.White,
               family = binomial(link="logit"), data=DDdata)
summary(DDlog2012)
Log.predict<-predict(DDlog2012,newdata=DDdata, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)


DDlog2013p<-glm(patientDF.GSTATUS_KI_T1y ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
                  Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
                  Candidate_diabetes_type.Missing + Recip_dialdur.Linear + 
                  Candidate_ethnicity.Latino + Candidate_race.Asian + Candidate_PVD.Yes + 
                  Candidate_primary_insurance.Medicare + patientDF.PREMPTIVE + 
                  tx_PREV_TX_ANY.Y + Candidate_working_for_income.Yes + 
                  Candidate_Functional_Status.10 + Candidate_primary_diagnosis.Other + 
                  Candidate_BMI.Linear + Candidate_race.Other + Candidate_Functional_Status.20 + 
                  Candidate_previous_malignancy.No + Candidate_working_for_income.No + 
                  Candidate_Functional_Status.Missing + Candidate_primary_diagnosis.ReTX + 
                  tx_PREV_TX_KI.Y + Candidate_primary_diagnosis.Diabetes + 
                  Candidate_primary_insurance.Missing + Candidate_albumin.Linear + 
                  Candidate_albumin.Missing + Recip_dialdur.Missing + Candidate_END_cPRA.Linear + 
                  Candidate_BMI.Missing + Candidate_Functional_Status.50,
                   family = binomial(link="logit"), data=DDdata)
summary(DDlog2013p)
Log.predict<-predict(DDlog2013p,newdata=DDdata, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)




DDlog2013_365<-glm(patientDF.GSTATUS_KI_T1y ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
                     Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
                     Candidate_diabetes_type.Missing + Recip_dialdur.Linear + 
                     Candidate_PVD.Yes + tx_PREV_TX_ANY.Y + Candidate_ethnicity.Latino + 
                     Candidate_race.Asian + patientDF.PREMPTIVE + Candidate_Functional_Status.10 + 
                     Candidate_primary_insurance.Private + Candidate_BMI.Linear + 
                     Candidate_race.White + Candidate_BMI.Missing + Candidate_Functional_Status.20 + 
                     Candidate_albumin.Linear + Candidate_albumin.Missing + 
                     Candidate_primary_diagnosis.Other + Candidate_primary_insurance.Missing + 
                     Candidate_END_cPRA.Linear + Candidate_Functional_Status.90,
                    family = binomial(link="logit"), data=DDdata)
summary(DDlog2013_365)
Log.predict<-predict(DDlog2013_365,newdata=DDdata, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)



DDlog2014_365<-glm(patientDF.GSTATUS_KI_T1y ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
                     Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
                     Candidate_diabetes_type.Missing + Recip_dialdur.Linear + 
                     Candidate_PVD.Yes + Candidate_Functional_Status.10 + 
                     tx_PREV_TX_ANY.Y + Candidate_ethnicity.Latino + Candidate_race.Asian + 
                     Candidate_primary_insurance.Medicare + patientDF.PREMPTIVE + 
                     Candidate_race.White + Candidate_primary_insurance.Missing + 
                     Candidate_primary_diagnosis.Other + Candidate_BMI.Missing + 
                     Candidate_BMI.Linear + Candidate_albumin.Linear + Candidate_albumin.Missing + 
                     Candidate_END_cPRA.Linear + Candidate_primary_diagnosis.Glomerulonephritis + 
                     Candidate_primary_diagnosis.Hypertension,
                   family = binomial(link="logit"), data=DDdata)
summary(DDlog2014_365)
Log.predict<-predict(DDlog2014_365,newdata=DDdata, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)


DDlog2014p<-glm(patientDF.GSTATUS_KI_T ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
                 Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
                 Candidate_diabetes_type.Missing + Recip_dialdur.Linear + 
                 Candidate_ethnicity.Latino + Candidate_race.Asian + Candidate_PVD.Yes + 
                 patientDF.PREMPTIVE + tx_PREV_TX_ANY.Y + Candidate_working_for_income.Yes + 
                 Candidate_Functional_Status.10 + Candidate_primary_insurance.Medicare + 
                 Candidate_primary_diagnosis.Other + Candidate_race.Other + 
                 Candidate_BMI.Linear + Candidate_BMI.Missing + Candidate_ABO_AB + 
                 Candidate_primary_insurance.Missing + Candidate_Functional_Status.Missing + 
                 Candidate_primary_diagnosis.Hypertension + Candidate_primary_diagnosis.Glomerulonephritis + 
                 Candidate_END_cPRA.Linear + Candidate_albumin.Linear + 
                 Candidate_albumin.Missing + Candidate_Functional_Status.50 + 
                 Candidate_previous_malignancy.Yes
               ,
               family = binomial(link="logit"), data=DDdata)
summary(DDlog2014p)
Log.predict<-predict(DDlog2014p,newdata=DDdata, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)


DDlog2015p<-glm(patientDF.GSTATUS_KI_T ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
                  Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
                  Candidate_diabetes_type.Missing + Recip_dialdur.Linear + 
                  Candidate_ethnicity.Latino + Candidate_race.Asian + Candidate_PVD.Yes + 
                  Candidate_primary_insurance.Private + Candidate_Functional_Status.10 + 
                  patientDF.PREMPTIVE + Candidate_working_for_income.Yes + 
                  tx_PREV_TX_ANY.Y + Candidate_race.Other + Candidate_ABO_O + 
                  Candidate_primary_diagnosis.Diabetes + Candidate_Functional_Status.90 + 
                  Candidate_BMI.Linear + Candidate_BMI.Missing + Candidate_primary_insurance.Missing + 
                  Candidate_primary_insurance.Other + Candidate_Functional_Status.30 + 
                  Candidate_working_for_income.No + Candidate_Functional_Status.Missing + 
                  Candidate_ABO_AB,                ,
                family = binomial(link="logit"), data=DDdata)
summary(DDlog2015p)
Log.predict<-predict(DDlog2015p,newdata=DDdata, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata$patientDF.GSTATUS_KI_T,Log.predict)




#ALL GS  

GS_ALL<-glm(DDdata2$patientDF.GSTATUS_KI_T ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
              Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
              Candidate_diabetes_type.Missing + PHS + KDRI_AGE_ALL + 
              KDRI_AGE_LT18 + KDRI_AGE_GT50 + KDRI_HGT_ALL + KDRI_WGT_LT80 + 
              KDRI_BLACK + KDRI_HYPER + KDRI_DIAB + KDRI_COD_CVA + 
              KDRI_CREAT_ALL + KDRI_CREAT_GT15 + KDRI_HCV + KDRI_DCD + 
              DD_Donor + Recip_dialdur.Linear + Candidate_race.Asian + 
              Candidate_ethnicity.Latino + DD_SHARE_LOCAL + LD_HGT_CM + 
              Candidate_primary_insurance.Private + LD_ABO_INCOMP + 
              DD_COD_ANOXIA + Candidate_Functional_Status.20 + patientDF.PREMPTIVE + 
              Candidate_race.Other + Candidate_working_for_income.Missing + 
              Candidate_ABO_A + LD_HBC + Candidate_primary_insurance.Other + 
              Candidate_BMI.Linear + tx_PREV_TX_ANY.Y + Candidate_Functional_Status.10 + 
              Candidate_working_for_income.No + DD_OTHER_INF + Candidate_Functional_Status.60 + 
              DD_HBC + Candidate_Functional_Status.40 + LD_HIST_CANCER,
              family = binomial(link="logit"), data=DDdata2)
summary(GS_ALL)
Log.predict<-predict(GS_ALL,newdata=DDdata2, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata2$patientDF.GSTATUS_KI_T,Log.predict)


#1Y GS

GS_365<-glm(DDdata2$patientDF.GSTATUS_KI_T1y ~ Candidate_age.Linear + Candidate_age.Missing + Candidate_diabetes_type.Type_1 + 
              Candidate_diabetes_type.Type_2 + Candidate_diabetes_type.Unknown + 
              Candidate_diabetes_type.Missing + PHS + KDRI_AGE_ALL + 
              KDRI_AGE_LT18 + KDRI_AGE_GT50 + KDRI_HGT_ALL + KDRI_WGT_LT80 + 
              KDRI_BLACK + KDRI_HYPER + KDRI_DIAB + KDRI_COD_CVA + 
              KDRI_CREAT_ALL + KDRI_CREAT_GT15 + KDRI_HCV + KDRI_DCD + 
              DD_Donor + Recip_dialdur.Linear + DD_SHARE_LOCAL + Candidate_primary_insurance.Medicare + 
              LD_ABO_INCOMP + Candidate_race.Asian + Candidate_ethnicity.Latino + 
              Candidate_Functional_Status.20 + DD_BLOOD_O + Candidate_race.Other + 
              DD_PULM_INF + LD_HGT_CM + DD_HIST_CANCER + Candidate_Functional_Status.10 + 
              LD_HBC + DD_T4 + DD_COD_ANOXIA + Candidate_BMI.Missing + 
              Candidate_BMI.Linear + patientDF.PREMPTIVE + Candidate_race.White + 
              DD_OTHER_INF + Candidate_PVD.No + Candidate_Functional_Status.40 + 
              LD_UNRELATED,
            family = binomial(link="logit"), data=DDdata2)
summary(GS_365)
Log.predict<-predict(GS_365,newdata=DDdata2, type="response")   #will convert to probabilities by adding type="response")
roc.curve(DDdata2$patientDF.GSTATUS_KI_T1y,Log.predict)

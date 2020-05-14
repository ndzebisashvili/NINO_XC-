#########################
##  Stepwise COX ###
#install.packages("My.stepwise", "Rtools")
require(My.stepwise)

DDdata<-patient_XEvalSTAR
dim(DDdata)
DDdata <- subset(DDdata, (!is.na(DDdata$patientDF.TX_DATE)))
DDdata <- subset(DDdata, patientDF.AGE >= 18)
DDdata <- subset(DDdata, patientDF.Year> 2014) 
#select DD
#DDdata <- subset(DDdata, patientDF.DON_TY ==  'C') 
#Select KI only TX
DDdata <- subset(DDdata, (patientDF.ORGAN == 'KI'))
DDdata <- subset(DDdata, (patientDF.TXHRT == ''))  
DDdata <- subset(DDdata, (patientDF.TXLIV == ''))   
dim(DDdata)
#[>2014]  
#[>2011+]   
#[>2012+]
#[>2013+] 92193  1050
#[>2014+] 78910  1050
#[>2015+] 63831  1050

#Validation - split 75/25

set.seed(12345)
frac<-sample(nrow(DDdata), 0.75*nrow(DDdata))
DDdata1<- DDdata[frac,]
DDdata2<- DDdata[-frac,]

table(DDdata1$patientDF.Year)
table(DDdata2$patientDF.Year)



DDGS<- cbind("Candidate_age.Linear","Candidate_age.Missing",
             "Candidate_diabetes_type.Type_1", "Candidate_diabetes_type.Type_2", "Candidate_diabetes_type.Unknown", "Candidate_diabetes_type.Missing",
             "PHS", "KDRI_AGE_ALL", "KDRI_AGE_LT18", "KDRI_AGE_GT50", "KDRI_HGT_ALL", "KDRI_WGT_LT80", "KDRI_BLACK",
             "KDRI_HYPER", "KDRI_DIAB","KDRI_COD_CVA", "KDRI_CREAT_ALL", "KDRI_CREAT_GT15", "KDRI_HCV", "KDRI_DCD",
             "DD_Donor"
)

DDGS_X<- cbind( "Candidate_BMI.Linear", "Candidate_BMI.Missing",
                
                "Candidate_albumin.Linear", "Candidate_albumin.Missing",
                
                "Recip_dialdur.Linear", "Recip_dialdur.Missing", 
                
                "Candidate_END_cPRA.Linear", "Candidate_END_cPRA.Missing",
                
                "Candidate_primary_diagnosis.Congenital", "Candidate_primary_diagnosis.Diabetes", "Candidate_primary_diagnosis.Glomerulonephritis", 
                "Candidate_primary_diagnosis.Hypertension", "Candidate_primary_diagnosis.ReTX", "Candidate_primary_diagnosis.Other",
                
                "Candidate_Functional_Status.10", "Candidate_Functional_Status.20", "Candidate_Functional_Status.30",  "Candidate_Functional_Status.40", 
                "Candidate_Functional_Status.50", "Candidate_Functional_Status.60", "Candidate_Functional_Status.70", "Candidate_Functional_Status.80",
                "Candidate_Functional_Status.90", "Candidate_Functional_Status.100", "Candidate_Functional_Status.Missing",
                
                "tx_PREV_TX_KI.Y",  "tx_PREV_TX_ANY.Y", 
                
                "Candidate_ABO_A", "Candidate_ABO_AB", "Candidate_ABO_B", "Candidate_ABO_Missing",  "Candidate_ABO_O",
                
                "Candidate_ethnicity.Latino",
                "Candidate_race.White", "Candidate_race.Black", "Candidate_race.Asian", "Candidate_race.Other", "Candidate_race.Missing",
                
                "Candidate_previous_malignancy.Yes", "Candidate_previous_malignancy.No", "Candidate_previous_malignancy.Missing",
                
                "Candidate_PVD.Yes", "Candidate_PVD.No", "Candidate_PVD.Missing",
                
                "Candidate_primary_insurance.Medicaid", "Candidate_primary_insurance.Medicare", "Candidate_primary_insurance.Private", 
                "Candidate_primary_insurance.Other", "Candidate_primary_insurance.Missing",
                
                "Candidate_working_for_income.Yes", "Candidate_working_for_income.No", "Candidate_working_for_income.Missing", "patientDF.PREMPTIVE",
                
                "DD_GENDER_MALE", "LD_GENDER_MALE", "DD_AGE", "DD_BLOOD_A","DD_BLOOD_AB","DD_BLOOD_B","DD_BLOOD_O","DD_BLOOD_MISS",
                "DD_HGT_CM", "DD_WGT_KG", "LD_HGT_CM", "LD_WGT_KG",
                "DD_BUN","DD_PULM_INF","DD_CLIN_INF","DD_eGFR", "DD_BLOOD_INF",
                "DD_HBC", "DD_URINE_INF", "DD_OTHER_INF", "DD_HIST_CANCER",  "DD_PROTEIN_URINE",
                "DD_TATTOOS", "DD_ARGENINE", "DD_DIURETICS", "DD_T4", "DD_SMOKING",  "DD_VASODIALATORS",
                "DD_COD_ANOXIA", "DD_COD_HT", "DD_COD_CNS_TUMOR", "DD_COD_OTHER", "DD_COD_MISSING", "DD_SHARE_LOCAL",
                "DD_SHARE_REGIONAL", "DD_SHARE_NATIONAL", "DD_PUMPED", 
                "Don_coldtime.Linear", "DD_WHITE","DD_ASIAN","DD_HISPANIC",
                
                "LD_AGE", "LD_BLOOD_A", "LD_BLOOD_AB", "LD_BLOOD_B",  "LD_BLOOD_O", "LD_BLOOD_MISS", "LD_HGT_CM", "LD_WGT_KG", "LD_HBC",
                "LD_HIST_CANCER",  "LD_DIURETICS", "LD_SMOKING", "LD_EBV",
                "LD_BMI", "LD_AGE_50","LD_BLACK", "LD_BOTH_MALE", "LD_ABO_INCOMP", "LD_UNRELATED", "LD_DR_WGT_RATIO","LD_BP_SYS",
                "LD_WHITE", "LD_ASIAN", "LD_HISPANIC"
)

my.variable.list <- DDGS_X
#setwd('E:/UNOS')
#sink("DDGS.xls")

#uses DDdata2 (25% sample)
#Graft Survival - Overall (took 75 minutes on my computer, 50 minutes on Azure!!!!)
My.stepwise.coxph (Time = "patientDF.GTIME_KI_T", Status = "patientDF.GSTATUS_KI_T", variable.list = my.variable.list,
                   in.variable = DDGS, data = DDdata2, sle = 0.25, sls = 0.25)











#Graft Survival - 1 year
My.stepwise.coxph (Time = "patientDF.GTIME_KI_T1y", Status = "patientDF.GSTATUS_KI_T1y", variable.list = my.variable.list,
                   in.variable = DDGS, data = DDdata2, sle = 0.25, sls = 0.25)

#Patiet Survival - overall
My.stepwise.coxph (Time = "patientDF.PTIME_T", Status = "patientDF.DIED_TX_T", variable.list = my.variable.list,
                   in.variable = DDGS, data = DDdata2, sle = 0.25, sls = 0.25)

#sink()

#Need to find a way to extract the output into dataframe (somehting similar ro "coef")
#out <- capture.output(summary(My.stepwise.coxph))
#write.csv(summary(test),"test.csv")

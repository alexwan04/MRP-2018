s = finalSet2_NA
s[is.na(s)] <- 0.000001
s = cbind(s,patientSet$Hospital.LOS)
colnames(s)[73] = 'LOS_Cate'
colnames(s)[74] = 'LOS'

# remove test values since they have high correlation with test score
cor(s$ALB.Value,s$ALB.Score)  #0.417966
cor(s$ALP.Value,s$ALP.Score)  #0.8734058
cor(s$ALT.Value,s$ALT.Score)  #0.9096451
cor(s$AST.Value,s$AST.Score)  #0.9706466
cor(s$CA.Value,s$CA.Score)    #0.6713375
cor(s$CR.Value,s$CR.Score)    #0.9279048
cor(s$GLUR.Value,s$GLUR.Score)#0.8975342
cor(s$HGB.Value,s$HGB.Score)  # -0.172285
cor(s$IWBCR.Value,s$IWBCR.Score)#0.8860275
cor(s$K.Value,s$K.Score)      # 0.401948
cor(s$MCV.Value,s$MCV.Score)  #0.3880479
cor(s$PLT.Value,s$PLT.Score)  #0.1753233
cor(s$K.Value,s$K.Score)      #0.401948
cor(s$Sodium.Value,s$Sodium.Score)#0.2291473
cor(s$TNI.Value,s$TNI.Score)  #0.9818614
cor(s$IWBCR.Value,s$IWBCR.Score)#0.8860275
cor(s$VLACT.Value,s$VLACT.Score)#0.9335119

s$ALB.Score = NULL
s$ALP.Score = NULL
s$ALT.Score = NULL
s$AST.Score = NULL
s$CA.Score = NULL
s$CR.Score = NULL
s$GLUR.Score = NULL
s$HGB.Score = NULL
s$IWBCR.Score = NULL
s$K.Score = NULL
s$MCV.Score = NULL
s$PLT.Score = NULL
s$Sodium.Score = NULL
s$TNI.Score = NULL
s$VLACT.Score = NULL

s$LOS_Cate = NULL

s$ALB_Count = NULL
s$ALP_Count = NULL
s$ALT_Count = NULL
s$AST_Count = NULL
s$CA_Count = NULL
s$CR_Count = NULL
s$GLUR_Count = NULL
s$HGB_Count = NULL
s$IWBCR_Count = NULL
s$K_Count = NULL
s$MCV_Count = NULL
s$PLT_Count = NULL
s$Sodium_Count = NULL
s$TNI_Count = NULL
s$VLACT_Count = NULL

s$GIM_ER.CONSULT = NULL
s$TotalConsult_Count = NULL
# remove hasEncounter = -1
s <- s[s$hasEncounter!=-1,]
s <- s[s$readmissionFlag!=-1,]

# analysis on dependent var

# min = 3   max = 15090
# mean = 203.6609    med = 115   IQR = 156

boxplot(set_27var$LOS)
boxplot(set_27var$LOS,outline=FALSE) # no outliers

# bar chart of variables
counts = table(s$Admitting.Service)
b=barplot(counts, ylim = c(0,4500),xlab = "Admitting Service")
text(x=b,y=counts+100,labels = as.character(counts))

counts = table(s$LanguageType)
b=barplot(counts, ylim = c(0,18000),xlab = "Language Type")
text(x=b,y=counts+1000,labels = as.character(counts))

counts = table(set_27var$Gender)
b=barplot(counts, ylim = c(0,10000),xlab = "Gender")
text(x=b,y=counts+250,labels = as.character(counts))

counts = table(s$Admit.via.Ambulance)
b=barplot(counts, ylim = c(0,10000),xlab = "Admit via Ambulance")
text(x=b,y=counts+250,labels = as.character(counts))

counts = table(s$Triage.Level)
b=barplot(counts, ylim = c(0,10000),xlab = "Triage Level")
text(x=b,y=counts+250,labels = as.character(counts))

counts = table(set_27var$TriageMonth)
b=barplot(counts, ylim = c(0,1550),xlab = "Triage Month")
text(x=b,y=counts+75,labels = as.character(counts))

counts = table(set_27var$TriageDayOfWeek)
b=barplot(counts, ylim = c(0,3000),xlab = "Triage Day of Week")
text(x=b,y=counts+150,labels = as.character(counts))
set_27var$TriageDayOfWeek=droplevels(set_27var$TriageDayOfWeek)

counts = table(set_27var$NON_GIM_ER.CONSULT)
b=barplot(counts, ylim = c(0,16000),xlab = "Non GIM ER Consult")
text(x=b,y=counts+500,labels = as.character(counts))
set_27var$NON_GIM_ER.CONSULT=droplevels(set_27var$NON_GIM_ER.CONSULT)

counts = table(s$ConsultType_Count)
b=barplot(counts, ylim = c(0,16000),xlab = "Consult Type Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$TotalConsult_Count)
b=barplot(counts, ylim = c(0,16000),xlab = "Total Consult Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$DiagType3_Count)
b=barplot(counts, ylim = c(0,11000),xlab = "Diag Type3 Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$readmissionFlag)
b=barplot(counts, ylim = c(0,16000),xlab = "Readmission Flag")
text(x=b,y=counts+500,labels = as.character(counts))

hist(set_27var$WaitTime.to.Admit, xlab = 'Wait time to admit', main = 'Histogram of Wait Time to Admit')

counts = table(s$hasEncounter)
b=barplot(counts, ylim = c(0,16000),xlab = "Has Encounter")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$TriageHourF)
b=barplot(counts, ylim = c(0,11000),xlab = "Triage Hour F")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$ER.Code.new)
b=barplot(counts, ylim = c(0,8600),xlab = "ER Code New", cex.names =0.5 )
text(x=b,y=counts+500,labels = as.character(counts), cex= 0.5)

counts = table(s$AgeF)
b=barplot(counts, ylim = c(0,7000),xlab = "Age F", cex.names = .7)
text(x=b,y=counts+500,labels = as.character(counts))

hist(s$totalLow,xlab="Total Low",main="Histogram of Total Low")

hist(s$totalHigh,xlab="Total High",main="Histogram of Total High")

set_27var$CT_HEAD_Count=droplevels(set_27var$CT_HEAD_Count)
counts = table(set_27var$CT_HEAD_Count)
b=barplot(counts, ylim = c(0,14000),xlab = "CT Head Count")
text(x=b,y=counts+500,labels = as.character(counts))

set_27var$POR_CHEST_Count=droplevels(set_27var$POR_CHEST_Count)
counts = table(set_27var$POR_CHEST_Count)
b=barplot(counts, ylim = c(0,15000),xlab = "POR Chest Count")
text(x=b,y=counts+500,labels = as.character(counts))

set_27var$RAD_CHEST_Count=droplevels(set_27var$RAD_CHEST_Count)
counts = table(s$RAD_CHEST_Count)
b=barplot(counts, ylim = c(0,11000),xlab = "RAD Chest Count")
text(x=b,y=counts+500,labels = as.character(counts))

set_27var$RAD_LOWER_EXT_Count=droplevels(set_27var$RAD_LOWER_EXT_Count)
counts = table(set_27var$RAD_LOWER_EXT_Count)
b=barplot(counts, ylim = c(0,17500),xlab = "RAD Lower EXT Count")
text(x=b,y=counts+500,labels = as.character(counts))

set_27var$RAD_PELVIS_HIP_Count=droplevels(set_27var$RAD_PELVIS_HIP_Count)
counts = table(set_27var$RAD_PELVIS_HIP_Count)
b=barplot(counts, ylim = c(0,17500),xlab = "RAD Pelvis Hip Count")
text(x=b,y=counts+500,labels = as.character(counts))



counts = table(s$RADtype_Count)
b=barplot(counts, ylim = c(0,9500),xlab = "RAD Type Count")
text(x=b,y=counts+500,labels = as.character(counts))

hist(s$numTestTypesDone,xlab = "# of Tests Done",main="Histogram of # of Tests Done")

hist(s$ALB.Score,xlab = 'ALB Score', main="Histogram of ALB Score")
hist(s$ALP.Score,xlab = 'ALP Score', main="Histogram of ALP Score")
hist(s$ALT.Score,xlab = 'ALT Score', main="Histogram of ALT Score")
hist(s$AST.Score,xlab = 'AST Score', main="Histogram of AST Score")
hist(s$CA.Score,xlab = 'CA Score', main="Histogram of CA Score")
hist(s$CR.Score,xlab = 'CR Score', main="Histogram of CR Score")
hist(s$GLUR.Score,xlab = 'GLUR Score', main="Histogram of GLUR Score")
hist(s$HGB.Score,xlab = 'HGB Score', main="Histogram of HGB Score")
hist(s$IWBCR.Score,xlab = 'IWBCR Score', main="Histogram of IWBCR Score")
hist(s$K.Score,xlab = 'K Score', main="Histogram of K Score")
hist(s$MCV.Score,xlab = 'CMCV Score', main="Histogram of MCV Score")
hist(s$PLT.Score,xlab = 'PLT Score', main="Histogram of PLT Score")
hist(s$Sodium.Score,xlab = 'Sodium Score', main="Histogram of Sodium Score")
hist(s$TNI.Score,xlab = 'TNI Score', main="Histogram of TNI Score")
hist(s$VLACT.Score,xlab = 'VLACT Score', main="Histogram of VLACT Score")

counts = table(s$ALB_Count)
b=barplot(counts, ylim = c(0,11500),xlab = "ALB Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$ALP_Count)
b=barplot(counts, ylim = c(0,11500),xlab = "ALP Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$ALT_Count)
b=barplot(counts, ylim = c(0,11500),xlab = "ALT Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$AST_Count)
b=barplot(counts, ylim = c(0,11500),xlab = "AST Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$CA_Count)
b=barplot(counts, ylim = c(0,11500),xlab = "CA Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$CR_Count)
b=barplot(counts, ylim = c(0,17000),xlab = "CR Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$GLUR_Count)
b=barplot(counts, ylim = c(0,17000),xlab = "GLUR Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$HGB_Count)
b=barplot(counts, ylim = c(0,17000),xlab = "HGB Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$IWBCR_Count)
b=barplot(counts, ylim = c(0,17000),xlab = "IWBCR Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$K_Count)
b=barplot(counts, ylim = c(0,17000),xlab = "K Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$MCV_Count)
b=barplot(counts, ylim = c(0,17000),xlab = "MCV Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$PLT_Count)
b=barplot(counts, ylim = c(0,17000),xlab = "PLT Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$Sodium_Count)
b=barplot(counts, ylim = c(0,17000),xlab = "Sodium Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$TNI_Count)
b=barplot(counts, ylim = c(0,12000),xlab = "TNI Count")
text(x=b,y=counts+500,labels = as.character(counts))

counts = table(s$VLACT_Count)
b=barplot(counts, ylim = c(0,14000),xlab = "VLACT Count")
text(x=b,y=counts+500,labels = as.character(counts))

saveRDS(s,file = "s.rds")

# remove outliers (LOS > 4000h)
s = subset(s,s$LOS <=4000)
# change gender to Boolean: M = 1 F = 2
i = 1
levels(set_27var$Gender) = c(levels(set_27var$Gender),c(1,2))
while (i <= 16371){
  if (set_27var$Gender[[i]] == 1){
    set_27var$Gender[[i]] = "M"
    i = i + 1
  }
  else {
    set_27var$Gender[[i]] = "F"
    i = i + 1
  }
}
set_27var$Gender=factor(set_27var$Gender)
# change admit.via.ambulance to Boolean: N = 1 G = 2
i = 1
levels(s$Admit.via.Ambulance) = c(levels(s$Admit.via.Ambulance),c(1,2))
while (i <= 16371){
  if (s$Admit.via.Ambulance[[i]] == "N"){
    s$Admit.via.Ambulance[[i]] = 1
    i = i + 1
  }
  else {
    s$Admit.via.Ambulance[[i]] = 2
    i = i + 1
  }
}

# change NON_GIM_ER.Consult to Boolean: N = 1 G = 2
i = 1
levels(s$NON_GIM_ER.CONSULT) = c(levels(s$NON_GIM_ER.CONSULT),c(1,2))
while (i <= 16371){
  if (s$NON_GIM_ER.CONSULT[[i]] == "0"){
    s$NON_GIM_ER.CONSULT[[i]] = 1
    i = i + 1
  }
  else {
    s$NON_GIM_ER.CONSULT[[i]] = 2
    i = i + 1
  }
}
# change LanguageType to Boolean: Non_English = 1 English = 2
i = 1
levels(s$LanguageType) = c(levels(s$LanguageType),c(1,2))
while (i <= 16371){
  if (s$LanguageType[[i]] == "Non_English"){
    s$LanguageType[[i]] = 1
    i = i + 1
  }
  else {
    s$LanguageType[[i]] = 2
    i = i + 1
  }
}
saveRDS(s,file = "s.rds")

# change Admitting service to numeric
# TMA = 1
# TMB = 2
# TMC = 3
# TMD = 4
# TME = 5

i = 1
levels(s$Admitting.Service) = c(levels(s$Admitting.Service),c(1,2,3,4,5))

while (i<=16371){
  if (s$Admitting.Service[[i]] == "TMA"){
    s$Admitting.Service[[i]] = 1
    i = i + 1
  }
  else if (s$Admitting.Service[[i]] == "TMB"){
    s$Admitting.Service[[i]] = 2
    i = i + 1
  }
  else if (s$Admitting.Service[[i]] == "TMC"){
    s$Admitting.Service[[i]] = 3
    
    i = i + 1
  }
  else if (s$Admitting.Service[[i]] == "TMD"){
    s$Admitting.Service[[i]] = 4
    i = i + 1
  }
  else {
    s$Admitting.Service[[i]] = 5
    i = i + 1
  }
}
# change day of week in numeric 
# sunday = 1
# monday = 2
# tuesday = 3
# wednesday = 4
# thursday = 5
# friday = 6
# saturday = 7
i = 1
levels(s$TriageDayOfWeek) = c(levels(s$TriageDayOfWeek),c(1,2,3,4,5,6,7))
while (i <= 16371){
  if (s$TriageDayOfWeek[[i]] == "Sun"){
    s$TriageDayOfWeek[[i]] = 1
    i = i + 1
  }
  else if (s$TriageDayOfWeek[[i]] == "Mon"){
    s$TriageDayOfWeek[[i]] = 2
    i = i + 1
  }
  else if (s$TriageDayOfWeek[[i]] == "Tue"){
    s$TriageDayOfWeek[[i]] = 3
    i = i + 1
  }
  else if (s$TriageDayOfWeek[[i]] == "Wed"){
    s$TriageDayOfWeek[[i]] = 4
    i = i + 1
  }
  else if (s$TriageDayOfWeek[[i]] == "Thu"){
    s$TriageDayOfWeek[[i]] = 5
    i = i + 1
  }
  else if (s$TriageDayOfWeek[[i]] == "Fri"){
    s$TriageDayOfWeek[[i]] = 6
    i = i + 1
  }
  else if (s$TriageDayOfWeek[[i]] == "Sat"){
    s$TriageDayOfWeek[[i]] = 7
    i = i + 1
  }
}

# change age categories to 8 indicator variables
age15.30 = data.frame(); 
age30.40 = data.frame(); 
age40.50 = data.frame(); 
age50.60 = data.frame(); 
age60.70 = data.frame(); 
age70.80 = data.frame(); 
age80.90 = data.frame(); 
age90.113 = data.frame(); 

i = 1
while (i<=16371){
  if (s$AgeF[[i]] == "[ 15, 30)"){
    age15.30 = rbind(age15.30,2)
    age30.40 = rbind(age30.40,1)
    age40.50 = rbind(age40.50,1)
    age50.60 = rbind(age50.60,1)
    age60.70 = rbind(age60.70,1)
    age70.80 = rbind(age70.80,1)
    age80.90 = rbind(age80.90,1)
    age90.113 = rbind(age90.113,1)
    i = i + 1
  }
  else if (s$AgeF[[i]] == "[ 30, 40)"){
    age15.30 = rbind(age15.30,1)
    age30.40 = rbind(age30.40,2)
    age40.50 = rbind(age40.50,1)
    age50.60 = rbind(age50.60,1)
    age60.70 = rbind(age60.70,1)
    age70.80 = rbind(age70.80,1)
    age80.90 = rbind(age80.90,1)
    age90.113 = rbind(age90.113,1)
    i = i + 1
  }
  else if (s$AgeF[[i]] == "[ 40, 50)"){
    age15.30 = rbind(age15.30,1)
    age30.40 = rbind(age30.40,1)
    age40.50 = rbind(age40.50,2)
    age50.60 = rbind(age50.60,1)
    age60.70 = rbind(age60.70,1)
    age70.80 = rbind(age70.80,1)
    age80.90 = rbind(age80.90,1)
    age90.113 = rbind(age90.113,1)
    i = i + 1
  }
  else if (s$AgeF[[i]] == "[ 50, 60)"){
    age15.30 = rbind(age15.30,1)
    age30.40 = rbind(age30.40,1)
    age40.50 = rbind(age40.50,1)
    age50.60 = rbind(age50.60,2)
    age60.70 = rbind(age60.70,1)
    age70.80 = rbind(age70.80,1)
    age80.90 = rbind(age80.90,1)
    age90.113 = rbind(age90.113,1)
    i = i + 1
  }
  else if (s$AgeF[[i]] == "[ 60, 70)"){
    age15.30 = rbind(age15.30,1)
    age30.40 = rbind(age30.40,1)
    age40.50 = rbind(age40.50,1)
    age50.60 = rbind(age50.60,1)
    age60.70 = rbind(age60.70,2)
    age70.80 = rbind(age70.80,1)
    age80.90 = rbind(age80.90,1)
    age90.113 = rbind(age90.113,1)
    i = i + 1
  }
  else if (s$AgeF[[i]] == "[ 70, 80)"){
    age15.30 = rbind(age15.30,1)
    age30.40 = rbind(age30.40,1)
    age40.50 = rbind(age40.50,1)
    age50.60 = rbind(age50.60,1)
    age60.70 = rbind(age60.70,1)
    age70.80 = rbind(age70.80,2)
    age80.90 = rbind(age80.90,1)
    age90.113 = rbind(age90.113,1)
    i = i + 1
  }
  else if (s$AgeF[[i]] == "[ 80, 90)"){
    age15.30 = rbind(age15.30,1)
    age30.40 = rbind(age30.40,1)
    age40.50 = rbind(age40.50,1)
    age50.60 = rbind(age50.60,1)
    age60.70 = rbind(age60.70,1)
    age70.80 = rbind(age70.80,1)
    age80.90 = rbind(age80.90,2)
    age90.113 = rbind(age90.113,1)
    i = i + 1
  }
  else{
    age15.30 = rbind(age15.30,1)
    age30.40 = rbind(age30.40,1)
    age40.50 = rbind(age40.50,1)
    age50.60 = rbind(age50.60,1)
    age60.70 = rbind(age60.70,1)
    age70.80 = rbind(age70.80,1)
    age80.90 = rbind(age80.90,1)
    age90.113 = rbind(age90.113,2)
    i = i + 1
  }
}
colnames(age15.30) = c("Age15.30")
colnames(age30.40) = c("Age30.40")
colnames(age40.50) = c("Age40.50")
colnames(age40.50) = c("Age40.50")
colnames(age50.60) = c("Age50.60")
colnames(age60.70) = c("Age60.70")
colnames(age70.80) = c("Age70.80")
colnames(age80.90) = c("Age80.90")
colnames(age90.113) = c("Age90.113")

# change ER code to 22 indicator variables
ER.A09 = data.frame(); colnames(ER.A09) = c("ER.A09")
ER.A41 = data.frame(); colnames(ER.A41) = c("ER.A41")
ER.E87 = data.frame(); colnames(ER.E87) = c("ER.E87")
ER.F05 = data.frame(); colnames(ER.F05) = c("ER.F05")
ER.I50 = data.frame(); colnames(ER.I50) = c("ER.I50")
ER.I64 = data.frame(); colnames(ER.I64) = c("ER.I64")
ER.J18 = data.frame(); colnames(ER.J18) = c("ER.J18")
ER.J44 = data.frame(); colnames(ER.J44) = c("ER.J44")
ER.K85 = data.frame(); colnames(ER.K85) = c("ER.K85")
ER.K92 = data.frame(); colnames(ER.K92) = c("ER.K92")
ER.L03 = data.frame(); colnames(ER.L03) = c("ER.L03")
ER.N17 = data.frame(); colnames(ER.N17) = c("ER.N17")
ER.N39 = data.frame(); colnames(ER.N39) = c("ER.N39")
ER.OTHER = data.frame(); colnames(ER.OTHER) = c("ER.OTHER")
ER.R06 = data.frame(); colnames(ER.R06) = c("ER.R06")
ER.R10 = data.frame(); colnames(ER.R10) = c("ER.R10")
ER.R41 = data.frame(); colnames(ER.R41) = c("ER.R41")
ER.R50 = data.frame(); colnames(ER.R50) = c("ER.R50")
ER.R53 = data.frame(); colnames(ER.R53) = c("ER.R53")
ER.R55 = data.frame(); colnames(ER.R55) = c("ER.R55")
ER.R64 = data.frame(); colnames(ER.R64) = c("ER.R64")
ER.XX = data.frame(); colnames(ER.XX) = c("ER.XX")

i = 1
while (i<=16371){
  if (s$ER.Code.new[[i]] == "A09"){
    ER.A09 = rbind(ER.A09,1);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "A41"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,1);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "E87"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,1);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "F05"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,1);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "I50"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,1);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "I64"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,1);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "J18"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,1);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "J44"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,1);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "K85"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,1);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "K92"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,1);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "L03"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,1);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "N17"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,1);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "N39"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,1);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "OTHER"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,1);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "R06"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,1);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "R10"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,1);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "R41"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,1);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "R50"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,1);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "R53"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,1);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,1);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "R55"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,1);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else if (s$ER.Code.new[[i]] == "R64"){
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,1);    ER.XX = rbind(ER.XX,0);
    i = i + 1
  }
  else{
    ER.A09 = rbind(ER.A09,0);    ER.A41 = rbind(ER.A41,0);    ER.E87 = rbind(ER.E87,0);    ER.F05 = rbind(ER.F05,0);
    ER.I50 = rbind(ER.I50,0);    ER.I64 = rbind(ER.I64,0);    ER.J18 = rbind(ER.J18,0);    ER.J44 = rbind(ER.J44,0);
    ER.K85 = rbind(ER.K85,0);    ER.K92 = rbind(ER.K92,0);    ER.L03 = rbind(ER.L03,0);    ER.N17 = rbind(ER.N17,0);
    ER.N39 = rbind(ER.N39,0);    ER.OTHER = rbind(ER.OTHER,0);    ER.R06 = rbind(ER.R06,0);    ER.R10 = rbind(ER.R10,0);
    ER.R41 = rbind(ER.R41,0);    ER.R50 = rbind(ER.R50,0);    ER.R53 = rbind(ER.R53,0);    ER.R55 = rbind(ER.R55,0);
    ER.R64 = rbind(ER.R64,0);    ER.XX = rbind(ER.XX,1);
    i = i + 1
  }
  
}

s$AgeF = NULL
s$ER.Code.new = NULL
s$readmissionFlag = NULL
#current size
#5*2*2*2*5*12*7*2*3*3*59*2*3*47*250*124*189*2*318*17*158*21*5*103*450*142*342*5*16*10*14*2*2*2*2*2*8 
final = cbind(s,age15.30,age30.40,age40.50,age50.60,age60.70,age70.80,age80.90,age90.113)

final = final[c("Admitting.Service", "LanguageType","Gender","Admit.via.Ambulance","Triage.Level","TriageMonth",
                "TriageDayOfWeek","NON_GIM_ER.CONSULT","ConsultType_Count","DiagType3_Count","readmissionFlag","WaitTime.to.Admit",
                "hasEncounter", "TriageHourF" ,"Age15.30","Age30.40" ,"Age40.50","Age50.60","Age60.70", "Age70.80","Age80.90","Age90.113",
                "ALB.Value","ALP.Value","ALT.Value","AST.Value","CA.Value","CR.Value","GLUR.Value","HGB.Value","IWBCR.Value" ,
                "K.Value","MCV.Value","PLT.Value","Sodium.Value","TNI.Value" ,"VLACT.Value","numTestTypesDone", "totalLow","totalHigh",
                "CT_HEAD_Count","POR_CHEST_Count", "RAD_CHEST_Count","RAD_LOWER_EXT_Count" ,"RAD_PELVIS_HIP_Count","RADtype_Count",
                "LOS")]
saveRDS(final,file = "final.rds")
write.csv(final,file = 'final.csv')

# remove variables to lower the dimension
# keep CT_HEAD, RAD_CHEST, etc.
# Remove ER codes since they're 22 indicator variables

# Tips on how to remove variables:
# https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
library(caret)
cor_mat = cor(final)
print(cor_mat)
highlyCorrelated <- findCorrelation(cor_mat, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
# index of variables to remove:  60 55 57 62 54 53 45 46 47  8 16 13
# NON_GIM_er_consult, hasencounter, triagehourF

# Normali TriageHourF: 0->1 8->2  17->3
i = 1
levels(final$TriageHourF) = c(levels(final$TriageHourF),c(1,2,3))
while (i <= 16371){
  if (final$TriageHourF[[i]] == "0"){
    final$TriageHourF[[i]] = 1
    i = i + 1
  }
  else if (final$TriageHourF[[i]] == "8"){
    final$TriageHourF[[i]] = 1
    i = i + 1
  }
  else {
    final$TriageHourF[[i]] = 3
    i = i + 1
  }
}

# Remove variables to lower complexity
s$Admitting.Service = NULL
s$LanguageType = NULL
s$Admit.via.Ambulance = NULL
final$Triage.Level = NULL
final$DiagType3_Count = NULL
s$ConsultType_Count = NULL
s$hasEncounter = NULL
s$numTestTypesDone = NULL
s$TriageHourF = NULL
final$totalHigh = NULL
final$totalLow = NULL
final$RADtype_Count = NULL

#make CA (max 2.5) K (max 5.4) TNI (max 0.34265) VLACT (max 5.4) bigger
s$CA.Value = s$CA.Value*10
s$K.Value = s$K.Value*10
s$TNI.Value = s$TNI.Value*100
s$VLACT.Value = s$VLACT.Value*10

#floor all test values to be whole numbers for tensor indexing
s$ALB.Value = floor(s$ALB.Value)
s$ALP.Value = floor(s$ALP.Value)
s$ALT.Value = floor(s$ALT.Value)
s$AST.Value = floor(s$AST.Value)
s$CA.Value = floor(s$CA.Value)
s$CR.Value = floor(s$CR.Value)
s$GLUR.Value = floor(s$GLUR.Value)
s$HGB.Value = floor(s$HGB.Value)
s$IWBCR.Value = floor(s$IWBCR.Value)
s$K.Value = floor(s$K.Value)
s$MCV.Value = floor(s$MCV.Value)
s$PLT.Value = floor(s$PLT.Value)
s$Sodium.Value = floor(s$Sodium.Value)
s$TNI.Value = floor(s$TNI.Value)
s$VLACT.Value = floor(s$VLACT.Value)

s[s == 0] = 0.0001
s[is.na(s)] <- 0
s[s == 0.0001] = 1

i = 1
levels(final$hasEncounter) = c(levels(final$hasEncounter),c(1,2))
while (i <= 16371){
  if (final$hasEncounter[[i]] == 0){
    final$hasEncounter[[i]] = 1
    i = i + 1
  }
  else {
    final$hasEncounter[[i]] = 2
    i = i + 1
  }
}

i = 1
levels(final$CT_HEAD_Count) = c(levels(final$CT_HEAD_Count),c(1,2))
while (i <= 16371){
  if (final$CT_HEAD_Count[[i]] == 0){
    final$CT_HEAD_Count[[i]] = 1
    i = i + 1
  }
  else {
    final$CT_HEAD_Count[[i]] = 2
    i = i + 1
  }
}

i = 1
levels(final$POR_CHEST_Count) = c(levels(final$POR_CHEST_Count),c(1,2))
while (i <= 16371){
  if (final$POR_CHEST_Count[[i]] == 0){
    final$POR_CHEST_Count[[i]] = 1
    i = i + 1
  }
  else {
    final$POR_CHEST_Count[[i]] = 2
    i = i + 1
  }
}

i = 1
levels(final$RAD_CHEST_Count) = c(levels(final$RAD_CHEST_Count),c(1,2))
while (i <= 16371){
  if (final$RAD_CHEST_Count[[i]] == 0){
    final$RAD_CHEST_Count[[i]] = 1
    i = i + 1
  }
  else {
    final$RAD_CHEST_Count[[i]] = 2
    i = i + 1
  }
}
i = 1
levels(final$RAD_LOWER_EXT_Count) = c(levels(final$RAD_LOWER_EXT_Count),c(1,2))
while (i <= 16371){
  if (final$RAD_LOWER_EXT_Count[[i]] == 0){
    final$RAD_LOWER_EXT_Count[[i]] = 1
    i = i + 1
  }
  else {
    final$RAD_LOWER_EXT_Count[[i]] = 2
    i = i + 1
  }
}

i = 1
levels(final$RAD_PELVIS_HIP_Count) = c(levels(final$RAD_PELVIS_HIP_Count),c(1,2))
while (i <= 16371){
  if (final$RAD_PELVIS_HIP_Count[[i]] == 0){
    final$RAD_PELVIS_HIP_Count[[i]] = 1
    i = i + 1
  }
  else {
    final$RAD_PELVIS_HIP_Count[[i]] = 2
    i = i + 1
  }
}

saveRDS(set_27var,file="set_27var.rds")
write.csv(set_27var,file = 'set_27var.csv')

final$ALB.Value = NULL
final$ALP.Value = NULL
final$ALT.Value = NULL
final$AST.Value = NULL
final$CA.Value = NULL
final$CR.Value = NULL
final$GLUR.Value = NULL
final$HGB.Value = NULL
final$IWBCR.Value = NULL
final$K.Value = NULL
final$MCV.Value = NULL
final$PLT.Value = NULL
final$Sodium.Value = NULL
final$TNI.Value = NULL
final$VLACT.Value = NULL

saveRDS(set_27var,file="set_11var.rds")
write.csv(set_27var,file = 'set_11var.csv')

# See if I can add back some variables

final$numTestTypesDone=as.numeric(final$numTestTypesDone)+1
final$readmissionFlag=as.numeric(final$readmissionFlag)-2
final$ConsultType_Count=as.numeric(final$ConsultType_Count)+1
final$WaitTime.to.Admit=as.numeric(final$WaitTime.to.Admit)+1

saveRDS(final,file="final27var.rds")
write.csv(final,file = 'final27var.csv')

# split data into training / testing data
set.seed(123)
setsample = sample.int(n = nrow(set_27var), size = floor(.75*nrow(set_27var)), replace = F)
smh_train = set_27var[setsample,]
smh_test = set_27var[-setsample,]

write.csv(smh_train, file = "smh_train.csv")
write.csv(smh_test, file = "smh_test.csv")

# num of CT Head = 3421
# num of POR chest = 2077
# num of RAD chest = 6998
# num of RAD lower ext = 519
# num of RAD pelvis hip = 506

fit = lm(LOS~.,data = set_27var)
summary(fit)
y=predict(fit,set_27var)
mean(abs(set_27var$LOS-y)) #156.7804

fit = lm(LOS~., data = smh_train)
LR_ans=predict(fit,smh_test)
mean(abs(smh_test$LOS-y)) #154.3335

library(corrplot)
d = set_27var
d$LOS=NULL
indx = sapply(d, is.factor)
d[indx]= lapply(d[indx],function(x) as.numeric(as.character(x)))
d_cor = cor(d, method = "pearson")
d_plot = corrplot(d_cor, method = 'circle')

install.packages("R.matlab")
library(R.matlab)

SVD_ans = readMat("SVD_ans.mat")
SVD_ans = SVD_ans$y

TD_ans = readMat("TD_ans.mat")
TD_ans = TD_ans$Xhisto

KW = data.frame(set_27var$LOS,TD_ans,LR_ans,SVD_ans)
Error_rates = data.frame(abs(set_27var$LOS-TD_ans),
                         abs(set_27var$LOS-LR_ans),
                         abs(set_27var$LOS-SVD_ans))
colnames(Error_rates)=c("TD","LR","SVD")

i=1
mu=c()
while(i<=16371){
  mu=c(mu,mean(as.numeric(Error_rates[i,])))
  i=i+1
}
R=c(abs(set_27var$LOS-TD_ans),abs(set_27var$LOS-LR_ans),abs(set_27var$LOS-SVD_ans))
R=sort(R)
R[24557] # = 92

rank=Error_rates
i=1
while(i<=3){
  j=1
  while(j<=16371){
    x=1
    while(x<=49113){
      if(rank[j,i]==R[x]){
        rank[j,i]=x
        j=j+1
        x=49114
      }
      else{
        x=x+1
      }
    }
  }
  i=i+1
  print(i)
}

TD_error = abs(set_27var$LOS-TD_ans)
LR_error = abs(set_27var$LOS-LR_ans)
SVD_error = abs(set_27var$LOS-SVD_ans)

TD_rank=c()
LR_rank=c()
SVD_rank=c()

j=1
while(j<=16371){
  x=1
  while(x<=49113){
    if(TD_error[j]==R[x]){
      TD_rank[j]=x
      j=j+1
      x=49114
    }
    else{
      x=x+1
    }
  }
}

j=1
while(j<=16371){
  x=1
  while(x<=49113){
    if(LR_error[j]==R[x]){
      LR_rank[j]=x
      j=j+1
      x=49114
    }
    else{
      x=x+1
    }
  }
}

j=1
while(j<=16371){
  x=1
  while(x<=49113){
    if(SVD_error[j]==R[x]){
      SVD_rank[j]=x
      j=j+1
      x=49114
    }
    else{
      x=x+1
    }
  }
}

mean(TD_rank) # 14494
mean(LR_rank) # 27782
mean(SVD_rank) #29542

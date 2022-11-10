rm(list=ls())
options(stringsAsFactors = FALSE)

###--------------------------------------------------------------------------###

# load R data
load("../RA.RData")
phe <- subset(RA_data, select=c("Sample_ID", "Disease_Status", "Age", "Gender", "RF", 
                                "Anti_CCP", "Disease_Duration_Months", "Other_Disease_Status", 
                                "Disease_Type", "Family_History_Status", "Family_members_effected"))

###--------------------------------------------------------------------------###
###
###--------------------------------------------------------------------------###

# Read file
cnv <- read.table("FCGR3B.txt", header = T, sep = "\t")

d1 <- subset(cnv, Sample.Name!="")
d1$Sample_ID <- gsub(" ","", d1$Sample.Name)

str(d1)

###--------------------------------------------------------------------------###

fd1 <- merge(d1, phe, all.x=TRUE)

###--------------------------------------------------------------------------###

# str(fd1)

table(fd1$Disease_Status, useNA="ifany")

# fd1$Sample_ID[which(is.na(fd1$Disease_Status))]

###--------------------------------------------------------------------------###

fd1$CN_Catalog <- factor(ifelse(fd1$CN.Predicted<2, "< 2",
                                ifelse(fd1$CN.Predicted==2, "= 2",
                                       ifelse(fd1$CN.Predicted>2, "> 2", NA))),
                         levels=c("= 2","< 2","> 2"))


###--------------------------------------------------------------------------###

logit_orci <- function(coeff_matrix, vari=2:3){
  
  or <- round(exp(coeff_matrix[vari, 1]),2)
  
  lb_ci <- round(exp(coeff_matrix[vari, 1]-1.96*coeff_matrix[vari, 2]),2)
  ub_ci <- round(exp(coeff_matrix[vari, 1]+1.96*coeff_matrix[vari, 2]),2)
  ci <- paste0("(",lb_ci, " to ", ub_ci, ")")
  
  p <- round(coeff_matrix[vari,4],5)
  
  x <- cbind(or, ci, p)
  return(x)
}


###---For Disease---###
fit1 <- glm(Disease_Status ~ CN_Catalog + Age + Gender, data=fd1, family = "binomial")

###---For Anti_CCP and RF---###
fit1 <- glm(Anti_CCP ~ CN_Catalog + Age + Gender, data=fd1, family = "binomial")
fit1 <- glm(RF ~ CN_Catalog + Age + Gender, data=fd1, family = "binomial")

logit_orci(summary(fit1)$coef)

###---if P-value is too big to show then use only following command---###
summary(fit1)$coef

table(fd1$Disease_Status)
table(fd1$CN_Catalog, fd1$Disease_Status)
###--------------------------------------------------------------------------###

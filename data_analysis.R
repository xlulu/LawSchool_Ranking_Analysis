library(readxl)
library(ISLR)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
#library(ggplot2)

# Read the data file
ls_data <- read_excel("~/Desktop/Final v2 for Stats.xlsx")
# Encode for Emplyment column: 0 for 'N', and 1 for 'Y'
ls_data$Employed = factor( ls_data$Employed,
                          levels = c('N', 'Y'),
                          labels = c('0', '1'))
# For Bar Pass, 'Passed' was transformed to 1, all the rest ('FAILED', 'DID NOT TAKE', 'UNKNOWN' and '0') were transformed to 0
ls_data$Bar_Status = factor(ls_data$Bar_Status,
                          levels = c('PASSED', 'FAILED', 'DID NOT TAKE', 'UNKNOWN', '0'),
                          labels = c('1', '0', '0', '0', '0'))
# ls_data_m <- subset(ls_data, Gender_Applicant == 'M')
# ls_data_f <- subset(ls_data, Gender_Applicant == 'F')
# ls_data_bp_m <- subset(ls_data_bp, Gender_Applicant == 'M')


Total_Scholarship_Amt = ls_data$Total_Scholarship_Amt
Log_Total_Scholarship_Amt = log10(Total_Scholarship_Amt + 1)
hist(Total_Scholarship_Amt, breaks = 100, xlab = "Total Scholarship Amount", main = "DIstribution of Total Scholarship Amount")
hist(Log_Total_Scholarship_Amt, breaks = 100, xlab = "Log of Total Scholarship Amount", main = "Distribution of Log of Total Scholarship")

# ls_data$Total_Scholarship_Amt = (ls_data$Total_Scholarship_Amt)
summary(ls_data)

# PCA for 6 Semesters
six_semesters.pca <- prcomp(ls_data[,c(5:10)], center = TRUE, scale. = TRUE)
summary(six_semesters.pca)
str(six_semesters.pca)
six_semesters.pca$rotation
ggbiplot(six_semesters.pca)
#compute standard deviation of each principal component
std_dev <- six_semesters.pca$sdev
pr_var <- std_dev^2
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative score plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
# apply the first 2 factors of PCA for analysis for 6 semsters: six_semesters.pca$x[,1] + six_semesters.pca$x[,2]


# 1. Without controvariables
## Undergraduate GPA & first LSAT
glm_emply_1 = glm(Employed ~ Cumulative_GPA + High_LSAT_Score, family = binomial, data = ls_data) 
summary(glm_emply_1)
glm_bp_1 = glm(Bar_Status ~ Cumulative_GPA + High_LSAT_Score, family = binomial, data = ls_data)
summary(glm_bp_1)
## Year 1 only
glm_emply_2 = glm(Employed ~ Fall_1L + Spring_1L, family = binomial, data = ls_data)
summary(glm_emply_2)
glm_bp_2 = glm(Bar_Status ~ Fall_1L + Spring_1L, family = binomial, data = ls_data)
summary(glm_bp_2)
## Year 2 only
glm_emply_3 = glm(Employed ~ Fall_2L + Spring_2L, family = binomial, data = ls_data)
summary(glm_emply_3)
glm_bp_3 = glm(Bar_Status ~ Fall_2L + Spring_2L, family = binomial, data = ls_data)
summary(glm_bp_3)
## Undergraduate GPA & first LSAT & Year one
glm_emply_4 = glm(Employed ~ Cumulative_GPA + High_LSAT_Score + Fall_1L + Spring_1L, family = binomial, data = ls_data) 
summary(glm_emply_4)
glm_bp_4 = glm(Bar_Status ~ Cumulative_GPA + High_LSAT_Score + Fall_1L + Spring_1L, family = binomial, data = ls_data)
summary(glm_bp_4)
## All 6 semesters
glm_emply_5 = glm(Employed ~ six_semesters.pca$x[,1] + six_semesters.pca$x[,2], family = binomial, data = ls_data)
summary(glm_emply_5)
glm_bp_5 = glm(Bar_Status ~ six_semesters.pca$x[,1] + six_semesters.pca$x[,2], family = binomial, data = ls_data)
summary(glm_bp_5)


# 2. With control variables: Current_age_of_applicant, Matriculant_Year, Total_Scholarship_Amt, Gender_Applicant, Residency_Long
## Undergraduate GPA & first LSAT
glm_emply_cv_1 = glm(Employed ~ Cumulative_GPA + High_LSAT_Score + Current_age_of_applicant + Matriculant_Year + Total_Scholarship_Amt + Gender_Applicant + Residency_Long, family = binomial, data = ls_data) 
summary(glm_emply_cv_1)
glm_bp_cv_1 = glm(Bar_Status ~ Cumulative_GPA + High_LSAT_Score + Current_age_of_applicant + Matriculant_Year + Total_Scholarship_Amt + Gender_Applicant + Residency_Long, family = binomial, data = ls_data)
summary(glm_bp_cv_1)
## Year 1 only
glm_emply_cv_2 = glm(Employed ~ Fall_1L + Spring_1L + Current_age_of_applicant + Matriculant_Year + Total_Scholarship_Amt + Gender_Applicant + Residency_Long, family = binomial, data = ls_data)
summary(glm_emply_cv_2)
glm_bp_cv_2 = glm(Bar_Status ~ Fall_1L + Spring_1L + Current_age_of_applicant + Matriculant_Year + Total_Scholarship_Amt + Gender_Applicant + Residency_Long, family = binomial, data = ls_data)
summary(glm_bp_cv_2)
## Year 2 only
glm_emply_cv_3 = glm(Employed ~ Fall_2L + Spring_2L + Current_age_of_applicant + Matriculant_Year + Total_Scholarship_Amt + Gender_Applicant + Residency_Long, family = binomial, data = ls_data)
summary(glm_emply_cv_3)
glm_bp_cv_3 = glm(Bar_Status ~ Fall_2L + Spring_2L + Current_age_of_applicant + Matriculant_Year + Total_Scholarship_Amt + Gender_Applicant + Residency_Long, family = binomial, data = ls_data)
summary(glm_bp_cv_3)
## Undergraduate GPA & first LSAT & Year one
glm_emply_cv_4 = glm(Employed ~ Cumulative_GPA + High_LSAT_Score + Fall_1L + Spring_1L + Current_age_of_applicant + Matriculant_Year + Total_Scholarship_Amt + Gender_Applicant + Residency_Long, family = binomial, data = ls_data) 
summary(glm_emply_cv_4)
glm_bp_cv_4 = glm(Bar_Status ~ Cumulative_GPA + High_LSAT_Score + Fall_1L + Spring_1L + Current_age_of_applicant + Matriculant_Year + Total_Scholarship_Amt + Gender_Applicant + Residency_Long, family = binomial, data = ls_data)
summary(glm_bp_cv_4)
## All 6 semesters
glm_emply_cv_5 = glm(Employed ~ six_semesters.pca$x[,1] + six_semesters.pca$x[,2] + Current_age_of_applicant + Matriculant_Year + Total_Scholarship_Amt + Gender_Applicant + Residency_Long, family = binomial, data = ls_data)
summary(glm_emply_cv_5)
glm_bp_cv_5 = glm(Bar_Status ~ six_semesters.pca$x[,1] + six_semesters.pca$x[,2] + Current_age_of_applicant + Matriculant_Year + Total_Scholarship_Amt + Gender_Applicant + Residency_Long, family = binomial, data = ls_data)
summary(glm_bp_cv_5)


# 3. Matriculant_Year / Class Analysis
class_my = sort(unique(ls_data$Matriculant_Year))




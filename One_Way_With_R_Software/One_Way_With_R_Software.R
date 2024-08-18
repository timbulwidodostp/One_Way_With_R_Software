# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# One Way (ANOVA) Use R Software Complete Step By Step
# loading the appropriate libraries
install.packages("readr")
install.packages("ggplot2")
install.packages("multcompView")
install.packages("dplyr")
install.packages("openxlsx")
install.packages("misty")
library("readr")
library("ggplot2")
library("multcompView")
library("dplyr")
library("openxlsx")
library("misty")
# Use (Open) File Input From Github Olah Data Semarang (timbulwidodostp)
One_Way_With_R_Software = read.csv("https://raw.githubusercontent.com/timbulwidodostp/One_Way_With_R_Software/main/One_Way_With_R_Software/One_Way_With_R_Software.csv",sep = ";")
# Shapiro-Wilk normality test
shapiro.test(One_Way_With_R_Software$RR)
# Levene's Test based on the Arithmetic Mean
test.levene(RR ~ D, data = One_Way_With_R_Software, method = "mean")
# analysis of variance
One_Way_Use_R_Software <- aov(RR ~ as.factor(D), data = One_Way_With_R_Software)
summary(One_Way_Use_R_Software)
# table with factors, means and standard deviation
summary_post_hoc_tukey_One_way <- group_by(One_Way_With_R_Software, D) %>%
summarise(mean=mean(RR), sd=sd(RR)) %>%
arrange(desc(mean))
print(summary_post_hoc_tukey_One_way)
# Tukey's test
post_hoc_tukey_test <- TukeyHSD(One_Way_Use_R_Software)
print(post_hoc_tukey_test)
# creating the compact letter display
summary_post_hoc_tukey_test <- multcompLetters4(One_Way_Use_R_Software, post_hoc_tukey_test)
print(summary_post_hoc_tukey_test)
# adding the compact letter display to the table with means and sd
summary_post_hoc_tukey <- as.data.frame.list(summary_post_hoc_tukey_test$`as.factor(D)`)
summary_post_hoc_tukey_One_way$Tukey <- summary_post_hoc_tukey$Letters
print(summary_post_hoc_tukey_One_way)
# Export Result One Way (ANOVA) Use R Software Complete Step By Step To Excel (File Excel)
Result_One_Way_With_R_Software <- list('Sheet1' = One_Way_Use_R_Software, 'Sheet2' = summary_post_hoc_tukey_One_way)
openxlsx::write.xlsx(Result_One_Way_With_R_Software, "C:\\Result_One_Way_With_R_Software.xlsx")
# One Way (ANOVA) Use R Software Complete Step By Step
# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# Finished

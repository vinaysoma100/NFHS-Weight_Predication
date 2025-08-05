set.seed(123)  # For reproducibility

# Create dummy data with 60 classes and random marks
df <- data.frame(
  class = factor(rep(paste0("Class_", 1:60), each = 10)),  # 10 students per class
  marks = rnorm(600, mean = rep(rnorm(60, 70, 5), each = 10), sd = 10)  # Different means for each class
)

head(df)
anova_result <- aov(marks ~ class, data = df)
summary(anova_result)

tukey <- TukeyHSD(anova_result)
head(tukey$class)  # Shows some pairwise comparisons

library(multcompView)

# Extract p-values
tukey_p <- tukey$class[, "p adj"]
names(tukey_p) <- rownames(tukey$class)

# Generate group letters
group_letters <- multcompLetters(tukey_p)
print(group_letters$Letters)

boxplot(marks ~ class, data = df, las = 2, cex.axis = 0.6, main = "Marks by Class", col = "lightblue")

summary(df$marks)




#####################
library(readxl)
library(multcompView)
sama_df <- read_xlsx("D:/vinay/Pavithra/Triplicate value stat ease - Sheet1.xlsx",sheet = "sama")


#ANOVA
anova_result <- aov(Calcium ~ Group, data = sama_df)
summary(anova_result)

# tukey
tukey <- TukeyHSD(anova_result)

# Extract p-values
tukey_p <- tukey$Group[, "p adj"]
names(tukey_p) <- rownames(tukey$Group)

# Generate group letters
group_letters <- multcompLetters(tukey_p)
print(group_letters$Letters)

boxplot(Calcium ~ Group, data = sama_df, las = 2, cex.axis = 0.6, main = "values", col = "lightblue")




sama_df_1 <- sama_df %>% filter(Group %in% c("class_1","class_2","class_3","class_4","class_5"))


#ANOVA
anova_result <- aov(Calcium ~ Group, data = sama_df_1)
summary(anova_result)

# tukey
tukey <- TukeyHSD(anova_result)

# Extract p-values
tukey_p <- tukey$Group[, "p adj"]
names(tukey_p) <- rownames(tukey$Group)

# Generate group letters
group_letters <- multcompLetters(tukey_p)
print(group_letters$Letters)

boxplot(Calcium ~ Group, data = sama_df_1, las = 2, cex.axis = 0.6, main = "values", col = "lightblue")







# Problem 3 c)


# Make our data
r1 = c(371.71, 302.02, 449.7, 366.19, 199.31,  187.11)
r2 = c(492.58, 355.74, 459.58, 375.58, 204.85,  179.38)
r3 = c(664.72, 587.1, 726.96, 458.16, 245.04,  237.42)
r4 = c(515.29, 437.56, 604.29, 479.81, 261.19,  195.51)
r5 = c(589.25, 493.93, 621.07, 462.79, 278.33,  262.05)

m_rat = data.frame(
    'R1' = r1,
    'R2' = r2,
    'R3' = r3,
    'R4' = r4,
    'R5' = r5
)

treat1 = c(371.71, 492.58, 664.72, 515.29, 589.25)
treat2 = c(302.02, 355.74, 587.1, 437.56, 493.93)
treat3 = c(449.7, 459.58, 726.96, 604.29, 621.07)
control1 = c(366.19, 375.58, 458.16, 479.81, 462.79)
control2 = c(199.31, 204.85, 245.04, 261.19, 278.33)
control3 = c(187.11, 179.38, 237.42, 195.51, 262.05)

m_rat = t(m_rat)
colnames(m_rat) = c('Treat1', 'Treat2', 'Treat3', 'Control1', 'Control2', 'Control3')

frame_rat = as.data.frame(m_rat)

library(ggplot2)
library(tidyr)

rat_graph_mat <- data.frame(
    Category = c("Region1", "Region2", "Region3"),
    Treatment = c(mean(treat1), mean(treat2), mean(treat3)),
    Control = c(mean(control1), mean(control2), mean(control3))
    
)

rat_graph_long <- gather(rat_graph_mat, key = "Variable", value = "Value", Treatment, Control)

ggplot(rat_graph_long, aes(x = Category, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Dual Bar Graph (Side-by-Side)",
       x = "Rat brain Region",
       y = "Avg activation level among rats")

# Repeat for variance

rat_graph_mat2 <- data.frame(
    Category = c("Region1", "Region2", "Region3"),
    Treatment = c(sd(treat1), sd(treat2), sd(treat3)),
    Control = c(sd(control1), sd(control2), sd(control3))
    
)

rat_graph_long2 <- gather(rat_graph_mat2, key = "Variable", value = "Value", Treatment, Control)

ggplot(rat_graph_long2, aes(x = Category, y = Value, fill = Variable)) +
    geom_point(aes(color = Variable)) + 
    labs(title = "Dual Bar Graph (Side-by-Side)",
         x = "Rat brain Region",
         y = "SD of activation among rats")

t.test(treat1, control1, paired = TRUE)
t.test(treat2, control2, paired = TRUE)
t.test(treat3, control3, paired = TRUE)



# Problem 7

# a)
library(nlme)
library(psych)
library(ggpubr)
library(lattice)
data('MathAchieve')
print(?MathAchieve)


# LVL2 - School: random factor of school ID
# LVL1 - Minority: fixed factor per student
# LVL1 - Sex: Fixed factor per student
# LVL1 - SES: Fixed factor per student
# DEPV - MathAch: Dependent variable 
# LVL2 - MEANSES: Fixed factor per school, 
#        considered a random factor per student


# b)
print(describeBy(MathAchieve, group = MathAchieve$Sex))
compare_means(MathAch ~ Sex, MathAchieve)

print(describeBy(MathAchieve, group = MathAchieve$Minority))
compare_means(MathAch ~ Minority, MathAchieve)


#c) 
meandata = describeBy(MathAchieve, group = cut(MathAchieve$MEANSES, 10))
for (i in meandata) {
  print(paste(as.character(i$mean[5]), as.character(i$sd[5]), sep = ", "))
}

analysis_of_var = aov(MathAch ~ cut(MEANSES, 10), data = MathAchieve)
summary(analysis_of_var)

boxplot(MathAch ~ cut(MEANSES, 10), data = MathAchieve,
        col = "skyblue", 
        main = "MathAch by MEANSES",
        xlab = "SES Dectiles", 
        ylab = "Score")

#d) 
print(describeBy(MathAchieve, group = list(MathAchieve$Sex, MathAchieve$Minority)))
bwplot(MathAch ~ Sex + Minority | Sex * Minority, data = MathAchieve)

analysis_2 = aov(MathAch ~ Sex * Minority, data = MathAchieve)
summary(analysis_2)

#e)
model1 = lm(MathAch ~ SES, data = MathAchieve)
print(summary(model1))

# f)
model_grouped_minority = groupedData(MathAch ~ SES | Minority, data = MathAchieve)
lmList <- lmList(model_grouped_minority)

coefs = coef(lmList)

dot_colors <- ifelse(MathAchieve$Minority == "Yes", "tomato", "skyblue")

plot(MathAchieve$SES, MathAchieve$MathAch,
         xlab = "SES", ylab = "Math Achievement", 
         main = "Math Achievement vs SES by Minority Status",
         col = dot_colors, 
         pch = 16)

for (i in 1:nrow(coefs)) {
  abline(a = coefs[i, 1], 
         b = coefs[i, 2], 
         col = "black", 
         lwd = 3, 
         lty = i)
}

legend("bottomright", 
       legend = c("Minority: No", "Minority: Yes"), 
       col = c("skyblue", "tomato"), 
       pch = 16, 
       title = "Student Groups")

# Legend for Schools (the lines)
legend("topleft", 
       legend = rownames(coefs), 
       lty = 1:nrow(coefs), 
       lwd = 2, 
       title = "Best fit lines",
       cex = 0.7)

# g)
model_grouped_sex = groupedData(MathAch ~ SES | Sex, data = MathAchieve)
lmList2 <- lmList(model_grouped_sex)

coefs = coef(lmList2)

dot_colors <- ifelse(MathAchieve$Sex == "Male", "tomato", "skyblue")

plot(MathAchieve$SES, MathAchieve$MathAch,
     xlab = "SES", ylab = "Math Achievement", 
     main = "Math Achievement vs SES by Sex",
     col = dot_colors, 
     pch = 16)

for (i in 1:nrow(coefs)) {
  abline(a = coefs[i, 1], 
         b = coefs[i, 2], 
         col = "black", 
         lwd = 3, 
         lty = i)
}

legend("bottomright", 
       legend = c("Sex: Female", "Sex: Male"), 
       col = c("skyblue", "tomato"), 
       pch = 16, 
       title = "Student Groups")

# Legend for Schools (the lines)
legend("topleft", 
       legend = rownames(coefs), 
       lty = 1:nrow(coefs), 
       lwd = 2, 
       title = "Best fit lines",
       cex = 0.7)

# h)
means <- aggregate(MathAch ~ Sex + Minority, data = MathAchieve, FUN = mean)
sds <- aggregate(MathAch ~ Sex + Minority, data = MathAchieve, FUN = sd)


summary_stats <- cbind(means, sd = sds$MathAch)
colnames(summary_stats) <- c("Sex", "Minority", "Mean_MathAch", "SD_MathAch")

print(summary_stats)

MathAchieve$Combo <- interaction(MathAchieve$Sex, MathAchieve$Minority)
fit_list_combo <- lmList(MathAch ~ SES | Combo, data = MathAchieve)
model_coefs <- coef(fit_list_combo)


# Create 4 unique colors for the 4 combinations
# (Male/No, Male/Yes, Female/No, Female/Yes)
MathAchieve$GroupCombo <- interaction(MathAchieve$Sex, MathAchieve$Minority)
palette_colors <- c("skyblue", "blue", "tomato", "darkred")
dot_colors <- palette_colors[as.numeric(MathAchieve$GroupCombo)]

# Plot with the new 4-way color grouping
plot(MathAchieve$SES, MathAchieve$MathAch, 
     pch = 16, 
     col = dot_colors,
     xlab = "SES", ylab = "Math Achievement",
     main = "MathAch by SES (Grouped by Sex & Minority)")

# Add your thick black regression lines back on top
for (i in 1:nrow(model_coefs)) {
  abline(a = model_coefs[i, 1], b = model_coefs[i, 2], col = "black", lwd = 3, lty = i)
}

# Add a legend for the dots
legend("bottomright", 
       legend = levels(MathAchieve$GroupCombo), 
       col = palette_colors, 
       pch = 16, 
       title = "Sex.Minority",
       cex = 0.7)


# Legend for Schools (the lines)
legend("topleft", 
       legend = rownames(model_coefs), 
       lty = 1:nrow(model_coefs), 
       lwd = 2, 
       title = "Best fit lines",
       cex = 0.7)


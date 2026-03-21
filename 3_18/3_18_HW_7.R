# Project 1
library(dplyr)
library(knitr)
library(ggplot2)
library(lattice)

# Loading the data into a dataframe
rat_data <- read.table("../rat_pup.dat", header = TRUE)

# Variable Analysis: Weight table

# Generate the analysis variable table
analysis_table <- rat_data %>%
    group_by(treatment, sex) %>% 
    summarise(
        N_Obs = n(),
        N = n(),
        mean = mean(weight, na.rm = TRUE),
        std_dev = sd(weight, na.rm = TRUE),
        minimum = min(weight, na.rm = TRUE),
        maximum = max(weight, na.rm = TRUE),
    ) %>%
    mutate(treatment = factor(treatment, levels = c("High", "Low", "Control"))) %>%
    arrange(treatment, sex)


print(kable(analysis_table, 
      col.names = c("Treatment", "Sex", "N Obs", "N",  "mean", "std_dev", "minimum", "maximum"),
      digits = 2, 
      caption = "             Analysis Variable : weight\n----------------------------------------------------------------"))



# Ensure treatment is in the correct logical order
rat_data$treatment <- factor(rat_data$treatment, levels = c("High", "Low", "Control"))
rat_data$sex <- factor(rat_data$sex, levels = c("Female", "Male"))

# Create the Boxplot
bwplot(weight ~ sex | treatment, 
       data = rat_data,
       layout = c(3, 1), 
       main = "Figure 3.1 with Pronounced Median Lines",
       xlab = "Sex", 
       ylab = "Birth Weight",
       # Add in median line
       panel = function(x, y, ...) {
         panel.bwplot(x, y, ..., 
                      pch = "|",          
                      coef = 1.5,         
                      do.out = TRUE)
         m <- aggregate(y ~ x, FUN = mean)
         panel.points(m$x, m$y, pch = 18, col = "black", cex = 1.5)
       },
       par.settings = list(
         box.rectangle = list(col = "black", lwd = 1), # Box border thickness
         box.umbrella = list(col = "black", lty = 1),  # Whisker style
         plot.symbol = list(col = "black", pch = 1)    # Outlier style
       ))



# Figure 3.2
# Ensure factors are ordered correctly to match the book's logic
rat_data$treatment <- factor(rat_data$treatment, levels = c("High", "Low", "Control"))
# Sort by LitSize
rat_data$litter <- with(rat_data, reorder(litter, litsize))
# Reorder sex
rat_data$sex <- factor(rat_data$sex, levels = c("Male", "Female"))

bwplot(weight ~ litter | treatment * sex, 
       data = rat_data,
       layout = c(3, 2),
       main = "Figure 3.2: Boxplots of Rat Pup Weights",
       xlab = "Litter (Ordered by size, smallest to largest)",
       ylab = "Birth Weight",
       scales = list(x = list(draw = FALSE)),)

# Part b)

library(nlme)

ratpup = read.table("../rat_pup.dat", header = TRUE)
attach(ratpup)

ratpup$sex1[sex == "Female"] <- 1
ratpup$sex1[sex == "Male"] <- 0

# Model 3.1.
model3.1.fit <- lme(weight ~ treatment + sex1 + litsize +
                        treatment:sex1, random = ~ 1 | litter,
                      data = ratpup, method = "REML")

print("------- Summary 3.1 -----------------------------------")
print(summary(model3.1.fit))
print("----------------------------------- Anova 3.1 -----------------------------------")
print(anova(model3.1.fit))

# Display the random effects (EBLUPs) from the model.
print("------- EBLUPs -----------------------------------")
print(random.effects(model3.1.fit))

# Model 3.1A.
model3.1a.fit <- gls(weight ~ treatment + sex1 + litsize +
                           treatment:sex1, data = ratpup)
print("----------------------------------- Anova 3.1 vs. 3.1A -----------------------------------")
# Test Hypothesis 3.1.
print(anova(model3.1.fit, model3.1a.fit)) 

# Model 3.2A.
model3.2a.fit <- lme(weight ~ treatment + sex1 + litsize
                         + treatment:sex1, random = ~1 | litter, ratpup, method = "REML",
                         weights = varIdent(form = ~1 | treatment))
print("------- Summary 3.2A -----------------------------------")
print(summary(model3.2a.fit))

print("----------------------------------- Anova 3.1 vs. 3.2A -----------------------------------")
# Test Hypothesis 3.2.
print(anova(model3.1.fit, model3.2a.fit))


ratpup$trtgrp[treatment == "Control"] <- 1
ratpup$trtgrp[treatment == "Low" | treatment == "High"] <- 2

model3.2b.fit <- lme(weight ~ treatment + sex1 + litsize + treatment:sex1,
                     random = ~ 1 | litter, ratpup, method = "REML",
                     weights = varIdent(form = ~1 | trtgrp))

# Test Hypothesis 3.3.
print("----------------------------------- Anova 3.2A vs. 3.2B -----------------------------------")
print(anova(model3.2a.fit, model3.2b.fit))

# Test Hypothesis 3.4.
print("----------------------------------- Anova 3.1 vs. 3.2B -----------------------------------")
print(anova(model3.1.fit, model3.2b.fit))

print("------- Summary 3.2B -----------------------------------")
print(summary(model3.2b.fit))

# Test Hypothesis 3.5 (only one with F test).
print("----------------------------------- Anova 3.2B -----------------------------------")
print(anova(model3.2b.fit))



model3.3.ml.fit <- lme(weight ~ treatment + sex1 + litsize,
                       random = ~1 | litter, ratpup, method = "ML",
                       weights = varIdent(form = ~1 | trtgrp))
model3.3a.ml.fit <- lme(weight ~ sex1 + litsize,
                          random = ~1 | litter, ratpup, method = "ML",
                          weights = varIdent(form = ~1 | trtgrp))

# Test Hypothesis 3.6.
print("----------------------------------- Anova 3.3 vs. 3.3A -----------------------------------")
print(anova(model3.3.ml.fit, model3.3a.ml.fit))


# Model 3.3: Final Model.
model3.3.reml.fit <- lme(weight ~ sex1 + litsize + treatment,
                             random = ~1 | litter, ratpup, method = "REML",
                             weights = varIdent(form = ~1 | trtgrp))
print("------- Summary 3.3 -----------------------------------")
print(summary(model3.3.reml.fit))

print("-------  Anova 3.3 -----------------------------------")
print(anova(model3.3.reml.fit))


# part d)
# fig. 3.6
# Extract raw residuals
ratpup$resids <- resid(model3.3.reml.fit, type = "response")
ratpup$pooled_treat <- ifelse(ratpup$treatment == "Control", 
                                "Control", "High/Low")
ratpup$pooled_treat <- factor(ratpup$pooled_treat, levels = c("Control", "High/Low"))

histogram(~ resids | pooled_treat, 
          data = ratpup,
          layout = c(2, 1),
          nint = 9,           # Number of bins
          aspect = 1,          # Square panels
          type = "percent",    # Matches book's vertical scale
          main = "Figure 3.6: Histograms of Conditional Raw Residuals",
          xlab = "Conditional Raw Residuals",
          ylab = "percent",
          col = "gray80",)


# fig 3.7 - Q-Q plot

mu_val <- round(mean(ratpup$resids), 4)
sigma_val <- round(sd(ratpup$resids), 4)
qqmath(~ resids | pooled_treat, 
       data = ratpup,
       layout = c(1, 2),             # 2 columns (Control, High/Low)
       main = "Figure 3.7: Normal Q-Q Plots of Conditional Raw Residuals",
       xlab = "Standard Normal Quantiles",
       ylab = "Conditional Raw Residuals",
       key = list(
         space = "bottom",
         columns = 2,
         text = list(c(as.expression(substitute(mu == m, list(m = mu_val))),
                       as.expression(substitute(sigma == s, list(s = sigma_val))))),
         points = list(pch = c(NA, NA)) # Keeps it as text only
       ),
       panel = function(x, ...) {
         panel.qqmath(x, ...)
         panel.qqmathline(x, ...)    # Adds the reference line (y = x logic)
       },
       # par.settings to match the textbook's clean look
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))

# fig. 3.8
ratpup$fitted <- fitted(model3.3.reml.fit)
ratpup$resids <- resid(model3.3.reml.fit)

ratpup$pooled_treat <- factor(ratpup$pooled_treat, levels = c("High/Low", "Control"))


xyplot(resids ~ fitted | pooled_treat, 
       data = ratpup,
       layout = c(2, 1),
       main = "Figure 3.8: Conditional Residuals vs. Predicted Values",
       xlab = "Linear predictor",
       ylab = "Residual",
       # Add a reference line at zero
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(h = 0, lty = 2, col = "gray50") # Dashed zero line
       },
       # par.settings for a clean, professional look
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))


# fig. 3.9
ratpup$stud_resids <- resid(model3.3.reml.fit, type = "pearson")
ratpup$litter <- reorder(ratpup$litter, as.numeric(as.character(ratpup$litter)))

bwplot(stud_resids ~ litter, 
       data = ratpup,
       box.width = 0.4,
       main = "Figure 3.9: Studentized Residuals Ordered by Litter Size",
       xlab = "Litter ID (Sorted by ID)",
       ylab = "Studentized Residuals",
       scales = list(
         x = list(
           draw = TRUE,      # Turn the labels back on
           rot = 45,         # Rotate 90 degrees so they don't overlap
           cex = 0.7         # Make text slightly smaller to fit everyone
         )
       ),
       panel = function(x, y, ...) {
         # pch = "|" ensures the median is a horizontal line
         panel.bwplot(x, y, ..., pch = "|")
         
         # Calculate and draw the MEAN as an OPEN diamond (pch = 5)
         m <- aggregate(y ~ x, FUN = mean)
         panel.points(m$x, m$y, pch = 5, col = "black", cex = 1.1, lwd = 1.2)
         
         # Reference lines
         panel.abline(h = 0, lty = 2, col = "gray50")
         panel.abline(h = c(-2, -4, -6, 2), lty = 2, col = "gray50")       },
       par.settings = list(
         # Outliers
         plot.symbol = list(pch = 1, col = "black", cex = 0.6),
         # Box borders
         box.rectangle = list(col = "black", lwd = 1),
         # Whiskers
         box.umbrella = list(col = "black", lty = 1, lwd = 1)
       ))


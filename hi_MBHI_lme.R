
## Project: NYSG R/SHH-18 part 1.
## Purpose of study: investigate the effect of pre-growth condition and
## strain diversity on the nisin efficacy against Listeria monocytogenes
## on cold-smoked salmon.
## Purpose of script: build lme model for MBHI data.


#library(tidyverse);library(emmeans);library(lme4);library(plyr);library(lmerTest)
#library(car)


### Read in MBHI data.
pregrowth_mBHI <- read.delim("pregrowth_MBHI_raw.txt")
pregrowth_mBHI$Strain <- as.factor(pregrowth_mBHI$Strain)

### Remove columns that contain NA in outcome column (Log_Diff).
complete.cases(pregrowth_mBHI$Log_Diff)
table(complete.cases(pregrowth_mBHI$Log_Diff))
pregrowth_mBHI_rmna <- pregrowth_mBHI[complete.cases(pregrowth_mBHI$Log_Diff), ]

### Create a variable for random effects.
pregrowth_mBHI_rmna$Group <- paste(pregrowth_mBHI_rmna$Phase, pregrowth_mBHI_rmna$Condition, pregrowth_mBHI_rmna$Replicate, sep = "_")

### Change the strain names to the full FSL ID.
pregrowth_mBHI_rmna$Strain <- as.factor(pregrowth_mBHI_rmna$Strain)
fsl_id <- c("FSL L3-0051", "FSL L4-0060", "FSL N1-0061", "FSL F2-0237", "FSL F2-0310", "FSL L4-0396")
levels(pregrowth_mBHI_rmna$Strain) <- fsl_id


### Density plot for each phase and pre-growth condition combination.
ggplot(pregrowth_mBHI_rmna, aes(x = Log_Diff)) + geom_density() + facet_grid(Phase ~ Condition)

### Build linear mixed effects model for MBHI data.
# Fixed effects: Condition (pre-growth condition of L. monocytogenes), 
# Phase (growth phase of L. monocytogenes before inoculation), Strain.
# Random effects: Group (each experiment trial conducted at different time).
pregrowth_mBHI_lme <- lmer(Log_Diff ~ Condition*Phase*Strain + 
                             (1|Group), data = pregrowth_mBHI_rmna, REML = FALSE)

# Goodness-of-fit of the lme model.
AIC(pregrowth_mBHI_lme)
# Anova table of the lme model.
anova(pregrowth_mBHI_lme)


### Model diagnostics

# Test the overall linearity and homoscedasticity.
plot(fitted(pregrowth_mBHI_lme), residuals(pregrowth_mBHI_lme), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(pregrowth_mBHI_lme), residuals(pregrowth_mBHI_lme)))

# Test linearity in each variable (fixed effects).
# Condition
ggplot(data.frame(x1=pregrowth_mBHI_rmna$Condition,pearson=residuals(pregrowth_mBHI_lme,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
# Phase
ggplot(data.frame(x2=pregrowth_mBHI_rmna$Phase,pearson=residuals(pregrowth_mBHI_lme,type="pearson")),
       aes(x=x2,y=pearson)) +
  geom_point() +
  theme_bw()
# Strain
ggplot(data.frame(x3=pregrowth_mBHI_rmna$Strain,pearson=residuals(pregrowth_mBHI_lme,type="pearson")),
       aes(x=x3,y=pearson)) +
  geom_point() +
  theme_bw()


# Check collinearity (fixed effects).
# Since we have orthogonal categorical variables as fixed effects, multicollinearity should not be an issue.
vif(pregrowth_mBHI_lme)

# Check normality of the residuals.
qqPlot(residuals(pregrowth_mBHI_lme), main = "normal distribution check for pre-growth data (MBHI)")

# Check Sensitivity of data points.
ggplot(data.frame(lev=hatvalues(pregrowth_mBHI_lme),pearson=residuals(pregrowth_mBHI_lme,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()


### End.


















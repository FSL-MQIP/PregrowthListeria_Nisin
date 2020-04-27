
## Project: NYSG R/SHH-18 part 1.
## Purpose of study: investigate the effect of pre-growth condition and
## strain diversity on the nisin efficacy against Listeria monocytogenes
## on cold-smoked salmon.
## Purpose of script: build lme model for salmon data.


#library(tidyverse);library(emmeans);library(lme4);library(plyr);library(lmerTest)
#library(car)


### Read in data.
pregrowth_salmon <- read.csv("/Users/ruixichen/OneDrive - Cornell University/Cornell document/Food safety lab/Salmon Project/pregrowth_condition/Data/Processing/Dataframes/pre_growth_raw_RC021620.csv")
pregrowth_salmon$Day <- as.factor(pregrowth_salmon$Day)


### Build the lme model for salmon data.
# Fixed effects: Condition (pre-growth condition of L. monocytogenes), 
# Nisin (nisin treatment of the salmon samples), Serotype (serotype of the L. monocytogenes strains used),
# Source (source of isolation of the L. monocytogenes strains used), and Day (days in storage).
# Random effects: AgeGroup ("age" of the cold-smoked salmon used for this study).

pregrowth_salmon_lme <- lmer(Log_CFU ~ Nisin*Condition*Serotype + Nisin*Condition*Day +
                  Nisin*Serotype*Source + 
                  (1|AgeGroup), data = pregrowth_salmon, REML = FALSE)

# Goodness-of-fit of the salmon lme model.
AIC(pregrowth_salmon_lme)
# Anova table of the salmon lme model.
anova(pregrowth_salmon_lme)

### Model diagnostics.

# Test the overall linearity and homoscedasticity of the model.
plot(fitted(pregrowth_salmon_lme), residuals(pregrowth_salmon_lme), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(pregrowth_salmon_lme), residuals(pregrowth_salmon_lme)))

# Linearity in each of the variables (fixed effects).
# Condition
ggplot(data.frame(x1=pregrowth_salmon$Condition,pearson=residuals(pregrowth_salmon_lme,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
# Serotype
ggplot(data.frame(x2=pregrowth_salmon$Serotype,pearson=residuals(pregrowth_salmon_lme,type="pearson")),
       aes(x=x2,y=pearson)) +
  geom_point() +
  theme_bw()
# Day
ggplot(data.frame(x3=pregrowth_salmon$Day,pearson=residuals(pregrowth_salmon_lme,type="pearson")),
       aes(x=x3,y=pearson)) +
  geom_point() +
  theme_bw()
# Nisin
ggplot(data.frame(x4=pregrowth_salmon$Nisin,pearson=residuals(pregrowth_salmon_lme,type="pearson")),
       aes(x=x4,y=pearson)) +
  geom_point() +
  theme_bw()
# Source
ggplot(data.frame(x5=pregrowth_salmon$Source,pearson=residuals(pregrowth_salmon_lme,type="pearson")),
       aes(x=x5,y=pearson)) +
  geom_point() +
  theme_bw()


# Check collinearity (fixed effects).
# Since we have orthogonal categorical variables as fixed effects, multicollinearity should not be an issue.
vif(pregrowth_salmon_lme)

# Check normality of the residuals.
qqPlot(residuals(pregrowth_salmon_lme), main = "normal distribution check for salmon lme model")
hist(residuals(pregrowth_salmon_lme), breaks = 50)

# Check sensitivity of the data points.
ggplot(data.frame(lev=hatvalues(pregrowth_salmon_lme),pearson=residuals(pregrowth_salmon_lme,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()


# End.








## Project: NYSG R/SHH-18 part 1.
## Purpose of study: investigate the effect of pre-growth condition and
## strain diversity on the nisin efficacy against Listeria monocytogenes
## on cold-smoked salmon.
## Purpose of script: 
# i) post hoc analysis for the MBHI lme model.
# ii) making figures associated with the MBHI lme model.

#library(tidyverse);library(emmeans);library(lme4);library(plyr);library(lmerTest)
#library(car);library(effects);library(lemon)


### Read in MBHI data.
pregrowth_mBHI <- read.delim("/Users/ruixichen/OneDrive - Cornell University/Cornell document/Food safety lab/Salmon Project/pregrowth_condition/Data/Processing/Dataframes/mBHI_df_no37C_RC111819.txt")
pregrowth_mBHI$Strain <- as.factor(pregrowth_mBHI$Strain)

### Remove columns that contain NA in outcome column (Log_Diff).
complete.cases(pregrowth_mBHI$Log_Diff)
table(complete.cases(pregrowth_mBHI$Log_Diff))
pregrowth_mBHI_rmna <- pregrowth_mBHI[complete.cases(pregrowth_mBHI$Log_Diff), ]

### Create a variable for random effects.
pregrowth_mBHI_rmna$Group <- paste(pregrowth_mBHI_rmna$Phase, pregrowth_mBHI_rmna$Condition, pregrowth_mBHI_rmna$Replicate, sep = "_")


### Build linear mixed effects model for MBHI data.
pregrowth_mBHI_lme <- lmer(Log_Diff ~ Condition*Phase*Strain + 
                             (1|Group), data = pregrowth_mBHI_rmna, REML = FALSE)
# Anova table of the lme model.
anova(pregrowth_mBHI_lme)


# Make figure 1.
pregrowth_MBHI_lme_ef1 <- effect(term="Condition*Phase*Strain", mod=pregrowth_mBHI_lme)
pregrowth_MBHI_lme_ef1_df <- as.data.frame(pregrowth_MBHI_lme_ef1) %>% arrange(Phase, Condition, Strain)
levels(pregrowth_MBHI_lme_ef1_df$Strain)[7] <- NA

order_strain <- c("396", "237", "60", "51", "61", "310")
pregrowth_MBHI_lme_ef1_df <- pregrowth_MBHI_lme_ef1_df %>% mutate(Strain= factor(Strain, levels = order_strain)) %>% arrange(Strain)

# New facet label names for conditions.
condition.labs <- c("BHI", "BHI NaCl", "BHI pH=6.1", "BHI pH=9", "BHI Quat")
names(condition.labs) <- c("7CBHI", "7CNaCl", "7CpH", "7CpHshock", "7CQuat")


high_inoc_fg1 <- ggplot(data=pregrowth_MBHI_lme_ef1_df, aes(x=Strain, y=fit, group=Phase)) + 
  geom_point(aes(color=Phase)) + 
  geom_line(aes(color=Phase)) +
  scale_x_discrete(name= "Strain") +
  scale_y_continuous(name = "Log Reduction (Untreated vs Nisin-treated)") +
  geom_ribbon(aes(ymin=fit-se,ymax=fit+se, fill=Phase),alpha=0.3) + 
  #facet_wrap(~Condition) +
  #labs(title = "Interaction between \nCondition, Phase, and Strain ") +
  #scale_fill_discrete(guide=FALSE) +
  #scale_color_discrete(name = "Growth Phase", labels = c("Log", "Stationary")) +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(face='bold', size=14),
        axis.title.y=element_text(face='bold', size=14)) +
  facet_rep_wrap(~Condition, scales = "fixed", repeat.tick.labels = TRUE, labeller = labeller(Condition = condition.labs)) +
  scale_fill_manual(values=c("deepskyblue2", "springgreen2")) +
  guides(fill=FALSE) +
  scale_color_manual(values=c("deepskyblue2", "springgreen2"),name="Growth Phase",
                     breaks=c("Log", "Stationary"),
                     labels=c("Log", "Stationary"))



# End.
































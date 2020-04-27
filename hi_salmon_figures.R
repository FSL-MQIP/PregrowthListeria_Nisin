
## Project: NYSG R/SHH-18 part 1.
## Purpose of study: investigate the effect of pre-growth condition and
## strain diversity on the nisin efficacy against Listeria monocytogenes
## on cold-smoked salmon.
## Purpose of script: 
# i) post hoc analysis for the salmon lme model.
# ii) making figures associated with the salmon lme model.

#library(tidyverse);library(emmeans);library(lme4);library(plyr);library(lmerTest)
#library(car);library(effects)


### Read in data.
pregrowth_salmon <- read.csv("pregrowth_salmon_raw.csv")
pregrowth_salmon$Day <- as.factor(pregrowth_salmon$Day)

### Build salmon lme model.
pregrowth_salmon_lme <- lmer(Log_CFU ~ Nisin*Condition*Serotype + Nisin*Condition*Day +
                               Nisin*Serotype*Source + 
                               (1|AgeGroup), data = pregrowth_salmon, REML = FALSE)

# Anova table for the salmon lme model.
anova(pregrowth_salmon_lme)


### Make Figure 2.
# Calculate compact numbers on figure 2.
salmon_all_var.emm <- emmeans(pregrowth_salmon_lme, ~Condition+Serotype+Source+Day+Nisin)

nis_day_by_con_ser_sou.cld1 <- cld(salmon_all_var.emm, single = c("Nisin", "Day"), by = c("Condition", "Serotype", "Source"), Letters = LETTERS)
nis_day_by_con_ser_sou.cld1$letters <-trimws(nis_day_by_con_ser_sou.cld1$.group)
nis_day_by_con_ser_sou.cld1_df <- as.data.frame(nis_day_by_con_ser_sou.cld1)
nis_day_by_con_ser_sou.cld1_df %>% arrange(Condition, Serotype, Source, Day, Nisin) -> nis_day_by_con_ser_sou.cld1_df

# Add a strain variable to nis_day_by_con_ser_sou.cld1_df.
fg2_strainvec <- rep(NA, nrow(nis_day_by_con_ser_sou.cld1_df))
fg2_strainvec[which(nis_day_by_con_ser_sou.cld1_df$Serotype=="1/2a"&nis_day_by_con_ser_sou.cld1_df$Source=="Environment")] <- "FSL L4-0396"
fg2_strainvec[which(nis_day_by_con_ser_sou.cld1_df$Serotype=="1/2a"&nis_day_by_con_ser_sou.cld1_df$Source=="Salmon")] <- "FSL F2-0237"
fg2_strainvec[which(nis_day_by_con_ser_sou.cld1_df$Serotype=="1/2b"&nis_day_by_con_ser_sou.cld1_df$Source=="Environment")] <- "FSL L4-0060"
fg2_strainvec[which(nis_day_by_con_ser_sou.cld1_df$Serotype=="1/2b"&nis_day_by_con_ser_sou.cld1_df$Source=="Salmon")] <- "FSL L3-0051"
fg2_strainvec[which(nis_day_by_con_ser_sou.cld1_df$Serotype=="4b"&nis_day_by_con_ser_sou.cld1_df$Source=="Environment")] <- "FSL N1-0061"
fg2_strainvec[which(nis_day_by_con_ser_sou.cld1_df$Serotype=="4b"&nis_day_by_con_ser_sou.cld1_df$Source=="Salmon")] <- "FSL F2-0310"

nis_day_by_con_ser_sou.cld1_df$Strain <- fg2_strainvec
nis_day_by_con_ser_sou.cld1_df <- nis_day_by_con_ser_sou.cld1_df %>% mutate(Strain=factor(Strain, levels = c("FSL L4-0396", "FSL F2-0237", "FSL L4-0060",
                                                                               "FSL L3-0051", "FSL N1-0061", "FSL F2-0310"))) %>% arrange(Strain) -> nis_day_by_con_ser_sou.cld1_df
# Plot figure 2.
high_inoc_fg2 <- ggplot(nis_day_by_con_ser_sou.cld1_df, aes(x=Day, y=emmean, group=Nisin,color=Nisin)) + 
  scale_fill_manual(values=c("deepskyblue2", "springgreen2")) +
  guides(fill=FALSE) +
  scale_color_manual(values=c("deepskyblue2", "springgreen2"),name="Nisin Treatment",
                     breaks=c("Minus", "Plus"),
                     labels=c("Untreated", "Nisin-treated")) +
  facet_grid(Strain~Condition) + 
  geom_point(position = position_dodge(.2)) + 
  geom_line(position = position_dodge(.2)) +
  geom_text(aes(label=letters, group=Nisin), vjust=0.5, hjust=2, color = "black", size = 3, position = position_dodge(.2)) +
  geom_point(data = pregrowth_salmon, aes(x=Day, y=Log_CFU, fill=Nisin, group=AgeGroup),
             position = position_dodge(0.1), size=1, color="grey10", shape=23, alpha=.6) +
  labs(x="Storage Day", y="Log CFU/g") +
  theme(panel.background = element_rect(fill = "grey93", colour = "grey93", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))



### Make figure 3.

salmon_lme_ef1 <- effect(term="Nisin*Condition*Day", mod=pregrowth_salmon_lme)
salmon_lme_ef1_df <-as.data.frame(salmon_lme_ef1)

high_inoc_fg3 <- ggplot(data=salmon_lme_ef1_df, aes(x=Day, y=fit, group=Nisin)) + 
  geom_point(aes(color=Nisin)) + 
  geom_line(aes(color=Nisin))  + 
  scale_x_discrete(name="Storage Day",labels = c("1", "15", "30")) +
  scale_y_continuous(name = "Log CFU/g") +
  geom_ribbon(aes(ymin=fit-se,ymax=fit+se, fill=Nisin),alpha=0.3) + 
  facet_wrap(~Condition) + 
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(face='bold', size=14),
        axis.title.y=element_text(face='bold', size=14)) +
  scale_fill_manual(values=c("deepskyblue2", "springgreen2")) +
  guides(fill=FALSE) +
  scale_color_manual(values=c("deepskyblue2", "springgreen2"),name="Nisin Treatment",
                     breaks=c("Minus", "Plus"),
                     labels=c("Untreated", "Nisin-treated")) +
  theme(panel.background = element_rect(fill = "grey98", colour = "grey93", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey93"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey93"))


### Post hoc analysis for Nisin:Condition:Day.
### Make figure 4.
nis_con_by_day.emm <- emmeans(pregrowth_salmon_lme, ~Condition+Nisin|Day)
nis_con_by_day.ctr1 <- contrast(nis_con_by_day.emm, interaction = "pairwise", 
                                    simple = "Nisin", combine = TRUE, adjust = "tukey")

nis_con_by_day.ctr2_dun <- emmeans(nis_con_by_day.ctr1, specs = trt.vs.ctrl ~ Condition|Day)
nis_con_by_day.ctr2_dun$contrasts

# Manually assign significance label.
# 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1.
nis_con_by_day.ctr1_df <- as.data.frame(nis_con_by_day.ctr1)
nis_con_by_day.ctr1_df$significance <- c("", "*", "", "", 
                                         "", "", "***", "*", 
                                         "", "**", "***", "***")

# Read in log reduction dataset for plotting.
pregrowth_salmon_LR <- read.csv("pregrowth_salmon_raw_LR.csv")
pregrowth_salmon_LR$Day <- as.factor(pregrowth_salmon_LR$Day)

# Summarise raw data
pregrowth_salmon_LR_con_day <- ddply(pregrowth_salmon_LR, c("Condition", "Day"), summarise, 
                                  N = sum(!is.na(Log_RD)),
                                  mean = mean(Log_RD, na.rm = TRUE),
                                  sd = sd(Log_RD, na.rm = TRUE),
                                  se = sd/sqrt(N)
)

# Make annotation dataframe for cond_nis_day plot
fg4_anno_df <- data.frame("Day"=c("1","15","15","30","30","30"), x1 = c(1,1,1,1,1,1), x2 = c(2,3,4,2,3,4), 
                              y1 = c(1.5,2.0,2.0,1.7,1.7,1.7), y2 = c(2.20,2.10,2.20,1.75,1.85,1.95), y3 = c(2.0,1.25,1.5,1.5,1.3,1.1), xstar = c(1.5,2,2.5,1.5,2,2.5), 
                              ystar = c(2.25,2.15,2.25,1.8,1.9,2.0), lab = c("*","***","*","**","***","***"))
# Panel label.
fg4_anno_panel <- data.frame("xpos" = c(.8,.8,.8), "ypos" = c(2.2,2.2,2.2), "Label" = c("A", "B", "C"), "Day" = c("1", "15", "30"))

# Plot figure 4.
high_inoc_fg4 <- ggplot(nis_con_by_day.ctr1_df) +
  geom_bar(aes(x=Condition, y=estimate, group=Condition, 
               fill=Condition), stat = "identity", width = 0.7, alpha=0.6, color="black") +
  facet_grid(.~Day) +
  scale_x_discrete(name="Pre-growth Condition", labels=c("BHI", "NaCl", "pH", "Quat")) +
  scale_y_continuous(name="Log Reduction (Untreated vs Nisin-treated)", 
                     breaks = seq(0,2,.5), limits = c(0,2.3)) +
  scale_fill_manual(values=c("limegreen", "deepskyblue2", "coral", "orchid1"),name="Pre-growth Condition") +
  theme(axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(face='bold', size=16),
        axis.title.y=element_text(face='bold', size=16)) +
  theme(panel.background = element_rect(fill = "grey93", colour = "grey93", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) +
  geom_pointrange(data = pregrowth_salmon_LR_con_day, aes(x=Condition, y=mean, ymin=mean-se, ymax=mean+se, 
                                                       group=Condition, fill=Condition),shape=23,size=.3,color="black") + 
  geom_segment(data = fg4_anno_df, aes(x = x1, xend = x1, y = y1, yend = y2), colour = "black") +
  geom_segment(data = fg4_anno_df, aes(x = x2, xend = x2, y = y3, yend = y2), colour = "black") +
  geom_segment(data = fg4_anno_df, aes(x = x1, xend = x2, y = y2, yend = y2), colour = "black") + 
  geom_text(data = fg4_anno_df, aes(x = xstar,  y = ystar, label = lab), color = "red") +
  geom_text(data = fg4_anno_panel, aes(x=xpos, y=ypos, label=Label), color="black", size=6)



### Make figure 5.

salmon_lme_ef3 <- effect(term="Nisin*Serotype*Source", mod=pregrowth_salmon_lme)
salmon_lme_ef3_df <-as.data.frame(salmon_lme_ef3)

source.labs <- c("Environment", "Finished Product")
names(source.labs) <- c("Environment", "Salmon")

high_inoc_fg5 <- ggplot(data=salmon_lme_ef3_df, aes(x=Serotype, y=fit, group=Nisin)) + 
  geom_point(aes(color=Nisin)) + 
  geom_line(aes(color=Nisin))  + 
  scale_x_discrete(name="Strain (Serotype)", labels = c("(1/2a)", "(1/2b)", "(4b)")) +
  scale_y_continuous(name = "Log CFU/g") +
  geom_ribbon(aes(ymin=fit-se,ymax=fit+se, fill=Nisin),alpha=0.3) + 
  facet_wrap(~Source, labeller = labeller(Source = source.labs)) + 
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(face='bold', size=14),
        axis.title.y=element_text(face='bold', size=14)) +
  scale_fill_manual(values=c("deepskyblue2", "springgreen2")) +
  guides(fill=FALSE) +
  scale_color_manual(values=c("deepskyblue2", "springgreen2"),name="Nisin Treatment",
                     breaks=c("Minus", "Plus"),
                     labels=c("Untreated", "Nisin-treated")) +
  theme(panel.background = element_rect(fill = "grey98", colour = "grey93", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey93"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey93"))


### Post hoc analysis for Nisin:Serotype:Source.
### Make table 6.
nis_ser_by_sou.emm <- emmeans(pregrowth_salmon_lme, ~Serotype+Nisin|Source)
nis_ser_by_sou.ctr <- contrast(nis_ser_by_sou.emm, interaction = "pairwise", 
                                   simple = "Nisin", combine = TRUE, adjust = "tukey")
nis_ser_by_sou.ctr2 <- contrast(nis_ser_by_sou.ctr, interaction = "pairwise",
                                    simple = "Serotype", combine = TRUE, adjust = "tukey")


### Make figure 6.
salmon_lme_ef2 <- effect(term="Nisin*Condition*Serotype", mod=pregrowth_salmon_lme)
salmon_lme_ef2_df <-as.data.frame(salmon_lme_ef2)

high_inoc_fg6 <- ggplot(data=salmon_lme_ef2_df, aes(x=Condition, y=fit, group=Nisin)) + 
  geom_point(aes(color=Nisin)) + 
  geom_line(aes(color=Nisin))  + 
  scale_x_discrete(name="Pre-growth Condition",labels = c("BHI", "NaCl", "pH", "Quat")) +
  scale_y_continuous(name = "Log CFU/g") +
  geom_ribbon(aes(ymin=fit-se,ymax=fit+se, fill=Nisin),alpha=0.3) + 
  facet_wrap(~Serotype) + 
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(face='bold', size=14),
        axis.title.y=element_text(face='bold', size=14)) +
  scale_fill_manual(values=c("deepskyblue2", "springgreen2")) +
  guides(fill=FALSE) +
  scale_color_manual(values=c("deepskyblue2", "springgreen2"),name="Nisin Treatment",
                     breaks=c("Minus", "Plus"),
                     labels=c("Untreated", "Nisin-treated")) +
  theme(panel.background = element_rect(fill = "grey98", colour = "grey93", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey93"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey93")) +
  theme(text = element_text(family = "Times New Roman"))


### Post hoc analysis for Nisin:Serotype:Condition.
### Make figure 7.

# Read in raw log reduction data for plotting.
pregrowth_salmon_LR <- read.csv("pregrowth_salmon_raw_LR.csv")
pregrowth_salmon_LR$Day <- as.factor(pregrowth_salmon_LR$Day)

# Summarise raw data for plotting.
pregrowth_salmon_LR_con_sero <- ddply(pregrowth_salmon_LR, c("Condition", "Serotype"), summarise, 
                                   N = sum(!is.na(Log_RD)),
                                   mean = mean(Log_RD, na.rm = TRUE),
                                   sd = sd(Log_RD, na.rm = TRUE),
                                   se = sd/sqrt(N)
)


# Post hoc analysis for Nisin:Condition:Serotype
nis_con_by_ser.emm <- emmeans(pregrowth_salmon_lme, ~Nisin+Condition|Serotype)
nis_con_by_ser.emm_df <- as.data.frame(nis_con_by_ser.emm)
nis_con_by_ser.ctr1 <- contrast(nis_con_by_ser.emm, interaction="pairwise", simple="Nisin",
                                    combine=TRUE, adjust="tukey")
nis_con_by_ser.ctr1_df <- as.data.frame(nis_con_by_ser.ctr1)

nis_con_by_ser.ctr2_dun <- emmeans(nis_con_by_ser.ctr1, specs = trt.vs.ctrl ~ Condition|Serotype)
nis_con_by_ser.ctr2_dun$contrasts


# Manually assign the significance level.
# 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1.
nis_con_by_ser.ctr1_df$Significance <- c("","","**","",
                                             "","","","*",
                                             "","","***","*")

# Make a dataframe to denote significance (asterisks)
high_inoc_fg7_anno_df <- data.frame("Serotype"=c("1/2a","1/2b","4b","4b"), x1 = c(1,1,1,1), x2 = c(3,4,3,4), 
                                        y1 = c(1.6,1.8,1.7,1.7), y2 = c(1.65,1.85,1.75,1.85), y3 = c(0.8,1.25,1.1,1.1), xstar = c(2,2.5,2,2.5), 
                                        ystar = c(1.7,1.9,1.8,1.9), lab = c("**","*","***","*"))

high_inoc_fg7_anno_panel <- data.frame("xpos" = c(.8,.8,.8), "ypos" = c(2,2,2), "Label" = c("A", "B", "C"), "Serotype" = c("1/2a", "1/2b", "4b"))

high_inoc_fg7 <- ggplot(nis_con_by_ser.ctr1_df) +
  geom_bar(stat = "identity", aes(x=Condition, y=estimate, group=Condition, 
                                  fill=Condition), width = 0.7, alpha=0.6, color="black") +
  facet_grid(.~Serotype) +
  scale_x_discrete(name="Pre-growth Condition", labels=c("BHI", "NaCl", "pH", "Quat")) +
  scale_y_continuous(name="Log Reduction (Untreated vs Nisin-treated)", 
                     breaks = seq(0,2,.5), limits = c(0,2)) +
  scale_fill_manual(values=c("limegreen", "deepskyblue2", "coral", "orchid1"),name="Pre-growth Condition") +
  theme(axis.text.x=element_text(size=14), axis.text.y=element_text(size=14),
        axis.title.x=element_text(face='bold', size=16), axis.title.y=element_text(face='bold', size=16)) +
  theme(panel.background = element_rect(fill = "grey93", colour = "grey93", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white")) +
  geom_pointrange(data = pregrowth_salmon_LR_con_sero, aes(x=Condition, y=mean, ymin=mean-se, ymax=mean+se, 
                                                        group=Condition, fill=Condition),shape=23,size=.3,color="black") +
  geom_segment(data = high_inoc_fg7_anno_df, aes(x = x1, xend = x1, y = y1, yend = y2), colour = "black") +
  geom_segment(data = high_inoc_fg7_anno_df, aes(x = x2, xend = x2, y = y3, yend = y2), colour = "black") +
  geom_segment(data = high_inoc_fg7_anno_df, aes(x = x1, xend = x2, y = y2, yend = y2), colour = "black") + 
  geom_text(data = high_inoc_fg7_anno_df, aes(x = xstar,  y = ystar, label = lab), color = "red") +
  geom_text(data = high_inoc_fg7_anno_panel, aes(x=xpos, y=ypos, label=Label), color="black", size=6)


### Make supplementary table 2.

salmon_all_var.emm <- emmeans(pregrowth_salmon_lme, ~Serotype+Source+Condition+Day+Nisin)
nis_by_con_ser_sou_day.ctr <- contrast(salmon_all_var.emm, interaction = "pairwise", 
                                       simple = "Nisin", combine = TRUE, adjust = "tukey")
nis_by_con_ser_sou_day.ctr_df <- as.data.frame(nis_by_con_ser_sou_day.ctr) %>% arrange(Condition, Serotype, Source, Day)

# Adding a strain variable to nis_by_con_ser_sou_day.ctr_df.
supp_tb2_strainvec <- rep(NA, nrow(nis_by_con_ser_sou_day.ctr_df))
supp_tb2_strainvec[which(nis_by_con_ser_sou_day.ctr_df$Serotype=="1/2a"&nis_by_con_ser_sou_day.ctr_df$Source=="Environment")] <- "FSL L4-0396"
supp_tb2_strainvec[which(nis_by_con_ser_sou_day.ctr_df$Serotype=="1/2a"&nis_by_con_ser_sou_day.ctr_df$Source=="Salmon")] <- "FSL F2-0237"
supp_tb2_strainvec[which(nis_by_con_ser_sou_day.ctr_df$Serotype=="1/2b"&nis_by_con_ser_sou_day.ctr_df$Source=="Environment")] <- "FSL L4-0060"
supp_tb2_strainvec[which(nis_by_con_ser_sou_day.ctr_df$Serotype=="1/2b"&nis_by_con_ser_sou_day.ctr_df$Source=="Salmon")] <- "FSL L3-0051"
supp_tb2_strainvec[which(nis_by_con_ser_sou_day.ctr_df$Serotype=="4b"&nis_by_con_ser_sou_day.ctr_df$Source=="Environment")] <- "FSL N1-0061"
supp_tb2_strainvec[which(nis_by_con_ser_sou_day.ctr_df$Serotype=="4b"&nis_by_con_ser_sou_day.ctr_df$Source=="Salmon")] <- "FSL F2-0310"

# Attach Strain column to nis_by_con_ser_sou_day.ctr_df
nis_by_con_ser_sou_day.ctr_df$Strain <- supp_tb2_strainvec
nis_by_con_ser_sou_day.ctr_df <- nis_by_con_ser_sou_day.ctr_df[,-1]

# Arrange nis_by_con_ser_sou_day.ctr_df to make supplementary table 2.
nis_by_con_ser_sou_day.ctr_df_subset <- nis_by_con_ser_sou_day.ctr_df[c("Condition", "Day", "Strain", "estimate", "SE")]
nis_by_con_ser_sou_day.ctr_df_subset %>% arrange(Condition, Day, Strain) -> nis_by_con_ser_sou_day.ctr_df_subset
nis_by_con_ser_sou_day.ctr_df_subset$estimate <- round(nis_by_con_ser_sou_day.ctr_df_subset$estimate, digits = 2)
nis_by_con_ser_sou_day.ctr_df_subset$SE <- round(nis_by_con_ser_sou_day.ctr_df_subset$SE, digits = 2)

high_inoc_supp_table2 <- data.frame(matrix(nrow = 6, ncol = 12))
rownames(high_inoc_supp_table2) <- c("FSL F2-0237", "FSL F2-0310", "FSL L3-0051", "FSL L4-0060", "FSL L4-0396", "FSL N1-0061")
condition_vec <- c("BHI", "NaCl", "pH", "Quat")
day_vec <- c(1,15,30)
for (i in 1:4) {
  for (j in 1:3) {
    colnames(high_inoc_supp_table2)[3*(i-1)+j] <- paste(condition_vec[i],day_vec[j],sep = "_D")
  }
}

for (i in 1:ncol(high_inoc_supp_table2)) {
  high_inoc_supp_table2[,i] <- nis_by_con_ser_sou_day.ctr_df_subset$estimate[(6*(i-1)+1):(6*(i-1)+6)]
}
high_inoc_supp_table2 <- high_inoc_supp_table2[c(5,1,4,3,6,2),]


# End.















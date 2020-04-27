
## Project: NYSG R/SHH-18 part 1.
## Purpose of study: investigate the effect of pre-growth condition and
## strain diversity on the nisin efficacy against Listeria monocytogenes
## on cold-smoked salmon.
## Purpose of script: 
# i) Make phylogenetic tree (supplementary figure 1).

#library(tidyverse);library(emmeans);library(lme4);library(plyr);library(lmerTest)
#library(car);library(effects);library(lemon);library(dplyr)


setwd("/Users/ruixichen/OneDrive - Cornell University/Cornell document/Food safety lab/Salmon Project/pregrowth_condition/WGS work/6_strains_cgMLST/cgMLST Results_2/")

# Get list of *.results files
files <- list.files()[grepl(".results",list.files())]
# Read them all into a list of data.frames()
results <- lapply(files,read.table,col.names=c("genome","gene","allele"))

# Create empty data.frame, fill it, convert to "distance matrix"
shared_mat <- d_mat <- diag(0,length(results))
row.names(shared_mat) <- row.names(d_mat) <- abbreviate(unlist(lapply(results,FUN=function(x){as.character(x$genome[1])})), minlength=15)
colnames(shared_mat) <- colnames(d_mat) <- abbreviate(unlist(lapply(results,FUN=function(x){as.character(x$genome[1])})), minlength=15)

# Add asterisks before each of the strains used.
colnames(shared_mat)[93:98] <- row.names(shared_mat)[93:98] <- colnames(d_mat)[93:98] <- row.names(d_mat)[93:98] <- c("******FSL L4-0396", "******FSL L3-0051", "******FSL F2-0237", "******FSL F2-0310", "******FSL N1-0061", "******FSL L4-0060")

for( i in 1:(length(results)-1) ) {
  for( j in (i+1):length(results) ) {
    tmp <- left_join(results[[i]],results[[j]],by="gene") %>%
      mutate(diff=allele.x!=allele.y,
             shared=!(is.na(allele.x)|is.na(allele.y)))
    diff <- sum(tmp$diff,na.rm=T)
    shared <- sum(tmp$shared,na.rm=T)
    d_mat[i,j] <- d_mat[j,i] <- diff
    shared_mat[i,j] <- shared_mat[j,i] <- shared
  }
}

p_mat <- d_mat/shared_mat

# Make dendrogram from hclust object using complete linkage.
p_mat_dist <- as.dist(p_mat)
hc <- hclust(p_mat_dist,method="complete")
par(bg="white")
plot(hc, hang=-0.1, cex=0.65, xlab=" ", sub="Isolates", ylab="Percentage of difference in genome", main = " ")


# End.




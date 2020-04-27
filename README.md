# PregrowthListeria_Nisin
###### Project: NYSG R/SHH-18 part 1
###### Study: Pre-growth conditions and strain diversity affect nisin treatment efficacy against *Listeria monocytogenes* on cold-smoked salmon
###### Note: This repository contains codes and datasets for conducting statistical analysis and making figures & tables.

###### Description of the files.
1. pregrowth_MBHI_raw.txt
Difference in log10 transformed plate count (log10 (CFU/ml)) of *Listeria monocytogenes* between untreated and nisin-treated samples for MBHI experiment.
2. pregrowth_salmon_raw.csv
Log10 transformed plate count (log10 (CFU/g)) of *Listeria monocytogenes* for salmon experiment.
3. pregrowth_salmon_raw_LR.csv
Difference in log10 transformed plate count (log10 (CFU/g)) of *Listeria monocytogenes* between untreated and nisin-treated samples for salmon experiment.
4. hi_MBHI_lme.R
- Build linear mixed effects model for MBHI data using pregrowth_MBHI_raw.txt.
  - Response: difference in log10 (CFU/ml) of *Listeria monocytogenes* between untreated and nisin-treated samples.
  - Fixed effects:
    - pregrowth condition (Condition)
    - growth phase (Phase)
    - strain of *Listeria monocytogenes* (Strain)
  - Random effects:
    - each experiment trial conducted at different time (Group)
- Model diagnostics

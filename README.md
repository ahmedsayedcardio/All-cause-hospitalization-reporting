# All-cause-hospitalization-reporting
This repository contains the code related to an analysis of the reporting of all-cause hospitalization by heart failure trials reporting heart failure hospitalizations

Below is a brief elaboration on what each file does (the files should also be run/used in this order):

1. "**Import the data sheet.R**" will import the data in the "**Data Sheet.csv**" file. The "**Data Dictionary.xlsx**" file explains the different variables included in the datasheet.
3. "**Libraries.R**" will load the packages and functions required to run this analysis.
4. "**Meta-Analyses.R**" will run the meta-analyses required to determine whether treatment effects, on average, differ between trials that do versus do not report all-cause hospitalization.
5. "**Baseline Charactersitics Analyses.R**" will run descriptive analyses of baseline characteristics and the related logistic regression models.
6. "**Reporting of all-cause hospitalization according to fragility index.R**" will run analyses to determine whether the reporting of all-cause hospitalization is associated with fragility indices for significant heart failure hospitalization results.
7. "**Significance of all-cause hospitalization according to fragility index.R**" will run analyses to determine whether the statistical significance of all-cause hospitalization is associated with fragility indices for significant heart failure hospitalization results.
8. There are 4 figure files in this depository ("**Figure 2.R**", "**Figure 3.R**", "**Figure 4A.R**", and "**Figure 4B.R**"). Figure 1 is a PRISMA flow diagram that was not created on R.

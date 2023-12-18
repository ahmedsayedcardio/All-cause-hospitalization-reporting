#Calculate fragility index for HF hospitalization (depending on your device's computational power, this may take some time to run)
frag <- frag.studies(data= x %>% filter(!is.na(hf_hosp_events_i)),
                     e1 = hf_hosp_events_i,
                     e0 = hf_hosp_events_c,
                     n1 = hf_hosp_n_i,
                     n0 = hf_hosp_n_c)

#Attach fragility index and statistical significance for HFH to x
#Assign fragility indices for risk ratios and their P-value (dichotimized into statistical significance versus non-significance)
x[!is.na(x$hf_hosp_events_i), "rr_fi"] <- frag$FI %>% data.frame %>% {.[, "RR"]}
x[!is.na(x$hf_hosp_events_i), "rr_p"] <- frag$pval %>% data.frame %>% {.[, "RR"]}  %>% {ifelse(. < 0.05, "Sig", "NS")}
#Assign fragility indices for risk reductions and their P-value (dichotomized into statistical significance versus non-significance)
x[!is.na(x$hf_hosp_events_i), "rd_fi"] <- frag$FI %>% data.frame %>% {.[, "RD"]}
x[!is.na(x$hf_hosp_events_i), "rd_p"] <- frag$pval %>% data.frame %>% {.[, "RD"]}  %>% {ifelse(. < 0.05, "Sig", "NS")}

#Get N of studies that are statistically significant/non-significant
#By risk difference
x %>% group_by(rd_p) %>% summarise(n())
#By risk ratio
x %>% group_by(rr_p) %>% summarise(n())


#For all studies with significant results, run logistic regression models with
#the fragility index as the independent varible and all-cause hospitalization reporting
#as the dependent variable

###First, do this for all studies
##Fragility index for absolute risk difference
#Model
glm(data = x, family = binomial, subset = rd_p == "Sig",
    allh_reported ~ rd_fi) %>% anova(test = "LRT")
#Check if it is robust to the exclusion of the observed outlier in Figure 4B
glm(data = x, family = binomial, subset = rd_p == "Sig" & rd_fi < 180,
    allh_reported ~ rd_fi) %>% anova(test = "LRT")

#Test for interaction by protocol reporting
glm(data = x, family = binomial, subset = rd_p == "Sig",
    allh_reported ~ prot_report*rd_fi) %>% anova(.,
                                                 glm(data = x, family = binomial, subset = rd_p == "Sig",
                                                     allh_reported ~ rd_fi + prot_report),
                                                 test = "LRT")

##Fragility index for relative risk difference
#Model
glm(data = x, family = binomial, subset = rr_p == "Sig",
    allh_reported ~ rr_fi) %>% anova(test = "LRT")

#Test for interaction by protocol reporting
glm(data = x, family = binomial, subset = rr_p == "Sig",
    allh_reported ~ prot_report*rr_fi) %>% anova(.,
                                                 glm(data = x, family = binomial, subset = rr_p == "Sig",
                                                     allh_reported ~ rr_fi + prot_report),
                                                 test = "LRT")

###Then, do this for trials where all-cause hospitalization WAS pre-specified in the protocol
##Fragility index for absolute risk difference
#N of studies
x %>% filter(rd_p == "Sig") %>%
  group_by(prot_report, allh_reported) %>%
  summarise(n())
#Model
glm(data = x, family = binomial, subset = rd_p == "Sig" & prot_report == "Yes",
    allh_reported ~ rd_fi) %>% anova(test = "LRT")


##Fragility index for relative risk difference
#N of studies
x %>% filter(rr_p == "Sig") %>%
  group_by(prot_report, allh_reported) %>%
  summarise(n())
#Model
glm(data = x, family = binomial, subset = rr_p == "Sig" & prot_report == "Yes",
    allh_reported ~ rr_fi) %>% anova(test = "LRT")

#Test for interaction by protocol reporting
glm(data = x, family = binomial, subset = rr_p == "Sig" & prot_report == "Yes",
    allh_reported ~ prot_report*rr_fi) %>% anova(.,
                                                 glm(data = x, family = binomial, subset = rr_p == "Sig" & prot_report == "Yes",
                                                     allh_reported ~ rr_fi + prot_report),
                                                 test = "LRT")

###Then, do this for trials where all-cause hospitalization was NOT pre-specified in the protocol
##Fragility index for absolute risk difference
#N of studies
x %>% filter(rd_p == "Sig" & prot_report == "No") %>%
    group_by(allh_reported) %>%
    summarise(n())
#Model
glm(data = x, family = binomial, subset = rd_p == "Sig" & prot_report == "No",
    allh_reported ~ rd_fi) %>% anova(test = "LRT")

##Fragility index for relative risk difference
#N of studies
x %>% filter(rr_p == "Sig" & prot_report == "Yes") %>%
  group_by(allh_reported) %>%
  summarise(n())
#Model
glm(data = x, family = binomial, subset = rr_p == "Sig" & prot_report == "No",
    allh_reported ~ rr_fi) %>% anova(test = "LRT")


###Now, do this in studies where the statistically significant change was a reduction only
##Fragility index for absolute risk difference
glm(data = x, family = binomial, subset = rd_p == "Sig" & prot_report == "No" & hfh_ard_te < 0,
    allh_reported ~ rd_fi) %>% anova(test = "LRT")
##Fragility index for relative risk difference
glm(data = x, family = binomial, subset = rr_p == "Sig" & prot_report == "No" & hfh_rrr_te < 0,
    allh_reported ~ rr_fi) %>% anova(test = "LRT")


#In studies without a pre-specified protocol (taking their word in the trial manuscript)
x %>% filter(rd_p == "Sig") %>% group_by(prot_report_sup, allh_reported) %>% summarise(n())
##Fragility index for absolute risk difference
glm(data = x, family = binomial, subset = rd_p == "Sig" & prot_report_sup == "No",
    allh_reported ~ rd_fi) %>% anova(test = "LRT")
##Fragility index for relative risk difference
glm(data = x, family = binomial, subset = rr_p == "Sig" & prot_report_sup == "No",
    allh_reported ~ rr_fi) %>% anova(test = "LRT")


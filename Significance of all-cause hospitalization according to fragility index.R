# All-cause hosp ----------------------------------------------------------
#Calculate statistical significance for all-cause hospitalizations (the same fragility index function does this)
frag_all_hosp <- frag.studies(data= x %>% filter(!is.na(all_hosp_events_i)),
                              e1 = all_hosp_events_i,
                              e0 = all_hosp_events_c,
                              n1 = all_hosp_n_i,
                              n0 = all_hosp_n_c)

#Insert significance & P-value of all cause hosp by fragility index for HF (RD)
x[!is.na(x$all_hosp_events_i), "allhosp_rd_p"] <- frag_all_hosp$pval %>% data.frame %>% {.[, "RD"]}  %>% {ifelse(. < 0.05, "Sig", "NS")}
#Significance of all cause hosp by fragility index for HF (RR)
x[!is.na(x$all_hosp_events_i), "allhosp_rr_p"] <- frag_all_hosp$pval %>% data.frame %>% {.[, "RR"]}  %>% {ifelse(. < 0.05, "Sig", "NS")}

#For COAPT, replace with "Sig" to make it compatible with the P reported in the trial
#This has to be done because the frag.studies function calculates P-values in a slightly
#different manner to that used in the original trial. This trial (COAPT) is the only one
#where this difference in calculating P-values results in discrepant significant/non-significant classification
x[x$trial == "COAPT", c("allhosp_rd_p", "allhosp_rr_p")] <- "Sig"
#Insert statistical significance values for trials which did not report all-hosp on a per patient basis
#for which frag.studies could not be used (These values are obtained from the trial publications).
x[x$trial == "REHAB-HF", c("allhosp_rd_p", "allhosp_rr_p")] <- "NS"
x[x$trial == "Troughton et al., 2000", c("allhosp_rd_p", "allhosp_rr_p")] <- "NS"
x[x$trial == "CUPID 2", c("allhosp_rd_p", "allhosp_rr_p")] <- NA #Raw numbers for total number presented but not comapred
x[x$trial == "Val-HeFT", c("allhosp_rd_p", "allhosp_rr_p")] <- "NS"
x[x$trial == "EMPEROR-Preserved", c("allhosp_rd_p", "allhosp_rr_p")] <- "NS"
x[x$trial == "CHAMPION-HF", c("allhosp_rd_p", "allhosp_rr_p")] <- "Sig"
x[x$trial == "DELIVER", c("allhosp_rd_p", "allhosp_rr_p")] <- "Sig"
x[x$trial == "MONITOR-HF", c("allhosp_rd_p", "allhosp_rr_p")] <- NA #Raw numbers for total number presented but not comapred
#Factor it
x$allhosp_rd_p <- factor(x$allhosp_rd_p)
x$allhosp_rr_p <- factor(x$allhosp_rr_p)

##Model statistical significance of all-cause hospitalization finding
##according to the fragility index
#Using fragility index for absolute risk difference
glm(data = x, family = binomial, subset = rd_p == "Sig", 
    allhosp_rd_p ~ rd_fi) %>% anova(test = "LRT")
#Check if it is robust to the exclusion of the outlier seen in Figure 4A
glm(data = x, family = binomial, subset = rd_p == "Sig" & rd_fi < 150,
    allhosp_rd_p ~ rd_fi) %>% anova(test = "LRT")
#Using fragility index for relative risk difference
glm(data = x, family = binomial, subset = rr_p == "Sig", 
    allhosp_rr_p ~ rr_fi) %>% anova(test = "LRT")
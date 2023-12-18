
#Conduct meta-analysis subgrouped by whether or not trial reported all-cause hospitalization
#Absolute risk differences (for all trials)
meta_hfh_ard <- metabin(data = x,
                        event.e = hf_hosp_events_i,
                        event.c = hf_hosp_events_c,
                        n.e = hf_hosp_n_i,
                        n.c = hf_hosp_n_c,
                        sm = "RD",
                        subgroup = allh_reported,
                        tau.common = TRUE)
x$hfh_ard_te <- meta_hfh_ard$TE

#Relative risk differences (for all trials)
meta_hfh_rrr <- metabin(data = x,
                        event.e = hf_hosp_events_i,
                        event.c = hf_hosp_events_c,
                        n.e = hf_hosp_n_i,
                        n.c = hf_hosp_n_c,
                        sm = "RR",
                        subgroup = allh_reported,
                        tau.common = TRUE)
x$hfh_rrr_te <- meta_hfh_rrr$TE

#Absolute risk differences (pharmaceutical interventions)
meta_drugs_hfh_ard <- metabin(data = x %>% filter(intvn_class == "Pharmaceutical"),
                              event.e = hf_hosp_events_i,
                              event.c = hf_hosp_events_c,
                              n.e = hf_hosp_n_i,
                              n.c = hf_hosp_n_c,
                              sm = "RD",
                              subgroup = allh_reported,
                              tau.common = TRUE)
x$hfh_drugs_ard_te[x$intvn_class == "Pharmaceutical"] <- meta_drugs_hfh_ard$TE

#Relative risk differences (pharmaceutical interventions)
meta_drugs_hfh_rrr <- metabin(data = x %>% filter(intvn_class == "Pharmaceutical"),
                              event.e = hf_hosp_events_i,
                              event.c = hf_hosp_events_c,
                              n.e = hf_hosp_n_i,
                              n.c = hf_hosp_n_c,
                              sm = "RR",
                              subgroup = allh_reported)
x$hfh_drugs_rrr_te[x$intvn_class == "Pharmaceutical"] <- meta_drugs_hfh_rrr$TE

#Absolute risk differences (non-pharmaceutical interventions)
meta_non_drugs_hfh_ard <- metabin(data = x %>% filter(intvn_class != "Pharmaceutical"),
                                  event.e = hf_hosp_events_i,
                                  event.c = hf_hosp_events_c,
                                  n.e = hf_hosp_n_i,
                                  n.c = hf_hosp_n_c,
                                  sm = "RD",
                                  subgroup = allh_reported,
                                  tau.common = TRUE)
x$hfh_non_drugs_ard_te[x$intvn_class != "Pharmaceutical"] <- meta_non_drugs_hfh_ard$TE

#Relative risk differences (non-pharmaceutical interventions)
meta_non_drugs_hfh_rrr <- metabin(data = x %>% filter(intvn_class != "Pharmaceutical"),
                                  event.e = hf_hosp_events_i,
                                  event.c = hf_hosp_events_c,
                                  n.e = hf_hosp_n_i,
                                  n.c = hf_hosp_n_c,
                                  sm = "RR",
                                  by = allh_reported)
x$hfh_non_drugs_rrr_te[x$intvn_class != "Pharmaceutical"] <- meta_non_drugs_hfh_rrr$TE

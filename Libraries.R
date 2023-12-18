#These are the packages needed for this analysis
packs <- c("dplyr", "tidyr", "meta", "forcats", 
           "ggplot2", "readxl", "broom", "rms",
           "rmarkdown", "palmerpenguins", "quarto", "readxl",
           "stringr", "Hmisc", "doParallel", "psychometric",
           "betareg", "doParallel" , "grid", "ggpubr", "boot", "flextable",
           "officer", "fragility", "ggbeeswarm", "ggh4x", "ggsci")

lapply(packs, require, character.only = T)

#This is a self-contstructed function to retrieve effect sizes from meta objects
get_estimates <- function(meta_object, es) {
  
  #Get estimates
  point_estimate <- meta_object$TE.random.w
  lower_ci <- meta_object$lower.random.w
  upper_ci <- meta_object$upper.random.w
  upper_ci <- meta_object$upper.random.w
  subgroups <- meta_object$byvar %>% levels
  treatment_effects <- data.frame(uci = upper_ci, lci = lower_ci, 
                                  pe = point_estimate, sg = subgroups,
                                  es = es)
  
  
  #Do what is necessary according to effect size
  if(es == "rd") {
    treatment_effects %>% mutate(across(where(is.numeric), ~ (. * -100) %>% r1 %>% as.numeric))
  } else if(es == "rr") {
    treatment_effects %>% mutate(across(where(is.numeric), ~ {1 - exp(.)} %>% {. * -100} %>% r1 %>% as.numeric))
  }
  
}

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

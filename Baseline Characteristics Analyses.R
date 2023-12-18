#Get N of trials that report versus did not report on HF hospitalization
table(x$allh_reported)
table(x$allh_reported) %>% proportions * 100
#For each baseline characteristic:
#A) Run descriptive characteristics
#B) Assess association with reporting (or lack thereof) with all-cause hospitalization
#Using a likelihood ratio test
#Publication year
table(x$allh_reported) %>% proportions * 100
glm(data = x, family = binomial, allh_reported ~ pub_year) %>% anova(test = "LRT")

#Commercial funding
table(x$allh_reported, x$private)
table(x$allh_reported, x$private) %>% proportions(margin = 2) * 100
glm(data = x, family = binomial, allh_reported ~ private) %>% anova(test = "LRT")

#Class of intervention (pharmaceutical versus non-pharmaceutical)
table(x$allh_reported, x$intvn_class)
table(x$allh_reported, x$intvn_class) %>% proportions(margin = 2) * 100
glm(data = x, family = binomial, allh_reported ~ intvn_class) %>% anova(test = "LRT")

#Journal
table(x$allh_reported, x$journal)
table(x$allh_reported, x$journal) %>% proportions(margin = 2) * 100
glm(data = x, family = binomial, allh_reported ~ journal) %>% anova(test = "LRT")

#Type of HF
table(x$allh_reported, x$hf_type)
table(x$allh_reported, x$hf_type) %>% proportions(margin = 2) * 100
glm(data = x, family = binomial, allh_reported ~ hf_type) %>% anova(test = "LRT")

#LVEF
quantile(x$lvef, na.rm = TRUE)
glm(data = x, family = binomial, allh_reported ~ lvef) %>% anova(test = "LRT")

#NYHA
quantile(x$nyha, na.rm = TRUE)
glm(data = x, family = binomial, allh_reported ~ nyha) %>% anova(test = "LRT")

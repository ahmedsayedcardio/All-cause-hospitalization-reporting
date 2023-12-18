#Read the CSV file
x <- read.csv("Data Sheet.csv")

#Make relevant variables facators
x <- x %>% make_factor(.q(allh_reported, prot_reported, intvn_class, hf_type))

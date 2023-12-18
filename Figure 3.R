#Build dataframe for figure 3 using the self-constructed function "get estimates"
fig3_df <- rbind(
get_estimates(meta_hfh_ard, "rd") %>% mutate(intvn_class = "Overall"),
get_estimates(meta_drugs_hfh_ard, "rd") %>% mutate(intvn_class = "Pharmaceutical"),
get_estimates(meta_non_drugs_hfh_ard, "rd") %>% mutate(intvn_class = "Non-pharmaceutical"),
get_estimates(meta_hfh_rrr, "rr") %>% mutate(intvn_class = "Overall"),
get_estimates(meta_drugs_hfh_rrr, "rr") %>% mutate(intvn_class = "Pharmaceutical"),
get_estimates(meta_non_drugs_hfh_rrr, "rr") %>% mutate(intvn_class = "Non-pharmaceutical")
)

#Rename content of es (rd and rr) to full names (absolute/relative risk differences)
fig3_df$es <- fig3_df$es %>% str_replace_all(c("rd" = "Absolute risk reduction",
                                 "rr" = "Relative risk reduction"))

#Factor intervention type and order it such that overall comes first and pharmaceutical comes second
fig3_df$intvn_class <- fig3_df$intvn_class %>%
  factor %>%
  fct_relevel("Overall", "Pharmaceutical")

#Plot it
ggplot(data = fig3_df,
               aes(x = sg,
                   y = pe,
                   col = sg)) +
  facet_grid(es ~ intvn_class, scales = "free_y") +
  geom_point(size = 6, shape = 18) +
  geom_errorbar(size = 1, width = 0.25,
                aes(
                  ymin = lci,
                  ymax = uci,
                  col = sg)) +
  scale_color_jama(name = "Reported on all-cause hospitalization?") +
  ggtitle("Effect sizes of interventions according to the reporting of all-cause hospitalization.") +
  scale_y_continuous(expand = expand_scale(0.05),
                     name = "\nEffect size on heart failure hospitalization(%)\n") +
  scale_x_discrete(name = NULL, labels = NULL) +
  theme_bw() +
  theme(text = element_text(size = 23, face = "bold"),
        plot.title=element_text(face = "bold",hjust = 0.5, size = 25),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 25, face = "bold"),
        axis.line = element_line(colour = "black", size = 0),
        legend.position = "bottom",
        legend.key.size = unit(1.25, "cm")) 


#Save
ggsave(filename = paste0(save_to_this_folder, "Figure 3.png"), 
       dpi = 600,
       height = 9, width = 16)


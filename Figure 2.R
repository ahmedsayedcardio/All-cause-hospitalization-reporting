#Plot trends across time
fig2 <- ggplot(data = x,
               aes(x = pub_year,
                   y = ..count..,
                   fill = allh_reported)) +
  geom_density(position = "fill") +
  ggtitle("Reporting of all-cause hospitalization among trials that\n reported heart failure hospitalization, 1991 to 2023") +
  scale_y_continuous(expand = expand_scale(0.01),
                     name = "\nReporting of all-cause hospitalization (%)\n\nYes                             No",
                     labels = seq(0, 100, 25)) +
  scale_x_continuous(expand = expand_scale(0),
                     name = "Year of publication",
                     breaks = seq(1995, 2020, 5),
                     limits = c(1990, 2024)) +
  scale_fill_discrete(name = "Reported all-cause\nhospitalization?") +
  theme_bw() +
  theme(text = element_text(size = 23, face = "bold"),
        plot.title=element_text(face = "bold",hjust = 0.5, size = 30),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 25, face = "bold"),
        axis.line = element_line(colour = "black", size = 0),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.key.size = unit(1.25, "cm")) 

ggsave(filename = paste0(save_to_this_folder, "Figure 2.png"), 
       dpi = 600,
       height = 9, width = 16)

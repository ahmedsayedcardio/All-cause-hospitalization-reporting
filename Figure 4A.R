#Create df for fig 4a
fig4a_df <- x %>% filter(rd_p == "Sig" & !is.na(allhosp_rd_p))
fig4a_df$allhosp_rd_p <- fig4a_df$allhosp_rd_p %>% str_replace_all(c(
  "Sig" = "Yes",
  "NS" = "No"
)) 

#Set axis title size
axis_tsize <- 20

#Set panel size
panel_size <- force_panelsizes(rows = unit(7.5, "in"),
                               cols = unit(6, "in"))

fig4a <- ggplot(data = fig4a_df,
               aes(x = allhosp_rd_p,
                   y = rd_fi,
                   col = allhosp_rd_p)) +
  geom_boxplot() +
  scale_color_jama() +
  geom_beeswarm(cex = 4, size = 4, alpha = 1) +
  scale_x_discrete(expand = expand_scale(1),
                     name = "Difference in all-cause hospitalization\nstatistically significant?") +
  scale_y_continuous(expand = expand_scale(0.05),
                     name = "\nFragility index for heart failure hospitalizations\n",
                     breaks = seq(0, 10000, 40)) +
  theme_bw() +
  theme(text = element_text(size = 23, face = "bold"),
        plot.title=element_text(face = "bold",hjust = 0.5, size = 30),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = axis_tsize, face = "bold"),
        axis.title.y = element_text(size = axis_tsize, face = "bold"),
        axis.line = element_line(colour = "black", size = 0),
        legend.position = "none",
        legend.key.size = unit(1.25, "cm")) +
  panel_size


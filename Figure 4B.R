#Create fig5_df
fig4b_df <- x %>% filter(rd_p == "Sig")

#ggplot it
fig4b <- ggplot(data = fig4b_df,
               aes(x = allh_reported,
                   y = rd_fi,
                   col = allh_reported)) +
  geom_boxplot(outlier.shape = NA) +
  geom_beeswarm(cex = 4, size = 4, alpha = 1) +
  scale_color_jama() +
  scale_y_continuous(expand = expand_scale(0.05),
                     name = "\nFragiltiy index for heart failure hospitalizations\n",
                     breaks = seq(0, 1000, 20)) +
  scale_x_discrete(expand = expand_scale(1),
                     name = "Reported on all-cause hospitalization?\n") +
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


#Merge Figure 4B with Figure 4A
ggarrange(fig4a, fig4b, nrow = 1, labels = c("A", "B"),
          font.label = list(size = 28), heights = c(0.8, 0.8)) +
  bgcolor("White")

#Insert a vertical line to separate them
grid.polygon(id = c(1, 1),
             x = c(0.5, 0.5),
             y = c(0.9975, 0.0025),
             gp = gpar(lwd = 3)
             )
#Save this output to "fig4" object
fig4 <- grid.grab() %>% ggplotify::as.ggplot()

#Save the figure
ggsave(filename = paste0(save_to_this_folder, "Figure 4.png"), 
       dpi = 600,
       height = 9, width = 16)

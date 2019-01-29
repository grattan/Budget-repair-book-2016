library(grattan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readxl)
library(lubridate)

read_excel("FIgure1-1.xlsx") %>%
  mutate(Age = factor(Age, levels = unique(.$Age), ordered = TRUE)) %>%
  gather(fy_year, average_net_benefit, -Age) %>%
  filter(complete.cases(.)) %>%
  {
    grplot(., aes(x = Age, y = average_net_benefit, fill = fy_year), reverse = TRUE) + 
      geom_bar(stat = "identity", position = "dodge") + 
      scale_y_continuous(label = grattan_dollar) + 
      theme(panel.grid.major.y = element_line()) + 
      geom_hline(yintercept = 0) +
      annotate("rect", 
               xmin = 1, xmax = 2, 
               ymin = 18e3 + (0:4) * 2500, 
               ymax = 18e3 + (1:5) * 2500, 
               colour = "black",
               fill = gpal(5)) + 
      annotate("text", 
               x = 1.5,
               size = 20/(14/5),
               y = 18e3 + 2500 * (1:5) - 2500/2, 
               label = rev(unique(.$fy_year)),
               #fontface = "bold",
               colour = c("white", rep("black", length(unique(.$fy_year)) - 1)))
  }
dev.copy(device = cairo_pdf, file = "Figure1-1a.pdf", width = 11, height = 7)
dev.off()


read_excel("FIgure1-1.xlsx") %>%
  mutate(Age = factor(Age, levels = unique(.$Age), ordered = TRUE)) %>%
  gather(fy_year, average_net_benefit, -Age) %>%
  filter(complete.cases(.)) %>%
  group_by(Age) %>%
  mutate(age_label = 
           if (all(average_net_benefit < 0)){
             ifelse(average_net_benefit == min(average_net_benefit), 
                    as.character(Age), 
                    NA_character_)
           } else {
             ifelse(average_net_benefit == max(average_net_benefit), 
                    as.character(Age), 
                    NA_character_)
           }, 
         vjust = if (all(average_net_benefit < 0)) "top" else "bottom", 
         Year = as.Date(paste0(fy2yr(fy_year), "-01-01"))) %>%
  {
    grplot(., aes(x = Year, y = average_net_benefit, fill = Age, colour = Age, group = Age), reverse = TRUE) + 
      geom_area(alpha = 0.5, colour = NA) + 
      geom_text(aes(label = age_label, y = average_net_benefit + sign(average_net_benefit) * 500, vjust = vjust),
                hjust = "inward",
                size = 20/(14/5),
                family = "helvet",
                fontface = "bold",
                na.rm = TRUE) + 
      geom_line(size = 1.5) + 
      scale_y_continuous(label = grattan_dollar) + 
      scale_x_date(label = function(x) ifelse(year(x) %% 10 == 5, as.character(year(x)), "")) +
      theme(panel.grid.major.y = element_line()) + 
      facet_grid(~Age) + 
      theme_hugh(base_family = "helvet") + 
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_blank(),
            strip.background = element_blank(), 
            strip.text = element_blank())
  }
dev.copy(device = pdf, file = "Figure1-1area-helvet.pdf", width = 11, height = 7)

showtext::showtext.end()
dev.off()

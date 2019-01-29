# Figure 3.5 and 3.6 from super report, multiple implementations

library(ggplot2)
library(grattan)
library(data.table)
library(dplyr)
library(tidyr)
library(readxl)
library(grid)

fig35 <- 
  read_excel("fig35.xlsx") %>%
  mutate(housing_inclusion = factor(housing_inclusion, levels = c("Including housing", "Excluding housing")))

asfa_standards <- 
  read_excel("ASFA-retirement-standards.xlsx") %>%
  gather(housing_inclusion, expenditure, -Standard, -relationship_status) %>%
  spread(Standard, expenditure) 

# Original
fig35 %>%
  merge(asfa_standards) %T>%
  {stopifnot(nrow(.) == nrow(fig35))} %>%
  filter(complete.cases(.)) %>%
  filter(relationship_status == "Single") %>%
  mutate(asfa_comfortable_label = ifelse(Percentile == 10 & housing_inclusion == "Including housing", 
                                         "ASFA\nComfortable", 
                                         NA_character_), 
         asfa_modest_label = ifelse(Percentile == 10 & housing_inclusion == "Including housing", 
                                    "ASFA\nModest", 
                                    NA_character_)) %>%
  mutate(Percentile = factor(Percentile)) %>%
  grplot() + 
  geom_bar(aes(x = Percentile, fill = working, y = expenditure), 
           stat = "identity", position = "dodge") +
  geom_hline(aes(yintercept = `ASFA comfortable`), colour = theGrey, size = 1.25) +
  geom_text(aes(x = Percentile, y = `ASFA comfortable`, label = asfa_comfortable_label),
            hjust = 0, 
            nudge_x = -0.5,
            vjust = -0.1,
            colour = theGrey,
            fontface = "bold", 
            lineheight = 0.9,
            size = 20/(14/5),
            na.rm = TRUE) +
  geom_text(aes(x = Percentile, y = `ASFA modest`, label = asfa_modest_label),
            hjust = 0, 
            nudge_x = -0.5,
            vjust = -0.1,
            colour = theGrey,
            lineheight = 0.9,
            size = 20/(14/5),
            na.rm = TRUE) +
  geom_hline(aes(yintercept = `ASFA modest`), colour = theGrey) +
  facet_grid(~ housing_inclusion) + 
  theme(strip.background = element_rect(fill = theGrey), 
        strip.text = element_text(colour = "white", face = "bold")) + 
  geom_blank(aes(x = Percentile, y = expenditure * 1.03)) + 
  scale_y_continuous(label = grattan_dollar, breaks = seq(0, 120e3, by = 20e3), expand = c(0,0))

fig35 %>% 
  merge(asfa_standards) %T>%
  {stopifnot(nrow(.) == nrow(fig35))} %>%
  filter(complete.cases(.)) %>%
  filter(relationship_status == "Couple") %>%
  mutate(asfa_comfortable_label = ifelse(Percentile == 10 & housing_inclusion == "Including housing", 
                                         "ASFA\nComfortable", 
                                         NA_character_), 
         asfa_modest_label = ifelse(Percentile == 10 & housing_inclusion == "Including housing", 
                                    "ASFA\nModest", 
                                    NA_character_)) %>%
  mutate(Percentile = factor(Percentile)) %>%
  grplot() + 
  geom_bar(aes(x = Percentile, fill = working, y = expenditure), 
           stat = "identity", position = "dodge") +
  geom_hline(aes(yintercept = `ASFA comfortable`), colour = theGrey, size = 1.25) +
  geom_text(aes(x = Percentile, y = `ASFA comfortable`, label = asfa_comfortable_label),
            hjust = 0, 
            nudge_x = -0.5,
            vjust = -0.1,
            colour = theGrey,
            fontface = "bold", 
            lineheight = 0.9,
            size = 20/(14/5),
            na.rm = TRUE) +
  geom_text(aes(x = Percentile, y = `ASFA modest`, label = asfa_modest_label),
            hjust = 0, 
            nudge_x = -0.5,
            vjust = -0.1,
            colour = theGrey,
            lineheight = 0.9,
            size = 20/(14/5),
            na.rm = TRUE) +
  geom_hline(aes(yintercept = `ASFA modest`), colour = theGrey) +
  facet_grid(~ housing_inclusion) + 
  theme(strip.background = element_rect(fill = theGrey), 
        strip.text = element_text(colour = "white", face = "bold")) + 
  geom_blank(aes(x = Percentile, y = expenditure * 1.03)) + 
  scale_y_continuous(label = grattan_dollar, breaks = seq(0, 120e3, by = 20e3), expand = c(0,0))
  

fig35p <- 
  fig35 %>%
  merge(asfa_standards) %T>%
  {stopifnot(nrow(.) == nrow(fig35))} %>%
  filter(complete.cases(.)) %>%
  mutate(Percentile = factor(Percentile, levels = c(1:9)*10, ordered = TRUE)) %>%
  {
    grplot(., aes(x = Percentile, fill = working, y = expenditure)) + 
      geom_bar(stat = "identity", position = "dodge") +
      geom_hline(aes(yintercept = `ASFA comfortable`), colour = theGrey, size = 1.25) +
      geom_hline(aes(yintercept = `ASFA modest`), colour = theGrey) +
      facet_grid(relationship_status ~ housing_inclusion, scales = "free_y", switch = "y") + 
      theme(strip.background = element_rect(fill = theGrey), 
            strip.text = element_text(colour = "white", face = "bold")) + 
      geom_blank(aes(x = Percentile, y = expenditure * 1.01)) + 
      scale_y_continuous(label = grattan_dollar,  expand = c(0,0)) + 
      geom_text(data = data_frame(x = c(9.75, 9.75),
                                  y = c(42861, 58784),
                                  working = c("Retired", "Retired"),
                                  label = c("ASFA\ncomfortable", "ASFA\ncomfortable"),
                                  relationship_status = c("Single", "Couple"),
                                  housing_inclusion = "Including housing"),
                mapping = aes(x = x, y = y, label = label),
                size = 20/(14/5),
                fontface = "bold", 
                colour = theGrey,
                lineheight = 0.9,
                vjust = 1, 
                hjust = 0) + 
      theme(plot.margin = grid::unit(c(0.7, 7, 0.5, 0), "lines"))
  }

gt <- ggplot_gtable(ggplot_build(fig35p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.newpage()
grid.draw(gt)
dev.copy2pdf(file = "fig3-5and6-a.pdf", width = 11*1.6, height = 6*1.6)

fig35 %>%
  grplot(aes(y = 100 - Percentile, x = expenditure)) + 
  geom_line(aes(
    group = interaction(relationship_status, housing_inclusion, working), 
    color = interaction(relationship_status, housing_inclusion, working) 
    )) + 
  facet_grid(relationship_status ~ housing_inclusion)
  

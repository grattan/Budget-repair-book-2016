data_fig_16 <- 
  read.table(text="Gearing ratio 	Current EMTRs	25% CGT discount, quarantining NG
Equity financed           (No gearing)	40.6%	48.8%
Borrowing 40% investment value (Positive gearing)	29.3%	37.2%
Borrowing 80% of investment value (Negative gearing)	18.8%	26.8%
", sep = "\t", header = TRUE, check.names = FALSE, as.is = TRUE) %>%
  gather(fill, y, -`Gearing ratio`) %>%
  mutate(y = as.numeric(gsub("%", "", as.character(y))) / 100, 
         fill = gsub("EMTRS", "", fill, ignore.case = TRUE))

data_fig_16 %>%
  grplot(aes(x = `Gearing ratio`, y, fill = fill), reverse = TRUE) + 
  geom_bar(stat = "identity", width = 0.8, position = "dodge") + 
  scale_y_continuous(label = percent, expand = c(0,0), limits = c(0, 0.61)) + 
  theme(legend.position = c(1, 1), 
        legend.background = element_blank(),
        legend.justification = c(1,1),
        legend.margin = unit(0, "lines"),
        legend.title = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  geom_hline(yintercept = 0.47, linetype = "dashed") + 
  annotate("text", 
           x = Inf, 
           hjust = 1,
           y = 0.465, 
           size = 20/(14/5),
           vjust = 1, 
           label = "Nominal tax rate") 

dev.copy2pdf(file = "Figure16-R.pdf", width = 11, height = 7)

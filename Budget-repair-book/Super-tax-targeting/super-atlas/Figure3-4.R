library(grattan)
library(ggplot2)
library(scales)
library(foreign)
library(dplyr)
library(magrittr)
library(survey)

library(data.table)
library(tidyr)

reload_hilda = TRUE

# Most wealthy retirees have annual incomes above $50,000,
# mostly from sources outside super

# <<where-is-gitignore>>=
where_is_gitignore <- function(search.ceiling = 10L){
  if (file.exists(".gitignore")){
    return(".")
  } else {
    current.dir <- "."
    for (i in 1:search.ceiling){
      current.dir <- file.path(current.dir, "..")
      if (file.exists(file.path(current.dir, ".gitignore"))){
        break;
      }
      if (i == search.ceiling){
        stop(".gitignore not found in any parent directory.", "\n", "Ensure you are in a Github repository.")
      }
      
    }
    return(current.dir)
  }
}
#' @

# % I'm so sorry everyone
# <<git-path>>=
.my.git.path <- where_is_gitignore()
#' 

negative_to_NA <- function(x) {
    if (is.numeric(x)){
      x[x < 0] <- NA
    }
    
    return(x)
  }

hilda_hh <- 
  read.dta(file.path(.my.git.path, "../Data/HILDA/Wave14/Household_n140c.dta")) %>%
  mutate_each(funs(negative_to_NA))

# hilda_data_dictionary <- fread(file.path(.my.git.path, "../Data/HILDA/data_dictionary.csv"))

hilda_wealth_basic <- 
  hilda_hh %>%
  data.table %>%
  select(hh_id = nhhrhid, 
         hh_pop_wt = nhhwth, 
         positive_income = nhifeftp, 
         negative_income = nhifeftn, 
         positive_net_wealth = nhwnwip,
         negative_net_wealth = nhwnwin, 
         total_super_asset = nhwsuper) %>%
  mutate(hh_income = positive_income - negative_income, 
         net_wealth = positive_net_wealth - negative_net_wealth) %>%
  select(-starts_with("positive"), -starts_with("negative"))

wealth_quintiles <- 
  hilda_wealth_basic %>%
  svydesign(ids = ~hh_id, weights = ~hh_pop_wt, data = .) %>%
  svyquantile(~net_wealth, design = ., quantiles = c(0:5)/5)

hh_income_quantiles <- 
  hilda_wealth_basic %>%
  svydesign(ids = ~hh_id, weights = ~hh_pop_wt, data = .) %>%
  svyquantile(~hh_income, design = ., quantiles = c(0:40)/40)  

hilda_wealth_basic %<>%
  mutate(wealth_quintile = .bincode(net_wealth, 
                                    breaks = wealth_quintiles, 
                                    include.lowest = TRUE)) %>%
  setkey(hh_id)

if (reload_hilda){
  hilda_rp <- 
    read.dta(file.path(.my.git.path, "../Data/HILDA/Wave14/Rperson_n140c.dta")) %>%
    mutate_each(funs(negative_to_NA))
}

hilda_income_basic_rp <- 
  hilda_rp %>%
  data.table %>%
  select(hh_id = nhhrhid, 
         xwaveid, 
         age = nhgage,
         wages = nwsfes, 
         super_income = noifsupi, 
         investment_income_p = noifinip, 
         investment_income_n = noifinin, 
         gross_regular_income_p =  ntifefp, 
         gross_regular_income_n = ntifefn) %>%
  mutate(investment_income = investment_income_p - investment_income_n, 
         gross_regular_income = gross_regular_income_p - gross_regular_income_n) %>%
  group_by(hh_id) %>%
  mutate(all_over_65 = all(age >= 65), 
         prop_over_65 = mean(age >= 65)) %>%
  ungroup %>%
  select(-ends_with("_p"), -ends_with("_n"))

hilda_basic_income_by_hh <- 
  hilda_income_basic_rp %>%
  group_by(hh_id) %>%
  summarise_each(funs(sum), -xwaveid) %>%
  setkey(hh_id)

hilda_basic_income_by_hh_65p <- 
  hilda_income_basic_rp %>%
  filter(all_over_65) %>%
  select(-age, -all_over_65, -prop_over_65) %>%
  group_by(hh_id) %>%
  summarise_each(funs(sum), -xwaveid) %>%
  ungroup %>%
  setkey(hh_id) 

# ====
p <- 
  hilda_basic_income_by_hh_65p %>%
  merge(hilda_wealth_basic) %>%
  {
    dot = .
    income_percentiles <- 
      svydesign(~hh_id, weights = ~hh_pop_wt, data = dot) %>%
      svyquantile(~gross_regular_income, design = ., quantiles = (0:10)/10)
    
    dot %>%
      mutate(income_percentile = .bincode(gross_regular_income, 
                                          breaks = income_percentiles, 
                                          include.lowest = TRUE))
  } %>%
  mutate(other_income = gross_regular_income - super_income) %>%
  gather(income_source, income, other_income, super_income) %>%
  data.table %>%
  group_by(wealth_quintile2 = wealth_quintile == 5, 
           income_percentile, income_source) %>%
  summarise(total_income = sum(income * hh_pop_wt), 
            mean_income = weighted.mean(income, hh_pop_wt)) %>%
  ungroup %>%
  filter(income_percentile < max(income_percentile), 
         wealth_quintile2) %>%
  mutate(income_source = factor(income_source, 
                                levels = c("super_income", "other_income"), 
                                labels = c("Super\nincome", "Other"), 
                                ordered = TRUE)) %>%
  mutate(text.label = ifelse(income_percentile == max(income_percentile), 
                       as.character(income_source), 
                       NA_character_)) %>%
  group_by(wealth_quintile2, income_percentile) %>%
  arrange(income_source) %>%
  mutate(text.y = -mean_income/2 + cumsum(mean_income), 
         income_percentile2 = as.double(income_percentile - .5)) %>%
  grplot(aes(x = income_percentile2, 
             y = mean_income, 
             fill = income_source, order = income_source)) + 
  geom_text(aes(label = text.label, y = text.y, colour = income_source),
            hjust = 0,
            nudge_x = 0.2,
            fontface = "bold", 
            lineheight = 0.9,
            size = 20/(14/5),
            na.rm = TRUE) + 
  geom_area(position = "stack", colour = "black") + 
  scale_y_continuous(label = grattan_dollar, expand = c(0,0)) + 
  scale_x_continuous(limits = c(0.5, 9.5), 
                     expand = c(0,0), 
                     label = function(x) percent(x / 10)) +
  theme(legend.position = "none", 
        plot.margin = unit(c(0.7, 5.0, 0.5, 0), "lines"))
dev.off()
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

xx %<>%
  mutate(x = factor(income_percentile, ordered = TRUE), y = mean_income, fill = income_source)



stacked_with_right_labels <- function(.data, 
                                      geom = "bar", 
                                      right_margin = 0.5,
                                      scale_y_breaks,
                                      scale_y_limits, 
                                      scale_y_labeler,
                                      scale_x = list()){
  stopifnot(all(c("x", "y", "fill") %in% names(.data)))
  stopifnot(is.ordered(.data$fill))
  stopifnot(is.factor(.data$x), is.ordered(.data$x))
  
  plot_width <- max(as.numeric(.data$x)) - min(as.numeric(.data$x))
  
  .plot.data <- 
    .data %>%
    # our label should only appear at the last x
    mutate(text.label = ifelse(x == max(x), 
                               as.character(fill), 
                               NA_character_)) %>%
    # it should be as high as the corresponding bar:
    # all the way up the previous, then half of the corresponding height
    dplyr::arrange(fill) %>%
    dplyr::group_by(x) %>%
    dplyr::mutate(text.y = -y/2 + cumsum(y), 
                  text.x = max(as.numeric(.data$x)) + 0.5)
  
  
  
  label_max_width <- 
    # longest spell between '\n <---> \n'
    strsplit(as.character(unique(.data$fill)), split = "\n") %>%
    unlist %>%
    nchar %>%
    max
  
  if (geom == "bar"){
    p <- 
      grplot(.plot.data) + 
      ggplot2::geom_bar(ggplot2::aes(x = x, y = y, fill = fill), stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(label = text.label, 
                    x = text.x,
                    y = text.y, 
                    colour = fill), 
                na.rm = TRUE,
                hjust = 0,
                lineheight = 0.9,
                size = 20/(14/5),
                fontface = "bold") 
    
    if (!missing(scale_x)){
      p <- p + ggplot2::scale_x_discrete(limits = scale_x$limits, 
                                expand = scale_x$expand, 
                                breaks = scale_x$breaks, 
                                labels = scale_x$labels) 
    }
    
    if (!missing(scale_y_labeler) && !missing(scale_y_limits) && !missing(scale_y_breaks)){ 
      y_label_fun <- match.fun(scale_y_labeler)
      p <- p + ggplot2::scale_y_continuous(limits = scale_y_limits, 
                                  expand = c(0,0), 
                                  breaks = scale_y_breaks, 
                                  labels = y_label_fun) 
    }
    
    if (!missing(scale_y_labeler) && missing(scale_y_limits) && !missing(scale_y_breaks)){ 
      y_label_fun <- match.fun(scale_y_labeler)
      p <- p + ggplot2::scale_y_continuous(expand = c(0,0), 
                                  breaks = scale_y_breaks, 
                                  labels = y_label_fun) 
    }
    
    if (!missing(scale_y_labeler) && !missing(scale_y_limits) && missing(scale_y_breaks)){ 

      y_label_fun <- match.fun(scale_y_labeler)
      p <- p + ggplot2::scale_y_continuous(limits = scale_y_limits, 
                                  expand = c(0,0),  
                                  labels = y_label_fun) 
    }
    
    if (!missing(scale_y_labeler) && missing(scale_y_limits) && missing(scale_y_breaks)){ 
      y_label_fun <- match.fun(scale_y_labeler)
      p <- p + ggplot2::scale_y_continuous(expand = c(0,0),  
                                  labels = y_label_fun) 
    }
    
    if (missing(scale_y_labeler) && missing(scale_y_limits) && !missing(scale_y_breaks)){ 
      # scale_y_breaks,
      # scale_y_limits, 
      # scale_y_labeler,
      p <- p + ggplot2::scale_y_continuous(expand = c(0,0), 
                                  breaks = scale_y_breaks) 
    }
    
    if (missing(scale_y_labeler) && !missing(scale_y_limits) && !missing(scale_y_breaks)){ 
      # scale_y_breaks,
      # scale_y_limits, 
      # scale_y_labeler,
      p <- p + ggplot2::scale_y_continuous(limits = scale_y_limits, 
                                  expand = c(0,0), 
                                  breaks = scale_y_breaks) 
    }
    
    if (missing(right_margin))
      p <- p + ggplot2::theme(plot.margin = grid::unit(c(0.7, label_max_width, 0.5, 0), "lines"))
    else 
      p <- p + ggplot2::theme(plot.margin = grid::unit(c(0.7, right_margin, 0.5, 0), "lines"))
    
  } else {
    stop()
  }
  dev.off()
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
}


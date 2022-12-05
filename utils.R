
library(scales)
library(readr)

read_in_per_sample_intros <- function(per_sample_intro_handle, vocs_to_plot){
  per_sample_intros <- read_delim(per_sample_intro_handle, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
  per_sample_intros <- per_sample_intros %>%  mutate(voc_for_plots = ifelse(annotation_1 %in% vocs_to_plot, annotation_1, 'not_voc_of_interest'))
  per_sample_intros <- per_sample_intros %>% group_by(introduction_node) %>%  mutate(introduction_node_for_plotting = ifelse(n() >= 5, introduction_node, 'less_than_5')) %>% ungroup()
  
  #Split <- sapply(str_split(per_sample_intros$sample, "|"), tail, 1)
  # get the sampling date from the sample name
  per_sample_intros <- per_sample_intros %>% mutate(sampling_date = lubridate::ymd(str_split_i(sample, "\\|", -1)))
  per_sample_intros$week <- as.Date(cut(per_sample_intros$sampling_date, breaks = "week"))
  
  
  return(per_sample_intros)
}


read_in_national_case_numbers <- function(national_cases_handle, countries_to_include){
  national_data <- read_csv("~/Dropbox/COVID/our_world_in_data/2022.12.05/owid-covid-data.csv")
  national_data <- national_data %>% filter(iso_code %in% countries_to_include)
  #national_cases$date <- as.Date(national_cases$date, format = '%d/%m/%Y')
  
  national_data$Month <- as.Date(cut(national_data$date, breaks = "month"))
  national_data$Week <- as.Date(cut(national_data$date, breaks = "week"))
  
  return(national_data)
}


read_in_restrictions <- function(restrictions_handle, countries_to_include){
  restrictions <- read_csv(restrictions_handle, col_types = cols(Day = col_date(format = "%Y-%m-%d")))
  restrictions <- restrictions %>% filter(Code %in% countries_to_include)
}


get_first_and_last_week <- function(per_sample_intros, voc_to_analyse){
  per_sample_intros_this_voc <- per_sample_intros[per_sample_intros$voc_for_plots == voc_to_analyse,]
  first <- min(per_sample_intros_this_voc$week)
  last <- max(per_sample_intros_this_voc$week)
  first_and_last <- c(first, last)
  return(first_and_last)
}


filter_and_plot_national_cases <- function(all_national_cases, date_range, country_iso){
  filtered_national_cases <- all_national_cases %>% filter(iso_code == country_iso) %>% filter(between(Week, date_range[1], date_range[2]))
  ai_df_wk <- filtered_national_cases %>% group_by(Week) %>% summarise(n = sum(new_cases))
  
  nc_bw <- ggplot(ai_df_wk, aes(x = Week, y = n)) + 
    geom_bar(stat = "identity") + 
    ylab("National\ncases") + 
    theme(text=element_text(size=20)) +
    scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b %y")) +
    scale_y_continuous(labels = label_comma())
  #theme(axis.title.y = element_text(angle = 0, vjust = 0.5), text=element_text(size=20))
  
  return(nc_bw)
}


filter_and_plot_restrictions <- function(restriction_df, date_range, country_iso){
  filtered_restrictions <- restriction_df %>% filter(between(Day, date_range[1], date_range[2])) %>% filter(Code == country_iso)
  restrictions_plot <- ggplot(filtered_restrictions, aes(x = Day, y = international_travel_controls)) + 
    geom_line() +
    ylab('Restriction\nlevel') + 
    theme(text=element_text(size=20)) +
    #theme(axis.title.y = element_text(angle = 0, vjust = 0.5), text=element_text(size=20))
    scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %y"))
  return(restrictions_plot)
}


filter_and_plot_samples <- function(per_sample_intros, voc){
  filtered_samples <- per_sample_intros %>% filter(voc_for_plots == voc)
  bw <- ggplot(filtered_samples, aes(x = week, y = 1, fill = introduction_node_for_plotting)) + 
    geom_bar(stat = "identity") + 
    theme(text=element_text(size=20)) +
    #theme(axis.title.y = element_text(angle = 0, vjust = 0.5), text=element_text(size=20))
    ylab('Frequency') + 
    xlab('Week') + 
    scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %y"))
  return(bw)
  
  
}
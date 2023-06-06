library(scales)
library(readr)
library(stringr)

give_sensible_names_to_intros <- function(per_sample_intros){
  sizes <- per_sample_intros %>% distinct(introduction_node, .keep_all = TRUE) %>% arrange(desc(cluster_size), introduction_node) %>% mutate(new_intro_name = paste('pg', row_number(), sep = '_')) %>% select(introduction_node, new_intro_name)
  #View(sizes)
  per_sample_intros <- left_join(per_sample_intros, sizes, by = 'introduction_node')
  return(per_sample_intros)
}


get_vocs_to_plot <- function(per_sample_intros, threshold_for_plotting){
  # group by the anotation and the intro node (could just group by the intro node, but convenient to take the annotation along as well)
  # count each intro node 
  # take the ones with more samples than threshold.
  # get the distinct annotations
  #View(per_sample_intros)
  #View(threshold_for_plotting)
  #View(per_sample_intros %>% group_by(annotation_1, introduction_node) %>% summarise(n = n()) %>% filter(n >= threshold_for_plotting))
  vocs_to_plot <- per_sample_intros %>% group_by(annotation_1, introduction_node) %>% summarise(n = n()) %>% filter(n >= threshold_for_plotting) %>% distinct(annotation_1)
  vocs_to_plot <- as.list(vocs_to_plot$annotation_1)
  return(vocs_to_plot)
}


read_in_per_sample_intros <- function(per_sample_intro_handle, threshold_for_plotting){
  per_sample_intros <- read_delim(per_sample_intro_handle, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
  
  vocs_to_plot <- get_vocs_to_plot(per_sample_intros, threshold_for_plotting)
  #View(vocs_to_plot)
  per_sample_intros <- per_sample_intros %>%  mutate(voc_for_plots = ifelse(annotation_1 %in% vocs_to_plot, annotation_1, 'not_voc_of_interest'))
  per_sample_intros <- give_sensible_names_to_intros(per_sample_intros)
  
  per_sample_intros <- per_sample_intros %>% group_by(new_intro_name) %>%  mutate(introduction_node_for_plotting = ifelse(n() >= threshold_for_plotting, new_intro_name, paste('fewer_than_', threshold_for_plotting, sep = ''))) %>% ungroup()
  #Split <- sapply(str_split(per_sample_intros$sample, "|"), tail, 1)
  # get the sampling date from the sample name
  #per_sample_intros <- per_sample_intros %>% mutate(sampling_date = lubridate::ymd(str_split_i(sample, "\\|", -1)))
  per_sample_intros <- per_sample_intros %>% mutate(sampling_date = lubridate::parse_date_time(str_split_i(sample, "\\|", -1), c('ymd', 'ym', 'y')))
  per_sample_intros$week <- as.Date(cut(per_sample_intros$sampling_date, breaks = "week"))
  per_sample_intros$month <- as.Date(cut(per_sample_intros$sampling_date, breaks = "month"))
  
  #View(per_sample_intros)
  output <- list(per_sample_intros = per_sample_intros, vocs_to_plot = vocs_to_plot)
  return(output)
}


read_in_national_case_numbers <- function(national_cases_handle, countries_to_include){
  national_data <- read_csv("~/Dropbox/COVID/our_world_in_data/2022.12.05/owid-covid-data.csv")
  national_data <- national_data %>% filter(iso_code %in% countries_to_include) %>% filter(!is.na(new_cases))
  
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
  #View(per_sample_intros_this_voc)
  first <- min(per_sample_intros_this_voc$week)
  last <- max(per_sample_intros_this_voc$week)
  first_and_last <- c(first, last)
  return(first_and_last)
}


filter_and_plot_national_cases <- function(all_national_cases, date_range, country_iso){
  #print(country_iso)
  #View(all_national_cases)
  filtered_national_cases <- all_national_cases %>% filter(iso_code == country_iso) %>% filter(between(Week, date_range[1], date_range[2]))
  #View(filtered_national_cases)
  ai_df_wk <- filtered_national_cases %>% group_by(Week) %>% summarise(n = sum(new_cases))
  
  nc_bw <- ggplot(ai_df_wk, aes(x = Week, y = n)) + 
    geom_bar(stat = "identity") + 
    ylab("National\ncases") + 
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b %y"), limits = date_range) +
    scale_y_continuous(labels = label_comma())
  #theme(axis.title.y = element_text(angle = 0, vjust = 0.5), text=element_text(size=20))
  
  return(nc_bw)
}


filter_and_plot_restrictions <- function(restriction_df, date_range, country_iso){
  filtered_restrictions <- restriction_df %>% filter(between(Day, date_range[1], date_range[2])) %>% filter(Code == country_iso)
  restrictions_plot <- ggplot(filtered_restrictions, aes(x = Day, y = international_travel_controls)) + 
    geom_line() +
    ylab('Restriction\nlevel') + 
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    #theme(axis.title.y = element_text(angle = 0, vjust = 0.5), text=element_text(size=20))
    scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %y"), limits = date_range)
  return(restrictions_plot)
}


first_identification <- function(per_sample_intros, voc_to_analyse, date_range, filter_voc){
  if (filter_voc == FALSE){
    filtered_samples <- per_sample_intros %>% filter(annotation_1 == voc_to_analyse)
  } else {
    filtered_samples <- per_sample_intros
  }
  
  filtered_samples <- filtered_samples %>% arrange(sampling_date)
  first <- filtered_samples %>% distinct(introduction_node, .keep_all = TRUE)
  #print(date_range[1])
  #print(date_range[2])
  first$sampling_date <- as.Date(first$sampling_date)
  fi <- ggplot(first, aes(x = sampling_date)) +
    stat_ecdf() +
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    ylab('Cumulative Introduction\nFrequency') + 
    xlab('Date') +
    scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %y"), limits = date_range)
    
  
  return(fi)
}


filter_and_plot_samples <- function(per_sample_intros, voc, date_range){
  filtered_samples <- per_sample_intros %>% filter(voc_for_plots == voc)
  bw <- ggplot(filtered_samples, aes(x = week, y = 1, fill = introduction_node_for_plotting)) + 
    geom_bar(stat = "identity") + 
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    #theme(axis.title.y = element_text(angle = 0, vjust = 0.5), text=element_text(size=20))
    ylab('Frequency') + 
    xlab('Week') + 
    scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %y"), limits = date_range)
  return(bw)
}


filter_and_plot_all_samples <- function(per_sample_intros, date_range){
  #View(per_sample_intros)
  bw <- ggplot(per_sample_intros, aes(x = month, y = 1, fill = voc_for_plots)) + 
    geom_bar(stat = "identity") + 
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    #theme(axis.title.y = element_text(angle = 0, vjust = 0.5), text=element_text(size=20))
    ylab('Frequency') + 
    xlab('Month') + 
    scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %y"), limits = date_range)
  return(bw)
}


make_per_VOC_plots <- function(per_sample_intros, voc, national_cases, restrictions, country_iso){
  fnl <- get_first_and_last_week(per_sample_intros, voc)
  #View(fnl)
  nc <- filter_and_plot_national_cases(national_cases, fnl, country_iso)
  fi <- first_identification(per_sample_intros, voc, fnl, FALSE)
  restrictions <- filter_and_plot_restrictions(restrictions, fnl, country_iso)
  #beta_restrictions
  samples <- filter_and_plot_samples(per_sample_intros, voc, fnl)
  #beta_samples
  p <- nc / restrictions / fi / samples  + plot_annotation(title = paste(country_iso, voc, sep = '-'), theme = theme(plot.title = element_text(size = 24, face = 'bold'))) + plot_layout(heights = c(1, 1, 1, 2))
  print(p)
}


make_overall_plots <- function(per_sample_intros, national_cases, restrictions, country_iso){
  first <- min(per_sample_intros$week)
  last <- max(per_sample_intros$week)
  fnl <- c(first, last)
  nc <- filter_and_plot_national_cases(national_cases, fnl, country_iso)
  fi <- first_identification(per_sample_intros, NA, fnl, TRUE)
  restrictions <- filter_and_plot_restrictions(restrictions, fnl, country_iso)# + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  samples <- filter_and_plot_all_samples(per_sample_intros, fnl)
  p <- nc / restrictions / fi  / samples  + plot_annotation(title = paste(country_iso, sep = '-'), theme = theme(plot.title = element_text(size = 24, face = 'bold'))) + plot_layout(heights = c(1, 1, 1, 2))
  print(p)
}

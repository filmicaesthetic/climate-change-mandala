# Cowtan, Kevin & National Center for Atmospheric Research Staff (Eds). Last modified 09 Sep 2019. "The Climate Data Guide: Global surface temperatures: BEST: Berkeley Earth Surface Temperatures." Retrieved from https://climatedataguide.ucar.edu/climate-data/global-surface-temperatures-best-berkeley-earth-surface-temperatures.

library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(geomtextpath)
library(ggforce)
library(showtext)

# import Google font
font_add_google("Nunito", "Nunito")
gfont <- "Nunito"
showtext_auto()

# import dataset from Berkeley
global_daily <- read.table("http://berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_daily.txt", skip = 23)

# set column names
colnames(global_daily) <- c("date_number", "year", "month", "day", "day_of_year", "anomaly")

# create date column
global_daily_labelled <- global_daily %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")),
         week = week(date))

# use 1881 as non-leap year to lookup week / month day_of_year values
day_of_year_lkp <- global_daily %>%
  filter(year == 1881) %>%
  select(month, day, day_of_year)

# remove february 29th from leap years to avoid distorting the chart
global_daily_no_feb29 <- global_daily_labelled %>%
  select(-date_number, -day_of_year) %>%
  filter(!(month == 2 & day == 29)) %>%
  left_join(day_of_year_lkp, by = c("month", "day"))

# add id column and arrange
global_daily_plot <- global_daily_no_feb29 %>%
  arrange(year, day_of_year) %>%
  mutate(id = 1:n())

# create day, week, month percentage values for plotting
global_daily_plot_percs <- global_daily_plot %>%
  mutate(day_perc = day_of_year / max(global_daily_plot$day_of_year),
         week_perc = week / max(global_daily_plot$week),
         month_perc = month / max(global_daily_plot$month)
  )

# create copy for plots
test_df <- global_daily_plot_percs

# calculate week and month mean anomaly from daily figures
daily <- test_df %>%
  group_by(year, week) %>%
  mutate(week_anomaly = mean(anomaly),
         week_doy = mean(day_of_year)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  mutate(month_anomaly = mean(anomaly),
         month_doy = mean(day_of_year)) %>%
  ungroup() %>%
  mutate(id = 1:n())

# select unique years from dataset
years_sel <- unique(daily$year)

# manually set adjusted min and max for plotting
max_temp <- 4
min_temp <- 16
# line colour hexcode
line_col <- "#1d1d1d"

# this iterates over all years and saves high-res copies of the plot for every year
# adjust years_sel if you don't want all 140+ years!

for (i in 1:length(years_sel)) {
  
  # creating dataframe for circle pattern
  demo_df <- data.frame(week = rep(1:53, 22),
                        week_doy = rep(seq(1, 365, by = 7), 22),
                        week_anomaly = rep(seq(-6, 4.5, by = 0.5), each = 53)) %>%
    mutate(week_doy = ifelse((10 * week_anomaly) %% 2 == 0, week_doy, week_doy + 3.5))
  
  # create lookup to add values to demo_df with matching data points
  demo_week_check <- daily %>%
    filter(year %in% years_sel[i]) %>%
    group_by(id, week, week_anomaly) %>%
    summarise(year = mean(year)) %>%
    group_by(week, week_anomaly, year) %>%
    mutate(week_anomaly = ceiling(week_anomaly * 2) / 2,
           check = week_anomaly)
  
  # combine and arrange dataset for plotting
  demo_join <- demo_df %>%
    left_join(demo_week_check, by = c("week", "week_anomaly")) %>%
    group_by(year, week, week_doy, week_anomaly) %>%
    summarise(check = mean(check)) %>%
    arrange(-abs(week_anomaly), week) %>%
    filter(week <= 52)
  
  # filter by year iteration
  daily_plot <- daily %>% 
    filter(year == years_sel[i])
  
  # create months dataset for plot
  months <- daily_plot %>% group_by(month_doy) %>% 
    summarise(month = mean(month)) %>% 
    mutate(month_name = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                          "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
  # manually adjust december
  months$month_doy[12] <- 349.0625
  
  # add variables for use in plot
  mean_temp <- mean(daily_plot$anomaly)
  plot_year <- mean(daily_plot$year)
  
  # create plot
  daily_plot %>%
    mutate(id = as.numeric(id)) %>%
    ggplot(aes()) +
    geom_point(x = 1, y = 10, aes(color = min_temp, fill = min_temp)) +
    geom_point(x = 1, y = 12, aes(color = max_temp, fill = max_temp)) +
    geom_point(data = demo_join, aes(x = week_doy, y = 21.5), size = 11, shape = 21, fill = "#ffffff", color = line_col) +
    annotate("rect", xmin = 1, xmax = 366, ymin = 10, ymax = 21.5, fill = "#ffffff", color = "#ffffff") +
    annotate("rect", xmin = 0, xmax = 366, ymin = 15, ymax = 16.4, fill = "#deddd9", color = "#deddd9") +
    annotate("segment", x = 1, xend = 366, y = 16, yend = 16, color = "#deddd9") +
    annotate("segment", x = 1, xend = 366, y = 17.5, yend = 17.5, color = "#deddd9") +
    annotate("segment", x = 1, xend = 366, y = 21.5, yend = 21.5, color = line_col, size = 0.3) +
    geom_rect(aes(xmin = day_of_year, xmax = day_of_year + 1, ymin = 18.5, ymax = 21, fill = (ceiling(anomaly * 2) / 2) + 10, angle = 360 - ((day_of_year / 365) * 360)), color = "#ffffff") +
    geom_textpath(data = months, aes(x = month_doy, y = 16.75, label = toupper(month_name)), size = 10, color = line_col, family = gfont, fontface = "bold") +
    geom_point(data = demo_join, aes(x = week_doy, y = week_anomaly + 10, fill = check + 10, size = (week_anomaly + 8) / pi ), color = line_col, shape = 21) +
    annotate("text", x = 1, y = 0.1, label = plot_year, family = gfont, size = 20) +
    scale_size(range = c(5,10)) +
    scale_fill_gradientn(colors = c("#01143d", "#0d44ba", "#deddd9", "#b82207", "#420a00"), na.value = "#ffffff", breaks = c(4, 9.6, 10, 10.4, 16, 20)) +
    scale_color_gradientn(colors = c("#01143d", "#0d44ba", "#deddd9", "#b82207", "#420a00"), na.value = "#ffffff", breaks = c(4, 9.6, 10, 10.4, 16, 20)) +
    ylim(c(0, 22)) +
    xlim(c(1, 366)) +
    coord_polar() + 
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          axis.text = element_blank(),
          axis.title = element_blank(),
          text = element_text(family = gfont))
  
  # save each iteration
  ggsave(paste0("outputs/plot_",years_sel[i],".png"), width = 10, height = 10)
  
}

library(dplyr)
library(ggplot2)
library(tm)
library(syuzhet)
# install.packages(plyr) (need to also install plyr if not yet installed)

# need to add location of data files to list.files()
list_of_files <- list.files("")

data_list <- list()
for (i in 1:length(list_of_files)){
    data_list[[i]] <- read.csv(paste0("data/", list_of_files[i]), stringsAsFactors = F)
}

data_df <- plyr::ldply(data_list)
data_df$time <- lubridate::dmy_hms(data_df$time, tz = "UTC")
data_df$time_ntz <- format(data_df$time, tz="America/Detroit",usetz=TRUE)
data_df$day <- lubridate::yday(data_df$time_ntz)

data_df$hour <- lubridate::hour(data_df$time_ntz)

data_df$new_time <- paste0(data_df$day, ".", 
                           ifelse(nchar(data_df$hour) == 1, paste0(0, data_df$hour), data_df$hour))


data_df_ss <- filter(data_df, new_time > 312)

to_plot <- data_df_ss %>% count(new_time)

x <- 
    c("Monday, 12:00 am", "", "", "Monday, 3:00 am", "", "", "Monday, 6:00 am", "", "", "Monday, 9:00 am", "", "", "Monday, 12:00 pm", "", "", "Monday, 3:00 pm", "", "", "Monday, 6:00 pm", "", "", "Monday, 9:00 pm", "", "", 
      "Tuesday, 12:00 am", "", "", "Tuesday, 3:00 am", "", "", "Tuesday, 6:00 am", "", "", "Tuesday, 9:00 am", "", "", "Tuesday, 12:00 pm", "", "", "Tuesday, 3:00 pm", "", "", "Tuesday, 6:00 pm", "", "", "Tuesday, 9:00 pm", "", "", 
      "Wednesday, 12:00 am", "", "", "Wednesday, 3:00 am", "", "", "Wednesday, 6:00 am", "", "", "Wednesday, 9:00 am", "", "", "Wednesday, 12:00 pm", "", "", "Wednesday, 3:00 pm", "", "", "Wednesday, 6:00 pm", "", "")

ggplot(to_plot, aes(x = new_time, y = n)) +
    geom_point() +
    scale_x_discrete(labels = x) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::comma) +
    ylab("Number of Tweets Per Hour")
        
mySentiment <- get_nrc_sentiment(data_df_ss$text)
View(mySentiment)
mySentiment$time <- data_df_ss$new_time

prop(c(1:5))
for_p <- mySentiment %>% group_by(time) %>% 
    summarize_each(funs(sum, n())) %>% 
    select(time:positive_sum, n = anger_n)

for_p[, 2:11] <- for_p[, 2:11] / for_p$n
for_p <- for_p %>% tidyr::gather(key, val, -time, -n)

x <- 
    c("Tuesday, 12:00 am", "", "", "Tuesday, 3:00 am", "", "", "Tuesday, 6:00 am", "", "", "Tuesday, 9:00 am", "", "", "Tuesday, 12:00 pm", "", "", "Tuesday, 3:00 pm", "", "", "Tuesday, 6:00 pm", "", "", "Tuesday, 9:00 pm", "", "", 
      "Wednesday, 12:00 am", "", "", "Wednesday, 3:00 am", "", "", "Wednesday, 6:00 am", "", "", "Wednesday, 9:00 am", "", "", "Wednesday, 12:00 pm", "", "", "Wednesday, 3:00 pm", "", "", "Wednesday, 6:00 pm", "", "")

for_p <- filter(for_p, time >= 313)

ggplot(for_p, aes(x = time, y = val, group = key, color = key)) +
    geom_line() +
    geom_point() +
    scale_x_discrete(labels = x) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_discrete("", labels = c("Anger", "Anticipation", "Disgust", "Fear", "Joy",
                                    "Negative", "Positive", "Sadness", "Surprise", "Trust")) +
    ylab("Proportion of Tweets Per Hour") +
    xlab(NULL) +
    theme(plot.margin=unit(c(5.5, 5.5, 5.5, 30), "points")) +
    theme(text = element_text(size = 16))

ggsave("forecastemo.png")

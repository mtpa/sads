# Moving Fraction Plot: A Basketball Example (R)

library(grid)  # graphics utilities needed for split-plotting
library(ggplot2)  # graphics package with ribbon plot

# read game summary data for the Oklahoma City Thunder
OKC_data <- read.csv("okc_data_2014_2015.csv",  stringsAsFactors = FALSE)

# check the structure of the data
print(str(OKC_data))

# create winning fraction plot to explore streakiness during the season
# a window of twelve games is used following Albert and Bennett (2001)

# add game number to data frame
OKC_data$game_number <- seq(1:nrow(OKC_data))
  
# set binary indicator if this team has won
OKC_data$win_bin <- rep(0, length = nrow(OKC_data))
for (i in seq(along = OKC_data$win_bin)) 
    if (OKC_data$win_team_code[i] == "OKC") 
        OKC_data$win_bin[i] <- 1

# window across the season twelve games at a time
OKC_data$win_window <- rep(NA, length = nrow(OKC_data))
for (i in seq(along = OKC_data$win_window)) {
     if (i > 6)
          OKC_data$win_window[i] <- 
             mean(OKC_data$win_bin[(i - 6):(i + 5)]) * 100
     } 

moving_fraction_plotting_frame <- 
    OKC_data[, c("game_number", "win_window")]
moving_fraction_plotting_frame$ymax_topwhite <- 
    rep(NA, length = nrow(moving_fraction_plotting_frame))
moving_fraction_plotting_frame$ymax_bottomwhite <- 
    rep(NA, length = nrow(moving_fraction_plotting_frame))
moving_fraction_plotting_frame$ymin_topwhite <- 
    rep(NA, length = nrow(moving_fraction_plotting_frame))
moving_fraction_plotting_frame$ymin_bottomwhite <- 
    rep(NA, length = nrow(moving_fraction_plotting_frame))
    
for (i in seq(along = moving_fraction_plotting_frame$win_window)) {
    if (is.na(moving_fraction_plotting_frame$win_window[i])) {
        moving_fraction_plotting_frame$ymax_topwhite[i] <- 100
        moving_fraction_plotting_frame$ymin_topwhite[i] <- 50.1
        moving_fraction_plotting_frame$ymax_bottomwhite[i] <- 49.99 
        moving_fraction_plotting_frame$ymin_bottomwhite[i] <- 0    
        }        
        
    if (!is.na(moving_fraction_plotting_frame$win_window[i])) {
        if (moving_fraction_plotting_frame$win_window[i] > 50) {
            moving_fraction_plotting_frame$ymax_topwhite[i] <- 100
            moving_fraction_plotting_frame$ymin_topwhite[i] <- 
                moving_fraction_plotting_frame$win_window[i]
            moving_fraction_plotting_frame$ymax_bottomwhite[i] <- 49.99 
            moving_fraction_plotting_frame$ymin_bottomwhite[i] <- 0        
            }
        if (moving_fraction_plotting_frame$win_window[i] <= 50) {
            moving_fraction_plotting_frame$ymax_topwhite[i] <- 100
            moving_fraction_plotting_frame$ymin_topwhite[i] <- 50.1
            moving_fraction_plotting_frame$ymax_bottomwhite[i] <- 
               moving_fraction_plotting_frame$win_window[i]
            moving_fraction_plotting_frame$ymin_bottomwhite[i] <- 0    
            }   
        }
    }    
        
greenmin <- 50.01
greenmax <- 100
redmin <- 0
redmax <- 49.99

pdf(file = "fig_moving_fraction_plot.pdf", width = 8.8, height = 8.5)
ggobject <- ggplot() + 
geom_ribbon(data=moving_fraction_plotting_frame, 
mapping=aes(x=game_number, ymin=greenmin, ymax=greenmax), 
stat="identity",colour="white",fill="darkgreen") + 
geom_ribbon(data=moving_fraction_plotting_frame, 
mapping=aes(x=game_number, ymin=redmin, ymax=redmax), 
stat="identity",colour="white",fill="darkred") + 
geom_ribbon(data=moving_fraction_plotting_frame, 
mapping=aes(x=game_number, ymin=ymin_topwhite, ymax=ymax_topwhite), 
stat="identity",colour="white",fill="white") + 
geom_ribbon(data=moving_fraction_plotting_frame, 
mapping=aes(x=game_number, ymin=ymin_bottomwhite, ymax=ymax_bottomwhite), 
stat="identity",colour="white",fill="white") +
annotate("segment", x = 0, xend = 82, y = 49.99, yend = 50.01) +
ylab("Moving Fraction (Winning Percentage)") + 
xlab("Sequence of Games") 

print(ggobject)

dev.off()

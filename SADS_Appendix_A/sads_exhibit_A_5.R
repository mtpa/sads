# Visualizing Basketball Games (R)

library(lubridate)  # data/time functions and lakers data frame
# lubridate imports plyr package 
library(ggplot2)  # statistical graphics, grid graphics assumed

# functions used with grid graphics to split the plotting region
# to set margins and to plot more than one ggplot object on one page/screen
vplayout <- function(x, y) 
viewport(layout.pos.row=x, layout.pos.col=y) 

# user-defined function to plot a ggplot object with margins
ggplot.print.with.margins <- function(ggplot.object.name,
    left.margin.pct=10,
     right.margin.pct=10,top.margin.pct=10,bottom.margin.pct=10)
    { # begin function for printing ggplot objects with margins
      # margins expressed as percentages of total... use integers
    grid.newpage() 
    pushViewport(viewport(layout=grid.layout(100,100)))
    print(ggplot.object.name, 
    vp=vplayout((0 + top.margin.pct):(100 - bottom.margin.pct),
        (0 + left.margin.pct):(100 - right.margin.pct))) 
    } # end function for printing ggplot objects with margins

# lakers data frame from lubridate includes
# play-by-play data for the Los Angeles Lakers 2008-2009 season
# original data from Parker (2010) http://www.basketballgeek.com/data/
# date: year/month/day text to be converted to date object
# opponent: three-character abbreviation for other team
# game_type: home or away for Los Angeles Lakers
# time: clock time remaining minutes:seconds converted to duration
# period: period of play 1, 2, 3, or 4
# etype: one of ten event types: foul, free throw, jump ball,
#        rebound, shot, sub, timeout, turnover, violation
# team: three-character abbreviation for team 
#       or OFF for neither team as with a jump ball
# player: one of 371 players involved in Los Angeles Lakers games
# result: missed or made
# points: points scored
# type: one of 74 event descriptions: hook, off, layup,                    
#       shooting, personal, jump, pullup jump,              
#       def, driving layup, driving finger roll layup, regular,                  
#       offensive, 3pt, turnaround jump, putback layup,            
#       slam dunk, tip, dunk, defensive goaltending,    
#       hook bank, running layup, official, driving slam dunk,        
#       short, driving reverse layup, kicked ball, putback dunk,             
#       technical, alley oop dunk, turnaround fade away, running jump,             
#       delay of game, defense 3 second, fade away bank, floating jump,            
#       driving dunk, loose ball, running bank, running dunk,             
#       fade away jumper, finger roll layup, turnaround hook, reverse layup,            
#       reverse dunk, jump hook, jump bank, double technical,         
#       running hook, driving jump, turnaround bank, step back jump,           
#       turnaround bank hook, pullup bank, alley oop layup, putback slam dunk,        
#       flagrant type 1, running reverse layup, running finger roll layup, 
#       reverse slam dunk, hanging technical, running slam dunk, 
#       driving bank hook, jump ball, away from play, double personal, 
#       driving bank, running bank hook, driving hook, lane, clear path, 
#       jump bank hook, flagrant type 2, inbound                  
# x, y: shot location, standing behind the offensive team’s hoop then 
#       the X axis runs from left to right and the Y axis runs from 
#       bottom to top. The center of the hoop is located at (25, 5.25).

# 2008-2009 team abbreviations/code names
team_code_2008 = c("ATL", "BOS", "CHA", "CHI", "CLE", 
                   "DAL", "DEN", "DET", "GSW", "HOU", 
                   "IND", "LAC", "LAL", "MEM", "MIA", 
                   "MIL", "MIN", "NJN", "NOH", "NYK", 
                   "OKC", "ORL", "PHI", "PHX", "POR", 
                   "SAC", "SAS", "TOR", "UTA", "WAS")
              
# Note. Charlotte Bobcats later became the Charlotte Hornets
# New Orleans Hornets later became the New Orleans Pelicans
team_name_2008 = c("Atlanta Hawks", "Boston Celtics", 
                   "Charlotte Hornets", "Chicago Bulls", 
                   "Cleveland Cavaliers", "Dallas Mavericks", 
                   "Denver Nuggets", "Detroit Pistons", 
                   "Golden State Warriors", "Houston Rockets", 
                   "Indiana Pacers", "Los Angeles Clippers", 
                   "Los Angeles Lakers", "Memphis Grizzlies", 
                   "Miami Heat", "Milwaukee Bucks", 
                   "Minnesota Timberwolves", "New Jersey Nets", 
                   "New Orleans Hornets", "New York Knicks", 
                   "Oklahoma City Thunder", "Orlando Magic", 
                   "Philadelphia 76ers", "Phoenix Suns", 
                   "Portland Trail Blazers", "Sacramento Kings", 
                   "San Antonio Spurs", "Toronto Raptors", 
                   "Utah Jazz", "Washington Wizards")
# Note. Charlotte Bobcats became the Charlotte Hornets
# New Orleans Hornets became the New Orleans Pelicans
# New Jersey Nets became the Brooklyn Nets

# lakers$date <- ymd(lakers$date)  # code as date variable
lakers$time <- ms(lakers$time)  # code as period object
lakers$time <- as.duration(lakers$time)  # code as durations

# convert time to gametime with periods of 12 minutes, overtime 5 minutes
# here we convert to minutes and fractions of minutes
lakers$gametime <- dminutes(c(12, 24, 36, 48, 53)[lakers$period]) - 
    as.duration(lakers$time)  
lakers$minutes <- as.numeric(seconds(lakers$gametime))/60    
print(str(lakers))  # examine stucture of the data frame

# get rid of observations with team OFF (jump ball)
lakers_games <- lakers[(lakers$team != "OFF"),]

# route plots to external plotting device... pdf file
pdf(file = "plot_lakers_basketball_2008_2009.pdf",
    width = 8.5, height = 8.5)
# ---------------------------------------------
# cycle through all Lakers games for the season
gamedate <- unique(lakers_games$date)
for (igame in seq(along = gamedate)) { # begin for-loop for all games
    this_game <- lakers_games[lakers_games$date == gamedate[igame],]

# work with the current game, compute score as cumulative sum for each team
# using ddply function from the plyr package, required for lubridate package
    this_game_scores <- ddply(this_game, "team", transform,
        score = cumsum(points))

# identify team names and scores for this game
    first_team_name <- team_name_2008[which(team_code_2008 == "LAL")]
    second_team_name <- team_name_2008[which(team_code_2008 == 
        this_game_scores$opponent[1])]
    first_team_score <- 
        max(this_game_scores[(this_game_scores$team == "LAL"), "score"])    
    second_team_score <- 
        max(this_game_scores[(this_game_scores$team == 
            this_game_scores$opponent[1]), "score"])

# summary for this game to be used in plot title
    this_game_summary_text <- 
        paste(gsub(" UTC","", ymd(this_game_scores$date)[1]),
        "  ", first_team_name, " ", this_game_scores$game_type[1],
        " (", first_team_score, ") versus ",
        second_team_name, " ", "(", second_team_score, ")", sep = "")

# create visualization for this game with annotation shading
# for the second and fourth 12-minute periods in the game
# and with customized legend for the step function score lines
    ggplot_object <- 
        ggplot(data = subset(this_game_scores, team == "LAL"),
        aes(x = minutes, y = score)) +
        layer(geom = "step", size = 0.75, colour = "purple") + 
        layer(data = subset(this_game_scores, 
            team == this_game_scores$opponent[1]),
            geom = "step", size = 1.5, colour = "blue") +
        annotate("rect", xmin = 12.01, xmax = 24.00, 
            ymin = min(this_game_scores$score), 
            ymax = max(this_game_scores$score), 
            alpha = 0.1, fill = "black") +    
        annotate("rect", xmin = 36.01, xmax = 48.00, 
            ymin = min(this_game_scores$score), 
            ymax = max(this_game_scores$score), 
            alpha = 0.1, fill = "black") +    
        annotate("rect", xmin = 0, xmax = 24.00, 
            ymin = max(this_game_scores$score) - 15, 
            ymax = max(this_game_scores$score), 
            alpha = 1, colour = "darkgrey", fill = "white") + 
        geom_segment(x = 1.0, xend = 3.0, 
            y = max(this_game_scores$score) - 5, 
            yend = max(this_game_scores$score) - 5, 
            colour = "purple", size = 0.75) +  
        geom_segment(x = 1.0, xend = 3.0, 
            y = max(this_game_scores$score) - 10, 
            yend = max(this_game_scores$score) - 10, 
            colour = "blue", size = 1.5) +  
        annotate("text", x = 4, y = max(this_game_scores$score) - 5, 
            colour = "black", alpha = 0.9, size = 4.5, 
            label = first_team_name, hjust = 0) +
        annotate("text", x = 4, y = max(this_game_scores$score) - 10, 
            colour = "black", alpha = 0.9, size = 4.5, 
            label = second_team_name, hjust = 0) +    
        xlab("Game Minutes") + ylab("Score") +
        ggtitle(this_game_summary_text) +
        theme(axis.title.x = element_text(size = rel(1.25))) +
        theme(axis.title.y = element_text(size = rel(1.25)))
        
    ggplot.print.with.margins(ggplot_object,
        left.margin.pct = 5, right.margin.pct = 5, 
        top.margin.pct = 5, bottom.margin.pct = 5) 
        
    } # end for-loop for all games    
dev.off()  # closer the pdf plotting device    





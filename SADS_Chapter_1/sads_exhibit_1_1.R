# MLB, NBA, and NFL Player Salaries (R)

library(lattice)  # statistical graphics

# variables in contract data from spotrac.com (August 2015)
#   player: player name (contract years)
#   position: position on team	
#   team: team abbreviation	
#   teamsignedwith: team that signed the original contract 	
#   age: age in years as of August 2015	
#   years: 	years as player in league
#   contract: dollars in contract	
#   guaranteed: guaranteed dollars in contract 	
#   guaranteedpct: percentage of contract dollars guaranteed
#   salary: annual salary in dollares 	
#   yearfreeagent: year player becomes free agent
# 
#   additional created variables 
#   salarymm: salary in millions
#   leaguename: full league name
#   league: league abbreviation

# read data for Major League Baseball
mlb_contract_data <- read.csv("mlb_player_salaries_2015.csv")
mlb_contract_data$leaguename <- rep("Major League Baseball", 
    length = nrow(mlb_contract_data))      
mlb_contract_data$salarymm <- mlb_contract_data$salary/1000000    
mlb_contract_data$league <- rep("MLB", length = nrow(mlb_contract_data))
print(summary(mlb_contract_data))
# variables for plotting
mlb_data_plot <- mlb_contract_data[, c("salarymm","leaguename")]

nba_contract_data <- read.csv("nba_player_salaries_2015.csv")
nba_contract_data$leaguename <- rep("National Basketball Association", 
    length = nrow(nba_contract_data))       
nba_contract_data$salarymm <- nba_contract_data$salary/1000000    
nba_contract_data$league <- rep("NBA", length = nrow(nba_contract_data))  
print(summary(nba_contract_data))

# variables for plotting
nba_data_plot <- nba_contract_data[, c("salarymm","leaguename")]

nfl_contract_data <- read.csv("nfl_player_salaries_2015.csv")
nfl_contract_data$leaguename <- rep("National Football League", 
    length = nrow(nfl_contract_data))     
nfl_contract_data$salarymm <- nfl_contract_data$salary/1000000    
nfl_contract_data$league <- rep("NFL", length = nrow(nfl_contract_data))
print(summary(nfl_contract_data))
# variables for plotting
nfl_data_plot <- nfl_contract_data[, c("salarymm","leaguename")]

# merge contract data with variables for plotting
plotting_data_frame <- rbind(mlb_data_plot, nba_data_plot, nfl_data_plot) 

# generate the histogram lattice for comparing player salaries
# across the three leagues in this study
lattice_object <- histogram(~salarymm | leaguename, plotting_data_frame,
    type = "density", xlab = "Annual Salary ($ millions)", layout = c(1,3))

# print to file 
pdf(file = "fig_understanding_markets_player_salaries.pdf", 
     width = 8.5, height = 11)
print(lattice_object)
dev.off()

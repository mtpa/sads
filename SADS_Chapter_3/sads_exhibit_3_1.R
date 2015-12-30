# Assessing Team Strength by Unidimensional Scaling (R)

# NBA Regular Season 2014-2015 data from Basketball-Reference.com

# read comma-delimited text file to create data frame
nba_input <- read.csv("basketball_2014_2015_season.csv",
    stringsAsFactors = FALSE)
    
# add record number to records
record_number <- seq(nrow(nba_input))
nba_scores <- cbind(data.frame(record_number), nba_input)
  
# explore the data
with(nba_scores, plot(home_points, visitor_points))
with(nba_scores, table(overtime))

# define minutes played using overtime information
nba_scores$minutes <- rep(48, length = nrow(nba_scores))  # standard game
for (i in seq(along = nba_scores$minutes)) {
    if(nba_scores$overtime[i] == "OT") 
        nba_scores$minutes[i] <- nba_scores$minutes[i] + 5  # 1 OT period
    if(nba_scores$overtime[i] == "2OT") 
        nba_scores$minutes[i] <- nba_scores$minutes[i] + 10  # 2 OT periods 
    if(nba_scores$overtime[i] == "3OT") 
        nba_scores$minutes[i] <- nba_scores$minutes[i] + 15  # 3 OT periods
    }        
with(nba_scores, table(overtime, minutes))  # check minute calculations  

# compute points per minute for each team
nba_scores$visitor_ppm <- nba_scores$visitor_points / nba_scores$minutes
nba_scores$home_ppm <- nba_scores$home_points / nba_scores$minutes

# check calculations
print(head(nba_scores))
print(tail(nba_scores))

# explore points-per-minute data
with(nba_scores, plot(home_ppm, visitor_ppm))

# read in team names and abbreviations
team_info <- read.csv("nba_team_names_abbreviations.csv",
    stringsAsFactors = FALSE)

# append team information to nba_scores data frame
list_of_team_names <- team_info$team_name
# first do the visitor team
nba_scores$visitor_conference <- rep("", length = nrow(nba_scores))
nba_scores$visitor_division <- rep("", length = nrow(nba_scores))
nba_scores$visitor_code <- rep("", length = nrow(nba_scores))

for (i in seq(along = list_of_team_names)) {
   this_team_info <- 
       team_info[(team_info$team_name == list_of_team_names[i]),]
   indices_for_visitor_team <- 
       which (nba_scores$visitor_team == list_of_team_names[i])
   for (j in seq(along = indices_for_visitor_team)) {
       nba_scores$visitor_conference[indices_for_visitor_team[j]] <-
           this_team_info$conference[1]
       nba_scores$visitor_division[indices_for_visitor_team[j]] <-
           this_team_info$division[1]
       nba_scores$visitor_code[indices_for_visitor_team[j]] <-
               this_team_info$abbreviation[1]
       }
   }    
# next do the home team
nba_scores$home_conference <- rep("", length = nrow(nba_scores))
nba_scores$home_division <- rep("", length = nrow(nba_scores))
nba_scores$home_code <- rep("", length = nrow(nba_scores))
for (i in seq(along = list_of_team_names)) {
   this_team_info <- 
       team_info[(team_info$team_name == list_of_team_names[i]),]
   indices_for_home_team <- 
       which (nba_scores$home_team == list_of_team_names[i])
   for (j in seq(along = indices_for_home_team)) {
       nba_scores$home_conference[indices_for_home_team[j]] <-
           this_team_info$conference[1]
       nba_scores$home_division[indices_for_home_team[j]] <-
           this_team_info$division[1]
       nba_scores$home_code[indices_for_home_team[j]] <-
               this_team_info$abbreviation[1]
       }
   }    

# define winning team (Visitor or Home)
nba_scores$win_visitor_home <- rep("Home", length = nrow(nba_scores))
for (i in seq(along = nba_scores$record_number)) 
    if (nba_scores$visitor_ppm[i] > nba_scores$home_ppm[i]) 
        nba_scores$win_visitor_home[i] <- "Visitor" 
        
# who wins more games.. visitor or home team
with(nba_scores, table(win_visitor_home))
with(nba_scores, plot(home_ppm, visitor_ppm))

# define winning and losing teams by three-character abbreviation/code
nba_scores$win_team_code <- rep("", length = nrow(nba_scores))
nba_scores$lose_team_code <- rep("", length = nrow(nba_scores))
for (i in seq(along = nba_scores$record_number)) {
    if (nba_scores$visitor_ppm[i] > nba_scores$home_ppm[i]) { 
        nba_scores$win_team_code[i] <- nba_scores$visitor_code[i]
        nba_scores$lose_team_code[i] <- nba_scores$home_code[i]
        }
                
    if (nba_scores$visitor_ppm[i] < nba_scores$home_ppm[i]) {
        nba_scores$win_team_code[i] <- nba_scores$home_code[i] 
        nba_scores$lose_team_code[i] <- nba_scores$visitor_code[i]
        } 
    }

# check the nba_scores data frame
print(head(nba_scores))
print(tail(nba_scores))

# create OKC data frame for testing and visualization example
OKC_visitor <- nba_scores[(nba_scores$visitor_code == "OKC"),] 
OKC_home <- nba_scores[(nba_scores$home_code == "OKC"),] 
OKC_data <- rbind(OKC_visitor, OKC_home)
OKC_data <- OKC_data[sort.list(OKC_data$record_number),]
# save file for Oklahoma City Thunder for data visualization work
write.csv(OKC_data, file = "okc_data_2014_2015.csv", row.names = FALSE)

# write win team score and lose team score in points per minute
nba_scores$win_team_ppm <- rep(0, length = nrow(nba_scores))
nba_scores$lose_team_ppm <- rep(0, length = nrow(nba_scores))
for (i in seq(along = nba_scores$record_number)) {
    if (nba_scores$visitor_ppm[i] > nba_scores$home_ppm[i]) { 
        nba_scores$win_team_ppm[i] <- nba_scores$visitor_ppm[i]
        nba_scores$lose_team_ppm[i] <- nba_scores$home_ppm[i]
        }
    if (nba_scores$visitor_ppm[i] < nba_scores$home_ppm[i]) {
        nba_scores$win_team_ppm[i] <- nba_scores$home_ppm[i] 
        nba_scores$lose_team_ppm[i] <- nba_scores$visitor_ppm[i]
        }     
    }

# compute margin of victory in points per minute
nba_scores$margin_ppm <- nba_scores$win_team_ppm - nba_scores$lose_team_ppm

# initialize matrices for entire set of 30 teams
ordered_team_codes <- sort(team_info$abbreviation)
# total number of wins
wins_mat <- matrix(0, nrow = 30, ncol = 30, 
    dimnames = list(ordered_team_codes, ordered_team_codes))
prop_mat <- wins_mat  # proportion wins
ppm_mat <- wins_mat  # sum of ppm scores 

# build matrices for entire set of 30 teams
for (i in seq(along = nba_scores$record_number)) {
   # tally the number of times team k beats team j
   wins_mat[nba_scores$lose_team_code[i], nba_scores$win_team_code[i]] <- 
     wins_mat[nba_scores$lose_team_code[i], nba_scores$win_team_code[i]] + 1
   # sum the ppm scores for teams and enter in ppm_mat 
   # two enteries needed for each record in nba_scores
      
   ppm_mat[nba_scores$lose_team_code[i], nba_scores$win_team_code[i]] <- 
     ppm_mat[nba_scores$lose_team_code[i], nba_scores$win_team_code[i]] +
         nba_scores$win_team_ppm[i]
   ppm_mat[nba_scores$win_team_code[i], nba_scores$lose_team_code[i]] <- 
     ppm_mat[nba_scores$win_team_code[i], nba_scores$lose_team_code[i]] +
         nba_scores$lose_team_ppm[i] 
   }    

# check wins_mat entries... should be 82 games for every team
for (j in 1:length(ordered_team_codes))
    print(sum(wins_mat[,j]) + sum(wins_mat[j,]))

# compute matrix of proportions prop_mat
# proportion of times team j beats team k
for(j in 1:length(ordered_team_codes)) { # begin outer for-loop
    for(k in 1:length(ordered_team_codes)) { # begin inner for-loop
        if (j == k) prop_mat[j,k] <- NA   # set diagonal entries missing
        if (j > k) {  # begin outer if-block
            between_team_games <- 
                wins_mat[ordered_team_codes[j],ordered_team_codes[k]] +
                wins_mat[ordered_team_codes[k],ordered_team_codes[j]]
   
            # if teams never play others within the season
            # we set the entry in the cell 
            # to be the mean of the team
            if (between_team_games == 0) { # begin first inner if-block
                prop_mat[j,k] <- sum(wins_mat[j,]) / 
                    (sum(wins_mat[,j]) + sum(wins_mat[j,]))
                prop_mat[k,j] <- sum(wins_mat[k,]) / 
                    (sum(wins_mat[,k]) + sum(wins_mat[k,]))
                } # end first inner if-block  
                
            # when teams play other teams at least once 
            # we compute the proportion of times they beat one another
            if (between_team_games > 0) { # begin second inner if-block   
                prop_mat[j,k] <- 
                    wins_mat[ordered_team_codes[j],ordered_team_codes[k]]/
                        between_team_games
                prop_mat[k,j] <- 
                    wins_mat[ordered_team_codes[k],ordered_team_codes[j]]/
                        between_team_games
                }  # end second inner if-block
        } # end outer if-block
   } # end inner for-loop
} # end outer for-loop

# check prop_mat against known results for the season
# results should be close but not exact
# because prop_mat adjusts for strength of schedule
for(i in seq(along = ordered_team_codes))
    cat("\n", ordered_team_codes[i], ":", mean(prop_mat[,i], na.rm = TRUE))

# unidimensional scaling work begins here
# begin by defining the paired_comparisons scaling matrix
pc_mat <- prop_mat
for(j in 1:length(ordered_team_codes)) 
    for(k in 1:length(ordered_team_codes)) 
        if (j == k) pc_mat[j,k] <- 0.50   # set diagonal entries missing

nobjects <- length(ordered_team_codes)

mean_pc_mat <- numeric(nobjects)
object_quantile <- numeric(nobjects)
for(j in 1:nobjects)
 {
  mean_pc_mat[j] <- mean(pc_mat[,j])
  object_quantile[j] <- qnorm(mean_pc_mat[j])
 } 

# user-defined function provides mean 500 standard deviation 100
z.score.converter <- function(z) {round(((100*z) + 500),digits=0)}

scale_score <- numeric(nobjects)
for(j in 1:nobjects)
 scale_score[j] <- z.score.converter(object_quantile[j]) 

sorted_team_info <- team_info[sort.list(team_info$abbreviation),]
sorted_team_info$scale_score <- scale_score

sorted_team_info$name_with_score <- rep("", length = nrow(sorted_team_info))
for(i in seq(along = ordered_team_codes))
sorted_team_info$name_with_score[i] <- 
    paste(sorted_team_info$team_name[i], 
         " (", sorted_team_info$scale_score[i], ")", sep = "") 

scaling_frame <- 
    sorted_team_info[,c("abbreviation", "team_name", 
        "name_with_score", "scale_score")]
names(scaling_frame) <- 
    c("object.name", "long.object.name",
      "long.object.name.with.score", "scale_score")

ordered_scaling_frame <- 
    scaling_frame[sort.list(scale_score, decreasing=FALSE),] 

# plotting to external pdf file using standard R graphics
pdf(file = "fig_unidimensional_scaling_analysis.pdf",
    width = 11, height = 8.5)
text.angle = 45
par(mfrow=c(1,1), xpd=NA, cex=1, yaxt= "n" ,  lwd=3, bty="n", 
    srt=text.angle, mar=c(5, 0, 4, 0) + 0.1)    
        
plot(ordered_scaling_frame$scale_score,
    rep(0.1,length(ordered_scaling_frame$scale_score)), type="h",
xlab=paste("Team Scale Scores Adjusted for Strength of Schedule"),
ylab="",
ylim=c(0,2),
xlim=c(min(ordered_scaling_frame$scale_score) - 50,
    max(ordered_scaling_frame$scale_score) + 50))
    
text(ordered_scaling_frame$scale_score + 40,
    rep(0.65,length(ordered_scaling_frame$scale_score)),
    ordered_scaling_frame$long.object.name.with.score, pos=2, cex=1)               
      
dev.off()


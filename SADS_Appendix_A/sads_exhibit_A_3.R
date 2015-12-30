# Making Differential Runs Plots for Baseball (R)

# data visualization with R standard graphics

excess.runs.data.frame.ARI <- read.csv("MLB_2007_ARI_data_frame.csv")

pdf(file=paste("fig_differential_runs_ARI.pdf",sep=""),
    width = 11, height = 8.5)
par(mfrow=c(1,1), xpd=NA, cex=1)

plot(excess.runs.data.frame.ARI$excess.runs, type="h",
    las=1, xlab="Game Number and Opponent", ylab="Differential Runs",
    ylim=c(-(max(abs(excess.runs.data.frame.ARI$excess.runs),14) + 1),
    (max(abs(excess.runs.data.frame.ARI$excess.runs),14) + 1)))
abline(h=0,lty="solid",xpd=FALSE,lwd=.5)

legend("topright", title=NULL, 
    legend=c("Home Game ","Away Game "), pch=c(19,21))

# plot all as open circles first
points(excess.runs.data.frame.ARI$excess.runs, pch=21)  

# then fill in the circles for the home games
for(i in seq(along=excess.runs.data.frame.ARI$game.number))
  if(excess.runs.data.frame.ARI$home.away[i]=="home") 
  points(excess.runs.data.frame.ARI$game.number[i],
      excess.runs.data.frame.ARI$excess.runs[i], pch=19) 

for(i in seq(along=excess.runs.data.frame.ARI$game.number))
text(excess.runs.data.frame.ARI$game.number[i],-
    (max(abs(excess.runs.data.frame.ARI$excess.runs),15) + 1),
    excess.runs.data.frame.ARI$other.team.label[i], cex=.85, pos=4, srt=90)

text(excess.runs.data.frame.ARI$game.number[1], 
    (max(abs(excess.runs.data.frame.ARI$excess.runs),14) + 1), 
    "Arizona Diamondbacks",cex=2,pos=4)

dev.off()
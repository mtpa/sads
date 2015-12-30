# Payroll and Performance in Major League Baseball (R)

library(ggplot2)  # statistical graphics
library(grid)

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

# read in payroll and performance data 
# including annotation text for team abbreviations
mlb_data <- read.csv("mlb_payroll_performance_2014.csv")
mlb_data$millions <- mlb_data$payroll/1000000
mlb_data$winpercent <- mlb_data$wlpct * 100

cat("\nCorrelation between Payroll and Performance:\n")
with(mlb_data, print(cor(millions, winpercent)))

cat("\nProportion of win/loss percentage explained by payrolls:\n")
with(mlb_data, print(cor(millions, winpercent)^2))

pdf(file = "fig_understanding_markets_payroll_performance.pdf", 
     width = 5.5, height = 5.5)
ggplot_object <- ggplot(data = mlb_data, 
    aes(x = millions, y = winpercent)) +
    geom_point(colour = "darkblue", size = 3) +
    xlab("Team Payroll (Millions of Dollars)") +
    ylab("Percentage of Games Won") +
    geom_text(aes(label = textleft), size = 3, hjust = 1.3) +
    geom_text(aes(label = textright), size = 3, hjust = -0.25)
    
ggplot.print.with.margins(ggplot_object, left.margin.pct = 5,
    right.margin.pct = 5, top.margin.pct = 5, bottom.margin.pct = 5)
    
dev.off()    


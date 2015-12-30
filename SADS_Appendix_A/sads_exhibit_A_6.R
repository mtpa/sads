# Seeing Data Science as an Eclectic Discipline (R)

# Program for multidimensional scaling of ten academic disciplines

library(MASS)  # includes functions for multidimensional scaling
library(wordcloud)  # textplot utility to avoid overlapping text  

USE_METRIC_MDS <- FALSE  # metric versus non-metric toggle

# define a utility function for converting a distance structure 
# to a distance matrix as required for some routines and 
# for printing of the complete matrix for visual inspection.
make.distance.matrix <- function(distance_structure)
    { n <- attr(distance_structure, "Size")
      full <- matrix(0,n,n)
      full[lower.tri(full)] <- distance_structure
      full+t(full)
    }

# enter data into a distance structure as required for various 
# distance-based routines. That is, we enter the upper triangle 
# of the distance matrix as a single vector of distances  
distance_structure <- 
    as.single(c(35, 45, 40, 43, 18,  32,  3, 20, 19,      
                     8, 12,  9, 33,  30, 25,  7, 21,     
                         1, 15, 16,  13, 41, 17, 10,     
                            28, 36,  31, 42,  5, 26,     
                                39,  38, 37,  4, 44,     
                                      2, 11, 34, 24,    
                                         14, 29,  6,     
                                             23, 22,     
                                                 27))    
                                 
# provide a character vector of discipline names
disciplines <- c("Marketing", "Operations Research", "Mathematics", 
    "Statistics", "Computer Science", "Accounting", "Finance", 
    "Management", "Data Science", "Economics")

attr(distance_structure, "Size") <- length(disciplines)  # set size attribute 

# check to see that the distance structure has been entered correctly
# by converting the distance structure to a distance matrix 
# using the utility function make.distance.matrix, which we had defined
distance_matrix <- unlist(make.distance.matrix(distance_structure))
cat("\n","Distance Matrix of Academic Disciplines","\n")
print(distance_matrix)

if (USE_METRIC_MDS)
    {
    # apply the metric multidimensional scaling algorithm and plot the map
    mds_solution <- cmdscale(distance_structure, k=2, eig=T)
    
    
    
    First_Dimension <- mds_solution$points[,1]
    Second_Dimension <- mds_solution$points[,2]
    }
# apply the non-metric multidimensional scaling algorithm
# this is more appropriate for rank-order data
# and provides a more satisfactory solution here

if (!USE_METRIC_MDS)
    {
    mds_solution <- isoMDS(distance_matrix, k = 2, trace = FALSE)
    }

pdf(file = "plot_nonmetric_mds_data_science.pdf",
    width=11, height=8.5) # opens pdf plotting device
# use par(mar = c(bottom, left, top, right)) to set up margins on the plot 
par(mar=c(7.5, 7.5, 7.5, 5))

# original solution
First_Dimension <- mds_solution$points[,1]
Second_Dimension <- mds_solution$points[,2]

# set up the plot but do not plot points... use names for points
plot(First_Dimension, Second_Dimension, type = "n", cex = 1.5, 
    xlim = c(-25, 25), ylim = c(-25, 25))  # first page of pdf plots
# We plot the city names in the locations where points normally go.
text(First_Dimension, Second_Dimension, labels = disciplines,
    offset = 0.0, cex = 1.5)
title("Academic Disciplines (initial solution)") 

# reflect the horizontal dimension
# multiply the first dimension by -1 to get reflected image 
First_Dimension <- mds_solution$points[,1] * -1
Second_Dimension <- mds_solution$points[,2]
plot(First_Dimension, Second_Dimension, type = "n", cex = 1.5, 
    xlim = c(-25, 25), ylim = c(-25, 25))  # second page of pdf plots
text(First_Dimension, Second_Dimension, labels = disciplines,
    offset = 0.0, cex = 1.5)
title("Academic Disciplines (horizontal reflection)") 

# reflect the vertical dimension
# multiply the section dimension by -1 to get reflected image 
First_Dimension <- mds_solution$points[,1] 
Second_Dimension <- mds_solution$points[,2] * -1
plot(First_Dimension, Second_Dimension, type = "n", cex = 1.5, 
    xlim = c(-25, 25), ylim = c(-25, 25))  # third page of pdf plots
text(First_Dimension, Second_Dimension, labels = disciplines,
    offset = 0.0, cex = 1.5)
title("Academic Disciplines (vertical reflection)") 
# multiply the first and second dimensions by -1 
# for reflection in both horizontal and vertical directions
First_Dimension <- mds_solution$points[,1] * -1
Second_Dimension <- mds_solution$points[,2] * -1



plot(First_Dimension, Second_Dimension, type = "n", cex = 1.5, 
    xlim = c(-25, 25), ylim = c(-25, 25))  # fourth page of pdf plots
text(First_Dimension, Second_Dimension, labels = disciplines,
    offset = 0.0, cex = 1.5)
title("Academic Disciplines (horizontal and vertical reflection)") 
dev.off()  # closes the pdf plotting device

pdf(file = "plot_pretty_original_mds_data_science.pdf",
    width=8.5, height=8.5) # opens pdf plotting device
# use par(mar = c(bottom, left, top, right)) to set up margins on the plot 
par(mar=c(7.5, 7.5, 7.5, 5))
First_Dimension <- mds_solution$points[,1]  # no reflection
Second_Dimension <- mds_solution$points[,2]   # no reflection
# wordcloud utility for plotting with no overlapping text
textplot(x = First_Dimension, 
    y = Second_Dimension,
    words = disciplines, 
    show.lines = FALSE, 
    xlim = c(-28, 28),  # extent of horizontal axis range
    ylim = c(-20, 20),  # extent of vertical axis range
    xaxt = "n",  # suppress tick marks
    yaxt = "n",  # suppress tick marks
    cex = 1.25,  # size of text points
    mgp = c(0.85, 1, 0.85),  # position of axis labels
    cex.lab = 1.5,  # magnification of axis label text
    xlab = "Methods . . . . . . . . . . . . . . . Applications",
    ylab = "Mostly Numbers . . . . Numbers and Words")
dev.off()  # closes the pdf plotting device

pdf(file = "plot_reflected_mds_data_science.pdf",
    width=8.5, height=8.5) # opens pdf plotting device
# use par(mar = c(bottom, left, top, right)) to set up margins on the plot 
par(mar=c(7.5, 7.5, 7.5, 5))
First_Dimension <- mds_solution$points[,1] * -1  # reflect horizontal
Second_Dimension <- mds_solution$points[,2] * -1  # reflect vertical
# wordcloud utility for plotting with no overlapping text
textplot(x = First_Dimension, 
    y = Second_Dimension,
    words = disciplines, 
    show.lines = FALSE, 
    xlim = c(-28, 28),  # extent of horizontal axis range
    ylim = c(-20, 20),  # extent of vertical axis range
    xaxt = "n",  # suppress tick marks
    yaxt = "n",  # suppress tick marks
    cex = 1.25,  # size of text points
    mgp = c(0.85, 1, 0.85),  # position of axis labels
    cex.lab = 1.5,  # magnification of axis label text
    xlab = "Applications . . . . . . . . . . . . . . . Methods",
    ylab = "Words and Numbers . . . Mostly Numbers")
dev.off()  # closes the pdf plotting device


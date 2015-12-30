# Product Positioning of Entertainment Events and Activities (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for multivariate analysis
import numpy as np  # arrays and numerical processing
import scipy
import matplotlib.pyplot as plt  # 2D plotting

# alternative distance metrics for multidimensional scaling
from sklearn.metrics import euclidean_distances 
from sklearn.metrics.pairwise import linear_kernel as cosine_distances
from sklearn.metrics.pairwise import manhattan_distances as manhattan_distances

from sklearn import manifold  # multidimensional scaling

# These are the original data from one respondent
# Pairs of activities are judged on their similarity
# Smaller numbers are more similar to one another
# Zero on the diagonal means no difference
#    0    6   11    5    8   15   14  Comedy
#    6    0   10    3    2   19   18  Symphony
#   11   10    0    9    4   17   16  Zoo
#    5    3    9    0    7   13   12  Pop Music
#    8    2    4    7    0   21   20  Museum
#   15   19   17   13   21    0    1  Hockey
#   14   18   16   12   20    1    0  Football

# define a numpy array for these data
distance_matrix = np.array([\
 [0,    6,   11,    5,    8,   15,   14],\
 [6,    0,   10,    3,    2,   19,   18],\
[11,   10,    0,    9,    4,   17,   16],\
 [5,    3,    9,    0,    7,   13,   12],\
 [8,    2,    4,    7,    0,   21,   20],\
[15,   19,   17,   13,   21,    0,    1],\
[14,   18,   16,   12,   20,    1,    0]])
    
# check to see that the distance structure has been entered correctly
print(distance_matrix)    
print(type(distance_matrix))

# apply the multidimensional scaling algorithm and plot the map
mds_method = manifold.MDS(n_components = 2, random_state = 9999,\
    dissimilarity = 'precomputed')
mds_fit = mds_method.fit(distance_matrix)  
mds_coordinates = mds_method.fit_transform(distance_matrix) 
                                                                                                                                  
activity_label = ["Comedy", "Symphony", "Zoo", \
    "Pop Music", "Museum", "Hockey", "Football"]
    
    
    
# plot mds solution in two dimensions using activity labels
# defined by multidimensional scaling
plt.figure()
plt.scatter(mds_coordinates[:,0],mds_coordinates[:,1],\
    facecolors = 'none', edgecolors = 'none')  # points in white (invisible)
labels = activity_label
for label, x, y in zip(labels, mds_coordinates[:,0], mds_coordinates[:,1]):
    plt.annotate(label, (x,y), xycoords = 'data')
plt.xlabel('First Dimension')
plt.ylabel('Second Dimension')    
plt.show()
plt.savefig('fig_positioning_products_mds_activities_python.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='landscape', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)          


# Consumer Preferences for Sporting Events---Conjoint Analysis (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for analysis and modeling
import pandas as pd  # data frame operations
import numpy as np  # arrays and math functions
import statsmodels.api as sm  # statistical models (including regression)
import statsmodels.formula.api as smf  # R-like model specification
from patsy.contrasts import Sum

# read in conjoint survey profiles with respondent ranks
conjoint_data_frame = pd.read_csv('sporting_event_ranking.csv')

# set up sum contrasts for effects coding as needed for conjoint analysis
# using C(effect, Sum) notation within main effects model specification
main_effects_model = 'ranking ~ C(price, Sum) + C(seating, Sum) +  \
    C(boxvip, Sum) + C(frontrow, Sum) + C(promotion, Sum)'

# fit linear regression model using main effects only (no interaction terms)
main_effects_model_fit = \
    smf.ols(main_effects_model, data = conjoint_data_frame).fit()
print(main_effects_model_fit.summary()) 
conjoint_attributes = ['price', 'seating', 'boxvip', 'frontrow', 'promotion']

# build part-worth information one attribute at a time
level_name = []
part_worth = []
part_worth_range = []
end = 1  # initialize index for coefficient in params
for item in conjoint_attributes:
    level_set = set(conjoint_data_frame[item])
    nlevels = len(level_set)
    level_name.append(list(sorted(list(level_set)))) 
    begin = end 
    end = begin + nlevels - 1
    new_part_worth = list(main_effects_model_fit.params[begin:end])
    new_part_worth.append((-1) * sum(new_part_worth))         
    part_worth_range.append(max(new_part_worth) - min(new_part_worth))  
    part_worth.append(new_part_worth)   
    # end set to begin next iteration
    
# compute attribute relative importance values from ranges
attribute_importance = []
for item in part_worth_range:
    attribute_importance.append(round(100 * (item / sum(part_worth_range)),2))  
# user-defined dictionary for printing descriptive attribute names     
effect_name_dict = {'price' : 'Ticket Price', \
    'seating' : 'Seating Area','boxvip' : 'Box/VIP', \
    'frontrow' : 'Front Row',  'promotion' : 'Promotion'}  
 
 
 
# report conjoint measures to console 
index = 0  # initialize for use in for-loop
for item in conjoint_attributes:
    print('\nAttribute:', effect_name_dict[item])
    print('    Importance:', attribute_importance[index])
    print('    Level Part-Worths')
    for level in range(len(level_name[index])):
        print('       ',level_name[index][level], part_worth[index][level])       
    index = index + 1


# Script workflow

# Collect COVID data
source('./Scripts/virus_data.R')

# Collect static variables
source('./Scripts/Gather_GHSindex_data_static.R')

# Clean the data for implementation in the ML model
source('./Scripts/data_cleaning.R')

# Make a regression tree model

# Chris's model to srart
#source('./Scripts/real_data_tree.R')

# Jay's script (based on chris') using CARET so multiple models can be evaluated.



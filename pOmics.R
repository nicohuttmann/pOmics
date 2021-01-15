# To get the only non-CRAN package to load pOmics

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("UniProt.ws")


# Start of the analysis
# Builds the data structures use by this package
# Continue with prepare data when asked
# Set load.UniProt.ws = T to load the UniProt database and retrieve info

import_datasets()


# Now all data is loaded
# view_data() shows all data structures
# their names are hidden in the GlobalEnvironment
# show all by

ls(all.names = T)


# The basic data structures for now are

# Imported files are stored in .imports
.imports

# Processed datasets are in .datasets
.datasets

# Additional global information is stored in .info
.info

# External data bases are stored in .databases; for now only the UniProt.ws will be loaded
.databases


# View default data of datasets like this
attributes(.datasets[[1]])


# Datasets have three kinds of information
# Variables information (Protein IDs, Gene names, Protein groups, variables results are stored here as well)
.datasets[[1]][["variables"]]

# Variables can be specified by get_variables() it uses the syntax of dplyr::filter
get_variables(all == T)
get_variables(all) #works as well because all is a logical
get_variables(pvalue < 0.05 & fc.ttest > 0) # works once p value and other informations are stored in variables data


# Observations data (Names, groups, also results)
# Note: Observations can be defined in different observations sets (only important if observations change)
.datasets[[1]][["observations"]][["raw"]]

# Observations can be specified using the same logic as variables
get_observations(all)
get_observations(vesicle.type == "Exo")

# Actual data such as LFQ intensity or Identification results
# They have separate entries in the .datasets[[dataset]] list
.datasets[[1]][["LFQ"]][[1]]




# The main function used in this package to obtain data is the get_data function which uses
# get_variables and get_observations as its first inputs

get_data(variables = all, observations = all,
         observations.set =, # DEFAULT
         name = , # DEFAULT
         type = , # DEFAULT
         dataset = ) # DEFAULT


# Most arguments such as those specified obove have default values and do not need to be specified every time





# directory <- "/Users/amy/Documents/software/CatchAll" ## your local copy here
# setwd(directory)
# 
# ### Load test data
# butterfly <- read.csv("tests/butterfly.csv", header = F)
# butterfly <- butterfly[butterfly[,2] > 0, ]
# butterfly
# rownames(butterfly) <- NULL
# colnames(butterfly) <- c("index", "frequency")
# butterfly
# devtools::use_data(butterfly)
# 
# ### 
# butterfly_check <- read.csv("tests/butterfly_Analysis.csv", header = T)
# butterfly_catchall <- butterfly_check
# 
# devtools::use_data(butterfly_catchall)

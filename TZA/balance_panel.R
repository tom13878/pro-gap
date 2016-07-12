# -------------------------------------
# Balance TZA panel
# -------------------------------------

filePath <- "C:/users/tomas/documents/LEI"

# get the full data and get the panel key
source(file.path(filePath, "pro-gap/TZA/data_combineTZA.R"))
source(file.path(filePath, "pro-gap/TZA/panel_key.R"))

# -------------------------------------
# The TZA data is quite difficult to
# balance. In general, there are three
# steps to balancing the data
# 1. select the data that you want to
# analyse, for example maize farmers
# 2. Check that these farmers were
# followed for all three/two waves of
# the data
# 3. make sure that each household or
# individual actually responded to 
# your selection of the data. For example
# just because a farmer was followed for
# three years, does not necessarily mean
# he produced maize each year!
# -------------------------------------

# Example 1: maize farmers
# step 1: make your selection
maize <- filter(fullData081012, status=="HEAD", zaocode==11)

# step 2: check if the households were followed
# each year
maize <- filter(maize, hhid %in% KEY$hhid2012)

# step 3: check whether that farmer actually
# produced maize each year
x1 <- maize$hhid[maize$surveyyear==2008]
x2 <- maize$hhid[maize$surveyyear==2010]
x3 <- maize$hhid[maize$surveyyear==2012]
good <- Reduce(intersect, list(x1, x2, x3))
maize <- filter(maize, hhid %in% good)

# now we have a balanced panel of maize producers
# and their plots. However, we may also be interested
# in the maize farmers themselves and not their
# plots. In which case we can either start the
# whole process again, or just select the maize
# farmers from the current database. Here we 
# do both as a check.

# -------------------------------------
# Example 2: selecting maize producing
# households
# -------------------------------------

# step 1: select maize farmers as before
# The only difference with example 1 is that
# we only want household level variables

# find where the plot variables start
plot_var <- (which(names(fullData081012)=="plotnum")) - 1
maize2 <- filter(fullData081012, status=="HEAD", zaocode==11) %>%
  select(1:plot_var, surveyyear) %>% unique

# step 2: check if the households were followed
# each year
maize2 <- filter(maize2, hhid %in% KEY$hhid2012)

# step 3: check whether that farmer actually
# produced maize each year
x1 <- maize2$hhid[maize2$surveyyear==2008]
x2 <- maize2$hhid[maize2$surveyyear==2010]
x3 <- maize2$hhid[maize2$surveyyear==2012]
good <- Reduce(intersect, list(x1, x2, x3))
maize2 <- filter(maize2, hhid %in% good)

# and the second method would be just to select
# maize households from the maize plots
maize3 <- select(maize, 1:plot_var, surveyyear) %>% unique
identical(maize2, maize3) # TRUE

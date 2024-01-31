library(osfr)
library(R.matlab)

## get datga
# Your personal access token for authentication
access_token <- "3JaJO6hdZs4g3b45rSFmbOtKzKA4aabSrONYBy32XSt6Xh69Az38FXP3pxql6fgHBo9sCH"
osf_auth(access_token)

my_project <- osf_retrieve_node("xpnk8")
files <- osf_ls_files(my_project)



hmm_files <- osf_ls_files(my_project, path = "Hmeta-d matlab")
this_file <- dplyr::filter(hmm_files, name == "fitFc.mat")

this_file %>% osf_download(path = "data/", conflicts = "overwrite")

## import and plot


mat_file_path <- "data/allResults.mat"

# Read the .mat file
allResults <- readMat(mat_file_path)

sample_diff <- allResults$t$sample_diff

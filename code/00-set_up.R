# Set up token
library(googlesheets4)

# store secret token in a folder. Make sure it doesn't get uploaded to github

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")

# check the value of the option, if you like
gargle::gargle_oauth_cache()

# trigger auth on purpose to store a token in the specified cache
# a broswer will be opened
googlesheets4::sheets_auth()

# after authentication
# see your token file in the cache, if you like
list.files(".secrets/")

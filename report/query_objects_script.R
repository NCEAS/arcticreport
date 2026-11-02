library(devtools)
devtools::load_all("~/arcticreport/")

### Set TOKEN = "admin token" in environment variable

# Retrieve the token from environment variables
admin_token <- Sys.getenv("TOKEN")

# Sanity check: Ensure the token actually exists before proceeding
if (admin_token == "") {
  stop("Error: TOKEN environment variable is not set.")
}

options(dataone_token = admin_token)

# make sure you set a token!
# Cache tolerance is maximum number of days tolerable for age of cached results. 
# If the cached results are older than the cache tolerance, a new dataset will be cached and the old cache deleted. 
# To force a cache refresh, set tolerance to zero.

print("LOGGED IN, NOW RUNNING FUNCTION!")

objs <- query_objects(cache_tolerance = 1)

print("DONE DONE DONE")

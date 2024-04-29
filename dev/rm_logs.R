# This maintenance script simply deletes all files in the log folder of the app
# To run simply do Rscript inst/rm_logs.R
# --- #

library(assertthat)

# Get all log files
ll <- list.files("logs/", full.names = TRUE)
ll <- ll[assertthat::has_extension(ll, "rds")]

# No logs found
if(length(ll)==0){
  message("No logs found...")
} else {
  # Message
  message("Found a total of ", length(ll), " log files. Deleting them all?")
  if(utils::askYesNo(msg = "Delete?")){
    sapply(ll, file.remove)
  }
}

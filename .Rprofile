# R startup script for Singularity container environment
# Clean version - no global environment pollution

# Use local environment to avoid cluttering global environment
local({
  # Detect environment
  in_container <- file.exists("/singularity") || Sys.getenv("SINGULARITY_CONTAINER") != ""

  # Set paths based on environment
  if (in_container) {
    project_dir <- "/project"
    lib_path <- "/project/R_libs"
  } else {
    # Native R/RStudio
    project_dir <- getwd()
    lib_path <- file.path(project_dir, "R_libs")
  }

  # Create R_libs if it doesn't exist
  if (!dir.exists(lib_path)) {
    dir.create(lib_path, recursive = TRUE)
  }

  # Ensure project R library is first in path
  .libPaths(c(lib_path, .libPaths()))

  # Set working directory to project (for container)
  if (interactive() && in_container) {
    setwd(project_dir)
  }

  # Set options
  options(
    repos = c(CRAN = "https://cloud.r-project.org/"),
    BioC_mirror = "https://bioconductor.org",
    download.file.method = "libcurl",
    timeout = 600
  )

  # Source the tracking tools if available
  tools_file <- file.path(project_dir, "r_tools", "track.R")
  tracker_loaded <- FALSE

  if (file.exists(tools_file)) {
    tryCatch({
      source(tools_file)
      tracker_loaded <- TRUE
    }, error = function(e) {
      # Silent failure - will be reported in summary
    })
  }

  # Display minimal environment info
  if (interactive()) {
    r_version <- paste(R.version$major, R.version$minor, sep = ".")
    env_type <- if (in_container) "container" else "native"
    tracker_status <- if (tracker_loaded) "loaded" else "not loaded"
    
    cat(sprintf("trackR: %s | Library: %s | R %s (%s)\n", 
                tracker_status, lib_path, r_version, env_type))
  }
})

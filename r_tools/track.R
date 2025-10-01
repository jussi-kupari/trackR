#!/usr/bin/env Rscript
# trackR
# Version 2.3 - Fixed error handling, vector checks, and jsonlite dependency

# Create a namespace-like environment to avoid cluttering global environment
trackr <- new.env()

#' Ensure jsonlite is available
#' @return TRUE if jsonlite is available, FALSE otherwise
trackr$ensure_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    message("Installing required package: jsonlite")
    tryCatch({
      # Get project-local library path
      project_dir <- getwd()
      lib_path <- file.path(project_dir, "R_libs")
      
      if (!dir.exists(lib_path)) {
        dir.create(lib_path, recursive = TRUE)
      }
      
      install.packages("jsonlite", lib = lib_path, repos = "https://cloud.r-project.org/")
      
      # Try to load it
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("Failed to load jsonlite after installation")
      }
      message("jsonlite installed successfully")
      return(TRUE)
    }, error = function(e) {
      warning("Failed to install jsonlite: ", e$message)
      return(FALSE)
    })
  }
  return(TRUE)
}

#' Extract package name from various reference formats
#' 
#' @param pkg_string Package reference (e.g., "user/repo@ref", "package", etc.)
#' @return Extracted package name
trackr$extract_package_name <- function(pkg_string) {
  if (is.null(pkg_string) || length(pkg_string) == 0) {
    return(character(0))
  }
  
  # Ensure single string
  pkg_string <- as.character(pkg_string[1])
  
  # For GitHub/GitLab/Bitbucket: "user/repo@ref" -> "repo"
  # For regular packages: "package" -> "package"
  if (grepl("/", pkg_string)) {
    # Extract repo name from "user/repo" or "user/repo@ref"
    pkg_string <- sub("@.*$", "", pkg_string)  # Remove @ref if present
    pkg_string <- sub(".*/", "", pkg_string)   # Get last part after /
  }
  return(pkg_string)
}

#' Generate actual R command for installation (not tracking wrapper)
trackr$generate_actual_command <- function(packages, method = "cran", ...) {
  if (is.null(packages) || length(packages) == 0) {
    return("# No packages specified")
  }
  
  tryCatch({
    if (method == "cran") {
      if (length(packages) == 1) {
        sprintf('install.packages("%s")', packages[1])
      } else {
        sprintf('install.packages(c(%s))', paste0('"', packages, '"', collapse = ", "))
      }
    } else if (method == "bioc") {
      if (length(packages) == 1) {
        sprintf('BiocManager::install("%s")', packages[1])
      } else {
        sprintf('BiocManager::install(c(%s))', paste0('"', packages, '"', collapse = ", "))
      }
    } else if (method %in% c("github", "gitlab", "bitbucket")) {
      install_func <- paste0("remotes::install_", method)
      if (length(packages) == 1) {
        sprintf('%s("%s")', install_func, packages[1])
      } else {
        sprintf('%s(c(%s))', install_func, paste0('"', packages, '"', collapse = ", "))
      }
    } else if (method == "url") {
      if (length(packages) == 1) {
        sprintf('install.packages("%s", repos = NULL)', packages[1])
      } else {
        sprintf('install.packages(c(%s), repos = NULL)', paste0('"', packages, '"', collapse = ", "))
      }
    } else {
      "# Unknown installation method"
    }
  }, error = function(e) {
    warning("Error generating command: ", e$message)
    return("# Error generating command")
  })
}

#' Install and track R packages
#' 
#' @param packages Character vector of package names
#' @param method Installation method ("cran", "bioc", "github", "gitlab", "bitbucket", "url")
#' @param ... Additional arguments passed to installation function
#' @export
trackr$install_tracked <- function(packages, method = "cran", ...) {
  # Input validation
  if (is.null(packages) || length(packages) == 0) {
    stop("No packages specified")
  }
  
  if (!is.character(packages)) {
    stop("packages must be a character vector")
  }
  
  if (!method %in% c("cran", "bioc", "github", "gitlab", "bitbucket", "url")) {
    stop("Invalid method. Use 'cran', 'bioc', 'github', 'gitlab', 'bitbucket', or 'url'")
  }
  
  # Get project directory
  project_dir <- tryCatch(getwd(), error = function(e) ".")
  lib_path <- file.path(project_dir, "R_libs")
  
  # Ensure R_libs exists
  if (!dir.exists(lib_path)) {
    tryCatch({
      dir.create(lib_path, recursive = TRUE)
    }, error = function(e) {
      stop("Failed to create library directory: ", e$message)
    })
  }
  
  # CAPTURE PRE-INSTALL STATE
  packages_before <- if (dir.exists(lib_path)) {
    tryCatch({
      list.dirs(lib_path, full.names = FALSE, recursive = FALSE)
    }, error = function(e) {
      warning("Error reading library directory: ", e$message)
      character(0)
    })
  } else {
    character(0)
  }
  
  # Extract clean package names for tracking
  package_names <- tryCatch({
    sapply(packages, trackr$extract_package_name, USE.NAMES = FALSE)
  }, error = function(e) {
    warning("Error extracting package names: ", e$message)
    packages
  })
  
  # Generate the actual R command (without tracking wrapper)
  actual_command <- trackr$generate_actual_command(packages, method, ...)
  
  # Record installation attempt (only for explicitly requested packages)
  install_log <- list(
    timestamp = Sys.time(),
    packages = as.character(packages),  # Ensure character vector
    package_names = as.character(package_names),  # Ensure character vector
    method = as.character(method)[1],  # Ensure single string
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = as.character(R.version$platform)[1],
    command = paste(deparse(match.call()), collapse = " "),  # Tracking wrapper command - collapsed to single string
    actual_command = as.character(actual_command)[1],   # Actual R command
    success = FALSE,
    output = NULL
  )
  
  # Perform installation
  tryCatch({
    if (method == "cran") {
      install.packages(packages, lib = lib_path, ...)
    } else if (method == "bioc") {
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager", lib = lib_path)
      }
      BiocManager::install(packages, lib = lib_path, update = FALSE, ask = FALSE, ...)
    } else if (method == "github") {
      if (!requireNamespace("remotes", quietly = TRUE)) {
        install.packages("remotes", lib = lib_path)
      }
      remotes::install_github(packages, lib = lib_path, ...)
    } else if (method == "gitlab") {
      if (!requireNamespace("remotes", quietly = TRUE)) {
        install.packages("remotes", lib = lib_path)
      }
      remotes::install_gitlab(packages, lib = lib_path, ...)
    } else if (method == "bitbucket") {
      if (!requireNamespace("remotes", quietly = TRUE)) {
        install.packages("remotes", lib = lib_path)
      }
      remotes::install_bitbucket(packages, lib = lib_path, ...)
    } else if (method == "url") {
      install.packages(packages, lib = lib_path, repos = NULL, ...)
    }
    
    install_log$success <- TRUE
    install_log$output <- "Installation completed successfully"
    
    # CAPTURE POST-INSTALL STATE
    packages_after <- tryCatch({
      list.dirs(lib_path, full.names = FALSE, recursive = FALSE)
    }, error = function(e) {
      warning("Error reading library directory after install: ", e$message)
      packages_before
    })
    
    newly_installed <- setdiff(packages_after, packages_before)
    
    # Record what was actually installed (for debugging/info)
    install_log$all_installed <- newly_installed
    install_log$dependencies_installed <- setdiff(newly_installed, package_names)
    
    message(" Installation successful: ", paste(packages, collapse = ", "))
    if (length(install_log$dependencies_installed) > 0) {
      message("   Dependencies also installed: ", 
              paste(head(install_log$dependencies_installed, 10), collapse = ", "),
              if (length(install_log$dependencies_installed) > 10) "..." else "")
    }
    
  }, error = function(e) {
    install_log$success <<- FALSE
    install_log$output <<- as.character(e$message)[1]
    message(" Installation failed: ", e$message)
  })
  
  # Save to log (only logs explicitly requested packages)
  trackr$save_install_log(install_log, project_dir)
  
  invisible(install_log)
}

#' Save installation log
trackr$save_install_log <- function(log_entry, project_dir = getwd()) {
  # Ensure jsonlite is available
  if (!trackr$ensure_jsonlite()) {
    warning("Cannot save install log: jsonlite not available")
    return(invisible(NULL))
  }
  
  log_file_json <- file.path(project_dir, ".r_install_history.json")
  log_file_txt <- file.path(project_dir, ".r_install_history.txt")
  
  tryCatch({
    # Load existing logs
    if (file.exists(log_file_json)) {
      existing_logs <- jsonlite::fromJSON(log_file_json, simplifyVector = FALSE)
      if (!is.list(existing_logs)) {
        existing_logs <- list()
      }
    } else {
      existing_logs <- list()
    }
    
    # Append new log
    existing_logs[[length(existing_logs) + 1]] <- log_entry
    
    # Save JSON
    jsonlite::write_json(existing_logs, log_file_json, pretty = TRUE, auto_unbox = TRUE)
    
    # Save text version
    status <- if (isTRUE(log_entry$success)) "+" else "x"
    
    packages_str <- if (is.null(log_entry$packages)) {
      "unknown"
    } else if (length(log_entry$packages) > 0) {
      paste(log_entry$packages, collapse = ", ")
    } else {
      "none"
    }
    
    actual_cmd <- if (is.null(log_entry$actual_command)) {
      "unknown"
    } else {
      as.character(log_entry$actual_command)[1]
    }
    
    txt_entry <- sprintf(
      "[%s] %s %s\n  Packages: %s\n  Method: %s\n  Command: %s\n",
      format(log_entry$timestamp, "%Y-%m-%d %H:%M:%S"),
      status,
      log_entry$method,
      packages_str,
      log_entry$method,
      actual_cmd
    )
    
    # Add dependency info if available
    if (!is.null(log_entry$dependencies_installed) && length(log_entry$dependencies_installed) > 0) {
      deps_str <- paste(head(log_entry$dependencies_installed, 10), collapse = ", ")
      if (length(log_entry$dependencies_installed) > 10) {
        deps_str <- paste0(deps_str, "...")
      }
      txt_entry <- paste0(txt_entry, sprintf("  Dependencies: %s\n", deps_str))
    }
    txt_entry <- paste0(txt_entry, "\n")
    
    cat(txt_entry, file = log_file_txt, append = TRUE)
    
    message(" Installation logged")
    
  }, error = function(e) {
    warning("Failed to save installation log: ", e$message)
  })
  
  invisible(NULL)
}

#' Show installation history
#' 
#' @param recent Number of recent installations to show (NULL for all)
#' @param method Filter by installation method
#' @export
trackr$show_install_history <- function(recent = NULL, method = NULL) {
  # Ensure jsonlite is available
  if (!trackr$ensure_jsonlite()) {
    stop("Cannot show install history: jsonlite not available")
  }
  
  project_dir <- tryCatch(getwd(), error = function(e) ".")
  log_file <- file.path(project_dir, ".r_install_history.json")
  
  if (!file.exists(log_file)) {
    message("No installation history found")
    return(invisible(NULL))
  }
  
  history <- tryCatch({
    jsonlite::fromJSON(log_file, simplifyVector = FALSE)
  }, error = function(e) {
    stop("Failed to read installation history: ", e$message)
  })
  
  if (!is.list(history) || length(history) == 0) {
    message("No installation history found")
    return(invisible(NULL))
  }
  
  # Filter by method if specified
  if (!is.null(method)) {
    history <- Filter(function(x) {
      !is.null(x$method) && x$method == method
    }, history)
  }
  
  # Limit to recent if specified
  if (!is.null(recent) && is.numeric(recent) && recent > 0 && length(history) > recent) {
    history <- tail(history, recent)
  }
  
  cat("\n R Package Installation History\n")
  cat(strrep("=", 50), "\n\n")
  
  for (entry in history) {
    status <- if (isTRUE(entry$success)) "+" else "x"
    timestamp <- if (!is.null(entry$timestamp)) {
      format(as.POSIXct(entry$timestamp), "%Y-%m-%d %H:%M:%S")
    } else {
      "unknown"
    }
    
    cat(sprintf("%s %s\n", status, timestamp))
    cat(sprintf("   Method: %s\n", if (!is.null(entry$method)) entry$method else "unknown"))
    
    packages_str <- if (!is.null(entry$packages) && length(entry$packages) > 0) {
      paste(entry$packages, collapse = ", ")
    } else {
      "none"
    }
    cat(sprintf("   Packages: %s\n", packages_str))
    
    if (!is.null(entry$actual_command)) {
      cat(sprintf("   Command: %s\n", as.character(entry$actual_command)[1]))
    }
    
    if (!is.null(entry$dependencies_installed) && length(entry$dependencies_installed) > 0) {
      deps_str <- paste(head(entry$dependencies_installed, 5), collapse = ", ")
      if (length(entry$dependencies_installed) > 5) {
        deps_str <- paste0(deps_str, "...")
      }
      cat(sprintf("   Dependencies: %s\n", deps_str))
    }
    
    if (!isTRUE(entry$success) && !is.null(entry$output)) {
      cat(sprintf("   Error: %s\n", substr(as.character(entry$output)[1], 1, 100)))
    }
    cat("\n")
  }
  
  invisible(history)
}

#' Analyze installed R packages
#' 
#' @param lib_path Path to R library directory
#' @param rhistory_path Path to .Rhistory file
#' @export
trackr$analyze_r_packages <- function(lib_path = "R_libs", rhistory_path = ".Rhistory") {
  # Ensure jsonlite is available
  if (!trackr$ensure_jsonlite()) {
    stop("Cannot analyze packages: jsonlite not available")
  }
  
  # Check if lib_path exists
  if (!dir.exists(lib_path)) {
    message("R library directory not found: ", lib_path)
    return(list(manually_installed = list(), dependencies = list()))
  }
  
  # Read .Rhistory if available
  history_lines <- if (file.exists(rhistory_path)) {
    tryCatch({
      readLines(rhistory_path, warn = FALSE)
    }, error = function(e) {
      warning("Failed to read .Rhistory: ", e$message)
      character(0)
    })
  } else {
    character(0)
  }
  
  # Read install history log
  install_log <- list()
  log_file <- ".r_install_history.json"
  if (file.exists(log_file)) {
    install_log <- tryCatch({
      jsonlite::fromJSON(log_file, simplifyVector = FALSE)
    }, error = function(e) {
      warning("Failed to read install log: ", e$message)
      list()
    })
  }
  
  if (!is.list(install_log)) {
    install_log <- list()
  }
  
  # Define install command patterns
  install_patterns <- c(
    "install\\.packages\\(",
    "install_tracked\\(",
    "install_cran\\(",
    "install_bioc\\(",
    "install_github\\(",
    "install_gitlab\\(",
    "install_bitbucket\\(",
    "BiocManager::install\\(",
    "biocLite\\(",
    "devtools::install_github\\(",
    "remotes::install_github\\(",
    "remotes::install_cran\\(",
    "remotes::install_bioc\\(",
    "remotes::install_gitlab\\(",
    "remotes::install_bitbucket\\("
  )
  combined_pattern <- paste(install_patterns, collapse = "|")
  rhistory_commands <- grep(combined_pattern, history_lines, value = TRUE)
  
  # Get all package directories
  pkg_dirs <- tryCatch({
    list.dirs(lib_path, full.names = TRUE, recursive = FALSE)
  }, error = function(e) {
    warning("Error reading package directories: ", e$message)
    character(0)
  })
  
  manually_installed <- list()
  dependencies <- list()
  
  for (pkg_dir in pkg_dirs) {
    desc_file <- file.path(pkg_dir, "DESCRIPTION")
    pkg_name <- basename(pkg_dir)
    
    if (file.exists(desc_file)) {
      desc <- tryCatch({
        read.dcf(desc_file)
      }, error = function(e) {
        warning("Error reading DESCRIPTION for ", pkg_name, ": ", e$message)
        NULL
      })
      
      if (is.null(desc) || nrow(desc) == 0) next
      
      metadata <- as.list(desc[1, ])
      
      # Check install log first (most reliable)
      log_match <- NULL
      for (entry in install_log) {
        # Enhanced matching logic for GitHub packages
        package_matches <- tryCatch({
          any(
            pkg_name %in% entry$packages,
            pkg_name %in% entry$package_names,
            # Match GitHub format: "username/repo" or "username/repo@ref"
            !is.null(metadata$RemoteRepo) && 
              any(grepl(paste0("/", metadata$RemoteRepo, "(@|$)"), entry$packages)),
            !is.null(metadata$GithubRepo) && 
              any(grepl(paste0("/", metadata$GithubRepo, "(@|$)"), entry$packages)),
            # Match full GitHub URL format
            !is.null(metadata$RemoteRepo) && !is.null(metadata$RemoteUsername) &&
              any(grepl(paste0("github.com/", metadata$RemoteUsername, "/", metadata$RemoteRepo), entry$packages))
          )
        }, error = function(e) FALSE)
        
        if (isTRUE(package_matches)) {
          log_match <- entry
          break
        }
      }
      
      # Then check .Rhistory with flexible matching
      rhistory_match <- character(0)
      
      # Look for package name in quotes
      rhistory_match <- grep(sprintf('[\"\']%s[\"\']', pkg_name), rhistory_commands, value = TRUE)
      
      # Also check for GitHub references in .Rhistory
      if (length(rhistory_match) == 0 && !is.null(metadata$RemoteRepo) && !is.null(metadata$RemoteUsername)) {
        # Look for "username/repo" pattern
        github_ref <- paste0(metadata$RemoteUsername, "/", metadata$RemoteRepo)
        rhistory_match <- grep(sprintf('[\"\']%s[\"\']', github_ref), rhistory_commands, value = TRUE)
      }
      
      # Also check full GitHub URL
      if (length(rhistory_match) == 0 && !is.null(metadata$RemoteRepo) && !is.null(metadata$RemoteUsername)) {
        github_url <- paste0("github.com/", metadata$RemoteUsername, "/", metadata$RemoteRepo)
        rhistory_match <- grep(github_url, rhistory_commands, value = TRUE)
      }
      
      # Helper function to safely extract and convert to single string
      safe_extract <- function(x) {
        if (is.null(x) || length(x) == 0) return(NA_character_)
        if (is.list(x)) x <- unlist(x)
        # If multiple elements, collapse them (for deparsed commands)
        if (length(x) > 1) {
          return(paste(x, collapse = " "))
        }
        as.character(x)[1]
      }
      
      # Record installation info
      metadata$InstallCommandUsed <- if (!is.null(log_match)) {
        safe_extract(log_match$command)
      } else if (length(rhistory_match) > 0) {
        safe_extract(rhistory_match[1])
      } else {
        NA_character_
      }
      
      metadata$ActualInstallCommand <- if (!is.null(log_match) && !is.null(log_match$actual_command)) {
        safe_extract(log_match$actual_command)
      } else if (length(rhistory_match) > 0) {
        safe_extract(rhistory_match[1])
      } else {
        NA_character_
      }
      
      metadata$InstallTimestamp <- if (!is.null(log_match)) {
        safe_extract(log_match$timestamp)
      } else {
        NA_character_
      }
      
      metadata$InstallMethod <- if (!is.null(log_match)) {
        safe_extract(log_match$method)
      } else {
        NA_character_
      }
      
      # Reconstruct install command - ensure single values
      version <- safe_extract(metadata$Version)
      repo <- safe_extract(metadata$Repository)
      remote_type <- safe_extract(metadata$RemoteType)
      remote_repo <- safe_extract(metadata$RemoteRepo)
      remote_user <- safe_extract(metadata$RemoteUsername)
      remote_ref <- safe_extract(metadata$RemoteRef)
      
      if (!is.na(remote_type) && remote_type %in% c("github", "gitlab", "bitbucket") &&
          !is.na(remote_repo) && !is.na(remote_user)) {
        ref <- if (!is.na(remote_ref)) remote_ref else "HEAD"
        metadata$ReproduceInstall <- paste0("remotes::install_", remote_type, "(\"",
                                            remote_user, "/", remote_repo, "@", ref, "\")")
      } else if (!is.na(repo) && grepl("BioC", repo)) {
        metadata$ReproduceInstall <- paste0("BiocManager::install(\"", pkg_name, 
                                            "\") # version: ", version)
      } else if (!is.na(repo) && repo == "CRAN") {
        metadata$ReproduceInstall <- paste0("remotes::install_version(\"", pkg_name, 
                                            "\", version = \"", version, "\")")
      } else {
        if (!is.na(version)) {
          metadata$ReproduceInstall <- paste0("remotes::install_version(\"", pkg_name, 
                                              "\", version = \"", version, "\")")
        } else {
          metadata$ReproduceInstall <- paste0("# Unknown source for ", pkg_name)
        }
      }
      
      # Safe check for installation evidence
      install_cmd_used <- metadata$InstallCommandUsed
      is_manual <- !is.na(install_cmd_used) && nchar(install_cmd_used) > 0
      
      if (is_manual) {
        manually_installed[[pkg_name]] <- metadata
      } else {
        dependencies[[pkg_name]] <- metadata
      }
    }
  }
  
  list(
    manually_installed = manually_installed, 
    dependencies = dependencies,
    summary = list(
      total_packages = length(c(manually_installed, dependencies)),
      manually_installed_count = length(manually_installed),
      dependencies_count = length(dependencies),
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      lib_path = lib_path
    )
  )
}

#' Print package analysis summary
#' 
#' @param analysis Result from analyze_r_packages()
#' @export
trackr$print_package_summary <- function(analysis = NULL) {
  if (is.null(analysis)) {
    analysis <- trackr$analyze_r_packages()
  }
  
  if (is.null(analysis$summary)) {
    stop("Invalid analysis object: missing summary")
  }
  
  cat("\n R Package Analysis Summary\n")
  cat(strrep("=", 50), "\n")
  cat(sprintf("R version: %s\n", analysis$summary$r_version))
  cat(sprintf("Library path: %s\n", analysis$summary$lib_path))
  cat(sprintf("Total packages: %d\n", analysis$summary$total_packages))
  cat(sprintf("Manually installed: %d\n", analysis$summary$manually_installed_count))
  cat(sprintf("Dependencies: %d\n", analysis$summary$dependencies_count))
  
  if (analysis$summary$manually_installed_count > 0) {
    cat("\nManually installed packages:\n")
    for (pkg_name in names(analysis$manually_installed)) {
      pkg <- analysis$manually_installed[[pkg_name]]
      version <- if (!is.null(pkg$Version)) as.character(pkg$Version)[1] else "unknown"
      method <- if (!is.null(pkg$InstallMethod) && !is.na(pkg$InstallMethod)) {
        as.character(pkg$InstallMethod)[1]
      } else {
        "unknown"
      }
      cat(sprintf("  - %s (%s) [%s]\n", pkg_name, version, method))
    }
  }
  
  if (analysis$summary$dependencies_count > 0) {
    cat("\nAuto-installed dependencies (no install history):\n")
    dep_names <- names(analysis$dependencies)
    if (length(dep_names) > 20) {
      cat(sprintf("  %s\n", paste(head(dep_names, 20), collapse = ", ")))
      cat(sprintf("  ... and %d more\n", length(dep_names) - 20))
    } else {
      cat(sprintf("  %s\n", paste(dep_names, collapse = ", ")))
    }
  }
  
  invisible(analysis)
}

#' Generate reproducibility report
#' 
#' @param output_file Output JSON file path
#' @export
trackr$generate_reproducibility_report <- function(output_file = NULL) {
  # Ensure jsonlite is available
  if (!trackr$ensure_jsonlite()) {
    stop("Cannot generate report: jsonlite not available")
  }
  
  analysis <- trackr$analyze_r_packages()
  
  report <- list(
    generated_at = Sys.time(),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = as.character(R.version$platform)[1],
    lib_path = analysis$summary$lib_path,
    total_packages = analysis$summary$total_packages,
    manually_installed = analysis$manually_installed,
    dependencies = analysis$dependencies
  )
  
  # Generate default filename if not provided
  if (is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- paste0("r_reproducibility_report_", timestamp, ".json")
  }
  
  tryCatch({
    jsonlite::write_json(report, output_file, pretty = TRUE, auto_unbox = TRUE)
    
    message(" Reproducibility report saved: ", output_file)
    message("   Total packages: ", report$total_packages)
    message("   Manually installed: ", length(analysis$manually_installed))
    message("   Dependencies: ", length(analysis$dependencies))
    
  }, error = function(e) {
    stop("Failed to write report: ", e$message)
  })
  
  invisible(report)
}

#' Generate install script
#' 
#' @param package_info Result from analyze_r_packages()
#' @param command_type Which command to use: "actual", "reproduce", or "original"
#' @param include_dependencies Include dependency packages
#' @param output_file Output file path
#' @export
trackr$generate_install_script <- function(package_info = NULL,
                                                    command_type = c("actual", "reproduce", "original"),
                                                    include_dependencies = FALSE,
                                                    output_file = "install_r_packages.R") {
  
  command_type <- match.arg(command_type)
  
  if (is.null(package_info)) {
    package_info <- trackr$analyze_r_packages()
  }
  
  get_command <- function(pkg_data) {
    if (is.null(pkg_data)) return(NULL)
    
    # Priority based on command_type
    if (command_type == "actual") {
      # Use ActualInstallCommand if available, fall back to ReproduceInstall
      if (!is.null(pkg_data$ActualInstallCommand) && !is.na(pkg_data$ActualInstallCommand)) {
        return(as.character(pkg_data$ActualInstallCommand)[1])
      } else if (!is.null(pkg_data$ReproduceInstall)) {
        return(as.character(pkg_data$ReproduceInstall)[1])
      }
    } else if (command_type == "reproduce") {
      # Use ReproduceInstall if available, fall back to ActualInstallCommand
      if (!is.null(pkg_data$ReproduceInstall)) {
        return(as.character(pkg_data$ReproduceInstall)[1])
      } else if (!is.null(pkg_data$ActualInstallCommand) && !is.na(pkg_data$ActualInstallCommand)) {
        return(as.character(pkg_data$ActualInstallCommand)[1])
      }
    } else {  # "original"
      # Use InstallCommandUsed (original command from history)
      if (!is.null(pkg_data$InstallCommandUsed) && !is.na(pkg_data$InstallCommandUsed)) {
        return(as.character(pkg_data$InstallCommandUsed)[1])
      } else if (!is.null(pkg_data$ActualInstallCommand) && !is.na(pkg_data$ActualInstallCommand)) {
        return(as.character(pkg_data$ActualInstallCommand)[1])
      }
    }
    return(NULL)
  }
  
  manual_cmds <- lapply(package_info$manually_installed, get_command)
  manual_cmds <- Filter(Negate(is.null), manual_cmds)
  
  dep_cmds <- character(0)
  if (include_dependencies) {
    dep_cmds <- lapply(package_info$dependencies, function(pkg_data) {
      if (!is.null(pkg_data$ReproduceInstall)) {
        return(as.character(pkg_data$ReproduceInstall)[1])
      } else {
        return(NULL)
      }
    })
    dep_cmds <- Filter(Negate(is.null), dep_cmds)
  }
  
  # Create script with header
  script_lines <- c(
    "#!/usr/bin/env Rscript",
    "# R Package Installation Script",
    paste("#", "Generated:", Sys.time()),
    paste("#", "R version:", paste(R.version$major, R.version$minor, sep = ".")),
    paste("#", "Command type:", command_type),
    paste("#", "Primary packages:", length(manual_cmds)),
    paste("#", "Dependency packages:", if(include_dependencies) length(dep_cmds) else "excluded"),
    "",
    "# Set library path",
    "lib_path <- 'R_libs'",
    "if (!dir.exists(lib_path)) dir.create(lib_path, recursive = TRUE)",
    ".libPaths(c(lib_path, .libPaths()))",
    ""
  )
  
  # Add primary packages section
  if (length(manual_cmds) > 0) {
    script_lines <- c(script_lines, 
                      "# Primary packages (explicitly installed)",
                      unlist(manual_cmds),
                      "")
  }
  
  # Add dependency packages section with separator comment
  if (include_dependencies && length(dep_cmds) > 0) {
    script_lines <- c(script_lines,
                      "# Dependency packages (auto-installed)",
                      "# These are dependencies that were automatically installed",
                      unlist(dep_cmds),
                      "")
  }
  
  script <- paste(script_lines, collapse = "\n")
  
  if (!is.null(output_file)) {
    tryCatch({
      writeLines(script_lines, output_file)
      Sys.chmod(output_file, mode = "0755")
      message(" Install script saved: ", output_file)
      message("   Command type: ", command_type)
      message("   Primary packages: ", length(manual_cmds))
      message("   Dependency packages: ", if(include_dependencies) length(dep_cmds) else "excluded")
    }, error = function(e) {
      stop("Failed to write install script: ", e$message)
    })
  }
  
  invisible(script)
}

#' Check R versions in library
#' 
#' @param lib_base Base library path (default: current directory)
#' @export
trackr$check_r_versions <- function(lib_base = ".") {
  cat("\n Checking for R library installations\n")
  cat(strrep("=", 50), "\n\n")
  
  # Check for R_libs directories
  r_lib_patterns <- c("R_libs", "renv", ".Rlibs")
  
  found_libs <- character(0)
  for (pattern in r_lib_patterns) {
    lib_path <- file.path(lib_base, pattern)
    if (dir.exists(lib_path)) {
      found_libs <- c(found_libs, lib_path)
    }
  }
  
  if (length(found_libs) == 0) {
    cat(" No R library directories found\n")
    cat("   Expected directories: R_libs/\n")
    return(invisible(NULL))
  }
  
  for (lib_path in found_libs) {
    pkg_dirs <- tryCatch({
      list.dirs(lib_path, full.names = FALSE, recursive = FALSE)
    }, error = function(e) {
      warning("Error reading ", lib_path, ": ", e$message)
      character(0)
    })
    pkg_count <- length(pkg_dirs)
    
    cat(sprintf(" %s: %d packages\n", basename(lib_path), pkg_count))
    cat(sprintf("   Path: %s\n", lib_path))
  }
  
  cat(sprintf("\nCurrent R: %s.%s\n", R.version$major, R.version$minor))
  cat(sprintf("Will analyze: %s\n", found_libs[1]))
  
  invisible(found_libs)
}

#' Generate container-ready install script
#' 
#' @param package_info Result from analyze_r_packages()
#' @param command_type Which command to use: "actual" (default) or "reproduce"
#' @param include_dependencies Include auto-installed dependencies
#' @param output_file Output file path
#' @param format Output format: "r_script" or "singularity_post"
#' @export
trackr$generate_container_install_script <- function(package_info = NULL,
                                                              command_type = c("actual", "reproduce"),
                                                              include_dependencies = FALSE,
                                                              output_file = "install_for_container.R",
                                                              format = c("r_script", "singularity_post")) {
  
  format <- match.arg(format)
  command_type <- match.arg(command_type)
  
  if (is.null(package_info)) {
    package_info <- trackr$analyze_r_packages()
  }
  
  # Get actual R commands (not tracking wrappers)
  get_actual_command <- function(pkg_data) {
    if (is.null(pkg_data)) return(NULL)
    
    if (command_type == "actual") {
      # Use ActualInstallCommand if available (from tracking)
      if (!is.null(pkg_data$ActualInstallCommand) && !is.na(pkg_data$ActualInstallCommand)) {
        return(as.character(pkg_data$ActualInstallCommand)[1])
      }
      # Otherwise use ReproduceInstall (version-pinned)
      if (!is.null(pkg_data$ReproduceInstall)) {
        return(as.character(pkg_data$ReproduceInstall)[1])
      }
    } else {  # "reproduce"
      # Use ReproduceInstall (version-pinned)
      if (!is.null(pkg_data$ReproduceInstall)) {
        return(as.character(pkg_data$ReproduceInstall)[1])
      }
      # Fall back to ActualInstallCommand
      if (!is.null(pkg_data$ActualInstallCommand) && !is.na(pkg_data$ActualInstallCommand)) {
        return(as.character(pkg_data$ActualInstallCommand)[1])
      }
    }
    return(NULL)
  }
  
  # Get commands for manually installed packages
  manual_cmds <- lapply(package_info$manually_installed, get_actual_command)
  manual_cmds <- Filter(Negate(is.null), manual_cmds)
  
  # Get commands for dependencies if requested
  dep_cmds <- character(0)
  if (include_dependencies) {
    dep_cmds <- lapply(package_info$dependencies, function(pkg_data) {
      if (!is.null(pkg_data$ReproduceInstall)) {
        return(as.character(pkg_data$ReproduceInstall)[1])
      }
      return(NULL)
    })
    dep_cmds <- Filter(Negate(is.null), dep_cmds)
  }
  
  all_cmds <- unique(c(manual_cmds, dep_cmds))
  
  if (format == "r_script") {
    # Regular R script format
    script_lines <- c(
      "#!/usr/bin/env Rscript",
      "# R Package Installation Script for Container",
      paste("#", "Generated:", Sys.time()),
      paste("#", "R version:", paste(R.version$major, R.version$minor, sep = ".")),
      paste("#", "Command type:", command_type),
      paste("#", "Primary packages:", length(manual_cmds)),
      paste("#", "Dependency packages:", if(include_dependencies) length(dep_cmds) else "excluded"),
      "",
      "# Note: This script uses actual R commands, not tracking wrappers",
      "# Suitable for use in Singularity container %post section",
      "",
      "cat('Installing packages...\\n')",
      ""
    )
    
    # Add primary packages section
    if (length(manual_cmds) > 0) {
      script_lines <- c(script_lines, 
                        "# Primary packages (explicitly installed)",
                        unlist(manual_cmds),
                        "")
    }
    
    # Add dependency packages section with separator comment
    if (include_dependencies && length(dep_cmds) > 0) {
      script_lines <- c(script_lines,
                        "# Dependency packages (auto-installed)",
                        "# These are dependencies that were automatically installed",
                        unlist(dep_cmds),
                        "")
    }
    
    script_lines <- c(script_lines, "cat('Installation complete!\\n')")
    
  } else {
    # Singularity %post format
    script_lines <- c(
      "# Add these lines to your Singularity definition %post section",
      paste("#", "Generated:", Sys.time()),
      paste("#", "Based on R version:", paste(R.version$major, R.version$minor, sep = ".")),
      paste("#", "Command type:", command_type),
      paste("#", "Primary packages:", length(manual_cmds)),
      paste("#", "Dependency packages:", if(include_dependencies) length(dep_cmds) else "excluded"),
      ""
    )
    
    # Add primary packages section
    if (length(manual_cmds) > 0) {
      script_lines <- c(script_lines, 
                        "# Primary packages (explicitly installed)",
                        sapply(manual_cmds, function(cmd) {
                          paste0('    R -e "', cmd, '"')
                        }),
                        "")
    }
    
    # Add dependency packages section with separator comment
    if (include_dependencies && length(dep_cmds) > 0) {
      script_lines <- c(script_lines,
                        "# Dependency packages (auto-installed)",
                        "# These are dependencies that were automatically installed",
                        sapply(dep_cmds, function(cmd) {
                          paste0('    R -e "', cmd, '"')
                        }))
    }
  }
  
  script <- paste(script_lines, collapse = "\n")
  
  if (!is.null(output_file)) {
    tryCatch({
      writeLines(script_lines, output_file)
      if (format == "r_script") {
        Sys.chmod(output_file, mode = "0755")
      }
      message(" Container install script saved: ", output_file)
      message("   Format: ", format)
      message("   Command type: ", command_type)
      message("   Primary packages: ", length(manual_cmds))
      message("   Dependency packages: ", if(include_dependencies) length(dep_cmds) else "excluded")
    }, error = function(e) {
      stop("Failed to write container script: ", e$message)
    })
  }
  
  invisible(script)
}

#' Complete reproducibility workflow
#' 
#' @export
trackr$generate_full_reproducibility <- function() {
  cat(" Generating complete R reproducibility package...\n\n")
  
  cat("1. Analyzing packages...\n")
  analysis <- tryCatch({
    trackr$analyze_r_packages()
  }, error = function(e) {
    stop("Failed to analyze packages: ", e$message)
  })
  
  cat("2. Generating reproducibility report...\n")
  report_file <- tryCatch({
    trackr$generate_reproducibility_report()
  }, error = function(e) {
    stop("Failed to generate report: ", e$message)
  })
  
  cat("3. Generating install script...\n")
  script_file <- tryCatch({
    trackr$generate_install_script(analysis, command_type = "actual")
  }, error = function(e) {
    stop("Failed to generate install script: ", e$message)
  })
  
  cat("4. Package summary:\n")
  trackr$print_package_summary(analysis)
  
  cat("\n Reproducibility package complete!\n")
  cat("Files created:\n")
  cat(sprintf("  - %s\n", report_file))
  cat(sprintf("  - %s\n", script_file))
  cat("\nTo reproduce this environment:\n")
  cat(sprintf("  Rscript %s\n", script_file))
  
  invisible(list(analysis = analysis, report_file = report_file, script_file = script_file))
}

#' Setup project with trackR
#' 
#' Creates r_tools/ directory and .Rprofile for package tracking.
#' Handles existing .Rprofile files interactively.
#' 
#' @param project_dir Project directory (default: current directory)
#' @param force If TRUE, overwrites existing .Rprofile without asking (default: FALSE)
#' @export
trackr$setup_project <- function(project_dir = getwd(), force = FALSE) {
  
  cat("\n trackR - Project Setup\n")
  cat(strrep("=", 50), "\n\n")
  
  # Standard .Rprofile template
  rprofile_template <- '# R startup script for Singularity container environment
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

  # Create R_libs if it doesn\'t exist
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
    
    cat(sprintf("trackR: %s | Library: %s | R %s (%s)\\n", 
                tracker_status, lib_path, r_version, env_type))
  }
})
'
  
  # Step 1: Create r_tools/ directory
  r_tools_dir <- file.path(project_dir, "r_tools")
  
  if (!dir.exists(r_tools_dir)) {
    tryCatch({
      dir.create(r_tools_dir, recursive = TRUE)
      cat("Created directory: r_tools/\n")
    }, error = function(e) {
      stop("Failed to create r_tools/ directory: ", e$message)
    })
  } else {
    cat("Directory already exists: r_tools/\n")
  }
  
  # Check if track.R is in r_tools/
  tracker_file <- file.path(r_tools_dir, "track.R")
  if (!file.exists(tracker_file)) {
    cat("\nNote: Copy track.R to r_tools/ directory\n")
  }
  
  # Step 2: Create R_libs/ directory
  r_libs_dir <- file.path(project_dir, "R_libs")
  
  if (!dir.exists(r_libs_dir)) {
    tryCatch({
      dir.create(r_libs_dir, recursive = TRUE)
      cat("Created directory: R_libs/\n")
    }, error = function(e) {
      stop("Failed to create R_libs/ directory: ", e$message)
    })
  } else {
    cat("Directory already exists: R_libs/\n")
  }
  
  # Step 3: Handle .Rprofile
  rprofile_path <- file.path(project_dir, ".Rprofile")
  
  if (file.exists(rprofile_path)) {
    cat("\nExisting .Rprofile found\n")
    
    if (!force && interactive()) {
      cat("\nOptions:\n")
      cat("  1. Skip (keep existing .Rprofile)\n")
      cat("  2. Backup existing and create new .Rprofile\n")
      cat("  3. Append tracking code to existing .Rprofile\n")
      cat("  4. Overwrite existing .Rprofile\n")
      
      choice <- readline(prompt = "\nEnter choice (1-4): ")
      choice <- as.integer(choice)
      
      if (is.na(choice) || choice < 1 || choice > 4) {
        cat("Invalid choice. Skipping .Rprofile setup\n")
        return(invisible(NULL))
      }
      
      if (choice == 1) {
        cat("Keeping existing .Rprofile\n")
        cat("\nSetup complete\n")
        cat("To use trackR, ensure .Rprofile sources r_tools/track.R\n")
        return(invisible(NULL))
      } else if (choice == 2) {
        # Backup existing
        backup_path <- paste0(rprofile_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
        tryCatch({
          file.copy(rprofile_path, backup_path)
          cat("Backed up existing .Rprofile to:", basename(backup_path), "\n")
        }, error = function(e) {
          stop("Failed to backup .Rprofile: ", e$message)
        })
        # Create new
        writeLines(rprofile_template, rprofile_path)
        cat("Created new .Rprofile\n")
      } else if (choice == 3) {
        # Append
        existing_content <- readLines(rprofile_path)
        new_content <- c(
          existing_content,
          "",
          "# trackR setup (appended by setup_project())",
          "if (file.exists('r_tools/track.R')) {",
          "  suppressMessages(source('r_tools/track.R'))",
          "}"
        )
        writeLines(new_content, rprofile_path)
        cat("Appended tracking code to existing .Rprofile\n")
      } else if (choice == 4) {
        # Overwrite
        writeLines(rprofile_template, rprofile_path)
        cat("Overwritten .Rprofile\n")
      }
    } else if (force) {
      # Force mode: backup and overwrite
      backup_path <- paste0(rprofile_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
      tryCatch({
        file.copy(rprofile_path, backup_path)
        cat("Backed up existing .Rprofile to:", basename(backup_path), "\n")
      }, error = function(e) {
        warning("Failed to backup .Rprofile: ", e$message)
      })
      writeLines(rprofile_template, rprofile_path)
      cat("Created new .Rprofile (forced)\n")
    } else {
      # Non-interactive and not force: skip
      cat("Existing .Rprofile found. Use force=TRUE to overwrite or run interactively for options\n")
      return(invisible(NULL))
    }
  } else {
    # No existing .Rprofile: create it
    tryCatch({
      writeLines(rprofile_template, rprofile_path)
      cat("Created .Rprofile\n")
    }, error = function(e) {
      stop("Failed to create .Rprofile: ", e$message)
    })
  }
  
  # Step 4: Summary
  cat("\nSetup complete\n")
  cat(strrep("=", 50), "\n")
  cat("Project structure:\n")
  cat("  .Rprofile           - Auto-loads trackR\n")
  cat("  r_tools/            - Module directory\n")
  cat("  R_libs/             - Project-local packages\n")
  cat("\nNext steps:\n")
  cat("  1. Restart R to load the new .Rprofile\n")
  cat("  2. Install packages with trackr$install_cran() etc.\n")
  cat("  3. Use trackr$help() for available functions\n")
  
  invisible(NULL)
}

#' Display help information for trackR functions
#' 
#' @export
trackr$help <- function() {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat(" TRACKR - HELP GUIDE\n")
  cat(strrep("=", 60), "\n\n")
  
  cat("INSTALLATION FUNCTIONS:\n")
  cat(strrep("-", 40), "\n")
  
  cat("trackr$install_tracked() - Install packages with tracking\n")
  cat("  Examples:\n")
  cat("    trackr$install_tracked(\"dplyr\", method = \"cran\")\n")
  cat("    trackr$install_tracked(c(\"ggplot2\", \"tidyr\"), method = \"cran\")\n")
  cat("    trackr$install_tracked(\"tidyverse/ggplot2\", method = \"github\")\n\n")
  
  cat("trackr$install_cran() - Install from CRAN\n")
  cat("  Examples:\n")
  cat("    trackr$install_cran(\"dplyr\")\n")
  cat("    trackr$install_cran(c(\"ggplot2\", \"readr\", \"stringr\"))\n\n")
  
  cat("trackr$install_github() - Install from GitHub\n")
  cat("  Examples:\n")
  cat("    trackr$install_github(\"tidyverse/dplyr\")\n")
  cat("    trackr$install_github(\"user/repo@branch\")\n\n")
  
  cat("trackr$install_bioc() - Install from Bioconductor\n")
  cat("  Examples:\n")
  cat("    trackr$install_bioc(\"DESeq2\")\n")
  cat("    trackr$install_bioc(c(\"limma\", \"edgeR\"))\n\n")
  
  cat("ANALYSIS & HISTORY FUNCTIONS:\n")
  cat(strrep("-", 40), "\n")
  
  cat("trackr$show_install_history() - View installation history\n")
  cat("  Examples:\n")
  cat("    trackr$show_install_history()  # Show all\n")
  cat("    trackr$show_install_history(recent = 5)  # Last 5 entries\n")
  cat("    trackr$show_install_history(method = \"github\")  # GitHub only\n\n")
  
  cat("trackr$analyze_r_packages() - Analyze installed packages\n")
  cat("  Examples:\n")
  cat("    analysis <- trackr$analyze_r_packages()\n")
  cat("    analysis$manually_installed  # Explicitly installed packages\n")
  cat("    analysis$dependencies        # Auto-installed dependencies\n\n")
  
  cat("trackr$print_package_summary() - Print analysis summary\n")
  cat("  Examples:\n")
  cat("    trackr$print_package_summary()\n")
  cat("    analysis <- trackr$analyze_r_packages()\n")
  cat("    trackr$print_package_summary(analysis)\n\n")
  
  cat("REPRODUCIBILITY FUNCTIONS:\n")
  cat(strrep("-", 40), "\n")
  
  cat("trackr$generate_install_script() - Create install script\n")
  cat("  Examples:\n")
  cat("    # Default: actual commands, no dependencies\n")
  cat("    trackr$generate_install_script()\n")
  cat("    # With dependencies and version-pinned commands\n")
  cat("    trackr$generate_install_script(command_type = \"reproduce\", include_dependencies = TRUE)\n")
  cat("    # Custom output file\n")
  cat("    trackr$generate_install_script(output_file = \"my_install_script.R\")\n\n")
  
  cat("trackr$generate_container_install_script() - Container-ready script\n")
  cat("  Examples:\n")
  cat("    # Regular R script format\n")
  cat("    trackr$generate_container_install_script()\n")
  cat("    # Singularity definition format\n")
  cat("    trackr$generate_container_install_script(format = \"singularity_post\")\n")
  cat("    # Include all dependencies\n")
  cat("    trackr$generate_container_install_script(include_dependencies = TRUE)\n\n")
  
  cat("trackr$generate_reproducibility_report() - Detailed JSON report\n")
  cat("  Examples:\n")
  cat("    trackr$generate_reproducibility_report()\n")
  cat("    trackr$generate_reproducibility_report(\"my_report.json\")\n\n")
  
  cat("trackr$generate_full_reproducibility() - Complete workflow\n")
  cat("  Examples:\n")
  cat("    result <- trackr$generate_full_reproducibility()\n")
  cat("    # Creates analysis, report, and install script in one call\n\n")
  
  cat("UTILITY FUNCTIONS:\n")
  cat(strrep("-", 40), "\n")
  
  cat("trackr$setup_project() - Initialize project structure\n")
  cat("  Examples:\n")
  cat("    trackr$setup_project()  # Interactive setup\n")
  cat("    trackr$setup_project(force = TRUE)  # Non-interactive\n\n")
  
  cat("trackr$check_r_versions() - Check library locations\n")
  cat("  Examples:\n")
  cat("    trackr$check_r_versions()\n")
  cat("    trackr$check_r_versions(\"../other_project\")\n\n")
  
  cat("WORKFLOW EXAMPLES:\n")
  cat(strrep("-", 40), "\n")
  cat("1. Track new installations:\n")
  cat("   trackr$install_cran(\"dplyr\")\n")
  cat("   trackr$install_github(\"tidyverse/ggplot2\")\n\n")
  
  cat("2. Analyze and reproduce environment:\n")
  cat("   trackr$analyze_r_packages()\n")
  cat("   trackr$generate_install_script(include_dependencies = TRUE)\n\n")
  
  cat("3. Full reproducibility package:\n")
  cat("   trackr$generate_full_reproducibility()\n")
  cat("   # Then run: Rscript install_r_packages.R\n\n")
  
  cat("KEY FEATURES:\n")
  cat(strrep("-", 40), "\n")
  cat("+ Tracks installations in .r_install_history.json\n")
  cat("+ Separates primary packages from dependencies\n")
  cat("+ Supports CRAN, GitHub, Bioconductor, GitLab, Bitbucket\n")
  cat("+ Generates reproducible installation scripts\n")
  cat("+ Creates container-ready installation commands\n")
  cat("+ Maintains project-local library in R_libs/\n\n")
  
  cat("For more details, see function documentation with ?function_name\n")
  cat(strrep("=", 60), "\n")
}

# Convenience wrapper functions (in namespace)
trackr$install_cran <- function(...) trackr$install_tracked(..., method = "cran")
trackr$install_bioc <- function(...) trackr$install_tracked(..., method = "bioc")
trackr$install_github <- function(...) trackr$install_tracked(..., method = "github")
trackr$install_gitlab <- function(...) trackr$install_tracked(..., method = "gitlab")
trackr$install_bitbucket <- function(...) trackr$install_tracked(..., method = "bitbucket")
trackr$install_url <- function(...) trackr$install_tracked(..., method = "url")

# Make trackr available globally (only this object)
assign("trackr", trackr, envir = .GlobalEnv)

# Ensure jsonlite is available on load (silently)
suppressMessages(trackr$ensure_jsonlite())

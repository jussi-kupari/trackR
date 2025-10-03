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
  
  cat("SINGULARITY CONTAINER FUNCTIONS:\n")
  cat(strrep("-", 40), "\n")
  
  cat("trackr$generate_singularity_def() - Generate Singularity definition\n")
  cat("  Examples:\n")
  cat("    # Generate definition only (default)\n")
  cat("    trackr$generate_singularity_def()\n")
  cat("    # Generate and build container\n")
  cat("    trackr$generate_singularity_def(build_container = TRUE)\n")
  cat("    # With dependencies and custom base image\n")
  cat("    trackr$generate_singularity_def(\n")
  cat("      base_image = \"docker://rocker/tidyverse:4.3.0\",\n")
  cat("      include_dependencies = TRUE\n")
  cat("    )\n\n")
  
  cat("trackr$build_singularity_container() - Build container from definition\n")
  cat("  Examples:\n")
  cat("    trackr$build_singularity_container()\n")
  cat("    trackr$build_singularity_container(\"Singularity.def\", \"my_project.sif\")\n\n")
  
  cat("trackr$compare_with_singularity() - Compare with definition\n")
  cat("  Examples:\n")
  cat("    trackr$compare_with_singularity()\n")
  cat("    trackr$compare_with_singularity(\"Singularity.def\")\n\n")
  
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
  
  cat("4. Iterative container development:\n")
  cat("   # Install packages during project\n")
  cat("   trackr$install_cran(\"dplyr\")\n")
  cat("   trackr$install_bioc(\"DESeq2\")\n")
  cat("   # Check what's new\n")
  cat("   trackr$compare_with_singularity()\n")
  cat("   # Regenerate definition with all packages\n")
  cat("   trackr$generate_singularity_def()\n")
  cat("   # Build container\n")
  cat("   trackr$build_singularity_container()\n\n")
  
  cat("KEY FEATURES:\n")
  cat(strrep("-", 40), "\n")
  cat("+ Tracks installations in .r_install_history.json\n")
  cat("+ Separates primary packages from dependencies\n")
  cat("+ Supports CRAN, GitHub, Bioconductor, GitLab, Bitbucket\n")
  cat("+ Generates reproducible installation scripts\n")
  cat("+ Creates container-ready installation commands\n")
  cat("+ Generates complete Singularity definitions\n")
  cat("+ Supports iterative container rebuilding\n")
  cat("+ Maintains project-local library in R_libs/\n\n")
  
  cat("For more details, see function documentation with ?function_name\n")
  cat(strrep("=", 60), "\n")
}

#' Generate Singularity definition file
#' 
#' Creates a complete Singularity definition file that can reproduce the
#' R environment. By default only generates the definition file.
#' 
#' @param output_file Output Singularity definition file path
#' @param base_image Base Docker image (default: rocker/r-ver with current R version)
#' @param include_dependencies Include auto-installed dependencies
#' @param system_deps Additional system dependencies to install
#' @param build_container If TRUE, also builds the container after generating definition
#' @param container_name Output container name (only used if build_container=TRUE)
#' @param builder_script Path to create_container.sh script
#' @export
trackr$generate_singularity_def <- function(
    output_file = "Singularity.def",
    base_image = NULL,
    include_dependencies = FALSE,
    system_deps = NULL,
    build_container = FALSE,
    container_name = NULL,
    builder_script = NULL
) {
  
  # Default base image uses current R version
  if (is.null(base_image)) {
    r_version <- paste(R.version$major, R.version$minor, sep = ".")
    base_image <- sprintf("docker://rocker/r-ver:%s", r_version)
  }
  
  # Analyze current packages
  message("Analyzing R packages...")
  analysis <- trackr$analyze_r_packages()
  
  if (length(analysis$manually_installed) == 0) {
    stop("No manually installed packages found. Install packages first with trackr$install_* functions.")
  }
  
  # Build definition file
  def_lines <- c(
    "Bootstrap: docker",
    paste0("From: ", base_image),
    "",
    "%post",
    "    # Update system and install build dependencies",
    "    apt-get update && apt-get install -y --no-install-recommends \\",
    "        build-essential \\",
    "        cmake \\",
    "        libcurl4-openssl-dev \\",
    "        libssl-dev \\",
    "        libxml2-dev \\",
    "        libgit2-dev \\",
    "        libcairo2-dev \\",
    "        libxt-dev \\",
    "        pkg-config \\",
    "        gfortran \\",
    "        libblas-dev \\",
    "        liblapack-dev \\",
    "        libffi-dev \\",
    "        zlib1g-dev \\",
    "        libbz2-dev \\",
    "        liblzma-dev"
  )
  
  # Add custom system dependencies if provided
  if (!is.null(system_deps) && length(system_deps) > 0) {
    for (dep in system_deps) {
      def_lines <- c(def_lines, paste0("        ", dep, " \\"))
    }
  }
  
  # Close apt-get line
  last_line <- def_lines[length(def_lines)]
  def_lines[length(def_lines)] <- sub(" \\\\$", "", last_line)
  
  def_lines <- c(
    def_lines,
    "",
    "    # Clean up apt cache",
    "    apt-get clean",
    "    rm -rf /var/lib/apt/lists/*",
    "",
    "    # Install R packages",
    "    # Manually installed packages"
  )
  
  # Helper function to safely extract single string
  safe_extract <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (is.list(x)) x <- unlist(x)
    if (length(x) > 1) return(paste(x, collapse = " "))
    as.character(x)[1]
  }
  
  # Add manually installed packages
  pkg_count <- 0
  for (pkg_name in sort(names(analysis$manually_installed))) {
    pkg_info <- analysis$manually_installed[[pkg_name]]
    
    # Get the reproducible install command
    cmd <- safe_extract(pkg_info$ReproduceInstall)
    
    if (!is.na(cmd) && nchar(cmd) > 0) {
      # Add comment with package info
      version <- safe_extract(pkg_info$Version)
      method <- safe_extract(pkg_info$InstallMethod)
      
      def_lines <- c(
        def_lines,
        sprintf('    # %s (%s) [%s]', pkg_name, 
                if (!is.na(version)) version else "unknown",
                if (!is.na(method)) method else "unknown"),
        sprintf('    R -e "%s"', cmd)
      )
      pkg_count <- pkg_count + 1
    }
  }
  
  # Add dependencies if requested
  dep_count <- 0
  if (include_dependencies && length(analysis$dependencies) > 0) {
    def_lines <- c(
      def_lines,
      "",
      "    # Auto-installed dependencies",
      "    # These were automatically installed as dependencies"
    )
    
    for (pkg_name in sort(names(analysis$dependencies))) {
      pkg_info <- analysis$dependencies[[pkg_name]]
      cmd <- safe_extract(pkg_info$ReproduceInstall)
      
      if (!is.na(cmd) && nchar(cmd) > 0) {
        version <- safe_extract(pkg_info$Version)
        def_lines <- c(
          def_lines,
          sprintf('    # %s (%s) [dependency]', pkg_name,
                  if (!is.na(version)) version else "unknown"),
          sprintf('    R -e "%s"', cmd)
        )
        dep_count <- dep_count + 1
      }
    }
  }
  
  # Add environment section
  def_lines <- c(
    def_lines,
    "",
    "%environment",
    "    export LC_ALL=C.UTF-8",
    "    export LANG=C.UTF-8",
    "",
    "%runscript",
    '    R "$@"',
    "",
    "%labels",
    "    Generated_by trackR",
    sprintf("    Generated_at %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("    R_version %s", paste(R.version$major, R.version$minor, sep = ".")),
    sprintf("    Base_image %s", base_image),
    sprintf("    Packages %d", pkg_count)
  )
  
  if (include_dependencies) {
    def_lines <- c(def_lines, sprintf("    Dependencies %d", dep_count))
  }
  
  # Write definition file
  tryCatch({
    writeLines(def_lines, output_file)
    
    message(sprintf("\n Singularity definition saved: %s", output_file))
    message(sprintf("   Base image: %s", base_image))
    message(sprintf("   R version: %s", paste(R.version$major, R.version$minor, sep = ".")))
    message(sprintf("   Manually installed packages: %d", pkg_count))
    if (include_dependencies) {
      message(sprintf("   Dependencies included: %d", dep_count))
    }
    
    # Optionally build container
    if (build_container) {
      message("\nBuilding container...")
      trackr$build_singularity_container(
        def_file = output_file,
        container_name = container_name,
        builder_script = builder_script
      )
    } else {
      message("\nTo build container:")
      message(sprintf("   trackr$build_singularity_container(\"%s\")", output_file))
      message("\nOr manually:")
      container_sif <- if (!is.null(container_name)) container_name else sub("\\.def$", ".sif", output_file)
      message(sprintf("   singularity build --fakeroot %s %s", container_sif, output_file))
    }
    
  }, error = function(e) {
    stop("Failed to write Singularity definition: ", e$message)
  })
  
  invisible(output_file)
}

#' Build Singularity container from definition
#' 
#' @param def_file Singularity definition file
#' @param container_name Output container name (default: derived from def_file)
#' @param builder_script Path to create_container.sh script
#' @export
trackr$build_singularity_container <- function(
    def_file = "Singularity.def",
    container_name = NULL,
    builder_script = NULL
) {
  
  if (!file.exists(def_file)) {
    stop("Singularity definition file not found: ", def_file)
  }
  
  # Look for create_container.sh in common locations
  if (is.null(builder_script)) {
    possible_locations <- c(
      "create_container.sh",
      "r_tools/create_container.sh",
      "~/bin/create_container.sh",
      file.path(dirname(def_file), "create_container.sh")
    )
    
    for (loc in possible_locations) {
      expanded <- path.expand(loc)
      if (file.exists(expanded)) {
        builder_script <- expanded
        break
      }
    }
  }
  
  # Build the container
  if (!is.null(builder_script) && file.exists(builder_script)) {
    message(sprintf("Using builder script: %s", builder_script))
    
    if (is.null(container_name)) {
      cmd <- sprintf("bash %s %s", shQuote(builder_script), shQuote(def_file))
    } else {
      cmd <- sprintf("bash %s %s %s", shQuote(builder_script), shQuote(def_file), shQuote(container_name))
    }
    
    message(sprintf("Running: %s", cmd))
    result <- system(cmd)
    
  } else {
    # Fallback to direct singularity command
    message("Builder script not found, using direct singularity command")
    
    if (is.null(container_name)) {
      basename <- sub("\\.def$", "", basename(def_file))
      container_name <- paste0(basename, ".sif")
    }
    
    cmd <- sprintf("singularity build --fakeroot %s %s", 
                   shQuote(container_name), shQuote(def_file))
    
    message(sprintf("Running: %s", cmd))
    result <- system(cmd)
  }
  
  if (result == 0) {
    message("\n Container build completed successfully")
    if (!is.null(container_name) && file.exists(container_name)) {
      message(sprintf("   Container: %s", container_name))
      message(sprintf("   Size: %.1f MB", file.size(container_name) / 1024^2))
    }
  } else {
    stop("Container build failed. Check singularity_build.out for details.")
  }
  
  invisible(result)
}

#' Compare current packages with Singularity definition
#' 
#' Shows which packages are in the current environment but not in the
#' Singularity definition file (useful before rebuilding)
#' 
#' @param def_file Singularity definition file path
#' @export
trackr$compare_with_singularity <- function(def_file = "Singularity.def") {
  
  if (!file.exists(def_file)) {
    stop("Singularity definition file not found: ", def_file)
  }
  
  # Read definition file
  def_lines <- readLines(def_file)
  
  # Extract package names from R -e commands
  def_packages <- character(0)
  
  for (line in def_lines) {
    if (grepl('R -e', line)) {
      # Try to extract package name
      if (grepl('install_version', line)) {
        match <- regmatches(line, regexec('install_version\\("([^"]+)"', line))
        if (length(match[[1]]) > 1) {
          def_packages <- c(def_packages, match[[1]][2])
        }
      } else if (grepl('install_github|install_gitlab|install_bitbucket', line)) {
        match <- regmatches(line, regexec('install_[^(]+\\("([^/]+)/([^"@]+)', line))
        if (length(match[[1]]) > 2) {
          def_packages <- c(def_packages, match[[1]][3])
        }
      } else if (grepl('install\\.packages|BiocManager::install', line)) {
        match <- regmatches(line, regexec('install[^(]*\\("([^"]+)"', line))
        if (length(match[[1]]) > 1) {
          def_packages <- c(def_packages, match[[1]][2])
        }
      }
    }
  }
  
  def_packages <- unique(def_packages)
  
  # Get current packages
  analysis <- trackr$analyze_r_packages()
  current_packages <- names(analysis$manually_installed)
  
  # Compare
  new_packages <- setdiff(current_packages, def_packages)
  removed_packages <- setdiff(def_packages, current_packages)
  
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat(" Package Comparison: Current vs Singularity Definition\n")
  cat(strrep("=", 60), "\n")
  cat(sprintf("Definition file: %s\n", def_file))
  cat(sprintf("Packages in definition: %d\n", length(def_packages)))
  cat(sprintf("Packages currently installed: %d\n", length(current_packages)))
  
  if (length(new_packages) > 0) {
    cat("\n")
    cat("NEW packages (not in definition):\n")
    cat(sprintf("  %s\n", paste(new_packages, collapse = ", ")))
    cat("\n")
    cat("⚠ Run trackr$generate_singularity_def() to update definition\n")
  }
  
  if (length(removed_packages) > 0) {
    cat("\n")
    cat("REMOVED packages (in definition but not currently installed):\n")
    cat(sprintf("  %s\n", paste(removed_packages, collapse = ", ")))
  }
  
  if (length(new_packages) == 0 && length(removed_packages) == 0) {
    cat("\n")
    cat("✓ Definition is up to date with current environment\n")
  }
  
  cat(strrep("=", 60), "\n")
  
  invisible(list(
    new = new_packages,
    removed = removed_packages,
    in_def = def_packages,
    current = current_packages
  ))
}

#' Generate installation script from renv.lock file
#' 
#' Parses an renv.lock file and generates an R installation script
#' that can reproduce the environment.
#' 
#' @param lock_file Path to renv.lock file
#' @param output_file Output R script path
#' @param pin_versions If TRUE, install exact versions from lock file; if FALSE, install latest versions (default: FALSE)
#' @param include_bioconductor Include Bioconductor packages
#' @param include_github Include GitHub packages
#' @export
trackr$generate_script_from_renv_lock <- function(
    lock_file = "renv.lock",
    output_file = "install_from_renv.R",
    pin_versions = FALSE,
    include_bioconductor = TRUE,
    include_github = TRUE
) {
  
  # Ensure jsonlite is available
  if (!trackr$ensure_jsonlite()) {
    stop("Cannot parse renv.lock: jsonlite not available")
  }
  
  if (!file.exists(lock_file)) {
    stop("renv.lock file not found: ", lock_file)
  }
  
  message("Parsing renv.lock file...")
  
  # Read and parse the lock file
  lock_data <- tryCatch({
    jsonlite::fromJSON(lock_file, simplifyVector = FALSE)
  }, error = function(e) {
    stop("Failed to parse renv.lock: ", e$message)
  })
  
  # Extract R version info
  r_version <- if (!is.null(lock_data$R$Version)) {
    lock_data$R$Version
  } else {
    paste(R.version$major, R.version$minor, sep = ".")
  }
  
  # Extract Bioconductor version if available
  bioc_version <- if (!is.null(lock_data$Bioconductor$Version)) {
    lock_data$Bioconductor$Version
  } else {
    NULL
  }
  
  # Process packages
  packages <- lock_data$Packages
  
  if (is.null(packages) || length(packages) == 0) {
    stop("No packages found in renv.lock")
  }
  
  # Categorize packages
  cran_packages <- list()
  bioc_packages <- list()
  github_packages <- list()
  other_packages <- list()
  
  for (pkg_name in names(packages)) {
    pkg <- packages[[pkg_name]]
    
    # Skip renv itself
    if (pkg_name == "renv") next
    
    # Determine source
    source <- if (!is.null(pkg$Source)) pkg$Source else "Repository"
    
    if (source == "GitHub") {
      if (include_github) {
        github_packages[[pkg_name]] <- pkg
      }
    } else if (source == "Bioconductor") {
      if (include_bioconductor) {
        bioc_packages[[pkg_name]] <- pkg
      }
    } else if (source == "Repository" && !is.null(pkg$Repository)) {
      if (pkg$Repository == "CRAN") {
        cran_packages[[pkg_name]] <- pkg
      } else if (grepl("BioC", pkg$Repository)) {
        if (include_bioconductor) {
          bioc_packages[[pkg_name]] <- pkg
        }
      } else {
        other_packages[[pkg_name]] <- pkg
      }
    } else {
      other_packages[[pkg_name]] <- pkg
    }
  }
  
  # Generate script
  script_lines <- c(
    "#!/usr/bin/env Rscript",
    "# R Package Installation Script",
    "# Generated from renv.lock",
    paste("#", "Generated:", Sys.time()),
    paste("#", "Source lock file:", lock_file),
    paste("#", "R version (from lock):", r_version),
    if (!is.null(bioc_version)) paste("#", "Bioconductor version:", bioc_version) else NULL,
    paste("#", "Version pinning:", if (pin_versions) "exact versions" else "latest versions"),
    paste("#", "Total packages:", length(cran_packages) + length(bioc_packages) + length(github_packages)),
    "",
    "# Set library path",
    "lib_path <- 'R_libs'",
    "if (!dir.exists(lib_path)) dir.create(lib_path, recursive = TRUE)",
    ".libPaths(c(lib_path, .libPaths()))",
    "",
    "cat('Installing packages from renv.lock...\\n')",
    if (pin_versions) "cat('Using exact versions from lock file\\n')" else "cat('Using latest available versions\\n')",
    ""
  )
  
  # Add CRAN packages
  if (length(cran_packages) > 0) {
    script_lines <- c(
      script_lines,
      "# CRAN packages",
      paste("#", length(cran_packages), "packages from CRAN"),
      ""
    )
    
    if (pin_versions) {
      # Install exact versions using remotes::install_version
      for (pkg_name in sort(names(cran_packages))) {
        pkg <- cran_packages[[pkg_name]]
        version <- if (!is.null(pkg$Version)) pkg$Version else NULL
        
        if (!is.null(version)) {
          script_lines <- c(
            script_lines,
            sprintf('# %s (%s)', pkg_name, version),
            sprintf('remotes::install_version("%s", version = "%s", lib = lib_path)', 
                    pkg_name, version)
          )
        } else {
          script_lines <- c(
            script_lines,
            sprintf('# %s (no version info)', pkg_name),
            sprintf('install.packages("%s", lib = lib_path)', pkg_name)
          )
        }
      }
    } else {
      # Install latest versions using install.packages
      for (pkg_name in sort(names(cran_packages))) {
        pkg <- cran_packages[[pkg_name]]
        version <- if (!is.null(pkg$Version)) pkg$Version else NULL
        
        script_lines <- c(
          script_lines,
          sprintf('# %s%s', pkg_name, if (!is.null(version)) paste0(" (lock had ", version, ")") else ""),
          sprintf('install.packages("%s", lib = lib_path)', pkg_name)
        )
      }
    }
    script_lines <- c(script_lines, "")
  }
  
  # Add Bioconductor packages
  if (length(bioc_packages) > 0) {
    script_lines <- c(
      script_lines,
      "# Bioconductor packages",
      paste("#", length(bioc_packages), "packages from Bioconductor"),
      "",
      "# Install BiocManager if needed",
      "if (!requireNamespace('BiocManager', quietly = TRUE)) {",
      "  install.packages('BiocManager', lib = lib_path)",
      "}",
      ""
    )
    
    if (!is.null(bioc_version)) {
      script_lines <- c(
        script_lines,
        sprintf("# Set Bioconductor version to %s", bioc_version),
        sprintf('BiocManager::install(version = "%s", lib = lib_path, update = FALSE, ask = FALSE)', 
                bioc_version),
        ""
      )
    }
    
    for (pkg_name in sort(names(bioc_packages))) {
      pkg <- bioc_packages[[pkg_name]]
      version <- if (!is.null(pkg$Version)) pkg$Version else NULL
      
      if (pin_versions && !is.null(version)) {
        script_lines <- c(
          script_lines,
          sprintf('# %s (%s)', pkg_name, version),
          sprintf('BiocManager::install("%s", lib = lib_path, update = FALSE, ask = FALSE)', 
                  pkg_name)
        )
      } else {
        script_lines <- c(
          script_lines,
          sprintf('# %s%s', pkg_name, if (!is.null(version) && !pin_versions) paste0(" (lock had ", version, ")") else ""),
          sprintf('BiocManager::install("%s", lib = lib_path, update = FALSE, ask = FALSE)', 
                  pkg_name)
        )
      }
    }
    script_lines <- c(script_lines, "")
  }
  
  # Add GitHub packages
  if (length(github_packages) > 0) {
    script_lines <- c(
      script_lines,
      "# GitHub packages",
      paste("#", length(github_packages), "packages from GitHub"),
      "",
      "# Install remotes if needed",
      "if (!requireNamespace('remotes', quietly = TRUE)) {",
      "  install.packages('remotes', lib = lib_path)",
      "}",
      ""
    )
    
    for (pkg_name in sort(names(github_packages))) {
      pkg <- github_packages[[pkg_name]]
      
      # Construct GitHub reference
      remote_user <- if (!is.null(pkg$RemoteUsername)) pkg$RemoteUsername else NULL
      remote_repo <- if (!is.null(pkg$RemoteRepo)) pkg$RemoteRepo else NULL
      remote_sha <- if (!is.null(pkg$RemoteSha)) pkg$RemoteSha else NULL
      remote_ref <- if (!is.null(pkg$RemoteRef)) pkg$RemoteRef else NULL
      
      if (!is.null(remote_user) && !is.null(remote_repo)) {
        # Use SHA if available for exact reproducibility, otherwise use ref
        if (!is.null(remote_sha)) {
          github_ref <- paste0(remote_user, "/", remote_repo, "@", remote_sha)
        } else if (!is.null(remote_ref)) {
          github_ref <- paste0(remote_user, "/", remote_repo, "@", remote_ref)
        } else {
          github_ref <- paste0(remote_user, "/", remote_repo)
        }
        
        script_lines <- c(
          script_lines,
          sprintf('# %s', pkg_name),
          sprintf('remotes::install_github("%s", lib = lib_path)', github_ref)
        )
      } else {
        script_lines <- c(
          script_lines,
          sprintf('# %s - Warning: incomplete GitHub info', pkg_name)
        )
      }
    }
    script_lines <- c(script_lines, "")
  }
  
  # Add note about other packages
  if (length(other_packages) > 0) {
    script_lines <- c(
      script_lines,
      sprintf("# Note: %d packages from other sources were not included", 
              length(other_packages)),
      sprintf("# Other packages: %s", paste(names(other_packages), collapse = ", ")),
      ""
    )
  }
  
  script_lines <- c(script_lines, "cat('Installation complete!\\n')")
  
  # Write script
  tryCatch({
    writeLines(script_lines, output_file)
    Sys.chmod(output_file, mode = "0755")
    
    message(sprintf("\n ✓ Installation script saved: %s", output_file))
    message(sprintf("   R version (from lock): %s", r_version))
    message(sprintf("   Version pinning: %s", if (pin_versions) "exact versions" else "latest versions"))
    if (!is.null(bioc_version)) {
      message(sprintf("   Bioconductor version: %s", bioc_version))
    }
    message(sprintf("   CRAN packages: %d", length(cran_packages)))
    if (include_bioconductor) {
      message(sprintf("   Bioconductor packages: %d", length(bioc_packages)))
    }
    if (include_github) {
      message(sprintf("   GitHub packages: %d", length(github_packages)))
    }
    if (length(other_packages) > 0) {
      message(sprintf("   Other packages (not included): %d", length(other_packages)))
    }
    message("\nTo install packages:")
    message(sprintf("   Rscript %s", output_file))
    
  }, error = function(e) {
    stop("Failed to write install script: ", e$message)
  })
  
  invisible(list(
    cran = names(cran_packages),
    bioconductor = names(bioc_packages),
    github = names(github_packages),
    other = names(other_packages)
  ))
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

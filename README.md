# trackR

A lightweight module for tracking R package installations in project-specific environments. Maintains installation history, separates primary packages from dependencies, and generates reproducible installation scripts.

## Features

- Tracks all package installations with timestamps and methods
- Maintains project-local library in `R_libs/`
- Distinguishes between explicitly installed packages and auto-installed dependencies
- Supports multiple installation sources: CRAN, Bioconductor, GitHub, GitLab, Bitbucket
- Generates reproducible installation scripts
- Creates container-ready installation commands for Singularity/Docker
- Exports detailed JSON reports for environment reproducibility

## Installation

### Quick Setup

```r
# If you already have trackr.R sourced
trackr$setup_project()
```

This interactive setup will:
1. Create `r_tools/` directory
2. Create `R_libs/` directory
3. Create or update `.Rprofile` (with options for existing files)
4. Guide you through next steps

### Manual Setup

1. Create `r_tools/` directory in your project root
2. Place `trackr.R` in `r_tools/`
3. Add `.Rprofile` to project root to auto-load on startup

The module will automatically install `jsonlite` to your project library if not available.

## Basic Usage

### Installing Packages

```r
# CRAN packages
trackr$install_cran("dplyr")
trackr$install_cran(c("ggplot2", "tidyr", "readr"))

# Bioconductor packages
trackr$install_bioc("DESeq2")

# GitHub packages
trackr$install_github("tidyverse/dplyr")
trackr$install_github("username/repo@branch")

# GitLab and Bitbucket
trackr$install_gitlab("username/repo")
trackr$install_bitbucket("username/repo")
```

### Viewing Installation History

```r
# Show all installations
trackr$show_install_history()

# Show last 5 installations
trackr$show_install_history(recent = 5)

# Filter by method
trackr$show_install_history(method = "github")
```

### Analyzing Packages

```r
# Analyze installed packages
analysis <- trackr$analyze_r_packages()

# View manually installed packages
analysis$manually_installed

# View auto-installed dependencies
analysis$dependencies

# Print summary
trackr$print_package_summary()
```

## Reproducibility

### Generate Installation Script

```r
# Default: actual commands used, primary packages only
trackr$generate_install_script()

# Include dependencies
trackr$generate_install_script(
  command_type = "actual",
  include_dependencies = TRUE
)

# Version-pinned commands
trackr$generate_install_script(
  command_type = "reproduce",
  output_file = "install_packages.R"
)
```

Command types:
- `actual`: Commands that were actually used during installation
- `reproduce`: Version-pinned commands for exact reproduction
- `original`: Original commands from .Rhistory (if available)

### Container Installation Scripts

```r
# R script format (executable)
trackr$generate_container_install_script()

# Singularity definition format
trackr$generate_container_install_script(
  format = "singularity_post",
  output_file = "singularity_packages.txt"
)
```

### Full Reproducibility Package

```r
# Generates analysis, report, and install script
trackr$generate_full_reproducibility()
```

Creates:
- `r_reproducibility_report_TIMESTAMP.json` - Detailed package information
- `install_r_packages.R` - Executable installation script

## Project Structure

```
project/
├── .Rprofile                       # Auto-loads tracker
├── .r_install_history.json         # Installation log (JSON)
├── .r_install_history.txt          # Installation log (human-readable)
├── R_libs/                         # Project-local packages
├── r_tools/
│   └── trackr.R           # This module
└── install_r_packages.R            # Generated script
```

## Log Files

### .r_install_history.json
Complete installation history with metadata, searchable and machine-readable.

### .r_install_history.txt
Human-readable log with timestamps, commands, and status indicators.

## Environment Detection

The `.Rprofile` detects whether R is running natively or in a container and adjusts paths accordingly:
- Native: Uses `getwd()/R_libs`
- Container: Uses `/project/R_libs`

## Help

```r
# View all available functions with examples
trackr$help()

# Get started with a new project
trackr$setup_project()
```

## Advanced Usage

### Handling Existing .Rprofile

If you have an existing `.Rprofile` with custom settings, `setup_project()` offers options:
- Skip setup and keep your existing file
- Backup existing and create new (preserves your original)
- Append tracking code to your existing file
- Overwrite (creates backup first)

For non-interactive/scripted setup:
```r
trackr$setup_project(force = TRUE)  # Backs up and overwrites
```

## Use Cases

### Setting Up a New Project
```r
# First time: setup the project structure
trackr$setup_project()

# Then restart R and start installing
trackr$install_cran("tidyverse")
trackr$install_github("user/package")
```

### Reproducing an Environment
```r
# Analyze existing installation
trackr$analyze_r_packages()

# Generate installation script
trackr$generate_install_script(include_dependencies = TRUE)

# Run on another machine
Rscript install_r_packages.R
```

### Creating Container Definitions
```r
# Generate commands for Singularity %post section
trackr$generate_container_install_script(
  format = "singularity_post",
  include_dependencies = FALSE
)
```

### Auditing Package Sources
```r
analysis <- trackr$analyze_r_packages()

# Check which packages came from GitHub
lapply(analysis$manually_installed, function(pkg) {
  if (!is.null(pkg$RemoteType) && pkg$RemoteType == "github") {
    list(name = pkg$Package, repo = pkg$RemoteRepo)
  }
})
```

## Notes

- Installation history only tracks packages installed through the tracker functions
- Dependencies are identified by absence of installation history
- The module uses `.Rhistory` as a fallback for identifying manually installed packages
- All tracking data is stored in JSON format for portability
- The module handles vector operations safely and includes comprehensive error handling

## Version

2.3 - Improved error handling, vector safety checks, automatic jsonlite installation

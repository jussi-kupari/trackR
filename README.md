# trackR

A lightweight module for tracking R package installations in project-specific environments. Maintains installation history, separates primary packages from dependencies, and generates reproducible installation scripts and Singularity container definitions.

## Features

- Tracks all package installations with timestamps and methods
- Maintains project-local library in R_libs/
- Distinguishes between explicitly installed packages and auto-installed dependencies
- Supports multiple installation sources: CRAN, Bioconductor, GitHub, GitLab, Bitbucket
- Generates reproducible installation scripts
- Creates container-ready installation commands for Singularity/Docker
- **Generates complete Singularity definition files**
- **Supports iterative container rebuilding during projects**
- Exports detailed JSON reports for environment reproducibility

## Installation

### Quick Setup

```r
# First source the trackR module
source("r_tools/track.R")

# Then proceed with the project setup
trackr$setup_project()
```

This interactive setup will:
- Create r_tools/ directory
- Create R_libs/ directory
- Create or update .Rprofile (with options for existing files)
- Guide you through next steps

### Manual Setup

1. Create r_tools/ directory in your project root
2. Place track.R in r_tools/
3. Add .Rprofile to project root to auto-load on startup

The module will automatically install jsonlite to your project library if not available.

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
- **actual**: Commands that were actually used during installation
- **reproduce**: Version-pinned commands for exact reproduction
- **original**: Original commands from .Rhistory (if available)

### Generate Script from renv.lock

Convert an renv.lock file to a trackR-compatible installation script:

```r
# Generate from renv.lock
trackr$generate_script_from_renv_lock()

# With custom options
trackr$generate_script_from_renv_lock(
  lock_file = "renv.lock",
  output_file = "install_from_renv.R",
  include_bioconductor = TRUE,
  include_github = TRUE
)

# Exclude GitHub packages (CRAN + Bioconductor only)
trackr$generate_script_from_renv_lock(
  include_github = FALSE
)
```

This function:
- Parses renv.lock JSON format
- Extracts package versions and sources
- Generates version-pinned installation commands
- Preserves GitHub commit SHAs for reproducibility
- Sets Bioconductor version if specified

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

## Singularity Container Support

### Generate Singularity Definition

```r
# Generate definition file only (default)
trackr$generate_singularity_def()

# Generate and build container in one step
trackr$generate_singularity_def(build_container = TRUE)

# With custom options
trackr$generate_singularity_def(
  output_file = "Singularity.def",
  base_image = "docker://rocker/tidyverse:4.3.0",
  include_dependencies = TRUE
)
```

### Build Container from Definition

```r
# Build using default settings
trackr$build_singularity_container()

# With custom names
trackr$build_singularity_container(
  def_file = "Singularity.def",
  container_name = "my_project.sif"
)
```

The builder automatically searches for `create_container.sh` in common locations and uses it if available, otherwise falls back to direct `singularity build` command.

### Compare with Existing Definition

```r
# Check what's changed since last definition
trackr$compare_with_singularity()

# Shows:
# - NEW packages (not in definition)
# - REMOVED packages (in definition but not installed)
```

### Iterative Container Development Workflow

```r
# 1. Work on project, install packages as needed
trackr$install_cran("dplyr")
trackr$install_bioc("DESeq2")
trackr$install_github("user/package")

# 2. Check what's new since last container build
trackr$compare_with_singularity()

# 3. Regenerate definition with ALL packages (cumulative)
trackr$generate_singularity_def()

# 4. Build updated container
trackr$build_singularity_container()

# Repeat as needed throughout project
```

**Key benefits:**
- Definition file always contains complete environment (not incremental patches)
- Each rebuild starts from clean base image with all tracked packages
- No accumulation of errors from incremental updates
- Easy to share and version control

## Project Structure

```
project/
├── .Rprofile                       # Auto-loads tracker
├── .r_install_history.json         # Installation log (JSON)
├── .r_install_history.txt          # Installation log (human-readable)
├── R_libs/                         # Project-local packages
├── r_tools/
│   └── track.R                     # This module
├── Singularity.def                 # Generated container definition
├── install_r_packages.R            # Generated install script
└── create_container.sh             # Optional: container builder script
```

## Log Files

### .r_install_history.json
Complete installation history with metadata, searchable and machine-readable.

### .r_install_history.txt
Human-readable log with timestamps, commands, and status indicators.

## Environment Detection

The .Rprofile detects whether R is running natively or in a container and adjusts paths accordingly:
- **Native**: Uses `getwd()/R_libs`
- **Container**: Uses `/project/R_libs`

## Help

```r
# View all available functions with examples
trackr$help()

# Get started with a new project
trackr$setup_project()
```

## Advanced Usage

### Handling Existing .Rprofile

If you have an existing .Rprofile with custom settings, `setup_project()` offers options:
1. Skip setup and keep your existing file
2. Backup existing and create new (preserves your original)
3. Append tracking code to your existing file
4. Overwrite (creates backup first)

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
# Method 1: Generate complete Singularity definition
trackr$generate_singularity_def()

# Method 2: Generate %post section only
trackr$generate_container_install_script(
  format = "singularity_post",
  include_dependencies = FALSE
)

# Method 3: From renv.lock file
trackr$generate_script_from_renv_lock("renv.lock")
# Then manually add to Singularity definition
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

### Multi-Round Container Building

```r
# Round 1: Initial development
trackr$install_cran(c("dplyr", "ggplot2"))
trackr$generate_singularity_def()
trackr$build_singularity_container()

# ... continue working in container ...

# Round 2: Add more packages
trackr$install_bioc("DESeq2")
trackr$install_github("user/package")

# Check what's new
trackr$compare_with_singularity()

# Regenerate definition (includes ALL packages, old + new)
trackr$generate_singularity_def()

# Rebuild container from scratch with complete environment
trackr$build_singularity_container()
```

### Converting from renv to trackR

```r
# If you have an existing renv.lock file
trackr$generate_script_from_renv_lock("renv.lock", "install_packages.R")

# Run the generated script to install to R_libs/
Rscript install_packages.R

# Now packages are in trackR's tracking system
trackr$analyze_r_packages()
```

## Notes

- Installation history only tracks packages installed through the tracker functions
- Dependencies are identified by absence of installation history
- The module uses .Rhistory as a fallback for identifying manually installed packages
- All tracking data is stored in JSON format for portability
- The module handles vector operations safely and includes comprehensive error handling
- Singularity definitions are cumulative - each generation includes all tracked packages
- Container rebuilds start from clean base image to avoid accumulation errors
- renv.lock files can be converted to trackR install scripts for integration with trackR workflow

## Version

2.3 - Improved error handling, vector safety checks, automatic jsonlite installation, Singularity definition generation and iterative container support, renv.lock import functionality

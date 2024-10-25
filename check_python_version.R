# First function remains largely unchanged - just lists all installations
check_python_installations <- function() {
  # Platform-specific settings
  is_windows <- .Platform$OS.type == "windows"
  path_separator <- if(is_windows) ";" else ":"
  python_cmd <- if(is_windows) "where python" else "which -a python3 python"
  version_cmd <- if(is_windows) "python -V 2>&1" else "python3 -V 2>&1"

  # Get PATH environment variable
  path <- Sys.getenv("PATH")
  path_dirs <- strsplit(path, path_separator)[[1]]

  # Common installation directories to check
  additional_dirs <- if(is_windows) {
    c(
      "C:/Python*",
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Python/Python*"),
      file.path(Sys.getenv("LOCALAPPDATA"), "r-reticulate/r-reticulate/pyenv/pyenv-win/versions*"),
      file.path(Sys.getenv("PROGRAMFILES"), "Python*"),
      file.path(Sys.getenv("PROGRAMFILES(X86)"), "Python*")
    )
  } else {
    c(
      "/usr/bin/python*",
      "/usr/local/bin/python*",
      "~/anaconda*/bin/python*",
      "~/miniconda*/bin/python*"
    )
  }

  # Function to get Python version from executable
  get_python_version <- function(python_path) {
    tryCatch({
      cmd <- paste(shQuote(python_path), "-V 2>&1")
      version_str <- suppressWarnings(system(cmd, intern = TRUE))
      # Extract version number using regex
      version <- sub("Python ", "", version_str)
      # Check if version contains numbers, if not set to "0.0.0"
      if(!grepl("[0-9]", version)) {
        version <- "0.0.0"
      }
      list(path = normalizePath(python_path, winslash = "/"), version = version)
    }, error = function(e) NULL,
    warning = function(w) NULL)
  }

  # Store unique installations using path as key
  python_installations <- new.env(hash = TRUE)

  # Check PATH directories
  for(dir in path_dirs) {
    if(is_windows) {
      potential_paths <- list.files(dir, pattern = "^python\\.exe$", full.names = TRUE)
    } else {
      potential_paths <- list.files(dir, pattern = "^python3?$", full.names = TRUE)
    }

    for(path in potential_paths) {
      version_info <- get_python_version(path)
      if(!is.null(version_info)) {
        python_installations[[version_info$path]] <- version_info
      }
    }
  }

  # Check additional common directories
  for(pattern in additional_dirs) {
    dirs <- Sys.glob(file.path(pattern, "*"))
    for(dir in dirs) {
      if(is_windows) {
        python_path <- file.path(dir, "python.exe")
      } else {
        python_path <- file.path(dir, "python")
      }

      if(file.exists(python_path)) {
        version_info <- get_python_version(python_path)
        if(!is.null(version_info)) {
          python_installations[[version_info$path]] <- version_info
        }
      }
    }
  }

  # Convert environment to list
  unique_installations <- as.list(python_installations)

  # Sort installations by version number
  version_order <- order(sapply(unique_installations, function(x) as.character(numeric_version(x$version))))

  unique_installations <- unique_installations[version_order]
}

# New function to find highest compatible version
find_compatible_python <- function(min_version = "3.9", max_version = "3.12",
                                   allow_user_selection = FALSE,
                                   install_if_missing = TRUE) {
  # Get all installations
  all_installations <- check_python_installations()

  # Function to check if version is within acceptable range
  is_version_compatible <- function(version_str) {
    tryCatch({
      # Skip version "0.0.0"
      if(version_str == "0.0.0") return(FALSE)

      # Extract the major.minor version for comparison
      version_parts <- strsplit(version_str, "\\.")[[1]]
      major_minor <- paste(version_parts[1:2], collapse = ".")

      version_num <- numeric_version(major_minor)
      min_num <- numeric_version(min_version)
      max_num <- numeric_version(max_version)

      return(version_num >= min_num && version_num <= max_num)
    }, error = function(e) {
      cat(sprintf("Error checking version compatibility for %s: %s\n", version_str, e$message))
      return(FALSE)
    })
  }

  # Filter compatible versions and convert to list
  compatible_installations <- Filter(
    function(x) is_version_compatible(x$version),
    all_installations
  )

  # If no compatible versions found and install_if_missing is TRUE
  if(length(compatible_installations) == 0 && install_if_missing) {
    cat(sprintf("No compatible Python installations found. Installing Python %s...\n", max_version))
    tryCatch({
      reticulate::install_python(version = max_version,
      )
      # Refresh the installations list
      all_installations <- check_python_installations()
      compatible_installations <- Filter(
        function(x) is_version_compatible(x$version),
        all_installations
      )
    }, error = function(e) {
      cat(sprintf("Error installing Python: %s\n", e$message))
    })
  }

  # Print found compatible versions
  if(length(compatible_installations) > 0) {
    cat(sprintf("Found compatible Python installations (between %s and %s):\n",
                min_version, max_version))

    # Create a numbered list of installations
    for(i in seq_along(compatible_installations)) {
      install <- compatible_installations[[i]]
      cat(sprintf("%d. Version: %s\nPath: %s\n\n",
                  i, install$version, install$path))
    }

    # If user selection is allowed
    if(allow_user_selection && length(compatible_installations) > 1) {
      cat("Multiple compatible versions found.\n")
      selection <- NA
      while(is.na(selection) || selection < 1 || selection > length(compatible_installations)) {
        user_input <- readline(sprintf("Enter number 1-%d to select version (press Enter for highest version): ",
                                       length(compatible_installations)))

        # If user just pressed Enter, use the highest version
        if(user_input == "") {
          return(compatible_installations[[length(compatible_installations)]])
        }

        # Try to convert input to number
        selection <- tryCatch({
          as.numeric(user_input)
        }, error = function(e) NA)

        if(is.na(selection) || selection < 1 || selection > length(compatible_installations)) {
          cat("Invalid selection. Please try again.\n")
        }
      }
      return(compatible_installations[[selection]])
    } else {
      # Return the highest version installation
      return(compatible_installations[[length(compatible_installations)]])
    }
  } else {
    cat(sprintf("No Python installations found between versions %s and %s\n",
                min_version, max_version))
    return(NULL)
  }
}

# # Example usage:
# List all Python installations
# cat("All Python installations found:\n")
# all_installations <- check_python_installations()
# for(install in all_installations) {
#   cat(sprintf("Version: %s\nPath: %s\n\n", install$version, install$path))
# }

# # Example usage:
# # To use with automatic installation if no compatible version found
# python_info <- find_compatible_python()
#
# # To use with user selection enabled
# python_info <- find_compatible_python(allow_user_selection = TRUE)
#
# # To use with both features disabled
# python_info <- find_compatible_python(allow_user_selection = FALSE,
#                                       install_if_missing = TRUE)

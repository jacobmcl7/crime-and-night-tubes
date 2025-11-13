# this is a preliminary file that removes all irrelevant files from the police data folder, to save space

# CAREFUL WITH THIS!!

# Define the main folder path
main_folder <- "C:/Users/jpmcl/OneDrive/Documents/Economics/Papers (WIP)/Crime and night tubes EXTRA DATA/2017-12"  # Update this path

# Define the patterns to keep
keep_patterns <- c(
  "hertfordshire-outcomes",
  "kent-outcomes",
  "surrey-outcomes",
  "essex-outcomes",
  "thames-valley-outcomes",
  "metropolitan-outcomes"
)

# Get all subfolders matching the YYYY-MM pattern
subfolders <- list.dirs(main_folder, recursive = FALSE)
subfolders <- subfolders[grepl("\\d{4}-\\d{2}$", basename(subfolders))]

# Counter for deleted files
deleted_count <- 0

# Loop through each subfolder
for (subfolder in subfolders) {
  # Get all CSV files in the subfolder
  csv_files <- list.files(subfolder, pattern = "\\.csv$", full.names = TRUE)
  
  # Loop through each CSV file
  for (csv_file in csv_files) {
    filename <- basename(csv_file)
    
    # Check if the filename matches any of the keep patterns
    should_keep <- FALSE
    for (pattern in keep_patterns) {
      if (grepl(pattern, filename)) {
        should_keep <- TRUE
        break
      }
    }
    
    # Delete if not in keep list
    if (!should_keep) {
      cat("Deleting:", csv_file, "\n")
      file.remove(csv_file)
      deleted_count <- deleted_count + 1
    }
  }
}

cat("\nTotal files deleted:", deleted_count, "\n")
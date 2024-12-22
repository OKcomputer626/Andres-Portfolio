# Saving normal images (no patchwork)
save_plot <- function(plot, 
                      type = c("tidytuesday", "swd", "standalone"),
                      year = format(Sys.Date(), "%Y"),
                      week = NULL,
                      month = NULL,
                      date = NULL,
                      name = NULL, 
                      height = 8, 
                      width = 10) {
  
  # Match argument and error handling
  type <- match.arg(type)
  
  # Base paths setup
  base_paths <- list(
    tidytuesday = here::here("visualization/TidyTuesday", year),
    swd = here::here("visualization/SWD Challenge", year),
    standalone = here::here("projects/standalone_visualizations")
  )
  
  # Construct file name based on type
  file_name <- switch(
    type,
    tidytuesday = sprintf("tt_%d_%02d.png", year, week),
    swd = sprintf("swd_%d_%02d.png", year, month %||% as.numeric(format(Sys.Date(), "%m"))),
    standalone = if (!is.null(name)) {
      paste0(name, ".png")
    } else {
      sprintf("sa_%d-%02d-%02d.png", 
              year,
              month %||% as.numeric(format(Sys.Date(), "%m")),
              date %||% as.numeric(format(Sys.Date(), "%d")))
    }
  )
  
  # Set up paths
  base_path <- base_paths[[type]]
  main_file <- file.path(base_path, file_name)
  thumb_file <- file.path(base_path, "thumbnails", file_name)
  
  # Create directories if they don't exist
  dir.create(dirname(main_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(thumb_file), recursive = TRUE, showWarnings = FALSE)
  
  # Input validation
  if (type == "tidytuesday" && is.null(week)) {
    stop("Week parameter is required for TidyTuesday plots")
  }
  if (type == "swd" && is.null(month)) {
    warning("Month not specified for SWD plot, using current month")
  }
  
  # Save main plot
  ggsave(
    filename = main_file,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300
  )
  
  # Create thumbnail using magick
  magick::image_read(main_file) |> 
    magick::image_resize("400") |> 
    magick::image_write(thumb_file)
  
  # Return the paths invisibly
  invisible(list(main = main_file, thumbnail = thumb_file))
}
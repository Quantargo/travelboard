get_prefix <- function() {
  prefix <- "~/workshop"
  if(dir.exists("workshop")) {
    prefix <- "workshop"
  }
  prefix
}
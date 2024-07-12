

# path = "F:/Nextcloud GR/04 - Comite Pediatrie/NIVOGLIO/Analyses/Analyse DRM"
#' @importFrom rlang check_installed
init_project = function(path){
  check_installed("usethis")
  if(path=="."){
    cli_abort("Project should not be created in the current directory.")
  }
  w = setwd(path)
  on.exit(setwd(w))
  
  dir.create("data")
  dir.create("R")
  dir.create("report")
  dir.create("graph")
  
  #TODO copy RProj
  writeLines(con="R/init.R", "#init")
  writeLines(con="R/read.R", "#read")
  writeLines(con="R/report.R", "#report")
  writeLines(con="R/description.R", "#description")
  
  
  data <- list(Project = "My STUDY", 
               Package = "My STUDY", 
               Version = NULL, 
               InitialBullet = "Initial version", 
               Rmd = FALSE, on_github = FALSE, 
               github_spec = NULL)
  usethis::create_project(open=FALSE)
  usethis::use_template("project-README", "README.md", data = data, open = TRUE)
  usethis::use_template("NEWS.md", data = data, open = TRUE)
  usethis::use_blank_slate(scope="project")
  #TODO: see ?usethis::use_template for custom templates
}

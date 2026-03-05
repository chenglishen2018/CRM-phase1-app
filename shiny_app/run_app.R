# Install packages if needed
packages <- c('shiny', 'shinydashboard', 'ggplot2', 'dplyr', 'DT', 'gridExtra', 'scales')

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = 'http://cran.r-project.org')
    library(pkg, character.only = TRUE)
  }
}

# Run the Shiny app
options(shiny.port = 3838)
options(shiny.host = '127.0.0.1')

shiny::runApp('CRM_App_Fresh.R', launch.browser = FALSE)

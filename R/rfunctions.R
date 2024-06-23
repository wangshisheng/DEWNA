#' Run the DEWNA Shiny web application locally.
#' @export
DEWNA_app <- function() {
  shiny::runApp(system.file('DEWNAapp', package='DEWNA'),
                host=getOption("0.0.0.0"), port =getOption("8989"))
}

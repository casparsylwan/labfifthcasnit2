#' Runs the RC-API and opens the shiny
#'
#' @return
#' @export
#'

shinyapi<-function(){
  apiwebb<-shiny_api$new()
  return(apiwebb$shiny_app())
}

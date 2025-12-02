#' Shiny components
#' @description
#' UI and server components of the Shiny app. Can be used to integrate into
#' other Shiny apps.
#'
#' @param input,output,session Internal arguments for shiny server functions.
#'
#' @returns \code{drawsf_ui} returns the app UI, \code{drawsf_server} returns
#' nothing useful and is only used for its shiny functionality.
#'
#' @export
#'
#' @examples
#' shiny::shinyApp(drawsf_ui, drawsf_server)
drawsf_ui <- function() {
  shiny::tags$head(
    shiny::tags$script(shiny::HTML("
      #controls {
        /* Appearance */
        background-color: white;
        padding: 0 20px 20px 20px;
        cursor: move;
        /* Fade out while not hovering */
        opacity: 0.65;
        zoom: 0.9;
        transition: opacity 500ms 1s;
      }
    "))
  )

  shiny::fillPage(
    leaflet::leafletOutput("map", height = "100%"),
    shiny::absolutePanel(
      id = "controls",
      class = c("panel", "panel-default"),
      fixed = TRUE,
      draggable = TRUE,
      top = "auto",
      left = 10,
      right = "auto",
      bottom = 10,
      width = 120,
      height = "auto",

      shiny::uiOutput("coord_track")
    ),

    shiny::absolutePanel(
      shiny::actionButton(
        "close",
        label = "Finish",
        icon = shiny::icon("check"),
        width = 150,
        style = "height: 50px",
        class = "btn-success"
      ),
      fixed = TRUE,
      top = 10,
      left = "auto",
      right = 10,
      bottom = "auto"
    )
  )
}

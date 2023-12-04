#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  r <- reactiveValues()
  r$probability <- .5
  r$chance <- c()
  r$bot_cooperou <- c()
  r$item <- 0
  r$cooperou <- c()

  output$ui <- renderUI(
    mod_apresentacao_ui("apresentacao_1")
  )



# Servers -----------------------------------------------------------------

  mod_apresentacao_server("apresentacao_1")
  mod_tcle_server("tcle_1", r)
  mod_teste_server("teste_1", r)
  mod_devolutiva_server("devolutiva_1", r)


}

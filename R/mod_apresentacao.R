#' apresentacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_apresentacao_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui"))
  )
}

#' apresentacao Server Functions
#'
#' @noRd
mod_apresentacao_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ui <- renderUI({
      tagList(

        tags$img(src = 'www/logo_catvante.png', id = 'catvante'),
        h1('Teste de cooperação'),
        p('Olá, esta é uma pesquisaxxxxx'),
        p('Vamos começar?'),
        radioButtons(
          ns("menor_maior"),
          label = "Em relação à sua idade, você tem?",
          choiceNames = c(
            "17 anos ou menos",
            "18 anos ou mais"
          ),
          choiceValues = c(
            "menor",
            "maior"
          ),
          selected = 'maior'
        ),
        actionButton(
          ns("avancar"),
          label = "Avançar"
        )

      )
    })


    # Ao clicar o botão
    observeEvent(input$avancar, {
      if(input$menor_maior == "menor"){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Uhm...",
          text = "É preciso ter mais de 18 anos para participar!",
          type = "error"
        )
      }else{
        output$ui <- renderUI(mod_tcle_ui("tcle_1"))
      }
    })

  })
}

## To be copied in the UI
#

## To be copied in the server
#

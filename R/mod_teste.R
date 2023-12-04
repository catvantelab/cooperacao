#' teste UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_teste_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('ui'))
  )
}

#' teste Server Functions
#'
#' @noRd
mod_teste_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$ui <- renderUI({
      tagList(
        p(db_instrument$enunciado[1]),
        fluidRow(
          column(
            6,
            actionButton(
              ns('cooperar'),
              label = 'Cooperar'
            )
          ),
          column(
            6,
            actionButton(
              ns('recusar'),
              label = 'Não cooperar'
            )
          )
        )
      )
    })


    observeEvent(input$cooperar, {
      isolate({
        r$cooperou <- c(r$cooperou, TRUE)
        r$item <- r$item + 1

        r$chance <- c(r$chance, runif(1))
        r$bot_cooperou <- c(r$bot_cooperou, r$chance[length(r$chance)] >= r$probability[length(r$probability)])

        print(
          glue::glue(
            'Cooperou.
          Probabilidade: {r$probability[length(r$probability)]}
          Chance: {round(r$chance[length(r$chance)], 2)}
          Bot cooperou: {r$bot_cooperou[length(r$bot_cooperou)]}'
          )
        )

        if(r$bot_cooperou[length(r$bot_cooperou)]){
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Neutro",
            text = db_instrument$neutral_feedback[r$item],
            type = "info"
          )
        } else if(!r$bot_cooperou[length(r$bot_cooperou)]){
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Positivo",
            text = db_instrument$positive_feedback[r$item],
            type = "success"
          )
        }

        if(r$item == nrow(db_instrument)){
          output$ui <- renderUI(mod_devolutiva_ui("devolutiva_1"))
        } else {
          output$ui <- renderUI({
            tagList(
              p(db_instrument$enunciado[r$item + 1]),
              fluidRow(
                column(
                  6,
                  actionButton(
                    ns('cooperar'),
                    label = 'Cooperar'
                  )
                ),
                column(
                  6,
                  actionButton(
                    ns('recusar'),
                    label = 'Não cooperar'
                  )
                )
              )
            )
          })
        }


      })
    })
    observeEvent(input$recusar, {
      isolate({
        r$cooperou <- c(r$cooperou, FALSE)
        r$item <- r$item + 1

        r$chance <- c(r$chance, runif(1))
        r$bot_cooperou <- c(r$bot_cooperou, r$chance[length(r$chance)] >= r$probability[length(r$probability)])

        print(
          glue::glue(
            'Não cooperou
            Probabilidade: {r$probability[length(r$probability)]}
            Chance: {round(r$chance[length(r$chance)], 2)}
            Bot cooperou: {r$bot_cooperou[length(r$bot_cooperou)]}'
          )
        )

        if(r$bot_cooperou[length(r$bot_cooperou)]){
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Negativo",
            text = db_instrument$negative_feedback[r$item],
            type = "error"
          )
        } else if(!r$bot_cooperou[length(r$bot_cooperou)]){
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Neutro",
            text = db_instrument$neutral_feedback[r$item],
            type = "info"
          )
        }

        if(r$item == nrow(db_instrument)){
          output$ui <- renderUI(mod_devolutiva_ui("devolutiva_1"))
        } else {
          output$ui <- renderUI({
            tagList(
              p(db_instrument$enunciado[r$item + 1]),
              fluidRow(
                column(
                  6,
                  actionButton(
                    ns('cooperar'),
                    label = 'Cooperar'
                  )
                ),
                column(
                  6,
                  actionButton(
                    ns('recusar'),
                    label = 'Não cooperar'
                  )
                )
              )
            )
          })
        }
      })
    })


  })
}

## To be copied in the UI
#

## To be copied in the server
#

#' devolutiva UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_devolutiva_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('ui'))
  )
}

#' devolutiva Server Functions
#'
#' @noRd
mod_devolutiva_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ui <- renderUI({

      table_cooperacao <- prop.table(
        table(r$cooperou, r$bot_cooperou)
      )

      indiceH_sujeito <- -log(c(1-mean(r$cooperou),mean(r$cooperou)), 2)
      indiceH_bot <- -log(c(1-mean(as.numeric(db_instrument$probability)),mean(as.numeric(db_instrument$probability))), 2)

      # Calcular informação mútua
      table_informacao_mutua <- matrix(
        nrow = 2,
        ncol = 2
      )
      table_informacao_condicional <- matrix(
        nrow = 2,
        ncol = 2
      )

      for(i in 1:2){
        for(j in 1:2){
          table_informacao_mutua[i,j] <- log(
            table_cooperacao[i,j] / (indiceH_sujeito[i]* indiceH_bot[j]), 2
          )
          table_informacao_condicional[i,j] <- indiceH_sujeito[i] - table_informacao_mutua[i,j]

        }
      }


      teste <- data.frame(table_informacao_condicional)

      tagList(
        tags$h3('Quanto você está disposto a cooperar quando o outro coopera?'),
        tags$p(round(teste[2,2], 2), style = 'text-align: center;'),
        tags$h3('Quanto você está disposto a cooperar quando o outro NÃO coopera?'),
        tags$p(round(teste[2,1], 2), style = 'text-align: center;'),
        tags$h3('Quanto você está disposto a NÃO cooperar quando o outro coopera?'),
        tags$p(round(teste[1,2], 2), style = 'text-align: center;'),
        tags$h3('Quanto você está disposto a NÃO cooperar quando o outro NÃO coopera?'),
        tags$p(round(teste[1,1], 2), style = 'text-align: center;'),
        tags$h3('Quantas vezes você cooperou?'),
        tags$p(paste0(round(sum(r$cooperou)*100/nrow(db_instrument)), '% das vezes'), style = 'text-align: center;')
      )

    })

  })
}

## To be copied in the UI
#

## To be copied in the server
#

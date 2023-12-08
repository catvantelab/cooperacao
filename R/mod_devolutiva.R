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

      x <- c(r$cooperou, 1, 1, 0, 0)
      y <- c(r$bot_cooperou, 1, 0, 1, 0)

      P_x <- prop.table(table(x))
      P_y <- prop.table(table(y))

      # Mudar para P
      table_cooperacao <- prop.table(
        table(x, y)
      )

      indiceH_sujeito <- -log(c(1-mean(x),mean(y)), 2)
      indiceH_bot <- -log(c(1-mean(as.numeric(db_instrument$probability)),mean(as.numeric(db_instrument$probability))), 2)

      # Calcular informação mútua
      table_informacao_mutua <- matrix(
        NA,
        nrow = 2,
        ncol = 2,
        dimnames = list(
          c('Coop', 'Comp'),
          c('Coop', 'Comp')
        )
      )
      table_informacao_condicional <- matrix(
        NA,
        nrow = 2,
        ncol = 2,
        dimnames = list(
          c('Coop', 'Comp'),
          c('Coop', 'Comp')
        )
      )

      for(i in 1:2){
        for(j in 1:2){
          table_informacao_mutua[i,j] <- log(
            table_cooperacao[i,j] / (P_x[i]* P_y[j]), 2
          )
          table_informacao_condicional[i,j] <- indiceH_sujeito[i] - table_informacao_mutua[i,j]

        }
      }


      teste <- data.frame(table_informacao_condicional)

      tagList(
        tags$h4(glue::glue('Você tem {round(teste[1,1]/teste[2,1], 2)} vezes a chance de cooperar quando o outro coopera'), style = 'text-align: center;'),
        tags$h4(glue::glue('Você tem {round(teste[1,2]/teste[2,2], 2)} vezes a chance de cooperar quando o outro não coopera'), style = 'text-align: center;'),
        # tags$h3('Quanto você está disposto a cooperar quando o outro coopera?'),
        # tags$p(round(teste[1,1], 2), style = 'text-align: center;'),
        # tags$h3('Quanto você está disposto a cooperar quando o outro NÃO coopera?'),
        # tags$p(round(teste[1,2], 2), style = 'text-align: center;'),
        # tags$h3('Quanto você está disposto a NÃO cooperar quando o outro coopera?'),
        # tags$p(round(teste[2,1], 2), style = 'text-align: center;'),
        # tags$h3('Quanto você está disposto a NÃO cooperar quando o outro NÃO coopera?'),
        # tags$p(round(teste[2,2], 2), style = 'text-align: center;'),
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

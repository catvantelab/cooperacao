#' tcle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tcle_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('ui'))
  )
}

#' tcle Server Functions
#'
#' @noRd
mod_tcle_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ui <- renderUI({
      tagList(
        h1("TERMO DE CONSENTIMENTO LIVRE E ESCLARECIDO"),
        h2("TESTE DE INTERESSES PROFISSIONAIS ADAPTATIVO: DESENVOLVIMENTO E PROPRIEDADES PSICOMÉTRICAS DE UMA MEDIDA DE AVALIAÇÃO COMPUTADORIZADA"),
        p("Ao clicar em “CONCORDO” abaixo, dou meu consentimento livre e esclarecido para participar como voluntário(a) do projeto de pesquisa supracitado, sob a responsabilidade do pesquisador Gustavo Henrique Martins do Programa de Pós-Graduação em Psicologia da Universidade São Francisco.
          Concordando com este Termo de Consentimento estou ciente de que:"
        ),
        tags$ol(
          tags$li("O objetivo da pesquisa é desenvolver o Teste de Interesses Profissionais Adaptativo (TIPA), um instrumento no formato adaptativo computadorizado que visa avaliar os interesses profissionais no modelo RIASEC;"),
          tags$li("Durante o estudo serão aplicados os instrumentos: questionário sociodemográfico, Teste de Interesses Profissionais Adaptativo (TIPA), 18REST, com duração aproximada de 20 minutos;"),
          tags$li("Obtive todas as informações necessárias para poder decidir conscientemente sobre minha participação na referida pesquisa; "),
          tags$li("A resposta a estes instrumentos não apresenta riscos conhecidos à minha saúde física e mental, mas podem causar desconforto emocional;"),
          tags$li("Estou livre para interromper a qualquer momento minha participação na pesquisa, não havendo qualquer prejuízo decorrente da decisão;"),
          tags$li("Meus dados pessoais serão mantidos em sigilo e os resultados gerais obtidos na pesquisa serão utilizados apenas para alcançar os objetivos do trabalho, expostos acima, incluída sua publicação na literatura científica especializada;"),
          tags$li("Poderei contatar o Comitê de Ética em Pesquisa da Universidade São Francisco, situado à Av. São Francisco de Assis, nº 218, bairro: Cidade Universitária, Cep: 12916-900, Bragança Paulista/SP para apresentar recursos ou reclamações em relação à pesquisa pelo telefone: (11) 2454-8981 ou e-mail: comite.etica@saofrancisco.edu.br;"),
          tags$li("Poderei entrar em contato com o responsável pelo estudo, Gustavo Henrique Martins, sempre que julgar necessário pelo telefone (19) 98443-4532 ou e-mail: gustavoh.martins95@gmail.com;"),
          tags$li("As informações contidas neste Termo de Consentimento Livre e Esclarecido serão salvas em um banco de dados seguro, sendo que uma cópia de igual teor poderá ser solicitada por meio do e-mail: gustavoh.martins95@gmail.com.")
        ),
        radioButtons(
          ns("tcle"),
          label = "",
          choiceNames = c(
            "Concordo e aceito participar da pesquisa",
            "Não concordo e/ou não aceito participar da pesquisa"
          ),
          choiceValues = c(
            "concordo",
            "nao_concordo"
          ),
          selected = 'concordo'
        ),
        actionButton(
          ns("avancar"),
          label = "Avançar"
        )

      )
    })


    # Ao clicar o botão
    observeEvent(input$avancar, {

      r$tcle <- input$tcle

      output$ui <- renderUI(mod_teste_ui("teste_1"))

    })

  })
}

## To be copied in the UI
#

## To be copied in the server
#

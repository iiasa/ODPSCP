#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash insertTab tabItem tabItems
#' @importFrom shiny actionButton tabsetPanel column
mod_Home_ui <- function(id){
  ns <- NS(id)

  bs4Dash::tabItem(
    tabName = "Home",

    shiny::fluidPage(

      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(width = 8,
               shiny::p("A reporting protocol for SCP", style = "padding-top: 10px; font-size: 30px; font-weight:bold;"),

               shiny::p("Systematic conservation planning (SCP) usually make use of
                 decision theoretical approaches to decide - given all available evidence - where,
                 when and/or what to do so that outcomes are most beneficial for biodiversity conservation.
                 SCP can be applied at the identification, planning, implementation and
                 monitoring phase of a conservation project. It usually is a multidisciplinary
                 approach integrating both qualitative and quantitative data and methodologies
                 from a range of scientific disciplines.",
             style= "font-size: 18px;"),

              shiny::p("Area-based and action-based conservation planning are critical scientific
                tools in reaching conservation policy objectives across scales.
                Yet despite decades of research and numerous scientific advances
                in Europe ",shiny::a(href = 'https://osf.io/8x2ug/', '(Jung et al. 2023)',
                                                    target = "_blank", .noWS = "outside"),".",
                " and globally ",shiny::a(href = 'https://www.annualreviews.org/doi/10.1146/annurev-environ-102016-060902',
                                   '(McIntosh et al. 2017)',
                                   target = "_blank", .noWS = "outside"),
                ", it often remains incomprehensible for those unfamiliar with the
                planning how decisions where reached or what factors were considered
                influential in determining the identified outcomes.",
               style= "font-size: 18px;"),

             # img(src = "", width = "60%", style="display: block; margin-left: auto; margin-right: auto;
             #     min-width:500px;"), br(),

             shiny::p("The ODPSCP protocol serves three main purposes. First, it provides a
               checklist for authors to detail all key steps in their planning work.
               Second, it introduces a standard approach to documentation that ensures
               transparency and reproducibility, facilitating peer review and
               expert evaluation of the conducted planning. Third, it helps study
               authors and decision makers to identify key strength, but also
               weaknesses of a systematic planning exercise. ", style= "font-size: 18px;"),

             shiny::p("This Shiny web application helps to implement the protocol through
               an easy understandable user interface and allows export the created
               protocols in a range of formats for further usages. We encourage the
               scientific community, publishers and editors and policy makers to make
               use of this protocol.",
               style= "font-size: 18px;"),
             # em(p("Please cite as follows:", style = "font-size: 18px;")),
             # p("<INSERT PREPRINT> DOI: ",
             #   a(href = '', '', target = "_blank", .noWS = "outside"), style= "font-size: 18px;"),
             shiny::hr(), shiny::br()
        )),

      # Button row
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(width = 8,
               # Button to get started
               shinyWidgets::actionBttn(
                 inputId = "start_new_protocol",
                 label = "Start a new protocol",
                 style = "float",
                 color = "royal",
                 size = "md",
                 block = TRUE,
                 icon = shiny::icon("play")
               )
          )
        )
      ) # End Fluid page
    )
}

#' Home Server Functions
#'
#' @noRd
mod_Home_server <- function(id, results){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Home_ui("Home_1")

## To be copied in the server
# mod_Home_server("Home_1")

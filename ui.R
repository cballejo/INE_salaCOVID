sidebar <- dashboardSidebar(
  sidebarMenu(id = "menu", sidebarMenuOutput("menu")))
#  menuItem("Estadísticas", tabName = "estadisticas", icon = icon("dashboard")),
# menuItem("Indicadores", icon = icon("th"), tabName = "indicadores"))



body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Estadisticas",
            h2("Zona Sanitaria VIII"),
          fluidRow(
            tabBox(
               title = "",
               id = "tabset",
               width = 12, height = "400px",
               tabPanel("Último día", "",
                     valueBoxOutput("zona1"),
                     valueBoxOutput("zona2"),
                     valueBoxOutput("zona3"),
                     valueBoxOutput("zona4"),
                     valueBoxOutput("zona5")
                ),
               tabPanel("Acumulados", "",             
                    valueBoxOutput("zona2_1"),
                    valueBoxOutput("zona2_2"),
                    valueBoxOutput("zona2_3"),
                    valueBoxOutput("zona2_4"),
                    valueBoxOutput("zona2_5"),
                    valueBoxOutput("zona2_6"),
                    valueBoxOutput("zona2_7")
             ),
             tabPanel("Determinaciones", "",             
                    valueBoxOutput("zona3_1"),
                    valueBoxOutput("zona3_2"),
                    valueBoxOutput("zona3_3")
               ) #tabPanel            
             ) #tabBox
          ) #fluidRow
    ), #tabItem
    tabItem(tabName = "Graficos_zona",
            h2("Gráficos Zona Sanitaria VIII"),
            fluidRow(
              tabBox(
                title = "",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset2",
                width = 12, height = "900px",
                tabPanel("Incidencia", "",
                         box(title = "", status = "primary", 
                              highchartOutput("inc_zona", height = "700px"), width = NULL)
                         ),
                tabPanel("Mortalidad", "",
                         box(title = "", status = "primary", 
                             highchartOutput("mort_zona", height = "700px"), width = NULL)
                         ),
                tabPanel("Media 7 días", "",
                         box(title = "", status = "primary",
                             highchartOutput("diario_zona", height = "500px"), width = 12),
                         box(title = "", status = "success", sliderInput("seleccion_fecha", "Seleccione el rango de meses",
                                                                         min = month(min(pos_acum$fecha, na.rm = T)),
                                                                         max = month(max(pos_acum$fecha, na.rm = T)),
                                                                         value = c(month(min(pos_acum$fecha, na.rm = T)), month(max(pos_acum$fecha, na.rm = T)))), width = 12)
                  ) #tabPanel
                ) #tabBox
               ) #fluidRow
              ), #tabItem
    tabItem(tabName = "Partidos",
            h2("Información por partidos"), 
             fluidRow(
               column(3,
                      selectInput(inputId = "partido",
                                label="Partido", choices =  diag_diario$partido),
                       valueBoxOutput("part_1", width = NULL),
                       valueBoxOutput("part_2", width = NULL),
                       valueBoxOutput("part_3", width = NULL),
                       valueBoxOutput("part_4", width = NULL),
                       valueBoxOutput("part_5", width = NULL),
                       valueBoxOutput("part_6", width = NULL),
                      sliderInput("seleccion_fecha1", "Seleccione el rango de meses",
                                  min = month(min(pos_acum$fecha, na.rm = T)),
                                  max = month(max(pos_acum$fecha, na.rm = T)),
                                  value = c(month(min(pos_acum$fecha, na.rm = T)), month(max(pos_acum$fecha, na.rm = T)))),
                      ),
               fluidRow(
                    box(title = "", status = "primary", 
                      highchartOutput("sexedad"),width = 4),
                    box(title = "", status = "primary", 
                        highchartOutput("posi"), width = 4),
                    box(title = "", status = "primary",
                        highchartOutput("diario_partido"), width = 8)
                    )
            
           )
        ),
               tabItem(tabName = "Mapa",
                   h2("Mapa Zona Sanitaria VIII"), 
                      fluidRow(
                        column(2,
                        radioButtons("info", "Información:",
                                     c("Casos acumulados" = "Casos",
                                       "Casos activos" = "Activos",
                                       "Incidencia acumulada" = "Incidencia",
                                       "Fallecidos" = "Fallecidos",
                                       "Tasa de mortalidad" = "Mortalidad"))
                             ),
                        column(10, 
                               box(title = "", status = "primary", 
                                   tmapOutput("mapa1"), width = NULL, height = "800px"))
              )
          )
    ) # tabItems 
) # dashboardBody
 


ui <- dashboardPage(
  dashboardHeader(title = "Sala de situación",
                  tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("Refresh1"))))),
                  tags$li(class = "dropdown", actionLink("actualizar", "Actualizar datos"))),
  sidebar,
  body
)
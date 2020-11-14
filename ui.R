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
               width = 12, height = "800px",
               tabPanel("Último día", "",
                     valueBoxOutput("zona1"),
                     valueBoxOutput("zona2"),
                     valueBoxOutput("zona3"),
                     valueBoxOutput("zona4"),
                     valueBoxOutput("zona5"),
                     box(title = "", status = "primary", 
                         highchartOutput("tree"), width = 10)
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
                width = 12, height = "700px",
                tabPanel("Incidencia", "",
                         box(title = "", status = "primary", 
                              highchartOutput("inc_zona", height = "600px"), width = NULL,
                         footer = strong("*calculada a partir del cociente de los casos positivos confirmados sobre la población de cada partido proyectada por el INDEC en 2020 por 100.000"))
                         ),
                tabPanel("Mortalidad", "",
                         box(title = "", status = "primary", 
                             highchartOutput("mort_zona", height = "600px"), width = NULL,
                             footer = strong("*calculada a partir del cociente de los fallecidos que fueron confirmados positivos sobre la población de cada partido proyectada por el INDEC en 2020 por 1.000.000"))
                         ),
                tabPanel("Mortalidad ajustada", "",
                         box(title = "", status = "primary", 
                             highchartOutput("mort_zona_aj", height = "600px"), width = NULL,
                             footer = strong("*calculada a partir de la mortalidad bruta de cada partido por 100.000 ajustando por método directo con la población mundial estándar de la OMS (2000-2025)"))
                ),
                tabPanel("Media 7 días", "",
                         boxPlus(title = "", status = "primary",
                                 enable_sidebar = T,
                                 closable = F,
                                 collapsible = T,
                                 sidebar_width = 20,
                                 sidebar_title = "Rango de meses",
                                 sidebar_start_open = F,
                                 sidebar_background = "#2E64FE",  
                                 sidebar_content =  sliderInput("seleccion_fecha", "Seleccione el rango de meses",
                                                                min = month(min(pos_acum$fecha, na.rm = T)),
                                                                max = month(max(pos_acum$fecha, na.rm = T)),
                                                                value = c(month(min(pos_acum$fecha, na.rm = T)), month(max(pos_acum$fecha, na.rm = T)))),
                             highchartOutput("diario_zona", height = "600px"), width = 12, footer = strong("*calculada a partir del promedio de casos diarios de los 7 días anteriores incluyendo la última fecha para toda la zona sanitaria"))
                  ),#tabPanel
                tabPanel("Incidencia - evolución comparativa", "",
                         box(title = "", status = "primary", 
                             highchartOutput("inc_partidos", height = "600px"), width = NULL,
                             footer = strong("*calculada a partir del cociente de los casos positivos confirmados sobre la población de cada partido proyectada por el INDEC en 2020 acumulada progresivamente en cada fecha por 100.000"))
                )
                ) #tabBox
               ) #fluidRow
              ), #tabItem
    tabItem(tabName = "Partidos",
            h2("Información por partidos"), 
             fluidRow(
               column(3,
                      selectInput(inputId = "partido",
                                label="Partido", choices =  diag_diario$partido, selected = "General Pueyrredón"),
                       valueBoxOutput("part_1", width = NULL),
                       valueBoxOutput("part_2", width = NULL),
                       valueBoxOutput("part_3", width = NULL),
                       valueBoxOutput("part_4", width = NULL),
                       valueBoxOutput("part_5", width = NULL),
                       valueBoxOutput("part_6", width = NULL)
                      # sliderInput("seleccion_fecha1", "Seleccione el rango de meses",
                      #             min = month(min(pos_acum$fecha, na.rm = T)),
                      #             max = month(max(pos_acum$fecha, na.rm = T)),
                      #             value = c(month(min(pos_acum$fecha, na.rm = T)), month(max(pos_acum$fecha, na.rm = T)))),
                      ),
                 column(9,
                    boxPlus(title = "Distribución de casos COVID-19 por edad y sexo", status = "primary",
                            closable = F,
                            collapsed = T,
                            collapsible = T,
                      highchartOutput("sexedad"),width = NULL),
                    boxPlus(title = "Casos COVID-19 por semana epidemiológica", status = "success",
                            closable = F,
                            collapsed = T,
                            collapsible = T,
                        highchartOutput("posi"), width = NULL),
                    boxPlus(title = "Casos diarios COVID-19 y media movil 7 días", status = "warning",
                        enable_sidebar = T,
                        closable = F,
                        collapsed = T,
                        collapsible = T,
                        sidebar_width = 20,
                        sidebar_title = "Rango de meses",
                        sidebar_start_open = F,
                        sidebar_background = "#2E64FE",  
                        sidebar_content =  sliderInput("seleccion_fecha1", "Seleccione el rango de meses",
                                                       min = month(min(pos_acum$fecha, na.rm = T)),
                                                       max = month(max(pos_acum$fecha, na.rm = T)),
                                                       value = c(month(min(pos_acum$fecha, na.rm = T)), month(max(pos_acum$fecha, na.rm = T)))),
                        
                        highchartOutput("diario_partido"), width = NULL)
                    )
               )
            
           
        ),
               tabItem(tabName = "Mapa",
                   h2("Mapa Zona Sanitaria VIII"), 
                      fluidRow(
                        column(2,
                        radioButtons(inputId = "info", label = "Información:", 
                                    choices = c("Casos acumulados" = "Casos",
                                       "Casos activos" = "Activos",
                                       "Incidencia acumulada" = "Incidencia",
                                       "Fallecidos" = "Fallecidos",
                                       "Tasa de mortalidad bruta" = "Mortalidad",
                                       "Tasa de mortalidad ajustada" = "Mortalidad_aj"))
                             ),
                        column(10, 
                               box(title = "", status = "primary", 
                                   tmapOutput("mapa1"), width = NULL, height = "600px"))
              )
          ),
    tabItem(tabName = "Datos",
            
            includeMarkdown("Datos.Rmd")
    )
               # tabItem(tabName = "Tablas",
               #         h2("Tablas Zona Sanitaria VIII"), 
               #         fluidRow(
               #         tabBox(
               #           title = "",
               #           # The id lets us use input$tabset1 on the server to find the current tab
               #           id = "tabset3",
               #           width = 12, height = "900px",
               #           tabPanel("Tabla individual", "",
               #                    downloadButton(outputId = "descarga", label = "Descargar datos"),
               #                    DTOutput('tbl1')),
               #           tabPanel("Casos acumulados por partido", "",
               #                    DTOutput('tbl2')),
               #           tabPanel("Tasa de mortalidad por partido", "",
               #                    reactableOutput('tbl3'))
               #         )
               #      )
               # )
    ) # tabItems 
) # dashboardBody
 
# span(img(src="Logo1.jpg", width = 190))
# img(src="logo.jpg", height = 50, align = "right")
header <- dashboardHeaderPlus(title = tagList(
  span(class = "logo-lg", "Sala de situación"), 
  img(src = 'https://www.dropbox.com/s/ln6pobp5zsqpylj/imageonline-co-transparentimage.png?dl=1', height = "25px")),
                          tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("Refresh1"))))),
                          tags$li(class = "dropdown", actionLink("actualizar", "Actualizar datos")),
                          tags$li(class = "dropdown", span(tags$a(href = 'https://ine.gov.ar/',
                     img(src = 'https://www.dropbox.com/s/ln6pobp5zsqpylj/imageonline-co-transparentimage.png?dl=1', hspace="5", vspace="10",
                         title = "Instituto Nacional de Epidemiología", height = "30px"),
                     style = "float: left;",
                     align = "middle"))))


# footer <- dashboardFooter(
#   left_text = tags$p(span(tags$a(href = 'https://ine.gov.ar/',
#                         img(src = 'https://www.dropbox.com/s/ln6pobp5zsqpylj/imageonline-co-transparentimage.png?dl=1', hspace="4",
#                             title = "Instituto Nacional de Epidemiología", height = "25px"
#                            ),
#                         style = "float: left;",
#                         align = "middle")
#                       ),
#                       'ANLIS - Instituto Nacional de Epidemiología "Dr. Juan H. Jara"',class = "dropdown"),
#   right_text = 'Mar del Plata, noviembre de 2020'
# )

# 'ANLIS - Instituto Nacional de Epidemiología "Dr. Juan H. Jara"'
# style = "padding-top:10px; padding-bottom:12px;")


ui <- dashboardPagePlus(
  enable_preloader = T,
  loading_duration = 2,
  header,
  sidebar,
  body,
  # footer,
  skin = "blue"
)

# ui <- dashboardPage(
#   header,
#   sidebar,
#   body,
#   skin = "blue"
# )
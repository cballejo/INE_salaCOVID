server <- function(input, output, session) {
  
#  session$onSessionEnded(stopApp)
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Zona Sanitaria VIII", tabName = "Estadisticas", icon = icon("dashboard")),
      menuItem("Gráficos", tabName = "Graficos", icon = icon("chart-area"),
             menuSubItem("Partidos de la Zona",
                  tabName = "Graficos_zona",
                  icon = icon('line-chart'))),
      menuItem("Información por partidos", tabName = "Partidos", icon = icon("dashboard")),
      menuItem("Mapa Zona", tabName = "Mapa", icon = icon("map-marked-alt")),
      menuItem("Datos", tabName = "Datos", icon = icon("table"))
      #          menuSubItem("Casos",
      #                      tabName = "Casos",
      #                      icon = icon('line-chart')),
      #          menuSubItem("Clinica",
      #                      tabName = "Clinica",
      #                      icon = icon('line-chart')),
      #          menuSubItem("Tiempo de duplicación",
      #                      tabName = "Tiempo",
      #                      icon = icon('line-chart')),
      #          menuSubItem("Mapa Zona",
      #                      tabName = "Mapa1",
      #                      icon = icon('map-marked-alt')))
          )
  })
  

  
  output$Refresh1 <- renderText({
    toString(format(today(tzone = "America/Buenos_Aires"), format = "%A  %d %b %Y"))
  })
  

  observeEvent(input$actualizar, {
    callModule(modulo_lectura, "actualizar")
    session$reload()
  })
  
  output$zona1 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(toString(format(ultimo_zona$fecha[1], format = "%d %b %Y")), style = "font-size: 70%;"), 
      subtitle = tags$p(paste0("Semana epidemiológica: ",  ultimo_zona[1,2]), icon = icon(name = "calendar-alt"), style = "font-size: 100%;"),
      color = "light-blue"
    )
  })

  output$zona2 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(ultimo_zona[1,3], style = "font-size: 70%;"),
             subtitle = tags$p("Total de nuevos eventos SISA",  style = "font-size: 90%;"),
      icon = icon(name = "user"),
      color = "blue"
    )
  })
  
  output$zona3 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(ultimo_zona[1,4], style = "font-size: 70%;"),
             subtitle = tags$p("Positivos diagnosticados", style = "font-size: 90%;"),  
             icon = icon(name = "virus"),
             color = "red"
    )
  })
  
  output$zona4 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(ultimo_zona[1,5], style = "font-size: 70%;"), 
             subtitle = tags$p("Por laboratorio", style = "font-size: 90%;"),  
             icon = icon(name = "microscope"),
             color = "orange"
    )
  })
  
  output$zona5 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(ultimo_zona[1,6], style = "font-size: 70%;"),
             subtitle = tags$p("Por criterio epidemiológico", style = "font-size: 90%;"),
             icon = icon(name = "user-md"),
             color = "teal"
    )
  })
  
  output$zona2_1 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(sum(acum_zona[,2]), style = "font-size: 70%;"),
             subtitle = tags$p("Total eventos acumulados SISA", style = "font-size: 90%;"),
             icon = icon(name = "user"),
             color = "blue"
    )
  })
  
  output$zona2_2 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(sum(acum_zona[,3]), style = "font-size: 70%;"),
             subtitle = tags$p("Positivos acumulados", style = "font-size: 90%;"),
             icon = icon(name = "viruses"),
             color = "red"
    )
  })
  
  output$zona2_3 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(sum(acum_zona[,4]), style = "font-size: 70%;"),
             subtitle = tags$p("Positivos por laboratorio acumulados", style = "font-size: 90%;"),
             icon = icon(name = "microscope"),
             color = "orange"
    )
  })
  
  output$zona2_4 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(sum(acum_zona[,5]), style = "font-size: 70%;"),
             subtitle = tags$p("Positivos por criterio epidemiológico acumulados", style = "font-size: 90%;"),
             icon = icon(name = "user-md"),
             color = "teal"
    )
  })
  
  output$zona2_5 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(sum(acum_zona[,6]), style = "font-size: 70%;"),
             subtitle = tags$p("Activos", style = "font-size: 90%;"),
             icon = icon(name = "viruses"),
             color = "maroon"
    )
  })
  
  output$zona2_6 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(sum(acum_zona[,7]), style = "font-size: 70%;"),
             subtitle = tags$p("Sospechosos sin determinar", style = "font-size: 90%;"),
             icon = icon(name = "user-secret"),
             color = "purple"
    )
  })
  
  output$zona2_7 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(sum(acum_zona[,8]), style = "font-size: 70%;"),
             subtitle = tags$p("Fallecidos acumulados", style = "font-size: 90%;"),
             icon = icon(name = "skull-crossbones"),
             color = "black"
    )
  })
  

  output$zona3_1 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(sum(deter_zona[,2]), style = "font-size: 70%;"),
             subtitle = tags$p("Total determinaciones acumuladas", style = "font-size: 90%;"),
             icon = icon(name = "vial"),
             color = "olive"
    )
  })
  
  output$zona3_2 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(sum(deter_zona[,3]), style = "font-size: 70%;"),
             subtitle = tags$p("Determinaciones positivas acumuladas", style = "font-size: 90%;"),
             icon = icon(name = "virus"),
             color = "maroon"
    )
  })
  
  output$zona3_3 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*sum(deter_zona[,3])/sum(deter_zona[,2]),1), "%"), style = "font-size: 70%;"),
             subtitle = tags$p("Positividad acumulada", style = "font-size: 90%;"),
             icon = icon(name = "plus"),
             color = color_posacum_zona
    )
  })
  
  output$tbl3 <-  renderReactable(

      reactable(deter_zona, 
      #minWidth = 50
      columns = list(
        partido = colDef(name = "Partido"),
        total = colDef(name = "Analizados"),
        pos = colDef(name = "Positivos"),
        porc = colDef(name = "Positividad (%)")
      ),
      defaultPageSize = 16,
      showPagination = F
    )
    )
  

  
  
  
  output$part_1 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(filter(acum_zona, partido == input$partido)[2], style = "font-size: 70%;"),
             subtitle = tags$p("Total acumulados", style = "font-size: 90%;"),
             icon = icon(name = "user"),
             color = "blue"
    )
  })
  
  output$part_2 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(filter(acum_zona, partido == input$partido)[3], style = "font-size: 70%;"),
             subtitle = tags$p("Positivos acumulados", style = "font-size: 90%;"),
             icon = icon(name = "virus"),
             color = "red"
    )
  })
  
  output$part_3 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(filter(acum_zona, partido == input$partido)[5], style = "font-size: 70%;"),
             subtitle = tags$p("Positivos por criterio epidemiológico acumulados", style = "font-size: 90%;"),
             icon = icon(name = "user-md"),
             color = "teal"
    )
  })
  
  output$part_4 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(filter(acum_zona, partido == input$partido)[6], style = "font-size: 70%;"),
             subtitle = tags$p("Activos", style = "font-size: 90%;"),
             icon = icon(name = "viruses"),
             color = "maroon"
    )
  })
  
  output$part_5 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(filter(acum_zona, partido == input$partido)[7], style = "font-size: 70%;"),
             subtitle = tags$p("Sospechosos sin determinar", style = "font-size: 90%;"),
             icon = icon(name = "user-secret"),
             color = "purple"
    )
  })
  
  output$part_6 <- renderVal <- renderValueBox({
    valueBox(value = tags$p(filter(acum_zona, partido == input$partido)[8], style = "font-size: 70%;"),
             subtitle = tags$p("Fallecidos acumulados", style = "font-size: 90%;"),
             icon = icon(name = "skull-crossbones"),
             color = "black"
    )
  })
  # output$casosActivos <- renderValueBox({
  #   valueBox(
  #     paste0(acum[2]),"Activos totales", icon = icon("virus"),
  #     color = "blue"
  #   )
  # })  
  # 
  # output$activosUltimos <- renderValueBox({
  #   valueBox(
  #     paste0(acum[3]),format(acum[4], format = "%d/%m/%Y"), icon = icon("virus"),
  #     color = "purple"
  #   )
  # })  
  # 
  #   
  # # output$Fecha <- renderValueBox({
  # #   valueBox(
  # #     format(fecha_ine, format = "%d/%m/%Y"), "Fecha", icon = icon(name = "calendar-alt"),
  # #     color = "aqua"
  # #   )
  # # })
  # 
  # output$testrealizados <- renderValueBox({
  #   valueBox(
  #     paste0(test[1]),"Test totales", icon = icon("dna"),
  #     color = "blue"
  #   )
  # })
  # 
  # output$testpositivos <- renderValueBox({
  #   valueBox(
  #     paste0(test[2]), "Test positivos", icon = icon("dna"),
  #     color = "purple"
  #   )
  # })
  # 
  # output$testUltimos <- renderValueBox({
  #   valueBox(
  #     paste0(test[3]),format(test[4], format = "%d/%m/%Y"), icon = icon("dna"),
  #     color = "purple"
  #   )
  # })
  # 
  # 
  # output$terapia <- renderValueBox({
  #   valueBox(
  #     paste0(cli_zona[3,2]), "Pacientes en terapia", icon = icon("bed"),
  #     color = "orange"
  #   )
  # })  
  # 
  # output$recuperados <- renderValueBox({
  #   valueBox(
  #     paste0(cli_zona[4,2]), "Pacientes recuperados", icon = icon("virus-slash"),
  #     color = "olive"
  #   )
  # })
  # 
  # output$fallecidos <- renderValueBox({
  #   valueBox(
  #     paste0(cli_zona[2,2]), "Pacientes fallecidos", icon = icon("skull-crossbones"),
  #     color = "navy"
  #   )
  # })  
  # 
  # output$importados <- renderValueBox({
  #   valueBox(
  #     paste0(acum[1,2], "  (",acum[2,2], "%)"), "Casos importados", icon = icon("plane"),
  #     color = "green"
  #   )
  # })  
  # 
  # output$contacto <- renderValueBox({
  #   valueBox(
  #     paste0(acum[1,3], "  (",acum[2,3], "%)"), "Casos por contacto", icon = icon("people-arrows"),
  #     color = "aqua"
  #   )
  # }) 
  # 
  # output$comunitario <- renderValueBox({
  #   valueBox(
  #     paste0(acum[1,4], "  (",acum[2,4], "%)"), "Casos comunitarios", icon = icon("users"),
  #     color = "maroon"
  #   )
  # }) 
  # 
  # output$estudio <- renderValueBox({
  #   valueBox(
  #     paste0(acum[1,5], "  (",acum[2,5], "%)"), "Casos en estudio", icon = icon("search"),
  #     color = "light-blue"
  #   )
  # }) 
  # 
  
  output$zonasexedad <- renderHighchart({
    
    sex_edad_zona <- pos_sex_edad %>% 
      group_by(grupo_edad) %>% 
      summarise(M = sum(M, na.rm = T), F = sum(F, na.rm = T)) %>% 
      arrange(grupo_edad) 
    
    categorias <- sex_edad_zona %>%
      drop_na() %>% 
      pull(grupo_edad)
    
    hombres <- sex_edad_zona %>% 
      drop_na() %>%
      mutate(M = M*-1) %>% 
      pull(M)
    
    mujeres <- sex_edad_zona %>% 
      drop_na() %>%
      pull(F)
    
    # Definir opciones de exportación
    export <- list(
      list(
        text = "PNG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
      ),
      list(
        text = "JPEG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
      ),
      list(
        text = "SVG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
      ),
      list(
        text = "PDF",
        onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
      )
    )
    
    hc <- highchart() %>% 
      hc_chart(type= 'bar') %>%
      
      hc_xAxis(
        list(categories=categorias,reversed=FALSE,labels=list(step= 1)),
        list(categories= categorias,opposite= TRUE,reversed= FALSE,linkedTo= 0,labels=list(step= 1))) %>%
      
      hc_tooltip(
        shared = FALSE,
        formatter = JS("function () {
                       return this.point.category + '<br/>' +
                       '<b>' + this.series.name + '</b> ' +
                       Highcharts.numberFormat(Math.abs(this.point.y),0);}")
      )%>%
      
      hc_yAxis(title= list(text= "Frecuencia"),
               labels=list(formatter=JS("function () {
                   return Math.abs(this.value);
                 }")))%>%
      
      hc_plotOptions(series=list(stacking= 'normal'),
                     bar = list(groupPadding = 0,
                                pointPadding = 0)) %>%
      
      hc_series(
        list(name= 'Hombres',
             data= hombres, 
             color = "olivedrab"),
        
        list(name= 'Mujeres',
             data= mujeres,
             color = "purple"))  
    
    hc %>% hc_title(text = "Distribución de casos COVID-19 por edad y sexo") %>% 
      hc_subtitle(text = "Zona Sanitaria VIII") %>%
      hc_caption(
        text = "<b>Fuente:</b> Base SISA publica abierta - Ministerio de Salud de la Nación") %>% 
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(target = "_blank"),
        buttons = list(contextButton = list(
          symbol = "menu",
          theme = list(fill = "transparent"),
          align = "right",
          verticalAlign = "top",
          width = 20,
          menuItems = export
        ))
      )
    
  })
  
  output$zonafalle_se <- renderHighchart({
    
    falle_sex_edad_zona <- fallecidos %>% 
      filter(clasificacion_resumen == "Confirmado", sexo != "NR") %>% 
      count(sexo,grupo_edad) %>% 
      pivot_wider(names_from = sexo, values_from = n) %>% 
      replace_na(list(F = 0, M = 0)) %>% arrange(grupo_edad) 
    
    
    categorias <- falle_sex_edad_zona %>%
      drop_na() %>% 
      pull(grupo_edad)
    
    hombres <- falle_sex_edad_zona %>% 
      drop_na() %>%
      mutate(M = M*-1) %>% 
      pull(M)
    
    mujeres <- falle_sex_edad_zona %>% 
      drop_na() %>%
      pull(F)
    
    # Definir opciones de exportación
    export <- list(
      list(
        text = "PNG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
      ),
      list(
        text = "JPEG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
      ),
      list(
        text = "SVG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
      ),
      list(
        text = "PDF",
        onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
      )
    )
    
    hc <- highchart() %>% 
      hc_chart(type= 'bar') %>%
      
      hc_xAxis(
        list(categories=categorias,reversed=FALSE,labels=list(step= 1)),
        list(categories= categorias,opposite= TRUE,reversed= FALSE,linkedTo= 0,labels=list(step= 1))) %>%
      
      hc_tooltip(
        shared = FALSE,
        formatter = JS("function () {
                       return this.point.category + '<br/>' +
                       '<b>' + this.series.name + '</b> ' +
                       Highcharts.numberFormat(Math.abs(this.point.y),0);}")
      )%>%
      
      hc_yAxis(title= list(text= "Frecuencia"),
               labels=list(formatter=JS("function () {
                   return Math.abs(this.value);
                 }")))%>%
      
      hc_plotOptions(series=list(stacking= 'normal'),
                     bar = list(groupPadding = 0,
                                pointPadding = 0)) %>%
      
      hc_series(
        list(name= 'Hombres',
             data= hombres, 
             color = "navy"),
        
        list(name= 'Mujeres',
             data= mujeres,
             color = "salmon"))  
    
    hc %>% hc_title(text = "Distribución de fallecidos COVID-19 por edad y sexo") %>% 
      hc_subtitle(text = "Zona Sanitaria VIII") %>%
      hc_caption(
        text = "<b>Fuente:</b> Base SISA publica abierta - Ministerio de Salud de la Nación") %>% 
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(target = "_blank"),
        buttons = list(contextButton = list(
          symbol = "menu",
          theme = list(fill = "transparent"),
          align = "right",
          verticalAlign = "top",
          width = 20,
          menuItems = export
        ))
      )
    
  })
  
   output$sexedad <- renderHighchart({
  
    data_sexedad <-  pos_sex_edad %>%
      drop_na() %>% 
      filter(partido == input$partido) %>% 
      arrange(grupo_edad)
     
    categorias <- data_sexedad %>%
        pull(grupo_edad)
      
      hombres <- data_sexedad %>% 
        mutate(M = M*-1) %>% 
        pull(M)
      
      mujeres <- data_sexedad %>% 
        pull(F)
    
      # Definir opciones de exportación
      export <- list(
        list(
          text = "PNG",
          onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
        ),
        list(
          text = "JPEG",
          onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
        ),
        list(
          text = "SVG",
          onclick = JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
        ),
        list(
          text = "PDF",
          onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
        )
      )
        
   hc <- highchart() %>% 
        hc_chart(type= 'bar') %>%
        
        hc_xAxis(
          list(categories=categorias,reversed=FALSE,labels=list(step= 1)),
          list(categories= categorias,opposite= TRUE,reversed= FALSE,linkedTo= 0,labels=list(step= 1))) %>%
        
        hc_tooltip(
          shared = FALSE,
          formatter = JS("function () {
                       return this.point.category + '<br/>' +
                       '<b>' + this.series.name + '</b> ' +
                       Highcharts.numberFormat(Math.abs(this.point.y),0);}")
        )%>%
        
        hc_yAxis(title= list(text= "Frecuencia"),
                 labels=list(formatter=JS("function () {
                   return Math.abs(this.value);
                 }")))%>%
        
        hc_plotOptions(series=list(stacking= 'normal'),
                       bar = list(groupPadding = 0,
                                     pointPadding = 0)) %>%
        
        hc_series(
          list(name= 'Hombres',
               data= hombres, 
               color = "olivedrab"),
          
          list(name= 'Mujeres',
               data= mujeres,
               color = "purple"))  
   
   hc %>% hc_title(text = "") %>% 
     hc_subtitle(text = as.character(input$partido)) %>%
     hc_caption(
       text = "<b>Fuente:</b> Base SISA publica abierta - Ministerio de Salud de la Nación") %>% 
     hc_exporting(
       enabled = TRUE,
       formAttributes = list(target = "_blank"),
       buttons = list(contextButton = list(
         symbol = "menu",
         theme = list(fill = "transparent"),
         align = "right",
         verticalAlign = "top",
         width = 20,
         menuItems = export
       ))
     )
   
   })
   
   
   
  output$posi <- renderHighchart({
  
   positivos <- pos_acum %>% 
     mutate(sepi = epiweek(fecha),
            yepi = epiyear(fecha)) %>% 
     group_by(partido, yepi, sepi) %>% 
     summarise(Casos = sum(casos, na.rm = T)) %>% 
     filter(partido == input$partido) %>% 
     ungroup() %>% 
     arrange(yepi,sepi) %>% 
        mutate(Acumulados = cumsum(Casos),
               sepi = paste0(yepi, " / ", sepi)) 
   
   # Definir opciones de exportación
   export <- list(
     list(
       text = "PNG",
       onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
     ),
     list(
       text = "JPEG",
       onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
     ),
     list(
       text = "SVG",
       onclick = JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
     ),
     list(
       text = "PDF",
       onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
     )
   )
   
   
hc <- highchart() %>% 
     hc_xAxis(type = "linear", categories = positivos$sepi, labels = list(step = 5, rotation = -45), title = list(text = "Semana epidemiológica")) %>% 
     hc_yAxis_multiples(list(title = list(text = "Nro. de confirmados"), opposite = F),
                        list(type = "logarithmic", title = list(text = "Acumulados (log)"), opposite = T)) %>%
     hc_plotOptions(line = list(dataLabels = list(enabled = F),
                                enableMouseTracking = T),
                    column = list(groupPadding = 0,
                                  pointPadding = 0)) %>% 
     hc_add_series(positivos, "column", hcaes(x = sepi, y = Casos), color = "maroon", name = "Positivos x sepi", yAxis = 0) %>% 
     hc_add_series(positivos, "spline", hcaes(x = sepi, y = Acumulados), color = "olivedrab", name = "Positivos acumulados", yAxis = 1) %>% 
     hc_tooltip(table = TRUE,
                sort = T) 

hc %>% 
     hc_title(text = "") %>% 
     hc_subtitle(text = as.character(input$partido)) %>%
     hc_caption(
       text = "<b>Fuente:</b> Base SISA publica abierta - Ministerio de Salud de la Nación") %>%
  hc_exporting(
     enabled = TRUE,
     formAttributes = list(target = "_blank"),
     buttons = list(contextButton = list(
       symbol = "menu",
       theme = list(fill = "transparent"),
       align = "right",
       verticalAlign = "top",
       width = 20,
       menuItems = export
     ))
   )
})
  
  
  output$diario_partido <- renderHighchart({
    
    acum <- pos_acum %>% 
      arrange(fecha) %>%
      filter(partido == input$partido) %>% 
      select(-partido, -acumulados)
    
    
    inicio <- acum$fecha[1]
    final <- acum$fecha[length(acum$fecha)]
    
    casos2 <- tibble(
      fecha = seq.Date(from = inicio, to = final, by = "day")
    )  
    
    casos2 <- casos2 %>% left_join(acum)
    
    casos2 <- casos2 %>%  mutate(casos = replace_na(casos, 0),
                                 acumulados = cumsum(casos))
    
    
    casos2$media <- 0
    
    x <- 6
    
    for(i in 1:(nrow(casos2)-6)) {
      
      j = i + 6
      x = x + 1
      casos2[x,"media"] <- trunc(sum(casos2[i:j,2])/7)
    }
    
    casos2[(length(casos2$media)-6):length(casos2$media),]$media <- NA
    
    var1 <- input$seleccion_fecha1[1]
    var2 <- input$seleccion_fecha1[2]
    
    # Definir opciones de exportación
    export <- list(
      list(
        text = "PNG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
      ),
      list(
        text = "JPEG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
      ),
      list(
        text = "SVG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
      ),
      list(
        text = "PDF",
        onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
      )
    )
    
    
   hc <- highchart() %>%
     hc_xAxis(type = "datetime", labels = list(rotation = 90, step = 1, padding = 1)) %>%
     hc_yAxis(title = list(text = "Frecuencia")) %>%
     hc_add_series(dplyr::filter(casos2, between(fecha, var1, var2)), "column", hcaes(x = fecha, y = casos), color = "steelblue", name = "Casos diarios") %>%
     hc_add_series(dplyr::filter(casos2, between(fecha, var1, var2)), "line", hcaes(x = fecha, y = media), name = "Media 7 días", color = "maroon") %>%
      hc_plotOptions(line = list(dataLabels = list(enabled = F),
                                 enableMouseTracking = T),
                     column = list(groupPadding = 0,
                                   pointPadding = 0))
  
  hc %>%
    hc_title(text = "") %>%
    hc_subtitle(text = as.character(input$partido))  %>%
    hc_caption(
      text = "<b>Fuente:</b> Base SISA publica abierta - Ministerio de Salud de la Nación") %>%
    hc_exporting(
      enabled = TRUE,
      formAttributes = list(target = "_blank"),
      buttons = list(contextButton = list(
        symbol = "menu",
        theme = list(fill = "transparent"),
        align = "right",
        verticalAlign = "top",
        width = 20,
        menuItems = export
      ))
    )
    
  })
  
  output$diario_zona <- renderHighchart({
    
    acumz <- pos_acum %>% 
      group_by(fecha) %>%
      summarise(casos = sum(casos, na.rm = T)) 
    
    
    inicio <- acumz$fecha[1]
    final <- acumz$fecha[length(acumz$fecha)]
    
    casos2z <- tibble(
      fecha = seq.Date(from = inicio, to = final, by = "day")
    )  
    
    casos2z <- casos2z %>% left_join(acumz)
    
    casos2z <- casos2z %>%  mutate(casos = replace_na(casos, 0),
                                 acumulados = cumsum(casos))
    
    
    casos2z$media <- 0
    
    x <- 6
    
    for(i in 1:(nrow(casos2z)-6)) {
      
      j = i + 6
      x = x + 1
      casos2z[x,"media"] <- trunc(sum(casos2z[i:j,2])/7)
    }
    
    casos2z[(length(casos2z$media)-6):length(casos2z$media),]$media <- NA
    
    var1 <- input$seleccion_fecha[1]
    var2 <- input$seleccion_fecha[2]
    
    # Definir opciones de exportación
    export <- list(
      list(
        text = "PNG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
      ),
      list(
        text = "JPEG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
      ),
      list(
        text = "SVG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
      ),
      list(
        text = "PDF",
        onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
      )
    )
    
    
    hc <- highchart() %>%
      hc_xAxis(type = "datetime", labels = list(rotation = 90, step = 1, padding = 1, xDateFormat = '%d-%m-%Y')) %>%
      hc_yAxis(title = list(text = "Frecuencia")) %>%
      hc_add_series(dplyr::filter(casos2z, between(fecha, var1, var2)), "column", hcaes(x = fecha, y = casos), color = "steelblue", name = "Casos diarios") %>%
      hc_add_series(dplyr::filter(casos2z, between(fecha, var1, var2)), "line", hcaes(x = fecha, y = media), name = "Media 7 días", color = "maroon") %>%
      hc_plotOptions(line = list(dataLabels = list(enabled = F),
                                 enableMouseTracking = T),
                     column = list(groupPadding = 0,
                                   pointPadding = 0)) 
        
       
    
    hc %>%
      hc_title(text = "Casos diarios y media movil 7 días") %>%
      hc_subtitle(text = "Zona Sanitaria VIII - Año 2020-2021")  %>%
      hc_caption(
        text = "<b>Fuente:</b> Base SISA publica abierta - Ministerio de Salud de la Nación") %>%
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(target = "_blank"),
        buttons = list(contextButton = list(
          symbol = "menu",
          theme = list(fill = "transparent"),
          align = "right",
          verticalAlign = "top",
          width = 20,
          menuItems = export
        ))
      )
    
  })
  # output$descarga1 <- downloadHandler(filename ="grafico1.png",
  #                                         content = function(file) {
  #                                           png(file)
  #                                           output$sexedad
  #                                           dev.off()
  #                                         },
  #                                         contentType = "image/png")
  # 
  
  output$mapa1 <- renderTmap({


    datos_mapa <- datos_mapa %>% dplyr::select(input$info) %>% pull()
    
    etiqueta_mapa <- case_when(
      input$info == "Casos" ~ "Casos positivos acumulados",
      input$info == "Activos" ~ "Casos positivos activos",
      input$info == "Incidencia" ~ "Incidencia acumulada <br> x 100.000 hab.",
      input$info == "Fallecidos" ~ "Fallecidos acumulados",
      input$info == "Mortalidad" ~ "Tasa de mortalidad bruta <br> x 1.000.000 hab.",
      input$info == "Mortalidad_aj" ~ "Tasa de mortalidad ajus. <br> x 1.000.000 hab."
    )
    
    color_mapa <- case_when(
      input$info == "Casos" ~ "YlOrRd",
      input$info == "Activos" ~ "YlOrBr",
      input$info == "Incidencia" ~ "YlGnBu",
      input$info == "Fallecidos" ~"OrRd",
      input$info == "Mortalidad" ~ "PuBu",
      input$info == "Mortalidad_aj" ~ "YlGnBu"
    )
    
    popu <- case_when(
      input$info == "Casos" ~ "Casos",
      input$info == "Activos" ~ "Activos",
      input$info == "Incidencia" ~ "Incidencia",
      input$info == "Fallecidos" ~"Fallecidos",
      input$info == "Mortalidad" ~ "Mortalidad",
      input$info == "Mortalidad_aj" ~ "Mortalidad_aj"
    )
    
    xx <- c("Casos", "Laboratorio", "Epidemiologia", "Sospechosos")
    xxx <- c("Fallecidos", "Letalidad")
    
    
    if (popu == "Casos") {
      popu <- xx
    }

    if (popu == "Fallecidos") {
      popu <- xxx
    }
  
  #   bins <- c(0,getJenksBreaks(datos_mapa, 5), +Inf)
  # #  bins <- c(0,seq(from = min(datos_mapa), to = max(datos_mapa), length.out = 3), +Inf)
  #   pal <- colorBin(color_mapa, domain = datos_mapa, bins = bins)
  #   
  #   labels <- sprintf(
  #     "<strong>%s</strong><br/>%g casos</sup>",
  #     mapa_parti$NAME_2, datos_mapa
  #   ) %>% lapply(htmltools::HTML)
    
    tm_basemap("OpenStreetMap") +
      tm_shape(mapa_parti) +
      tm_polygons(col = input$info,
                  alpha = 0.7,
                  palette = color_mapa,
                  style = "cont",
                  #breaks = c(0,5,10,15,20),
                  title = etiqueta_mapa,
                  textNA = "Sin dato",
                  id = "Partido",
                  popup.vars = popu,
                  legend.format=list(big.mark = ".", decimal.mark = ",", small.interval = 3),
                  popup.format=list(big.mark = ".", decimal.mark = ","),
      
                  
      ) +
      tm_scale_bar(position=c("right", "bottom")) 
    
  })
  # 
  # 
  # 
  # output$tabla1 <- renderDT(
  #   tabla1 %>% 
  #     datatable(rownames = FALSE, 
  #               colnames = c('Partido', 'Positivos acumulados', 'Positivos activos', 'Internados', 'UCI', 'Recuperados',  'Fallecidos'), 
  #               class = 'cell-border stripe',  
  #               extensions = 'Buttons',
  #               options = list(autoWidth = TRUE, 
  #                              pageLength = 16,
  #                              dom = 'Bfrtip',
  #                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>% 
  #     formatStyle('Partido',  color = 'white', backgroundColor = 'steelblue', fontWeight = 'bold')
  # )
  # 
  
  output$inc_zona <- renderHighchart({
    
    # Definir opciones de exportación
    export <- list(
      list(
        text = "PNG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
      ),
      list(
        text = "JPEG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
      ),
      list(
        text = "SVG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
      ),
      list(
        text = "PDF",
        onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
      )
    )
    
    inc <- acum_zona %>% arrange(desc(Incidencia))  
    
    hc <- highchart() %>%
      hc_xAxis(type = "category", labels = list(rotation = 45, step = 1, padding = 1)) %>%
      hc_yAxis(title = list(text = "Incidencia acumulada c/100.000 hab.")) %>%
      hc_add_series(inc, "column", hcaes(x = partido, y = Incidencia), name = 'Incidencia', color = "seagreen") %>% 
      hc_plotOptions(column = list(enableMouseTracking = T),
                     lang = list(decimalPoint = ",",
                                 thousandsSep = "."))
    
    hc %>%
      hc_title(text = "Incidencia acumulada cada 100.000 habitantes por partido") %>%
      hc_subtitle(text = "Zona Sanitaria VIII - Año 2020-2021")  %>%
      hc_caption(
        text = "<b>Fuente:</b> Base SISA publica abierta - Ministerio de Salud de la Nación") %>%
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(target = "_blank"),
        buttons = list(contextButton = list(
          symbol = "menu",
          theme = list(fill = "transparent"),
          align = "right",
          verticalAlign = "top",
          width = 20,
          menuItems = export
        ))
      )
    
  })
  
  output$mort_zona <- renderHighchart({
    
    # Definir opciones de exportación
    export <- list(
      list(
        text = "PNG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
      ),
      list(
        text = "JPEG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
      ),
      list(
        text = "SVG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
      ),
      list(
        text = "PDF",
        onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
      )
    )
    
    mort <- acum_zona %>% arrange(desc(Mortalidad))  
    
    hc <- highchart() %>%
      hc_xAxis(type = "category", labels = list(rotation = 45, step = 1, padding = 1)) %>%
      hc_yAxis(title = list(text = "Mortalidad específica c/1.000.000 hab.")) %>%
      hc_add_series(mort, "column", hcaes(x = partido, y = Mortalidad), name = 'Mortalidad', color = "royalblue") %>% 
      hc_plotOptions(column = list(enableMouseTracking = T),
                     lang = list(decimalPoint = ",",
                                 thousandsSep = "."))
    
    hc %>%
      hc_title(text = "Tasa de mortalidad por 1.000.000 habitantes por partido") %>%
      hc_subtitle(text = "Zona Sanitaria VIII - Año 2020-2021")  %>%
      hc_caption(
        text = "<b>Fuente:</b> Base SISA publica abierta - Ministerio de Salud de la Nación") %>%
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(target = "_blank"),
        buttons = list(contextButton = list(
          symbol = "menu",
          theme = list(fill = "transparent"),
          align = "right",
          verticalAlign = "top",
          width = 20,
          menuItems = export
        ))
      )
    
  })
  
  
  output$mort_zona_aj <- renderHighchart({
    
    # Definir opciones de exportación
    export <- list(
      list(
        text = "PNG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
      ),
      list(
        text = "JPEG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
      ),
      list(
        text = "SVG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
      ),
      list(
        text = "PDF",
        onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
      )
    )
    
    mort <- acum_zona %>% arrange(desc(Mortalidad_aj))  
    
    hc <- highchart() %>%
      hc_xAxis(type = "category", labels = list(rotation = 45, step = 1, padding = 1)) %>%
      hc_yAxis(title = list(text = "Mortalidad ajustada c/1.000.000 hab.")) %>%
      hc_add_series(mort, "column", hcaes(x = partido, y = Mortalidad_aj), name = 'Mortalidad ajustada', color = "saddlebrown") %>% 
      hc_plotOptions(column = list(enableMouseTracking = T),
                     lang = list(decimalPoint = ",",
                                 thousandsSep = "."))
    
    hc %>%
      hc_title(text = "Tasa de mortalidad ajustada por 1.000.000 habitantes por partido") %>%
      hc_subtitle(text = "Zona Sanitaria VIII - Año 2020-2021")  %>%
      hc_caption(
        text = "<b>Fuente:</b> Base SISA publica abierta - Ministerio de Salud de la Nación") %>%
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(target = "_blank"),
        buttons = list(contextButton = list(
          symbol = "menu",
          theme = list(fill = "transparent"),
          align = "right",
          verticalAlign = "top",
          width = 20,
          menuItems = export
        ))
      )
    
  })
  
  
  output$tree <- renderHighchart({
  
    options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 0)))
    
   tree <-  diag_diario %>% 
      mutate(porc = round(positivos/sum(positivos)*100,2)) %>% 
        treemap(index = "partido",
            vSize = "positivos", vColor = "porc",
            type = "value", palette = "YlOrRd", draw = F)
    
    hctreemap(tree, allowDrillToNode = TRUE) %>% 
      hc_title(text = paste0("Distribución de nuevos casos positivos por partido - ", toString(format(ultimo_zona$fecha[1], format = "%d %b %Y")))) %>% 
      hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                             Casos: {point.value:,.0f}<br>
                             Proporción: {point.valuecolor:,.2f} %")

  })
    
  
  output$inc_partidos <- renderHighchart({
    
    # Definir opciones de exportación
    export <- list(
      list(
        text = "PNG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
      ),
      list(
        text = "JPEG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
      ),
      list(
        text = "SVG",
        onclick = JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
      ),
      list(
        text = "PDF",
        onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
      )
    )
  
    acum2 <- pos_acum %>%
      arrange(partido) %>% 
      select(-acumulados) %>%
      pivot_wider(names_from = partido, values_from = casos) 
    
    
    inicio <- min(acum2$fecha)
    final <- max(acum2$fecha)
    
    casos3 <- tibble(
      fecha = seq.Date(from = inicio, to = final, by = "day")
    )  
    
    casos3 <- casos3 %>% left_join(acum2)
    
    casos3 <- casos3 %>%  mutate(across(where(is.numeric), ~replace_na(.x, replace = 0)))
    
    casos3 <- casos3 %>%  mutate(across(where(is.numeric), ~cumsum(.x)))
    
    for (i in 1:16) {
      
      j = i+1
      
      casos3[,j] <- round(casos3[,j]/pob$Poblacion_2020[i]*100000,1)
      
    }
    
    casos3 <- casos3 %>% filter(fecha > as.Date("2020-05-31"))
    
    casos3 <- casos3 %>%  pivot_longer(2:17, names_to = "Partido", values_to = "Incidencia")  
    
    hc <-  hchart(casos3, "line", hcaes(x = fecha, y = Incidencia, group = Partido)) %>% 
      hc_plotOptions(line = list(dataLabels = list(enabled = F),
                                 enableMouseTracking = T),
                     
                     lang = list(decimalPoint = ",",
                                 thousandsSep = ".")) %>% 
      hc_tooltip(table = T, sort = T)
    
    hc %>% hc_title(text = "Evolución de la incidencia acumulada por 100.000 habitantes comparativa según partido") %>%
      hc_subtitle(text = "Partidos de la Zona Sanitaria VIII - Año 2020-2021")  %>%
      hc_caption(
        text = "<b>Fuente:</b> Base SISA publica abierta - Ministerio de Salud de la Nación -
                (se muestran valores desde junio/2020 cuando comenzó el incremento de casos") %>%
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(target = "_blank"),
        buttons = list(contextButton = list(
          symbol = "menu",
          theme = list(fill = "transparent"),
          align = "right",
          verticalAlign = "top",
          width = 20,
          menuItems = export
        ))
      ) 
    
    
    
    
  })
    # output$tbl1 <-  renderDT(
    #   principal %>% 
    #     filter(clasificacion_resumen == "Confirmado") %>% 
    #     select(residencia_departamento_nombre, fecha) %>% 
    #     rename(Partido = "residencia_departamento_nombre", 
    #            Fecha = "fecha") %>% 
    #     count(Partido, Fecha, name = "Cantidad") %>% 
    #     mutate(Partido  = as.factor(Partido))  %>% 
    #     datatable(rownames = F, 
    #               width = "100%",
    #               height = "100%",
    #               #extensions = 'Scroller',
    #               filter = list(position = 'top'),
    #               style = "bootstrap4",
    #               #colnames = c('Partido', 'Fecha', 'SEpi', 'Sexo', 'Grupo etario', 'Clasificación', 'Activo', 'Fallecido', 'Fecha fallecimiento', 'Clasificación resumen'),
    #               class = 'display cell-border stripe',
    #               options = list(autoWidth = TRUE, 
    #                              pageLength = 15,
    #                              language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    #                              searchHighlight = F
    #                              #scrollX = T
    #                              #deferRender = TRUE,
    #                              #scrollY = 500,
    #                              #scroller = TRUE
    #                              
    #               )) %>% 
    #     formatStyle('Partido', fontWeight = 'bold'
    #                 
    #     )
    #   
    # )
    # 
    # individuales <- principal %>% 
    #   filter(clasificacion_resumen == "Confirmado") %>% 
    #   select(residencia_departamento_nombre, fecha) %>% 
    #   rename(Partido = "residencia_departamento_nombre", 
    #          Fecha = "fecha") %>% 
    #   count(Partido, Fecha, name = "Cantidad") %>% 
    #   mutate(Partido  = as.factor(Partido))
    # 
    # output$descarga <- downloadHandler(
    #   filename = function(){"casos_diarios.csv"},
    #   content = function(file){
    #     write.csv(individuales, file)
    #   }
    # )
    # 
    # output$tbl2 <-  renderDT(
    #   acum_zona %>% 
    #     select(-Poblacion_2020) %>% 
    #     rename(Partido = "partido")  %>% 
    #     datatable(rownames = F, 
    #               #colnames = c('Partido', 'Fecha', 'SEpi', 'Sexo', 'Grupo etario', 'Clasificación', 'Activo', 'Fallecido', 'Fecha fallecimiento', 'Clasificación resumen'),
    #               class = 'cell-border stripe',
    #               style = "bootstrap4",
    #               width = "100vw",
    #               extensions = c('Buttons','Scroller'),
    #               options = list(autoWidth = TRUE, 
    #                              pageLength = 16,
    #                              dom = 'Bfrtip',
    #                              buttons = list(
    #                                list(
    #                                  extend = 'csv',
    #                                  title = 'acumulados'), 
    #                                list(
    #                                  extend = 'excel',
    #                                  title = 'acumulados'),
    #                                list(
    #                                  extend = 'pdf',
    #                                  title = 'acumulados'),
    #                                list(
    #                                  extend = 'copy',
    #                                  text = "Copiar"),
    #                                list(
    #                                  extend = 'print',
    #                                  text = "Imprimir")
    #                              ),
    #                              scrollX = TRUE,
    #                              language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    #                              #deferRender = TRUE,
    #                              #scrollY = 500,
    #                              pageLength = 16
    #                              #scroller = TRUE,
    #                               # declare titles
    #                              )) %>% 
    #     formatStyle('Partido', fontWeight = 'bold') %>% 
    #     formatStyle(
    #       'Incidencia',
    #       color = styleInterval(c(1000, 3000), c('green', 'orange', 'red'))
    #     )
    # )
    # 
    # output$tbl3 <-  renderReactable(
    # 
    #     reactable(mfalle, defaultColDef = colDef(
    #     style = function(value) {
    #       if (!is.numeric(value)) return()
    #       normalized <- (value - min(mfalle)) / (max(mfalle) - min(mfalle))
    #       color <- GnYlRd(normalized)
    #       list(background = color)
    #     },
    #     minWidth = 50
    #   ),
    #   columns = list(
    #     .rownames = colDef(name = "Partido", sortable = TRUE, align = "left")
    #   ),
    #   bordered = TRUE,
    #   width = "100%",
    #   fullWidth = T,
    #   pagination = F
    #   ) 
    # )
    # 
    
  
}

server <- function(input, output, session) {

  output$image <- renderUI({
    dens <- input$density
    j <- input$app_use
    C0 <- input$contact_rate
    if(C0=='NA'){
      v <- input$velocity
    }else{
      v <- 'NA'
    } 
    f <- paste0('j_',j,'_c0_',C0,'_v_',v,'_dens_',dens)
    img(src=paste0(f,'.png'))
  })

  observeEvent(input$contact_rate, {
    if(input$contact_rate != 'NA'){
      shinyjs::disable("velocity")
    }else{
      updateRadioButtons(session, "velocity", label = "Velocity (km/day) (source: Spanish mobility):", 
                                choices = c(1.5, 2.5, 5.2), selected = 1.5, inline=T)
    }
  })


  output$table <- renderTable({
    dens <- input$density
    j <- input$app_use
    C0 <- input$contact_rate
    if(C0=='NA'){
      v <- input$velocity
    }else{
      v <- 'NA'
    } 
    f <- paste0('stats/j_',j,'_c0_',C0,'_v_',v,'_dens_',dens)
    table <- read.table(paste0(f,'.txt'),sep='\t',header=T)
  })


}
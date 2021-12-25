#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  
  output$data_netflix_title <- renderDT({DT_netflix_titles})
  
  output$g1 <- renderAmCharts({g1})
  
  output$g2 <-  renderPlotly({ g2() })
  
  output$g3 <- renderAmCharts({
    g3(input$genre_choice_g3)
  })
  
  output$g7 <- renderPlotly({
    g7()
  })
  
  output$title_c1_c2 <- renderText({
    input$go_staticmap
    isolate({
      if(input$scale_c1=="normal"){ 
        sprintf("Content from different countries (%s, %s) : ", input$type_choice_c1_c2,input$genre_choice_c1_c2)
      } else if(input$scale_c1=="sqrt"){
        sprintf("Content from different countries (%s, %s) - sqrt scale :",input$type_choice_c1_c2,input$genre_choice_c1_c2)
      }
    })    
  })
  
  output$c1_c2 <- renderPlot({
    input$go_staticmap
    isolate({
      switch(
        input$scale_c1,
        "normal" = c1(input$type_choice_c1_c2, input$genre_choice_c1_c2),
        "sqrt" = c2(input$type_choice_c1_c2, input$genre_choice_c1_c2)
      )
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      title_to_file_name <- function(title){
        file_name <- str_to_title(title)
        file_name <- str_replace_all(file_name,"[:blank:]","_")
        file_name <- str_replace_all(file_name,"[:punct:]","_")
        return(file_name)
      }
      title = title_to_file_name(sprintf("map %s %s %s",input$type_choice_c1_c2,input$genre_choice_c1_c2,input$scale_c1))
      title = paste(title,".png",sep = "")
      #"map.png"
    },
    content = function(file) {
      png(file)
      
      if(input$scale_c1=="normal"){
        static_map = c1(input$type_choice_c1_c2, input$genre_choice_c1_c2) +    
          ggtitle(sprintf("Content from different countries (%s, %s) : ", input$type_choice_c1_c2,input$genre_choice_c1_c2)) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
            #plot.background = element_rect(fill = "black"),
          )
      } else if(input$scale_c1=="sqrt"){
        static_map = c2(input$type_choice_c1_c2, input$genre_choice_c1_c2) +    
          ggtitle(sprintf("Content from different countries (%s, %s) - sqrt scale :",input$type_choice_c1_c2,input$genre_choice_c1_c2)) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
            #plot.background = element_rect(fill = "black"),
          )
      }
      print(static_map)
      dev.off()
    }
  )
  
  output$title_c3 <- renderText({
    input$go_interactive_map
    isolate({
      sprintf("Content from different countries (%s, %s) : ", input$type_choice_c3,input$genre_choice_c3)
    })    
  })
  
  output$c3 <-renderLeaflet({
    input$go_interactive_map
    isolate({
      c3(input$type_choice_c3, input$genre_choice_c3)
    })
  })
  
  
  
  output$g4 <- renderAmCharts({
    input$go_g4
    isolate({
      g4(input$type_choice_g4, input$genre_choice_g4)
    })
  })
  
  output$g14 <- renderPlotly({g14()})
  
  output$g6 <- renderPlotly({
    input$go_g6
    isolate({
      g6(input$genre_choice_g6,input$year_choice_g6)
    })
  })
  
  output$g8 <- renderPlotly({ g8() })
  
  output$g9 <- renderPlotly({ g9() })
  
  output$g11 <- renderPlotly({
    g11(input$n_g11)
  })
  
  output$g12 <- renderPlotly({
    input$go_g12
    isolate({
      g12(input$n_g12,input$type_choice_g12, input$country_choice_g12)
    })
  })
  
  output$g13 <-renderPlotly({
    input$go_g13
    isolate({
      g13(input$n_g13,input$type_choice_g13,input$country_choice_g13)
    })
  })
  
  output$g15 <- renderPlotly({
    switch(
      input$type_g15_g16_g17,
      "Total" = c1(input$type_choice_c1_c2, input$genre_choice_c1_c2),
      "Movie" = c2(input$type_choice_c1_c2, input$genre_choice_c1_c2)
      
    )
    g15(input$type_g15_g16_g17)
  })
  
  output$g16 <- renderPlotly({ g16() })
  
  output$g17 <- renderPlotly({  g17() })
  
  output$g18 <- renderPlotly({ g18(input$period_choice_age_oc[1],input$period_choice_age_oc[2]) })
  
  output$g19 <- renderPlotly({ g19(input$period_choice_age_oc[1],input$period_choice_age_oc[2]) })
  
  output$g20 <- renderPlotly({ g20(input$period_choice_age_oc[1],input$period_choice_age_oc[2]) })
  
  output$g21 <- renderPlotly({ g21(input$period_choice_age_oc[1],input$period_choice_age_oc[2]) })
  
  output$g22 <- renderPlotly({ g22() })
  
  output$g23 <- renderPlotly({ g23() })
  
  output$g24 <- renderPlotly({ g24() })
  
  output$g25 <- renderPlotly({ g25() })
  
  output$g26 <- renderPlotly({ g26() })
  
  output$g27 <- renderPlotly({ g27() })
  
  output$g28 <- renderPlotly({ g28() })
  
  output$g29 <- renderPlotly({ g29() })
  
  output$r1 <- renderPlotly({ r1() })
  
  output$summary_r1 <- renderPrint({
    summary(lm(IMDb~`Rotten Tomatoes`,data=rating))
  })
  
  output$r2 <- renderPlotly({
    r2(input$rating_method_r2)
  })
  
  output$summary_r2 <- renderPrint({
    if(input$rating_method_r2 == "IMDb"){
      summary(lm(IMDb~Year,data=rating))
    } else if (input$rating_method_r2=="Rotten Tomatoes"){
      summary(lm(`Rotten Tomatoes`~Year,data=rating))
    }
  })
  
  output$search_engine <- renderDT({
    input$go_search
    isolate({
      search_engine(input$search_engine_title,input$type_choice_se,input$period_choice_SE[1],input$period_choice_SE[2])
    })
  })
  
  output$top20_netflix <- renderDT({df_top_netflix})
  
  output$top20_pv <- renderDT({df_top_pv})
  
  output$top20_hulu <- renderDT({df_top_hulu})
  
  output$top20_disney <- renderDT({df_top_dis})
  
  
})

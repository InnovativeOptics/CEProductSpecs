library(tidyverse)
library(plotly)
library(shiny)

table_data <- readxl::read_excel("Technical_Information.xlsx") %>% 
    select(Product, `CE Specs`) 

specsdata <- readxl::read_excel("prod_specs_core_data.xlsx") %>% 
  pivot_wider(names_from = opmode,
              values_from = LB)

# Define server logic required to draw a histogram
function(input, output, session) {
  prod_specs <- eventReactive(input$run,{
    req(input$frame)
    req(input$lens)
    # filter to focus on the product
    specsdata_frame1 <- specsdata %>% 
      filter(product == input$frame) 
    
    specsdata_lens1 <- specsdata %>% 
      filter(product == input$lens)
    
    # minimum of maximums - right boundary
    min_of_max <- min(c(max(specsdata_frame1$wlend), max(specsdata_lens1$wlend)))
    # maximum of minimums - left boundary
    max_of_min <- max(c(min(specsdata_frame1$wlbegin), min(specsdata_lens1$wlbegin)))
    
    specsdata_frame <- specsdata_frame1 %>%
      mutate("wlbegin" = if_else(wlbegin >= max_of_min, wlbegin, 
                                 if_else(wlend >= min_of_max, wlbegin, max_of_min)),
             "wlend" = if_else(wlend <= max_of_min, wlend, 
                               if_else(wlbegin <= min_of_max, wlend, min_of_max))) %>% 
      filter(wlbegin <= wlend)
    
    specsdata_lens <- specsdata_lens1 %>%
      mutate("wlbegin" = if_else(wlbegin >= max_of_min, wlbegin, 
                                 if_else(wlend >= min_of_max, wlbegin, max_of_min)),
             "wlend" = if_else(wlend <= max_of_min, wlend, 
                               if_else(wlbegin <= min_of_max, wlend, min_of_max)))  %>% 
      filter(wlbegin <= wlend)
    
    prod_specs <- full_join(specsdata_frame,
                            specsdata_lens) %>% 
      pivot_longer(cols = c(D, I, R, M),
                   values_to = 'LB',
                   names_to = "opmode") %>% 
      drop_na()
    #
    #
    specsdata_frame <- prod_specs %>% 
      filter(type == "frame")
    
    specsdata_lens <- prod_specs %>% 
      filter(type == "lens") # create breaks based on what was input, to show on graphs
    frame_breaks1 <- unique(specsdata_frame$wlbegin)
    lens_breaks1 <- unique(specsdata_lens$wlbegin)
    frame_breaks2 <- unique(specsdata_frame$wlend)
    lens_breaks2 <- unique(specsdata_lens$wlend)
    # initialize vars for frame
    wl_begin_frame <- head(specsdata_frame$wlbegin,1)
    wl_end_frame <- head(specsdata_frame$wlend,1)
    LB_frame <- head(specsdata_frame$LB,1)
    opmode_frame <- head(as.factor(specsdata_frame$opmode),1)
    # create initial wavelength sequence data for frame
    wl_vec_frame <- seq(from = wl_begin_frame, to = wl_end_frame, by = 1)
    lb_vec_frame <- rep(x = LB_frame, length(wl_vec_frame))
    op_vec_frame <- rep(x = opmode_frame, length(wl_vec_frame))
    spectspecs_frame <- tibble("Wavelength" = wl_vec_frame, 
                               "LB Rating" = lb_vec_frame, 
                               "Operation Mode" = op_vec_frame,
                               "Product" = rep(input$frame, length(wl_vec_frame)))
    
    # initialize vars for lens
    wl_begin_lens <- head(specsdata_lens$wlbegin,1)
    wl_end_lens <- head(specsdata_lens$wlend,1)
    LB_lens <- head(specsdata_lens$LB,1)
    opmode_lens <- head(as.factor(specsdata_lens$opmode),1)
    # create initial wavelength sequence data for lens
    wl_vec_lens <- seq(from = wl_begin_lens, to = wl_end_lens, by = 1)
    lb_vec_lens <- rep(x = LB_lens, length(wl_vec_lens))
    op_vec_lens <- rep(x = opmode_lens, length(wl_vec_lens))
    spectspecs_lens <- tibble("Wavelength" = wl_vec_lens, 
                              "LB Rating" = lb_vec_lens, 
                              "Operation Mode" = op_vec_lens,
                              "Product" = rep(input$lens, length(wl_vec_lens)))
    for (i in 2:dim(specsdata_frame)[[1]]) {
      wl_begin_frame <- specsdata_frame$wlbegin[[i]]
      wl_end_frame <- specsdata_frame$wlend[[i]]
      LB_frame <- specsdata_frame$LB[[i]]
      opmode_frame <- as.factor(specsdata_frame$opmode[[i]])
      # create wavelength sequence data for frame
      wl_vec_frame <- seq(from = wl_begin_frame, to = wl_end_frame, by = 1)
      lb_vec_frame <- rep(x = LB_frame, length(wl_vec_frame))
      op_vec_frame <- rep(x = opmode_frame, length(wl_vec_frame))
      # form tibble with three prior functions
      transient_specs_frame <- tibble("Wavelength" = wl_vec_frame, 
                                      "LB Rating" = lb_vec_frame, 
                                      "Operation Mode" = op_vec_frame,
                                      "Product" = rep(input$frame, length(wl_vec_frame)))
      # subsequent tibble formation
      spectspecs_frame <- rbind(spectspecs_frame, 
                                transient_specs_frame)
    }
    
    
    
    for (i in 2:dim(specsdata_lens)[[1]]) {
      # Repeat for lens
      wl_begin_lens <- specsdata_lens$wlbegin[[i]]
      wl_end_lens <- specsdata_lens$wlend[[i]]
      LB_lens <- specsdata_lens$LB[[i]]
      opmode_lens <- as.factor(specsdata_lens$opmode[[i]])
      # create wavelength sequence, replicate the LB and op mode specs for each sequence
      wl_vec_lens <- seq(from = wl_begin_lens, to = wl_end_lens, by = 1)
      lb_vec_lens <- rep(x = LB_lens, length(wl_vec_lens))
      op_vec_lens <- rep(x = opmode_lens, length(wl_vec_lens))
      # form tibble with three prior functions
      transient_specs_lens <- tibble("Wavelength" = wl_vec_lens, 
                                     "LB Rating" = lb_vec_lens, 
                                     "Operation Mode" = op_vec_lens,
                                     "Product" = rep(input$lens, length(wl_vec_lens)))
      # repeat until product specs are populated
      # final tibble formation
      spectspecs_lens <- rbind(spectspecs_lens, 
                               transient_specs_lens)
    }
    spectspecs_lens <- spectspecs_lens %>% 
      group_by(Wavelength, `Operation Mode`) %>% 
      summarise("LB Rating" = max(`LB Rating`)) 
    
    spectspecs_frame <- spectspecs_frame %>% 
      group_by(Wavelength, `Operation Mode`) %>% 
      summarise("LB Rating" = max(`LB Rating`))
    
    inner_join(spectspecs_frame, spectspecs_lens, by=c("Wavelength", "Operation Mode")) %>%
      mutate("LB Rating" = if_else(`LB Rating.x` <= `LB Rating.y`, `LB Rating.x`, `LB Rating.y`)) %>% 
      group_by(Wavelength, `Operation Mode`) %>% 
      summarise("LB Rating" = min(`LB Rating`)) 
    })
  output$ce_plot <- renderPlotly({
    ce_plot <- ggplot(prod_specs(), aes(Wavelength, `LB Rating`, color = `Operation Mode`))+
      facet_grid(vars(`Operation Mode`))+
      geom_point()+
      theme(legend.position = "top")
    
    ggplotly(ce_plot)%>% 
      layout(dragmode = "zoom") %>% 
      style(hoverlabel = "label") %>% 
      config(displaylogo = F
      )
  })
  table_data1 <- eventReactive(input$run,{table_data %>% 
    filter(Product == input$frame | Product == input$lens)})
  
  output$ce_tables <- renderTable({
    table_data1()
  })
}

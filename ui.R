library(tidyverse)
library(plotly)
library(shiny)
library(bslib)
library(thematic)

specsdata <- readxl::read_excel("prod_specs_core_data.xlsx") %>% 
  pivot_wider(names_from = opmode,
              values_from = LB)

frame_choices <- specsdata %>% 
  filter(type == "frame")

lens_choices <- specsdata %>% 
  filter(type == "lens")


thematic_shiny()
page_fixed(theme=bs_theme(version = 5,
                          bootswatch = "darkly",
                          base_font = font_google("Karla")),
  card(
    card_header(fluidRow(
    column(6, align='center',
           selectInput("frame", "Frame", choices=unique(frame_choices$product))
           ),
    column(6, align='center',
           selectInput("lens", "Lens", choices=unique(lens_choices$product))
           
    )),fluidRow(
    column(6, align='center',
           br(),
           actionButton("run", "Run")
           
    ))),
    card_body(
           tableOutput("ce_tables"),
           plotlyOutput("ce_plot")           
           ),
    card_footer(strong("Product-level CE spec computer (under development)"))
  )
)
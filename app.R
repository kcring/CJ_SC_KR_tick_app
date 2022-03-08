#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(tidyverse)
library(here)
library(broom)
library(dplyr)
library(bslib)
library(plotly)
#install.packages("ggthemes")
library(ggthemes)
library(tmap)
library(sf)


long <- read_csv("long.csv")
tick_graph <-
    ggplot(data = long, aes(x = year, y = incidence, group = County)) +
    geom_line(aes(color = County)) +
    labs(x = "Year", y = "Lyme Disease Incidence", title = "Human Lyme Disease Incidence 2011-2020")
theme_minimal()

ca_counties_sf <- read_sf(here("ca_counties", "CA_Counties_TIGER2016.shp"))

ca_subset_sf <- ca_counties_sf %>% #sf = simple features object
    janitor::clean_names() %>%
    select(County= name, land_area = aland) #sf files have a sticky geometry (aka it automatically stays in the object)

lyme <- read_csv("human_lyme_incidence.csv")

incidence_map <- lyme %>%
    select(-last_col())%>%
    filter(!row_number() %in% c(59:61))  %>%
    rename(incidence = "Incidence/100000")

incidence <- inner_join(ca_subset_sf, incidence_map)

#tab 2 incidence data wrangling

tick_stage <- read_csv("tick_life_stage_by_county.csv")

tick_stage_map <- tick_stage %>%
  select(c(1:5, 8))

life_stage <- inner_join(ca_subset_sf, tick_stage_map)

## tab 3 data

ticks <- read_csv("Tejon_MixedModels_Dataset.csv")

# Create the user interface:
# using navbarPage() to setup tabs
ui <- navbarPage(theme = bs_theme(bootswatch = "flatly"),
                 # title
                 "Tick, Tick, Boom: Tick population (Family Acari) distributions in California",
                 # intro tab
                 tabPanel("About",
                          sidebarLayout(
                            sidebarPanel("About This Project"), 
                            mainPanel(
                              h1("First level title"),
                              img(src = "tick_picture.webp", height = 140, width = 140),
                              h4("Fourth level text")
                            )
                          )),
                 # first tab
                 tabPanel("Human Lyme Disease",
                          sidebarLayout(
                              # create sidebar panel that will house widgets
                              sidebarPanel("Double-click on counties in the figure legend to view county-level Lyme disease incidence"),
                              # add slider input
                              # add checkbox group
                              #checkboxGroupInput(inputId = "County",
                             # label = "Select Life Stage",
                              #choices = c("Sierra" ,"Sacramento","Santa Barbara" , "Calaveras" ,"Ventura", "Los Angeles","Sonoma", "San Francisco","Marin" ,"Mariposa","Lassen","Napa","Kings","San Diego","Placer", "San Francisco", "Marin","Mariposa",  "Lassen", "Napa", "Shasta","Monterey", "Trinity","Mendocino","Inyo","Mono","Tuolumne","Solano", "San Bernardino", "Contra Costa" ,"Alpine","El Dorado","Yolo", "Yuba", "San Benito", "Humboldt", "Riverside","Kern","Colusa" ,"Del Norte" ,"Modoc", "Fresno", "Madera", "Santa Clara", "Tehama" ,"San Joaquin" ,"Alameda","Nevada","Butte", "Merced", "Tulare" , "Stanislaus","Orange","Imperial","Sutter", "Amador", "Lake" ,"Plumas" ,"San Mateo", "Siskiyou", "Santa Cruz", "Glenn", "San Luis Obispo"
                              # ))),
                              # create main panel for output
                              mainPanel(tmapOutput(outputId = "lyme_map"),
                                        plotlyOutput(outputId = "lyme_plot"))
                          )),
                 # second tab
                 tabPanel("Life Stage Map",
                        #  sidebarLayout(
                              # create sidebar panel that will house widgets
                             # sidebarPanel("Adult Tick Distribution and Infection Prevalence"),
                                           # add radio button group
                                          # radioButtons(inputId = "Life_Stage",
                                                       # label = "Select Life Stage",
                                                      # choices = c("Larvae",
                                                               #     "Nymph",
                                                                #    "Adult"),
                                                       # selected = NULL),
                                           # add checkbox group
                                           #checkboxGroupInput(inputId = "island",
                                                             # label = "Select Counties",
                                                              #choices = c("Torgersen",
                                                                          #"Biscoe",
                                                                          #"Dream"),
                                                              #elected = "Torgersen")),
                              # create main panel for output
                              mainPanel(tmapOutput(outputId = "tick_map"))
                 ),

                 # third tab 
                tabPanel("Effects of Climate and Herbivore Loss on Tick Abundance",
                sidebarLayout(
                  sidebarPanel(NULL,
                               radioButtons(inputId = "ticks_site",
                                        label = "Choose Climate Treatment",
                                        choices = c("Arid", "Intermediate", "Mesic"))), #end sidebar panel
                  mainPanel(NULL,
                            plotOutput(outputId = "climate_plot")) #end tab3Panel
                )))
                
# Create the server function:
server <- function(input, output) ({
    ##Pt.1 Lyme incidence
    lyme_incidence <- reactive({
        long %>%
            #dplyr::group_by(County)
            dplyr::filter(County %in% input$County)
    })

    output$lyme_map <- renderTmap({
        tm_shape(incidence) +
            tm_fill("incidence", palette = "BuGn",legend.title = "Lyme disease incidence per 100,000 residents" )
    })

    output$lyme_plot <- renderPlotly({
        ggplot(data = long, aes(x = year, y = incidence, group = County)) +
            geom_line(aes(color = County)) +
            labs(x = "Year", y = "Lyme Disease Incidence", title = "Human Lyme Disease Incidence 2011-2020") +
            theme_minimal()
    })

    output$tick_map <- renderTmap({
      tm_shape(life_stage_long) +
        tm_fill(col = "Infection_prevalence", 
                palette = "Greens", 
                title = "Adult tick infection prevalence") +
        tm_bubbles("Count",  col = "Life_Stage",
                   border.col = "black", border.alpha = .5, 
                   style="fixed", 
                   palette="-RdYlBu", contrast=1, 
                   title.size="Tick abundace") +
        tm_layout(
          legend.title.size = 1,
          legend.text.size = 0.6)
    })

    ## Pt 3: Tick Seasonality 
    climate_select <- reactive({
      ticks %>%
        filter(site == input$ticks_site)
    }) #end climate_select reactive
    
    output$climate_plot <- renderPlot({
      ggplot(data = climate_select(), aes(x = site, y = total, fill = plot)) +
        geom_bar(stat = "identity", position = "dodge")+
        #geom_jitter(alpha = .15, width = .2, size = 3)+
        scale_fill_manual(values=c('darkseagreen1','darkseagreen3','darkseagreen4'))+
        theme_bw()
    })


})

# Combine them into an app:
shinyApp(ui = ui, server = server)

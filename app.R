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

tick_stage$Nymph <- as.numeric(tick_stage$Nymph)
tick_stage$Larvae <- as.numeric(tick_stage$Larvae)

tick_stage_long <- tick_stage %>% 
  as.data.frame() %>% 
  pivot_longer(cols = Adult:Larvae, names_to = "Life_Stage", values_to = "Count")

life_stage <- inner_join(ca_subset_sf, tick_stage_map)
life_stage_long <- inner_join(ca_subset_sf, tick_stage_long)

## tab 3 data

ticks <- read_csv("Tejon_MixedModels_Dataset.csv") %>%
  select(year, month, site, block, plot, total, deoc, ipac, deva, log_total)

Tick2 <- read_csv("Tejon_clean-main/data_visualizations/Viz_Tejon_MeanComparisons_Dataset.csv")

# Create the user interface:
# using navbarPage() to setup tabs
ui <- navbarPage(theme = bs_theme(bootswatch = "flatly"),
                 # title
                 "Tick, Tick, Boom: Tick population (Family Acari) distributions in California",
                 # intro tab
                 tabPanel("About",
                          sidebarLayout(
                            sidebarPanel(h4("App Authors"), 
                                         p("Kacie Ring, 
                                            Stephanie Copeland, 
                                            Conner Jainese")), 
                            mainPanel(
                              h2("An Exploration of Tick Dynamics in California"),
                              img(src = "tick_picture.png", 
                                  height = 443, width = 591),
                              p("Image Credit: James Gathany/CDC via AP"),
                              h4("Project Summaries"),
                              h5("Human Lyme Disease"), #put project summary after this heading
                              h5("Tick Life Stage Distribution in CA"), #put project summary after this heading
                              h5("Case Study: Effect of Climate and Herbivory Intensity on Tick Abundance"), #put project summary after this heading
                              p("The Tejon Ranch Exclosure Experiment (TREE) is an ongoing study consisting of 27 1ha plots, initiated in 2016 to understand the ecological effects of shifting wildlife and livestock assemblages across varying climates. The 27 experimental plots spanned three aridity levels, and each level included three replicate randomized blocks. Each block contained three treatment levels of large herbivores – a) no wild ungulates or livestock (total exclosure) which functionally excluded all large herbivores over 40 kg body mass, b) wild ungulates only (partial exclosure), and c) both wildlife + livestock (open). The unique life cycle of ticks consisting of an on-host and off host (susceptible to environmental variables) stages makes them particularly sensitive to the interactive effects of changing climate and herbivory in ecosystems.")
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
                tabPanel("Case Study",
                         sidebarLayout(position = "right",
                           # create sidebar panel that will house widgets
                           sidebarPanel(NULL,
                                        # add radiobutton group
                                        radioButtons(inputId = "ticks_site",
                                                     label = "Select climate",
                                                     choices = c("Arid", "Intermediate", "Mesic"))),
                           # create main panel for output
                           mainPanel(h4("Tick Abundance at the Tejon Ranch Exclosure Experiment (TREE)."),
                             h5("Tick Abundance by Climate Type and Herbivore Treatment"),
                                     plotOutput(outputId = "climate_plot"),
                             h5("TREE Study Design Visual"),
                             img(src = "Tejon.png",
                                 height = 443, width = 750),
                             p("SJArt")
                                     ))))
                
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
    })
    #end climate_select reactive
    
    output$climate_plot <- renderPlot({
      
      ggplot(data = climate_select(), aes(x = site, y = log(total), fill = plot)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c('darkseagreen1','darkseagreen3','darkseagreen4'))+
        scale_y_continuous(breaks = seq(0,5, by =1), limits = c(0,5))+
        labs(x = 'Climate Zone', y = 'Log(total) Ticks 2016-2019')+
        theme_bw()
    })
    
    #end tab 3 output 
})

# Combine them into an app:
shinyApp(ui = ui, server = server)

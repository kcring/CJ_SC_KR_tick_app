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
#install.packages("shinythemes")
library(shinythemes)
#install.packages("ggthemes")
library(ggthemes)
library(tmap)
library(sf)
library(leaflet)


pt_coords <- reactiveVal()

long <- read_csv("long.csv")
long$year <- as.character(long$year)
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


#Tick2 <- read_csv("Tejon_clean-main/data_visualizations/Viz_Tejon_MeanComparisons_Dataset.csv")


# Create the user interface:
# using navbarPage() to setup tabs
ui <- navbarPage(theme = shinytheme("sandstone"),
                 # title
                 "Tick, Tick, Boom: Tick population (Family Acari) distributions in California",
                 # intro tab
                 tabPanel("About",
                          sidebarLayout(position = "right",
                            sidebarPanel(
                              h4("App Authors"), 
                                         p("Kacie Ring, 
                                            Stephanie Copeland, 
                                            Conner Jainese"),
                                         img(src = c("kacie.jpeg"),
                                             height = 90, width =75),
                                         img(src = c("steph.jpg"),
                                          height = 90, width =75),
                                          img(src = c("corndog.jpg"),
                                              height = 90, width =75)
                              ), 
                            mainPanel(
                              h3("An Exploration of Tick Dynamics in California"),
                              h5("In the northern hemisphere, Lyme disease is the most common vector-borne disease, causing an estimated 300,000 cases annually in the U.S.¹ It is caused by the bacterial agent Borrelia burgdorferi (Bb) and vectored by Ixodes spp. ticks. In California, Lyme disease is vectored by Ixodes pacificus and maintained by vertebrate disease reservoirs, namely rodents.Encounters with ticks provide potential health risks in the form of Lyme Disease. Developing our understanding of tick population dynamics and distribution throughout California is important for prevention of Lyme disease. "),
                              img(src = "tick_pic.png", 
                                  height = 343, width = 491, align = "center"),
                              p("Ixodes pacificus tick Image Credit: CADPH"),

                              h4("Tick Life Cycle"),
                              h5("After hatching from an egg, western black legged ticks require a blood meal at every subsequent life stage (larva, nymph, adult) to survive. Juvenile larvae and nymphs are out primarily inthe Spring, whereas Adults tend to emerige in the Winter.  To become infected with the Lyme disease pathogen, ticks must feed on an infected reservoir host.Reptiles, mammals, and birds are all potential food sources for a tick, and humans can also become the source of a blood meal upon encountering a tick."),
                              img(src = "life_cycle.jpg", 
                                  height = 300, width = 375),
                              p("Image Credit: CDC"),
                              h4("Human Lyme Disease"), #put project summary after this heading
                              h5("If left untreated, individuals can experience early and late Lyme disease symptoms . Early symptoms include a round bullseye rash, Erythema migrans, along with fever, chills, fatigue, muscle/joint aches, and swollen lymph nodes. After about 30 days to a few months, Lyme disease symptoms can progress to include severe headache, facial palsy, arthritis, irregular heartbeats, inflammation of the brain/spinal cord, and nerve pain."),#put project summary after this heading
                              img(src = "early.jpg", 
                                  height = 260, width = 325),
                              img(src = "late.jpg", 
                                  height = 260, width = 325),
                              p("Symptoms of Lyme disease if left untreated. Early (left) versus late (right) dissmeintated. Image Source: Healthline ")
                            ))),
                 # first tab
                 tabPanel("Human Lyme Disease",
                           #sidebarLayout(
                            # create sidebar panel that will house widget
                            #checkboxGroupInput(inputId = "County",
                            #label = "Select Life Stage",
                            #choices = c("Sierra" ,"Sacramento","Santa Barbara" , "Calaveras" ,"Ventura", "Los Angeles","Sonoma", "San Francisco","Marin" ,"Mariposa","Lassen","Napa","Kings","San Diego","Placer", "San Francisco", "Marin","Mariposa",  "Lassen", "Napa", "Shasta","Monterey", "Trinity","Mendocino","Inyo","Mono","Tuolumne","Solano", "San Bernardino", "Contra Costa" ,"Alpine","El Dorado","Yolo", "Yuba", "San Benito", "Humboldt", "Riverside","Kern","Colusa" ,"Del Norte" ,"Modoc", "Fresno", "Madera", "Santa Clara", "Tehama" ,"San Joaquin" ,"Alameda","Nevada","Butte", "Merced", "Tulare" , "Stanislaus","Orange","Imperial","Sutter", "Amador", "Lake" ,"Plumas" ,"San Mateo", "Siskiyou", "Santa Cruz", "Glenn", "San Luis Obispo"
                            # ))),
                            # create main panel for output
                            mainPanel(
                              h3("Human Lyme disease incidence in California"),
                              p("Lyme disease incidence in California is low compared to the East coast of the United States, yet it is believed that human cases are underreported. Below is the human Lyme disease incidence in California from 2009-2019.Double click on counties in the the line plot to isolate the data."),
                              plotlyOutput(outputId = "lyme_plot"),
                              h3("California map of human Lyme disease incidence per 100,000 individuals"),
                              p("Hover over the county of interest on the map of California to isolate the average human Lyme disease incidence per 100,000 people from  2009-2019"),
                                      tmapOutput(outputId = "lyme_map"))
                          ),
                 # second tab
                 tabPanel("Life Stage Map",
                          sidebarLayout(position = "right",
                          sidebarPanel(
                            h4("Tick Life Stages"), 
                            img(src = c("life_stage.png"),
                                height = 400, width =175),
                          selectInput(inputId = "County", 
                                      label = h3("Select Counties"), 
                                      choices = list("Sierra" ,"Sacramento","Santa Barbara" , "Calaveras" ,"Ventura", "Los Angeles","Sonoma", "San Francisco","Marin" ,"Mariposa","Lassen","Napa","Kings","San Diego","Placer", "San Francisco", "Marin","Mariposa",  "Lassen", "Napa", "Shasta","Monterey", "Trinity","Mendocino","Inyo","Mono","Tuolumne","Solano", "San Bernardino", "Contra Costa" ,"Alpine","El Dorado","Yolo", "Yuba", "San Benito", "Humboldt", "Riverside","Kern","Colusa" ,"Del Norte" ,"Modoc", "Fresno", "Madera", "Santa Clara", "Tehama" ,"San Joaquin" ,"Alameda","Nevada","Butte", "Merced", "Tulare" , "Stanislaus","Orange","Imperial","Sutter", "Amador", "Lake" ,"Plumas" ,"San Mateo", "Siskiyou", "Santa Cruz", "Glenn", "San Luis Obispo"
                                      ), 
                                      multiple = FALSE,
                                      selected = "Santa Barbara")),
                          # sidebarPanel("Adult Tick Distribution and Infection Prevalence"),
                          # add radio button group
                          #radioButtons(inputId = "Life_Stage",
                          #label = "Select Life Stage",
                          #choices = c("Larvae",
                          #Nymph",
                          #"Adult"),
                          # selected = NULL),
                          mainPanel(tmapOutput(outputId = "tick_map"),
                            plotOutput(outputId = "tick_stage_map"))
                 )),
                 
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
  #end tab 1 human Lyme 
  
  life_stage_reactive <- reactive({
    tick_stage_long %>%
      group_by(County) %>%
      filter(County == input$County)
  }) #end of life stage reactive
  
  output$tick_stage_map <- renderPlot({
    ggplot(life_stage_reactive(), aes(x = Life_Stage, y=Count, fill = Life_Stage)) + 
      geom_bar(stat="identity") + 
      scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
      #geom_text(aes(label = tick_stage_long$Adults_positive_pools), hjust = -2, nudge_x = -.5,size = 4, fontface = "bold", fill = "white", label.size = 0) +
      ggtitle("Total Count of Collected Ticks by County") +
      xlab("Ixodes pacificus life stage") + 
      theme_bw() 
  }) #end of life stage output 
  
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
  })  #end of tab 2 life stage 
  
  
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

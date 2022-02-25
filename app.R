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

#penguins <- penguins

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
## Tejon Data and Organization
tejon_tick_2 <- read_csv("Tejon_MixedModels_Dataset.csv")

Full_no0 <- tejon_tick_2[which(tejon_tick_2$log_total != 0),]



#tejon_tick_app_2 <- Full_no0 %>%
#select(year, month, site, plot, total, deoc, ipac, deva, other) %>%
#group_by(across(all_of(group_cols))) %>%
#summarize(n = n())


# Create the user interface:
# using navbarPage() to setup tabs
ui <- navbarPage(theme = bs_theme(bootswatch = "flatly"),
                 # title
                 "Tick, Tick, Boom: Tick population (Family Acari) distributions in California",
                 # first tab
                 tabPanel("Human Lyme Disease",
                          sidebarLayout(
                              # create sidebar panel that will house widgets
                              sidebarPanel("Double-click on counties in the figure legend to view county-level Lyme disease incidence"),
                              # add slider input
                              # add checkbox group
                              #checkboxGroupInput(inputId = "County",
                              #label = "Select County",
                              #choices = c("Sierra" ,"Sacramento","Santa Barbara" , "Calaveras" ,"Ventura", "Los Angeles","Sonoma", "San Francisco","Marin" ,"Mariposa","Lassen","Napa","Kings","San Diego","Placer", "San Francisco", "Marin","Mariposa",  "Lassen", "Napa", "Shasta","Monterey", "Trinity","Mendocino","Inyo","Mono","Tuolumne","Solano", "San Bernardino", "Contra Costa" ,"Alpine","El Dorado","Yolo", "Yuba", "San Benito", "Humboldt", "Riverside","Kern","Colusa" ,"Del Norte" ,"Modoc", "Fresno", "Madera", "Santa Clara", "Tehama" ,"San Joaquin" ,"Alameda","Nevada","Butte", "Merced", "Tulare" , "Stanislaus","Orange","Imperial","Sutter", "Amador", "Lake" ,"Plumas" ,"San Mateo", "Siskiyou", "Santa Cruz", "Glenn", "San Luis Obispo"
                              # ))),
                              # create main panel for output
                              mainPanel(tmapOutput(outputId = "lyme_map"),
                                        plotlyOutput(outputId = "lyme_plot"))
                          )),
                 # second tab
                 tabPanel("Life Stage Map",
                          sidebarLayout(
                              # create sidebar panel that will house widgets
                              sidebarPanel("Adult Tick Distribution and Infection Prevalence"),
                                           # add radio button group
                                           #radioButtons(inputId = "species",
                                             #           label = "Select Life Stage",
                                             #           choices = c("Chinstrap",
                                                                   # "Gentoo",
                                                                    #"Adelie"),
                                              #          selected = "Chinstrap"),
                                           # add checkbox group
                                           #checkboxGroupInput(inputId = "island",
                                                             # label = "Select Counties",
                                                              #choices = c("Torgersen",
                                                                          #"Biscoe",
                                                                          #"Dream"),
                                                              #elected = "Torgersen")),
                              # create main panel for output
                              mainPanel(tmapOutput(outputId = "tick_map"))
                 )),

                 # third tab
                 tabPanel("Tejon Ticks: Climate and Host Change",
                          sidebarLayout(
                              # create sidebar panel that will house widgets
                              sidebarPanel("Time of Year and Exclosure Effects",
                                           # add checkbox group
                                           checkboxGroupInput(inputId = "month",
                                                              label = "Select Month",
                                                              choices = c("January",
                                                                          "February",
                                                                          "March",
                                                                          "April",
                                                                          "May",
                                                                          "June",
                                                                          "July",
                                                                          "August",
                                                                          "September",
                                                                          "October",
                                                                          "November",
                                                                          "December"),
                                                              selected = NULL),
                                           # add checkbox group #2
                                           checkboxGroupInput(inputId = "plot",
                                                              label = "Select Exclusion Treatment",
                                                              choices = c("Total", "Partial", "Control"),
                                                              selected = NULL)
                              ),
                              # create main panel for output
                              mainPanel("Graph/Map 3 Here",
                                        plotOutput(outputId = "tejon_tick"))
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
      tm_shape(life_stage) +
        tm_fill(col = "Infection_prevalence",
                palette = "Greens",
                title = "Adult tick infection prevalence") +
        tm_bubbles("Adult",
                   border.col = "black", border.alpha = .5,
                   style="fixed",
                   palette="BuGn", contrast=1,
                   title.size="Adult tick abundace")
    })

    ## Pt 3: Tick Seasonality - I'm not getting this to display and I don't know why
    tejon_tick_app_3 <- reactive({
        Full_no0 %>%
            select(year, month, site, plot, total, deoc, ipac, deva, other) %>%
            group_by(across(all_of(group_cols))) %>%
            summarize(n = n()) %>%
            filter(month == input$month)# end tejon reactive
    })

    #tejon_tick_app_2 <- reactive({
    #tejon_tick_app_2 %>%
    #filter(month == input$month) #%>%
    #filter(plot == input$plot)

    #end tejon reactive

    output$tejon_tick <- renderPlot({
        ggplot(data = tejon_tick_app_3(), aes(x = plot, y = n))+
            geom_bar(stat = "identity")+
            theme_bw()})


})

# Combine them into an app:
shinyApp(ui = ui, server = server)

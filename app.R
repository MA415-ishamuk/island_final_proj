#
# Title: "MA415 Final Project - Madagascar Island Shiny App"
# Date: May 09, 2025
#

library(shiny)
library(bslib)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(geojsonio) # used to read GeoJSON files to create an accurate boundary of madagascar on the map
library(plotly)

# define UI for application
ui <- navbarPage(
  title = "Madagascar",
  theme = bslib::bs_theme(bootswatch = "lumen"),
  
  tabPanel("Introduction",
           div(
             style = "
                      background-color: #bfcab4;  
                      margin: 5px;              
                      padding: 20px;             
                      height: calc(100vh - 100px); 
                      overflow: hidden;
                      color: #444;
                    ", # f0ead6 (tan)
           fluidRow(
             column(
               width = 12, align = "center",
               
               # add image to the webpage
               tags$img(src = "reserve_de_anja.jpg", height = "500px", 
                        style = "border: 8px solid #444; border-radius: 10px;"),
               
               # add caption below the image
               tags$div(
                 style = "margin-top: 10px; font-size: 10px;",
                 "Anja Community Reserve, Madagascar"
               )
             )
           ),
           
           # introduction text to the webpage
           tags$div(
             style = "margin-top: 50px; text-align: center;",
             tags$h1("Welcome to Madagascar"),
             tags$p(style = "font-size: 16px; max-width: 700px; margin: 0 auto;",
                    "An Introduction to the Island"
             ),
             tags$p(style = "font-size: 14px; max-width: 700px; margin: 20px auto;",
                    tags$span("By: Isha Mukundan"), tags$br(),
                    tags$span("MA415 - Data Science in R")
             )
           )
        )
  ),
  
  navbarMenu("General Information",
             tabPanel("About + Map",
                      column(
                        width = 12,
                        
                        # first box: about madagascar
                        div(
                          style = "border: 4px solid #444; background-color: #bfcab4; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                          fluidRow(
                            column(
                              width = 8,
                              h3("About Madagascar"),
                              p("Madagascar is the fourth largest island in the world and the second-largest island country, 
            situated in the Indian Ocean off the southeastern coast of Africa. Spanning approximately 587,000 
            square kilometers, it is internationally recognized for its extraordinary levels of biodiversity and
            endemism—over 90% of its plant and animal species are found nowhere else on the planet. The island’s 
            long-standing geographic isolation has fostered a distinct evolutionary trajectory, making it a global
            priority for biological research and conservation. Madagascar’s ecosystems range from tropical rainforests 
            and spiny deserts to coral reefs and highland plateaus, supporting a wide array of endemic species, including
            lemurs and baobab trees. Despite its ecological importance, the country faces significant socio-economic and 
            environmental challenges, including widespread poverty, deforestation, and climate-related vulnerabilities.")
                            ),
                            column(
                              width = 4,
                              tags$img(
                                src = "madagascar_flag.png",
                                height = "200px",
                                style = "float: right; border-radius: 20px; border: 2px solid #444;"
                              )
                            )
                          )
                        ),
                        
                        # second box: map
                        div(
                          style = "border: 4px solid #444; background-color: #dfe8d8; padding: 15px; border-radius: 8px;",
                          h3("Where is Madagascar?"),
                          p("Use this map to explore the island of Madagascar (highlighted in green) and better understand 
                            where it is located in the world."),
                          leafletOutput("madagascarMap", height = "400px")
                        )
                      )
             ),
             tabPanel("People",
                      column(
                        width = 12,
                        
                      div(
                        style = "border: 4px solid #444; background-color: #bfcab4; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                        fluidRow(
                          column(
                            width = 12,
                            h3("People of Madagascar"),
                            p("The people of Madagascar, known collectively as the Malagasy, are a unique blend of African and 
                              Austronesian heritage—a result of centuries of migration and cultural mixing. The island's population 
                              is traditionally divided into 18 official ethnic groups, each with distinct customs, dialects, and 
                              histories. The Merina, who inhabit the central highlands around Antananarivo, are the largest group 
                              and were historically dominant during the Malagasy kingdom. Other major groups include the Betsimisaraka 
                              along the east coast, the Sakalava in the west, and the Antandroy in the south. Genetic and linguistic 
                              evidence shows that Madagascar was first settled around 1,500 years ago by seafarers from Southeast Asia, 
                              likely from present-day Indonesia or Borneo, who later mixed with Bantu-speaking peoples from East Africa. 
                              Over time, Arab, Indian, European, and Comorian influences also left their mark, particularly in coastal 
                              trade hubs. Thi has created a diverse yet deeply interconnected Malagasy identity, 
                              reflected in the island’s languages, beliefs, and social structures."),
                            p("Madagascar's religious landscape is built on the cultural and ethnic diversity of its people. 
                            The population practices a wide range of faiths primarily Christianity, traditional beliefs, and Islam. Fomba Gasy
                            is one of the traditional Malagasy religions and has primarily been passed down orally telling the stort of the 
                            creator deity named Zanagary and how Heaven and Earth is divided between him and his son Andrianerinerina.
                            The pie chart below illustrates the religious composition of the island as of 2020, highlighting the major faith 
                            groups and the extent of religious diversity across the Malagasy population."),
                            div(
                              style = "background-color: #dfe8d8; border: 3px solid #444; border-radius: 10px; padding: 10px; margin-top: 20px;",
                              plotlyOutput("religion_chart")
                            )
                          )
                        )
                      ),
                      # second box: population 
                      div(
                        style = "border: 4px solid #444; background-color: #dfe8d8; padding: 15px; border-radius: 8px;",
                        h3("Population"),
                        p("The population of Madagascar has experienced steady and significant growth over the past six decades. In 1960, the 
                          population was just over 5 million, but by 2023 it had surpassed 30 million. This sixfold increase reflects broader 
                          demographic trends across many developing nations, driven by improved healthcare, decreasing infant mortality rates (seen below), 
                          and urban expansion. Interestingly, this population growth is being seen even with decreasing birth rates within the island. The consistent 
                          upward trajectory shown in the graph highlights the country’s rapid population growth, which has implications
                          for infrastructure, education, healthcare, and environmental sustainability."),
                        plotlyOutput("population_chart")
                      )
                    )
                      
             ),
             tabPanel("Economy",
                      h2("Madagascar's Economy"),
                      p("INFO")
             )
  ),
  
  tabPanel("Environment",
           p("INFO")
  )
)  

# define server logic 
server <- function(input, output) {
  output$madagascarMap <- renderLeaflet({
    # read geoJSON file
    madagascar_shape <- geojson_read("www/madagascar.geojson", what = "sp")
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 47.5, lat = -18.8, zoom = 5) %>%
      
      # define polygon for madagascar
      addPolygons(data = madagascar_shape,
                  fillColor = "#66c2a5",
                  color = "#238b45",
                  weight = 2,
                  fillOpacity = 0.5,
                  popup = "Madagascar") %>%
      
      # add marker for antananarivo (the capital of madagascar)
      addMarkers(
        lng = 47.5079, lat = -18.8792,
        popup = "Antananarivo (Capital)"
      )
  })

  # plot an interactive pie chart that shows the religion breakdown of Madagascar
  output$religion_chart <- renderPlotly({
    pastel_colors <- c(
      "#A7ABDE", "#FFB347", "#B0E0E6", "#FF6961", "#CBAACB", "#FDFD96",
      "#FAF8F6", "#FFD1DC", "#CFCFC4", "#F49AC2"
    )
    mdg_religion <- mdg_religion %>%
      mutate(Label = paste0(Religion, ": ", `Madagascar[x]`, "%"))

    plot_ly(mdg_religion,
            labels = ~Religion,
            values = ~`Madagascar[x]`,
            type = 'pie',
            text = ~Label,
            textinfo = 'text',
            textposition = 'outside',
            insidetextorientation = 'radial',
            pull = 0.01, 
            marker = list(colors = pastel_colors),
            hovertemplate = '%{label}: %{percent}<extra></extra>') %>%
      layout(title = 'Madagascar Religion Breakdown',
             showlegend = T,
             paper_bgcolor = '#dfe8d8', 
             plot_bgcolor = '#dfe8d8')
  })
  
  output$population_chart <- renderPlotly({
    plot_ly(mdg_pop, x = ~Year, y = ~Value, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'forestgreen', width = 3),
            marker = list(size = 5)) %>%
      layout(title = "Population of Madagascar Per Year",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Population (in Millions)"),
             plot_bgcolor = "#dfe8d8",
             paper_bgcolor = "#dfe8d8")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

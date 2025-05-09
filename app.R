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
library(scales)
library(slickR)
library(tidyr)
library(dplyr)
library(stringr)
library(rsconnect)

# data cleaning
# read in dataset - Health Indicators of Madagascar
health_ind <- read_csv("health_ind_mdg.csv", 
                       col_names = TRUE,
                       show_col_types = FALSE)

source("my_functions.R")

# check if read in table has any columns with only one value and remove those
# prt_val = T to show which columns had only one value and were dropped
mdg_ind <- health_ind |> drop_one_value_col(prt_val = T)

# remove columns that are not of interest for further data analysis
mdg_ind <- mdg_ind |> select(-`Indicator Code`)

# reorganize table so that years are rows rather than columns
mdg_long <- mdg_ind %>%
  pivot_longer(
    cols = `1960`:`2024`,   
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(Year = as.integer(Year))  # convert year column to integer

### create a separate table with just population information
mdg_pop <- mdg_long %>%
  filter(`Indicator Name` == "Population, total")

# remove the first column that has only one value and remove last row with no info
mdg_pop <- mdg_pop |> drop_one_value_col()
mdg_pop <- mdg_pop[-65,]

### create a separate table with just life expectancy information (male, female, total)
mdg_life_ex <- mdg_long %>%
  filter(str_starts(`Indicator Name`, "Life expectancy"))

# clean life expectancy table - remove redundant row names to leave just (female/male/total)
mdg_life_ex <- mdg_life_ex |> separate_wider_delim(cols = `Indicator Name`,
                                                   delim = ', ',
                                                   names = c("rem",
                                                             "Gender"),
                                                   too_many = "error",
                                                   too_few = "align_start")

mdg_life_ex <- mdg_life_ex |> separate_wider_delim(cols = `Gender`,
                                                   delim = ' ',
                                                   names = c("Gender",
                                                             "remove"),
                                                   too_many = "error",
                                                   too_few = "align_start")

mdg_life_ex <- mdg_life_ex |> select(-rem, -remove)

# pivot the table wider so that each classification (female/male/total) has its own column
#mdg_life_ex <- pivot_wider(mdg_life_ex, names_from = Gender, values_from = Value)
mdg_life_ex <- mdg_life_ex[-65,]
mdg_life_ex <- mdg_life_ex[-130,]
mdg_life_ex <- mdg_life_ex[-195,]

###  create a separate table with just infant mortality rate information
mdg_inf_mort <- mdg_long %>%
  filter(str_starts(`Indicator Name`, "Mortality rate, infant" ))


# clean infant mortality rate table - remove redundant row names to leave just (female/male/total)
mdg_inf_mort <- mdg_inf_mort |> separate_wider_delim(cols = `Indicator Name`,
                                                     delim = 'rate, ',
                                                     names = c("rem",
                                                               "Gender"),
                                                     too_many = "error",
                                                     too_few = "align_start")
mdg_inf_mort <- mdg_inf_mort %>%
  mutate(Gender = if_else(str_starts(Gender, fixed("infant (")), 
                          "infant, total (per 1,000 live births)", 
                          Gender))

mdg_inf_mort <- mdg_inf_mort |> separate_wider_delim(cols = `Gender`,
                                                     delim = 'infant, ',
                                                     names = c("remove",
                                                               "Gender"),
                                                     too_many = "error",
                                                     too_few = "align_start")


mdg_inf_mort <- mdg_inf_mort |> separate_wider_delim(cols = `Gender`,
                                                     delim = ' ',
                                                     names = c("Gender",
                                                               "remove2"),
                                                     too_many = "drop",
                                                     too_few = "align_start")

mdg_inf_mort <- mdg_inf_mort |> select(-rem, -remove, -remove2)

# pivot the table wider so that each classification (female/male/total) has its own column
#mdg_inf_mort <- pivot_wider(mdg_inf_mort, names_from = Gender, values_from = Value)

# remove rows where all columns are NA
mdg_inf_mort <- mdg_inf_mort %>%
  filter(!is.na(Value))

# read in dataset - Religions of Madagascar
mdg_religion <- read_csv("mdg_religion.csv", 
                         col_names = TRUE,
                         show_col_types = FALSE)

# keep only the column describing the religions within Madagascar
mdg_religion <- mdg_religion |> select(-`Eastern Africa[x]`, -`The World[x]`)

# filter through the religions to only keep the major religions (remove the comprising sects)
mdg_religion <- mdg_religion %>% filter(!str_starts(Religion, "--"))
mdg_religion <- mdg_religion %>% filter(!str_starts(`Madagascar[x]`, "--"))
mdg_religion <- mdg_religion[-7, ]

# make the percents in the Madagascar column numerical (to translate to the pie chart)
mdg_religion$`Madagascar[x]` <- as.numeric(gsub("%", "", mdg_religion$`Madagascar[x]`))

### create a table with the top 10 exports (in terms of trade value) in Madagascar
mdg_exports <- read_csv("mdg_exports.csv", 
                        col_names = TRUE,
                        show_col_types = FALSE)

# remove columns with unnecessary information
mdg_exports <- mdg_exports |> select(-HS2, -`HS2 ID`, -`HS4 ID`, -`Section ID`, -Year)

# reorder the table in decreasing order by trade value (to determine top 10 exports)
mdg_exports_10 <- mdg_exports %>%
  arrange(desc(`Trade Value`))

# rename data table columns for clarity 
mdg_exports_10 <- mdg_exports_10 %>%
  rename(Item = HS4)
mdg_exports_10 <- mdg_exports_10 %>%
  rename(Sector = Section)

# keep only the top 10 exports 
mdg_exports_10 <- mdg_exports_10[1:10,]

### create a separate table with just GDP (in $ USD) information
mdg_gdp <- mdg_long %>%
  filter(`Indicator Name` == "GDP (current US$)")

# remove unnecessary column Indicator Name and row 65 with an NA value
mdg_gdp <- mdg_gdp |> select(-`Indicator Name`)
mdg_gdp <- mdg_gdp[-65,]

### create a plot looking at how tree cover loss has changes over years
mdg_tree_loss <- read_csv("mdg_tree_loss.csv", 
                          col_names = TRUE,
                          show_col_types = FALSE)

# remove the first column that has only one value 
mdg_tree_loss <- mdg_tree_loss |> drop_one_value_col()

# rename data table columns for clarity
mdg_tree_loss <- mdg_tree_loss %>%
  rename(Year = umd_tree_cover_loss__year)
mdg_tree_loss <- mdg_tree_loss %>%
  rename(`Tree Cover Loss` = umd_tree_cover_loss__ha)
mdg_tree_loss <- mdg_tree_loss %>%
  rename(`CO2 Emissions` = gfw_gross_emissions_co2e_all_gases__Mg)

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
                          style = "border: 4px solid #444; background-color: #dfe8d8; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                          h3("Where is Madagascar?"),
                          p("Use this map to explore the island of Madagascar (highlighted in green) and better understand 
                            where it is located in the world. The capital of Madagascar, Antananarivo, is marked on the map."),
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
                        style = "border: 4px solid #444; background-color: #dfe8d8; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                        h3("Population"),
                        p("The population of Madagascar has experienced steady and significant growth over the past six decades. In 1960, the 
                          population was just over 5 million, but by 2023 it had surpassed 30 million. This sixfold increase reflects broader 
                          demographic trends across many developing nations, driven by improved healthcare, decreasing infant mortality rates (seen below), 
                          and urban expansion. Interestingly, this population growth is being seen even with decreasing birth rates within the island. The consistent 
                          upward trajectory shown in the graph highlights the country’s rapid population growth, which has implications
                          for infrastructure, education, healthcare, and environmental sustainability."),
                        plotlyOutput("population_chart")
                      ),
                      # third box: health indicators 
                      div(
                        style = "border: 4px solid #444; background-color: #bfcab4; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                        h3("Health Indicators"),
                        p("Understanding Madagascar’s population growth also requires examining key health indicators that shape demographic trends. 
                          Life expectancy and infant mortality rates provide essential insight into the island’s healthcare system, social conditions, 
                          and overall quality of life. While the population continues to grow, improvements in life expectancy and a steady decline in 
                          infant mortality signal positive developments in public health. These trends highlight the progress Madagascar has made in 
                          reducing early childhood deaths and extending lifespan, despite ongoing challenges in access to healthcare and infrastructure."),
                        h4("Life Expectancy"),
                        plotlyOutput("life_ex_chart"),
                        p(""),
                        p("The life expectancy trends in Madagascar show a consistent pattern across all categories— female, male, and total —indicating a general
                          upward trajectory over time. Notably, life expectancy for females is consistently higher than that of males throughout the observed
                          years. This gender gap in life expectancy reflects broader global patterns and highlights differences in health outcomes and 
                          longevity. In the interactive plot, you can explore these trends more closely by clicking on the legend items to toggle the 
                          visibility of each line. This feature allows you to isolate specific categories—such as viewing only female life expectancy or 
                          comparing male and total."),
                        h4("Infant Mortality Rate"),
                        plotlyOutput("inf_mort_chart"),
                        p(""),
                        p("The infant mortality rate (per 1,000 live births) in Madagascar has shown a steep and encouraging decline over time, following a peak
                          observed between 1984 and 1985. Since that high point, the rate has steadily decreased, reflecting improvements in healthcare access, 
                          maternal health, and child survival. Throughout the years, the infant mortality rate for females has consistently remained lower than that
                          of males, a trend often seen globally due to biological and healthcare-related factors. Infant mortality is a critical health indicator,
                          as it reflects the overall well-being of a population, the effectiveness of health systems, and broader socio-economic conditions. However, 
                          despite the significant progress, Madagascar’s infant mortality rate still remains on the higher side when compared to many other countries
                          around the world, highlighting the need for continued investment in healthcare infrastructure and maternal-child health services. Once again, 
                          the legend items can be toggled to change which information is visible on the graph.")
                      )
                    )
                      
             ),
             tabPanel("Economy",
                      column(
                        width = 12,
                        
                        div(
                          style = "border: 4px solid #444; background-color: #bfcab4; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                          fluidRow(
                            column(
                              width = 12,
                              h3("Economy of Madagascar"),
                              p("Madagascar’s economy is primarily based on agriculture, fishing, and forestry, with a significant portion of the population engaged in 
                                subsistence farming. The country is rich in natural resources, including vanilla, cloves, coffee, and minerals, and is the world’s largest
                                exporter of vanilla (exporting 80% of the world's vanilla). Despite these assets, Madagascar remains one of the poorest countries globally, with a large informal sector and 
                                limited industrial development. Economic growth has been uneven, often disrupted by political instability, inadequate infrastructure, 
                                and vulnerability to climate-related shocks such as cyclones and droughts. However, efforts are ongoing to strengthen sectors like tourism,
                                mining, and renewable energy, which hold potential for more sustainable and inclusive development in the future."),
                              p("Building on the overview of Madagascar’s economy, a closer look at its top exports in 2023 highlights the key sectors driving foreign revenue. The country's 
                                export composition is diverse, with leading products including nickel, vanilla, cloves, gold, titanium ore, knit sweaters, non-knit men’s suits, precious stones, 
                                crustaceans, and cobalt. These exports point to four major sectors of economic importance: metals (both precious and industrial), minerals, agricultural and animal products,
                                and textiles. This export profile reflects the country’s reliance on its natural resources and specialized goods to support economic growth and international trade. The following table
                                outlines the top ten exports by value, in USD, in 2023, offering a clearer picture of the goods that play a central role in the nation’s trade economy."),
                              div(
                                style = "background-color: #dfe8d8; border: 3px solid #444; border-radius: 10px; padding: 10px; margin-top: 20px;",
                                tags$div(
                                  style = "text-align: center; margin-top: 30px; display: flex; flex-direction: column; align-items: center;", # This ensures both title and table are centered
                                  tags$h3("Top 10 Exports of Madagascar (2023)"),
                                  div(
                                    style = "display: flex; justify-content: center; margin-top: 20px;",
                                    div(
                                      style = "width: 140%;",  
                                      tableOutput("exports_table")
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),
                        
                        # second box: GDP
                        div(
                          style = "border: 4px solid #444; background-color: #dfe8d8; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                          h3("Economic Indicator: GDP"),
                          p(""),
                          p("The plot of Madagascar's Gross Domestic Product (GDP) in USD over the years provides a visual representation of the country’s economic performance and growth 
                            trajectory. As one of the key economic indicators, GDP measures the total value of all goods and services produced within a country and is a critical gauge of 
                            economic health. This data can highlight trends in the national economy, such as periods of growth or recession, and reveal the impact of external factors like 
                            natural disasters, political instability, or global market conditions. By examining the GDP over time, policymakers, economists, and investors can better understand
                            the economic resilience and vulnerabilities of Madagascar, track progress in development, and identify sectors driving economic performance. In addition, GDP can be 
                            compared with other indicators, such as population growth and inflation rates, to assess living standards and the distribution of wealth within the country."),
                          plotlyOutput("gdp_chart"),
                          p(""),
                          p("The plot reveals a clear upward trend in Madagascar's GDP over time, with a particularly noticeable acceleration in growth over the past 15 years. This sharp 
                            increase stands in contrast to the earlier period between 1960 and 1975, where GDP growth was minimal, suggesting a stagnation in the economy during that time.
                            While Madagascar’s GDP has improved significantly in recent decades, especially when compared to its historical performance, it still remains relatively low in 
                            comparison to other countries, particularly when considering its size and natural resources. This highlights the ongoing challenges the nation faces in achieving
                            more sustainable and equitable economic development.")
                        )
                      )
             )
  ),
  tabPanel("Environment",
           column(
             width = 12,
             
             # First box: Climate
             div(
               style = "border: 4px solid #444; background-color: #bfcab4; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
               fluidRow(
                 column(
                   width = 12,
                   h3("Climate of Madagascar"),
                   p("Madagascar's climate is as diverse as its landscapes, ranging from tropical along the coast to temperate in the highlands.
             The island experiences two main seasons: a hot, rainy season from November to April, and a cooler, dry season from May to
             October. The eastern side of the island is influenced by moist trade winds, resulting in lush rainforests, while the western
             and southern regions are much drier, with semi-arid conditions and spiny forests. The island is also vulnerable to tropical
             cyclones, particularly during the rainy season, which can cause significant damage to both infrastructure and ecosystems.
             Madagascar's varied climate plays a crucial role in its rich biodiversity, but it also presents challenges for agriculture, 
             water resources, and overall climate resilience."),
                   
                   # Two side-by-side images with captions
                   fluidRow(
                     column(
                       width = 6,
                       align = "center",
                       tags$img(
                         src = "lowland_rainforest.jpg", 
                         alt = "Lowland Rainforest", 
                         style = "width: 100%; height: 455px; border-radius: 5px; border: 3px solid #444;"
                       ),
                       tags$p("Lowland Rainforest in Madagascar", style = "text-align: center; font-size: 10px;")
                     ),
                     column(
                       width = 6,
                       align = "center",
                       tags$img(
                         src = "baobab_trees.jpg", 
                         alt = "Baobab Trees", 
                         style = "width: 100%; height: auto; border-radius: 5px; border: 3px solid #444;"
                       ),
                       tags$p("Baobab (Spiny Tree) Forest in Madagascar", style = "text-align: center; font-size: 10px;")
                     )
                   )
                 )
               )
             ),
             
             # Second box: Forest
             div(
               style = "border: 4px solid #444; background-color: #dfe8d8; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
               h3("Deforestation"),
               p("Deforestation and tree cover loss in Madagascar pose serious threats not only to its unique biodiversity but also to the broader
               global climate. Madagascar is home to some of the world's most diverse and endemic ecosystems, many of which depend on the island's
               forests for survival. However, large-scale logging, slash-and-burn agriculture (known locally as tavy), and charcoal production have
               significantly reduced tree cover over the past decades. This loss of forest is more than a local environmental issue—it contributes
               directly to increased carbon dioxide (CO₂) emissions. Trees act as critical carbon sinks, absorbing CO₂ from the atmosphere and helping
               to mitigate climate change. When forests are destroyed or degraded, the stored carbon is released, accelerating global warming. 
               The decline in tree cover also disrupts water cycles, increases soil erosion, and threatens the livelihoods of local communities who rely 
               on forests for food, medicine, and materials. Protecting Madagascar’s forests is essential not only for preserving its rare species and
               ecosystems but also for contributing to global efforts in reducing greenhouse gas emissions and stabilizing the climate."),
               plotlyOutput("tree_loss_chart"),
               p(""),
               p("The plot illustrates the trend of tree cover loss and CO2 emissions in Madagascar from 2001 to 2023. The histogram reveals a sharp peak
               in tree cover loss in 2017, with a slight decline in the following years, although deforestation remains prevalent as tree cover loss has been increasing again. From 2001 to 2023, 
               Madagascar lost 4.92 million hectares of tree cover, a 29% decrease since 2000, emphasizing the ongoing issue of deforestation. The line graph 
               of CO2 emissions aligns with the tree cover loss, showing higher emissions during years of significant deforestation, particularly in 2017.
               This is due to the reduced ability of forests to absorb CO2, exacerbating climate change. Although tree cover loss has slightly decreased in 
               recent years, the issue remains critical as continued deforestation contributes to global warming, reduces biodiversity, and disrupts local ecosystems. 
              The relationship between tree loss and CO2 emissions underscores the importance of conservation efforts for both the environment and climate resilience.")
             ),
             
             # Third box: Animals
             div(
               style = "border: 4px solid #444; background-color: #bfcab4; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
               h3("Animals"),
               p("Madagascar hosts an extraordinary array of endemic flora and fauna, making it one of the most biologically unique regions in the world. Among its most
                 distinctive animal species are lemurs, a diverse group of primates found exclusively on the island, and the fossa, which is Madagascar’s largest carnivorous mammal. There is also
                 the Satanic leaf-tailed gecko, which has the ability of extreme camouflage that underscores the island’s evolutionary specialization. Madagascar’s flora is similarly unique, 
                 with the Madagascar periwinkle standing out for its pharmacological value in cancer treatment, and its forests filled with baobab trees. The image carousel below presents visual 
                 representations of these species in the order discussed, highlighting the island’s remarkable biodiversity."),
               tags$style(HTML("
                              .slick-slide {
                                background-color: #bfcab4; 
                              }
                            ")),
               slickROutput("animal_im")
             )
           )
  ),
  tabPanel("Conclusion",
           tags$style(HTML("
           .quad-box {
             height: 400px;
             border: 3px solid #444;
             border-radius: 10px;
             padding: 20px;
             color: #000;
             margin-bottom: 10px;
           }
           .q1 { background-color: #bfcab4; }
           .q2 { background-color: #dfe8d8; }
         ")),
           
           fluidRow(
             column(6, 
                    div(class = "quad-box q1", 
                        h4("Strengths"),
                        tags$ul(
                          tags$li(
                            tags$b("Biodiversity:"),
                            tags$ul(
                              tags$li("Madagascar is one of the world’s most important biodiversity hotspots, with over 90% of its wildlife found nowhere else, including species like lemurs and baobab trees. This gives it strong potential for ecotourism development, which can provide revenue incentives.")
                            )
                          ),
                          tags$li(
                            tags$b("Cultural Richness:"),
                            tags$ul(
                              tags$li("Madagascar has a diverse cultural heritage, with strong traditions in music, dance, crafts, and language that contribute to its national identity.")
                            )
                          ),
                          tags$li(
                            tags$b("Agricultural Resources:"),
                            tags$ul(
                              tags$li("The island’s varied climate and geography support the cultivation of crops like vanilla, cloves, and lychees, which are important exports for the country.")
                            )
                          )
                        )
                    )
             ),
             column(6, 
                    div(class = "quad-box q2", 
                        h4("Weaknesses"),
                        tags$ul(
                          tags$li(
                            tags$b("Environmental Degredation:"),
                            tags$ul(
                              tags$li("Madagascar faces severe deforestation, soil erosion, and biodiversity loss, largely due to slash-and-burn agriculture, illegal logging, and habitat destruction.")
                            )
                          ),
                          tags$li(
                            tags$b("Political Insability:"),
                            tags$ul(
                              tags$li("Recurrent political crises and weak governance have hindered long-term development and discouraged foreign investment.")
                            )
                          ),
                          tags$li(
                            tags$b("Poverty and Inequality:"),
                            tags$ul(
                              tags$li("A significant portion of the population lives in poverty, with limited access to healthcare, education, and clean water, especially in rural areas.")
                            )
                          )
                        )
                    )
             )
           ),
           fluidRow( 
             column(6, 
                    div(class = "quad-box q2", 
                        h4("Opportunities"),
                        tags$ul(
                          tags$li(
                            tags$b("Ecotourism Expansion:"),
                            tags$ul(
                              tags$li("Madagascar’s rich biodiversity and unique ecosystems provide strong potential for sustainable ecotourism, which can generate revenue while promoting conservation and local employment.")
                            )
                          ),
                          tags$li(
                            tags$b("Renewable Energy Development:"),
                            tags$ul(
                              tags$li("With abundant sunlight, wind, and hydro resources, Madagascar has significant potential to expand renewable energy infrastructure and reduce reliance on imported fossil fuels.")
                            )
                          ),
                          tags$li(
                            tags$b("Biodiversity Conservation Funding:"),
                            tags$ul(
                              tags$li("Madagascar’s status as a global biodiversity hotspot attracts international interest and funding, which can support conservation initiatives, education, and rural development.")
                            )
                          )
                        )
                    )
             ),
             column(6, 
                    div(class = "quad-box q1", 
                        h4("Threats"),
                        tags$ul(
                          tags$li(
                            tags$b("Climate Change:"),
                            tags$ul(
                              tags$li("Rising temperatures, shifting rainfall patterns, and more frequent extreme weather events (like cyclones and droughts) threaten agriculture, water security, and ecosystems.")
                            )
                          ),
                          tags$li(
                            tags$b("Biodiversity Loss:"),
                            tags$ul(
                              tags$li("Madagascar is a global biodiversity hotspot, but its unique species are under severe threat from habitat destruction, invasive species, and illegal wildlife trade.")
                            )
                          ),
                          tags$li(
                            tags$b("Deforestation:"),
                            tags$ul(
                              tags$li("Ongoing forest loss—often driven by subsistence farming and charcoal production—continues to reduce carbon sinks and imperil endemic species.")
                            )
                          )
                        )
                    )
             )
           )
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
  
  output$life_ex_chart <- renderPlotly({
   
    plot_ly(mdg_life_ex,
            x = ~Year,
            y = ~Value,
            color = ~Gender,
            type = 'scatter',
            mode = 'lines+markers',
            colors = c("female" = "#F49AC2", "male" = "#A7ABDE", "total" = "darkgreen"),
            line = list(width = 3),
            marker = list(size = 5)) %>%
      layout(title = "Life Expectancy of Madagascar by Gender",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Life Expectancy (Years)"),
             plot_bgcolor = "#bfcab4",
             paper_bgcolor = "#bfcab4",
             legend = list(title = list(text = "<b>Gender</b>")))
  })
  
  output$inf_mort_chart <- renderPlotly({
    plot_ly(mdg_inf_mort,
            x = ~Year,
            y = ~Value,
            color = ~Gender,
            type = 'scatter',
            mode = 'lines+markers',
            colors = c("female" = "#F49AC2", "male" = "#A7ABDE", "total" = "darkgreen"),
            line = list(width = 3),
            marker = list(size = 5)) %>%
      layout(title = "Infant Mortality Rate in Madagascar by Gender",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Infant Mortality Rate (Per 1,000 Live Births)"),
             plot_bgcolor = "#bfcab4",
             paper_bgcolor = "#bfcab4",
             legend = list(title = list(text = "<b>Gender</b>")))
  })
  
  output$exports_table <- renderTable({
    mdg_exports_10 %>%
      mutate(`Trade Value` = label_dollar(scale_cut = cut_short_scale())(`Trade Value`)) %>%
      select(Item, Sector, `Trade Value`)}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "m", align = 'c')
  
  output$gdp_chart <- renderPlotly({
    plot_ly(mdg_gdp, x = ~Year, y = ~Value, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'forestgreen', width = 3),
            marker = list(size = 5)) %>%
      layout(title = "GDP of Madagascar Per Year",
             xaxis = list(title = "Year"),
             yaxis = list(title = "GDP (in US$)"),
             plot_bgcolor = "#dfe8d8",
             paper_bgcolor = "#dfe8d8")
  })
  
  output$tree_loss_chart <- renderPlotly({
    plot_ly(data = mdg_tree_loss) %>%
      add_trace(
        x = ~Year,
        y = ~`Tree Cover Loss`,
        type = 'bar',
        name = 'Tree Cover Loss (ha)',
        marker = list(color = 'rgba(100, 200, 100, 0.7)')
      ) %>%
      add_trace(
        x = ~Year,
        y = ~`CO2 Emissions`,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'CO₂ Emissions (Mt)',
        yaxis = 'y2',
        line = list(color = '#FF6961', width = 3),
        marker = list(size = 6)
      ) %>%
      layout(
        title = "Tree Cover Loss and CO₂ Emissions in Madagascar",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Tree Cover Loss (hectares)"),
        yaxis2 = list(
          title = "CO₂ Emissions (Megatonnes)",
          overlaying = "y",
          side = "right"
        ),
        legend = list(x = 0.05, y = 0.95),
        bargap = 0.2,
        margin = list(r = 80),
        plot_bgcolor = "#dfe8d8",
        paper_bgcolor = "#dfe8d8"
      )
  })
  
  images <- c("lemur.jpg", "fossa.jpg", "gecko.jpg", "mdg_peri.jpg", "baobab.jpg")
  output$animal_im <- renderSlickR({
    slickR(images, height = 400, width = "100%")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

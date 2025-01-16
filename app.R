#Author: KHVAN Chandara


# Install packages if they are not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("plotly")) install.packages("plotly")
if (!require("treemapify")) install.packages("treemapify")
if (!require("scales")) install.packages("scales")
if (!require("leaflet")) install.packages("leaflet")
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
if (!require("sf")) install.packages("sf")
if (!require("DT")) install.packages("DT")


# Import the needed library
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(plotly)
library(treemapify)
library(scales)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(DT)


# This app consist of 5 section.
# 1. Loading and Pre-processing 
# 2. Visualization
# 3. Description
# 4. User Interface
# 5. Server 

#############1.Loading and Pre-processing#########################################################################################
# Loading data
wdi_df <- read.csv("wdiData.csv")
spi_df <- read.csv("SPI_index_labelled.csv")

# Select only the needed columns
vis1_df <- spi_df %>% select(c("country",
                               "iso3c",
                               "date",
                               "region", 
                               "income", 
                               "population",
                               "SPI.INDEX",
                               "SPI.INDEX.PIL1", 
                               "SPI.INDEX.PIL2", 
                               "SPI.INDEX.PIL3", 
                               "SPI.INDEX.PIL4", 
                               "SPI.INDEX.PIL5",
                               "population" ))

# Remove the first row of the table.
vis2_df <- vis1_df[-1,]
cols_to_convert <- c("SPI.INDEX",
                     "SPI.INDEX.PIL1", 
                     "SPI.INDEX.PIL2", 
                     "SPI.INDEX.PIL3", 
                     "SPI.INDEX.PIL4", 
                     "SPI.INDEX.PIL5") 

# Filtering out the Not Classified country
vis2_df <- vis2_df %>%
  mutate_at(vars(cols_to_convert), as.numeric)%>%filter(income != "Not classified")

# Changing column names
new_names <- c("country",
               "iso3c",
               "year",
               "region", 
               "income", 
               "population",
               "SPI_Overall_Score",
               "Data_Use_Score", 
               "Data_Services_Score", 
               "Data_Products_Score", 
               "Data_Sources_Score", 
               "Data_Infrastructure_Score")
colnames(vis2_df) <- new_names

# Filter the year
year_names <- c("2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")

df_2013_2022 <- vis2_df %>%
  filter(year %in% year_names) 


#############2. Visualization#########################################################################################
#############Visualization_Tap2#########################################################################################

# Filtering the for tab 1 visualization
vis3_df <- df_2013_2022 %>% select(country, year, income,SPI_Overall_Score)%>%
  filter(year == 2022 & income != "Not classified" & !is.na(SPI_Overall_Score))

# Define income order
income_order <- c("Low income" = "1", 
                  "Lower middle income" = "2",
                  "Upper middle income" = "3", 
                  "High income" = "4")


# Ordering the income factor
vis3_df$income <- factor(vis3_df$income, levels = names(income_order))


# Create the Plotly violin plot
plt1_tab2 <- plot_ly(
  data = vis3_df,
  x = ~income,
  y = ~SPI_Overall_Score,
  type = 'violin',
  box = list(visible = TRUE),
  meanline = list(visible = TRUE),
  color = ~income,
  colors = c("Low income" = "#a6cee3", 
             "Lower middle income" = "#1f78b4", 
             "Upper middle income" = "#6a3d9a", 
             "High income" = "#03396c"),
  line = list(width = 1),
  text = ~paste("Country:", country, "<br>Score:", SPI_Overall_Score),
  hoverinfo = 'text',
  showlegend = FALSE  # Ensure violin plot is included in the legend
)

# Add scatter plot on top of the violin plot
plt1_tab2 <- plt1_tab2 %>% add_trace(
  type = 'scatter',
  mode = 'markers',
  x = ~income,
  y = ~SPI_Overall_Score,
  text = ~paste("Country:", country, "<br>Score:", SPI_Overall_Score),
  hoverinfo = 'text',
  marker = list(size = 5, color = 'black'),
  showlegend = FALSE  # Exclude scatter plot from the legend
)

# Customize the layout
plt1_tab2 <- plt1_tab2 %>% layout(
  title = list(
    text = "Distribution of SPI Overall Score by Income Categories (Year 2022)",
    font = list(size = 15)  # Change the font size here
  ),
  xaxis = list(title = "", tickangle = 0, size = 11),  # Remove x-axis label
  yaxis = list(title = "SPI Overall Score", tickvals = seq(20, 100, by = 15), size = 11),
  plot_bgcolor = 'white'
  
)


# Compute the mean and the percentage of the SPI score in 2022
vis4_df1 <- vis2_df %>%
  filter(year == 2022 & income != "Not classified") %>%
  group_by(income) %>%
  summarize(
    SPI_Overall_Mean = mean(SPI_Overall_Score, na.rm = TRUE),
    Data_Use_Mean = mean(Data_Use_Score, na.rm = TRUE),
    Data_Services_Mean = mean(Data_Services_Score, na.rm = TRUE),
    Data_Products_Mean = mean(Data_Products_Score, na.rm = TRUE),
    Data_Sources_Mean = mean(Data_Sources_Score, na.rm = TRUE),
    Data_Infrastructure_Mean = mean(Data_Infrastructure_Score, na.rm = TRUE)
  ) %>%
  rowwise() %>%
  mutate(
    total_sum = sum(c_across(Data_Use_Mean:Data_Infrastructure_Mean)),
    Data_Use_Percent = Data_Use_Mean / total_sum,
    Data_Services_Percent = Data_Services_Mean / total_sum,
    Data_Products_Percent = Data_Products_Mean / total_sum,
    Data_Sources_Percent = Data_Sources_Mean / total_sum,
    Data_Infrastructure_Percent = Data_Infrastructure_Mean / total_sum
  ) %>%
  select(-total_sum)



# Plot using Plotly with custom colors
plt2_tab2 <- plot_ly(vis4_df1, x = ~Data_Use_Percent, y = ~income, type = 'bar', orientation = 'h', name = 'Data Use Pillar',
                     hovertemplate = ~paste("Mean Score: ", round(Data_Use_Mean, 2)), 
                     text = ~paste(round(Data_Use_Percent * 100, 0), "%"), textposition = "auto") %>%
  add_trace(x = ~Data_Services_Percent, name = 'Data Services Pillar',
            hovertemplate = ~paste("Mean Score: ", round(Data_Services_Mean, 2)), 
            text = ~paste(round(Data_Services_Percent * 100, 0), "%"), textposition = "auto") %>%
  add_trace(x = ~Data_Products_Percent, name = 'Data Products Pillar',
            hovertemplate = ~paste("Mean Score: ", round(Data_Products_Mean, 2)), 
            text = ~paste(round(Data_Products_Percent * 100, 0), "%"), textposition = "auto") %>%
  add_trace(x = ~Data_Sources_Percent, name = 'Data Sources Pillar',
            hovertemplate = ~paste("Mean Score: ", round(Data_Sources_Mean, 2)), 
            text = ~paste(round(Data_Sources_Percent * 100, 0), "%"), textposition = "auto") %>%
  add_trace(x = ~Data_Infrastructure_Percent, name = 'Data Infrastructure Pillar',
            hovertemplate = ~paste("Mean Score: ", round(Data_Infrastructure_Mean, 2)), 
            text = ~paste(round(Data_Infrastructure_Percent * 100, 0), "%"), textposition = "auto") %>%
  layout(
    barmode = 'stack',
    title = 'Percentage of Scores by Income',
    xaxis = list(
      title = 'Percentage',
      tickformat = ',.0%'
    ),
    yaxis = list(title = NULL),
    hovermode = 'closest',
    hoverdistance = 1,
    font = list(family = "Helvetica", size = 11),
    legend = list(
      orientation = 'h',
      x = 0.5,
      y = -0.2,
      xanchor = 'center',
      yanchor = 'top'
    )
  )

# Data Preparation for the plt02_tab2
vis4_df <- vis2_df %>%
  filter(year == 2022 & income != "Not classified") %>%
  group_by(income) %>%
  summarize(SPI_Overall_Mean = mean(SPI_Overall_Score, na.rm = TRUE),
            Data_Use_Mean = mean(Data_Use_Score, na.rm = TRUE),
            Data_Services_Mean = mean(Data_Services_Score, na.rm = TRUE),
            Data_Products_Mean = mean(Data_Products_Score, na.rm = TRUE),
            Data_Sources_Mean = mean(Data_Sources_Score, na.rm = TRUE),
            Data_Infrastructure_Mean = mean(Data_Infrastructure_Score, na.rm = TRUE))

# Pivot the dataset to long formate
vis4_df_long <- vis4_df %>%
  pivot_longer(cols = starts_with("Data_"), 
               names_to = "Category", 
               values_to = "Mean_Score") %>%
  group_by(income, Category) %>%
  summarize(Mean_Score = mean(Mean_Score, na.rm = TRUE)) %>%
  group_by(income) %>%
  mutate(Percentage = (Mean_Score / sum(Mean_Score)) * 100)


# Create costum label
custom_labels <- c("Data_Use_Mean" = "Data Use", 
                   "Data_Services_Mean" = "Data Services", 
                   "Data_Products_Mean" = "Data Products", 
                   "Data_Sources_Mean" = "Data Sources", 
                   "Data_Infrastructure_Mean" = "Data Infrastructure")

# Create customs color
custom_colors <- c("Data_Use_Mean" = "#f1eef6", 
                   "Data_Services_Mean" = "#bdc9e1", 
                   "Data_Products_Mean" = "#74a9cf", 
                   "Data_Sources_Mean" = "#2b8cbe", 
                   "Data_Infrastructure_Mean" = "#045a8d")

# Create a tree plot function for display the distribution of the pillar 
# For an income group.
plt3_tab2 <- function(data) {
  
  custom_labels <- c("Data_Use_Mean" = "Data Use", 
                     "Data_Services_Mean" = "Data Services", 
                     "Data_Products_Mean" = "Data Products", 
                     "Data_Sources_Mean" = "Data Sources", 
                     "Data_Infrastructure_Mean" = "Data Infrastructure")
  
  custom_colors <- c("Data_Use_Mean" = "#f1eef6", 
                     "Data_Services_Mean" = "#bdc9e1", 
                     "Data_Products_Mean" = "#74a9cf", 
                     "Data_Sources_Mean" = "#2b8cbe", 
                     "Data_Infrastructure_Mean" = "#045a8d")
  
  # Plotting TreeMap Graph
  ggplot(data, aes(area = Mean_Score, fill = Mean_Score, label = paste0(Category = custom_labels, "\n", sprintf("%.f", Mean_Score)))) + 
    geom_treemap(layout = "squarified", color="black") + 
    geom_treemap_text(place = "centre", size = 10) + 
    labs(title = "The Mean Score of Each Pillar Contributes to The Overall Score") +  
    scale_fill_distiller(name = "Mean score", direction = 1, palette = "Blues", type = "seq")
}

#############Visualization_Tab3#########################################################################################

# Data filtering 
select_col <- c("country",
                "year",
                "income",
                "SPI_Overall_Score",
                "Data_Use_Score",
                "Data_Sources_Score",
                "Data_Products_Score",
                "Data_Infrastructure_Score",
                "Data_Services_Score"
                )


# Pivoting the dataset
vis1_df_log <- df_2013_2022 %>% 
  select(select_col)%>%
  pivot_longer(cols = ends_with("_Score"), 
               names_to = "Category", 
               values_to = "Score")


# Using the custom_pillar mapping vector for replacing the values
custom_pillar <- c("SPI_Overall_Score" = "SPI Overall Score",
                   "Data_Use_Score" = "Data Use Pillar", 
                   "Data_Services_Score" = "Data Services Pillar", 
                   "Data_Products_Score" = "Data Products Pillar", 
                   "Data_Sources_Score" = "Data Sources Pillar", 
                   "Data_Infrastructure_Score" = "Data Infrastructure Pillar")

# Create pillar column.
vis1_df_log <- vis1_df_log %>% 
  mutate(
    Pillar = custom_pillar[Category]
  )


# Summarize the data by year and income
plt01_tab3_df <- vis1_df_log %>% filter(year>=2016)%>%
  group_by(year, income) %>% 
  summarise(Mean_Score = mean(Score, na.rm = TRUE)) %>%
  ungroup()


# Create the fitst plot for tab3.
# Employ plotly to make plot more interative. 
plt01_tab3 <- plot_ly(plt01_tab3_df, 
                      x = ~year, 
                      y = ~Mean_Score, 
                      color = ~income,
                      colors = c("Low income" = "#bfd3e6", 
                                 "Lower middle income" = "#8c96c6",
                                 "Upper middle income" = "#88419d",
                                 "High income" = "#4d004b"),
                      type = 'scatter', 
                      mode = 'lines+markers') %>%
  layout(
    title = list(text = "SPI Score Over Years", 
                 font = list(size = 16, 
                             color = "black", 
                             bold = TRUE)
    ),
    xaxis = list(
      title = list(
        text = "Year",
        font = list(size = 12, color = "black", bold = TRUE),
        standoff = 10  # adds some space between the title and the axis
      ),
      rangeslider = list(visible = TRUE),
      range = c(min(plt01_tab3_df$year), max(plt01_tab3_df$year))
    ),
    yaxis = list(
      title = list(
        text = "SPI Score",
        font = list(size = 12, color = "black", bold = TRUE),
        standoff = 10  # adds some space between the title and the axis
      )
    ),
    shapes = list(
      list(
        type = "line",
        x0 = min(plt01_tab3_df$year), x1 = min(plt01_tab3_df$year),
        y0 = min(plt01_tab3_df$Mean_Score), y1 = max(plt01_tab3_df$Mean_Score),
        line = list(color = "blue", width = 1)
      )
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      y = -0.5,
      xanchor = "center",
      yanchor = "top",
      font = list(size = 10)
    )
  )

# Create the option for the filtering the pillars
pillars <- c("Data Use Pillar", 
            "Data Sources Pillar",
            "Data Products Pillar",
            "Data Infrastructure Pillar",
            "Data Services Pillar")


# Visualization For the plt02_tab3
# Create a function that get pillar and data set as inputs then plot the tree.
# Then produce the tree plot for that specific pillar.
plt02_tap3 <- function(pillar, data){
  
  # Box plot of the score distribution (plot1) 
  income_order <- c("Low income", "Lower middle income", "Upper middle income", "High income")
  
  # Ensure that 'year' and 'income' in 'data' are treated as factors with specified levels
  data$year <- factor(data$year, levels = rev(unique(data$year)))  # Reversing the order of years
  data$income <- factor(data$income, levels = income_order)  # Ensuring income levels are ordered
  
  # Create the plot
  ggplot(data, aes(x = year, y = Score, fill = income)) +
    geom_boxplot(width = 0.9) +  # Increase the width of the boxes
    labs(x = "Year", y = "Score", fill = "Income Category") +
    ggtitle(bquote(bold(.(pillar))), subtitle = "Yearly Performances Score & Income Categories") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.8),
          axis.text.y = element_text(angle = 0, hjust = 0.8),
          legend.position = "bottom",
          panel.spacing = unit(0.4, "lines"),  # Adjust spacing between facets
          strip.background = element_blank(),
          strip.text = element_text(size = 20),
          plot.background = element_rect(fill="white")) +
    scale_fill_manual(values = c("Low income" = "#bfd3e6", 
                                 "Lower middle income" = "#8c96c6",
                                 "Upper middle income" = "#88419d",
                                 "High income" = "#4d004b")) + 
    scale_y_continuous(breaks = seq(20, 100, by = 20))
}


#############Visualization_Tab4#########################################################################################
# Create an array contain the need the column name.
vis6_df_names <- c("country",
                   "iso3c",
                   "income",
                   "year",
                   "SPI_Overall_Score",
                   "population")

# Create the array contain the year for filtering. 
names_10years <- c("2022", "2021", "2020", "2019", "2018", "2017",
                   "2016", "2015", "2014", "2013")

# Perform select and filtering to create needed dataset.
vis6_df <- df_2013_2022 %>% select(vis6_df_names) %>% filter(year %in% names_10years)


# Create labels for the Quantile
class_labels <- c("Q1", "Q2", "Q3", "Q4")

# Make the dataframe for mapping
map_df <- vis6_df %>% select(c("country",
                               "iso3c",
                               "income",
                               "SPI_Overall_Score",
                               "population")) %>% 
  group_by(country, iso3c, income) %>% 
  summarise(SPI_Overall_10Y_mean = as.numeric(mean(SPI_Overall_Score, na.rm = TRUE)))

# Create filtering function. This function will first filter the indicator name 
# form data1 then joint it  data2. This is for joint the SPI and WDI. 
filter_tap4 <- function(name, data1, data2){
  
  # Filter data1 to include only rows where Indicator.Name matches the specified name
  indicator_df <- data1 %>% 
    filter(Indicator.Name == name)
  
  # Perform a left join with data2 and select specific columns
  filtered_df <- left_join(data2, indicator_df, by = c("iso3c" = "Country.Code")) %>%
    select(Country.Name, Indicator.Name, SPI_Overall_10Y_mean, Mean_Across_Years, income)
  
  # Drop rows with any NA values in the specified columns
  filtered_df <- filtered_df %>% drop_na(SPI_Overall_10Y_mean, Mean_Across_Years, income)
  
  # Return the cleaned DataFrame
  return(filtered_df)
}

# Calculate quintiles (five quantiles for five classes)
quantiles <- quantile(map_df$SPI_Overall_10Y_mean, probs = seq(0, 1, by = 0.25), na.rm = TRUE)

# Use cut() function to create classes
map_df$Quantile <- cut(map_df$SPI_Overall_10Y_mean, breaks = quantiles, labels = class_labels, include.lowest = TRUE)

# Create the annotation for the 
class_range_led <- paste0(
  "<div style='background-color: rgba(255, 255, 255, 0.8); ",
  "padding: 4px; ",  # Reduced padding
  "border-radius: 5px; ",
  "font-size: 10px; ",  # Smaller font size
  "max-width: 200px; ",  # Max width set
  "box-sizing: border-box;'>",  # Include padding in width calculation
  "<strong>Quantile Range:</strong><br>",
  "Q1: 23.14 – 52.43<br>",
  "Q2: 52.431 – 62.23<br>",
  "Q3: 62.231 – 77.99<br>",
  "Q4: 77.991 – 91.03",
  "</div>"
)

# Create the color factor for the polygones on the map.
cpal <- colorFactor(palette = c('#fff7fb', "#bcbddc","#807dba","#034e7b"), 
                    domain = c("Q1", "Q2", "Q3", "Q4"))

# Get world countries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join map and filtered data
joined_geo_data <- left_join(world %>%
                               filter(sovereignt != "Antarctica"),
                             map_df,
                             by = c("wb_a3" = "iso3c"))

# Create the popup description when user click on the polygone.
PopUp <- paste("<strong>Country:</strong>", joined_geo_data$country,
               "<br><strong>SPI Score:</strong>", round(joined_geo_data$SPI_Overall_10Y_mean, 2),
               "<br><strong>Income:</strong>", joined_geo_data$income,
               "<br><strong>Quantile:</strong>", joined_geo_data$Quantile)

# Define the Quantile_color definition
Quantile_color <- c("Q1" = "#fff7fb", "Q2" = "#bcbddc", "Q3" = "#807dba", "Q4" = "#034e7b")


# Create the tooltip for the map
tootTipe <- paste("Country:", joined_geo_data$country,  
                  "\n\n, SPI Score:", round(joined_geo_data$SPI_Overall_10Y_mean, 2)
                  )


# Reference
# Title: Working with projections in Leaflet
# Author: Leaflet
# Date: n.d
# Source: https://rstudio.github.io/leaflet/articles/projections.html
# Define the Robinson projection CRS in PROJ4 format
robinsonCRS <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:54030",
  proj4def = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
  resolutions = 2^(15:0),
  origin = c(-20037508.342789244, 20037508.342789244)
)


# Assuming 'joined_geo_data' is correctly prepared with 'Quantile'
leaflet_map <-leaflet(joined_geo_data,options = leafletOptions(crs = robinsonCRS,  minZoom = 0, maxZoom = 10)) %>%
  setView(15.890556, 16.622167, 0)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~cpal(Quantile), 
    color = "#FFFFFF",  # Border color of the polygons
    weight = 1,  # Border width
    dashArray = "3",  # Style of the border (dashed in this case)
    fillOpacity = 0.7,  # Opacity of the fill color
    highlight = highlightOptions(
      weight = 5,
      color = "blue",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~tootTipe,  # Tooltip showing quantile
    popup = ~PopUp,  # Popup also showing quantile
    group = ~Quantile) %>%
  addLegend(
    pal = colorFactor(Quantile_color, domain = names(Quantile_color)),
    values = ~Quantile,
    opacity = 0.7,
    title = "Quantile",
    position = "bottomright"
  ) %>%
  addLayersControl(overlayGroups = unique(na.omit(joined_geo_data$Quantile)),
                   options = layersControlOptions(collapsed = FALSE))%>%
  addControl(html = class_range_led, position = "bottomleft")


############## 3. Description #########################################################################################
############# Description Tab1 #########################################################################################
title <- "Project Title:  Examine and Explore Specific Areas of Focus to Enhance the Statistical Capacity of Poor Nations, Leveraging the World Bank's Statistical Performance Indicators (SPI) Dataset."

# The text for the motivation for the project.
motivation_text <-"The motivation of this project is to identify the area that
                  needs a specific focus area to accelerate and use the data at 
                  its full potential. By doing so, we aim to uncover the strengths
                  and weaknesses of their capabilities. To achieve this objective,  
                  Data exploratory analysis and visualization are used to answer
                  the following questions."

questions <-  '
  1. How do SPI scores compare between high-income, upper-middle-income, lower-middle-income, and low-income countries?<br>
  2. What are the common <b> strong </b> and <b> weak </b> areas that contribute to the SPI of those countries?<br>
  3. What is the trend of each area over the observed period of these interesting countries?<br>
  4. Is the SPI score influenced by other development indicators (such as <b> GDP per capita, Government Effectiveness, and the Human Capital Index (HCI)) </b>? What is its relationship?<br>
  5. Which regions show significant inefficiencies? How can optimizing data utilization in these regions benefit global development?'                        
  

# Create the data frame to describe what is the pillars refer to.
pillar_data <- data.frame(
  Pillar = c("Data Use", "Data Services", "Data Products", "Data Sources", "Data Infrastructure"),
  Description = c(
    "Statistics hold significance solely when they are put into use. Therefore, the primary pillar is data utilization. An effective statistical framework generates data that are extensively and regularly utilized.",
    "Various services establish connections between data consumers and producers, fostering communication and trust while enhancing the perceived value of data.",
    "Interactions between producers and users influence the development and scope of statistical products, impacting their precision, promptness, frequency, comparability, and granularity. These products serve as indicators of countries' capacity to generate data relevant to the 17 Sustainable Development Goals.",
    "In order to develop valuable products, the statistical system must leverage sources both within and beyond governmental realms. Consequently, data collection extends beyond traditional censuses and surveys to incorporate administrative records, geospatial data, and information generated by private enterprises and individuals.",
    "An advanced statistical system possesses robust hard infrastructure, including legislation, governance, and standards, alongside flexible soft infrastructure, such as expertise and partnerships. Additionally, it has sufficient financial resources to produce valuable and widely utilized data products and services."
  ),
  stringsAsFactors = FALSE
)

############## Description Tab2 ######################################################################################

into_tab2_des <-"This section is to examination of the score distribution, 
                which aim to identify the pillars exerting significant influence 
                  on the Statistical Performance Index (SPI) in the current scenario. 
                  A high percentage of influence indicates strong performance in a particular area, 
                  whereas a low percentage suggests inefficient performance."

manual_tab2 <-'<div style="padding-left: 1em;">
  This tab consists on three plots, each plot have diffrent interactive element as discribed below.<br>
  <div style="padding-left: 1em;">
  1. Violin Plot:  User can hover to see the details of each county<br>
  2. Horizontal Stacked Bar Plot: User can hover to see the detail score of each pillar with corresponding percentage of contribution. <br>
  3. Tree Plot: User can filter to see the detail score of each pillar by income category. <br>
</div>
  </div>'


plt01_tab2_des <-"The distribution of statistical performance scores 
                  among countries across different income categories, 
                  based on 2022 data, shows a positive trend: as income 
                  levels increase, so do the mean and median statistical 
                  performance scores. However, the Upper-middle and 
                  High-income categories display a wider spread, indicating 
                  greater diversity in statistical performance among countries 
                  within these brackets."

plt02_tab2_des <-"The distribution and proportion of scores for each pillar across 
                  different income levels highlight how certain pillars are more 
                  prominent within specific income categories. The stacked bar 
                  plot shows that the lack of data sources is pronounced among 
                  Low, Lower-Middle, and Upper-Middle-Income countries, 
                  contributing less to the overall score, 
                  while data products contribute the highest. 
                  n contrast, High-Income countries provide good data services 
                  and effective use but have fewer data products. Note that this 
                  represents percentage contributions; the actual scores are discussed 
                  in the section below."

plt03_tab2_des <-"The tree plot captures the two scenarios in 
                  the average score contribution of each pillar. 
                  High-income countries typically excel in data use and data products, 
                  and although they exhibit a relatively low in data services, still the score is above average. 
                  In contrast, other income country groups, while strong in data service,  
                  infrastructure, and products (as high as, 78, 77, and 71 respectively), 
                  also reveal a limited capacity in data sourcing and data use (as low as 25 and 42 )"

summary_tab2 <-"This insight underscores a critical concern among countries below the
                High-income: a lack of capacity to fully leverage the benefits of data and identify 
                reliable data sources. Additionally, it suggests that these countries have not yet 
                effectively maximized their data infrastructures."

############## Description Tab3 ######################################################################################

into_tab3_des <-"This section examines the distribution of scores for the 
                overall SPI and individual pillars. The individual pillar scores are
                analyzed over 10 years, from 2013 to 2022. However, the overall SPI
                score is observed over 7 years, from 2016 to 2022, as some pillars 
                were only developed and data collected starting in 2016. A line plot
                will visualize the performance trends of the overall SPI scores across
                different income categories. Additionally, a second plot will use box
                plots to visualize the trends of individual pillar scores for each country
                during the specified period."

manual_tab3 <- '<div style="padding-left: 1em;">
        In this tab, users can observe the evolution of overall SPI score and the performance of each piller, 
through below interactive.<br>
        <div style="padding-left: 1em;">
            1. Line Chart: Users can hover over the line to observe score on each year.<br>
            2. Line Chart: Users can use the slider at the bottom of the plot to view a specific range of time.<br>
            3. Box Plot: User can filter to see detail performance of each pillar.<br>
            4. Box Plot: The description for box plot is dynamically updated as the plot undate.
        </div>
     </div>'


plt01_tab3_des <- "Overall, we observe an upward trend in the overall score across all income categories. 
                The scores for each year align with the 2022 overall score, with High-Income countries 
                consistently having the highest scores, followed by Upper Middle-income,
                Lower Middle-income, and Low-income countries. This historical pattern 
                remains consistent. Notably, there was a better improvement from 2019 to 2021,
                with a higher increase rate during this period compared to other durations. 
                However, after 2021, the performance plateaued across all income categories.<br>
                
                Interestingly, Low-income countries have shown an average SPI overall
                score increase from 43 in 2016 to 53.8 in 2022, with an annual increase 
                rate of 1.8. At this rate, they are projected to reach a score of 70 by 2031 
                if the same performance continues."

# Make the function for the dynamic text.
pillar_desc <- c(
  "Data Use Pillar" = "Noticeably, among the observed countries, 
                    the Data Use’s score witnessed an initial increase, followed by 
                    a plateau in 2016. Conversely, low-income countries exhibited 
                    a fluctuation from 2016 to 2022.",
  "Data Sources Pillar" = "Since 2016, the Data Sources Pillar has seen only slight 
                            progress. Furthermore, while the performance of Lower-Middle, 
                            Upper-Middle, and High-income countries has remained 
                            relatively stable, Low-income countries have shown a
                            noticeable decline over the analyzed period.",
  "Data Products Pillar" = "The pillar exhibited moderate growth over the 
                             analyzed period. Notably, Upper-Middle-income countries showed 
                             significant progress in producing quality data,
                             with their median often surpassing others. 
                             Additionally, High-income countries 
                             displayed considerable dispersion.",
  "Data Infrastructure Pillar" = "All income country groups have shown 
                                   significant progress in improving the infrastructures 
                                   supporting all aspects of data. The plot indicates 
                                   an upward trend over time, with low-income countries 
                                   consistently at the lowest levels and high-income 
                                   countries at the highest each year.",
  "Data Services Pillar" = "This pillar exhibited significant improvement 
                             from 2016 to 2018; however, it stagnated across all 
                             income brackets after 2018. Each year, low-income 
                             countries consistently ranked the lowest, 
                             while high-income countries ranked the highest. 
                             Interestingly, score variations 
                             decreased over time, as evidenced by 
                             the reduced dispersion."
)

summary_tab3 <- "To maximize data's potential, especially in lower-income countries,
              the key is to emphasize data utilization. Statistics gain 
            value when actively used, so enhancing Data Use is crucial. 
            Additionally, optimizing Data Sources involves diverse data 
            types beyond traditional sources like censuses and surveys. T
            his is vital for creating valuable data products, including admini
            strative and geospatial data, as well as data from private firms and citizens."


############## Description Tab4 ######################################################################################

into_tab4_des <-"This section is to examine a scatter plot depicting the
                Mean SPI score alongside three key development indicators of each country. 
                Those indicators include GDP per capita (measured in current US dollars), 
                Government Effectiveness, and the Human Capital Index (HCI) on a scale of 0 to 1. 
                Then we use the regression model to fit the appropriate function 
                and observe the relationship. 
                To evaluate the relationship between those variables we use the R² and P-value"
manual_tab4 <- '<div style="padding-left: 1em;">
        In this tab, users can observe the relationship between the SPI score and other development indicator.<br>
        <div style="padding-left: 1em;">
            1. Users can filter different indices from the drop-down list <b>Select Indicator Type</b>.<br>
            2. Users can then select the regression line from the drop-down list <b>Select Regression Type</b>.<br>
            3. The <b>Statistical Summary</b> section will update dynamically.
        </div>
     </div>'


plt_tap4_des <- "The SPI scores show a significant correlation with various 
                  development indicators. The relationship with GDP per capita
                  follows a more logarithmic form, rather than linear,
                  explaining 31.3% of the variance, indicating diminishing 
                  returns as GDP per capita increases. Conversely, the correlation 
                  between SPI scores and both the Human Development Index (HDI) and 
                  Government Effectiveness Index (GEI) is more linear. Higher adjusted 
                  R² values in linear regression models for HDI and GEI 
                  (56%, and 49% respectively) suggest a direct relationship with SPI scores.
                  This highlights that effective governance and well-informed citizens are 
                  crucial for maximizing SPI, underscoring the importance of government efficiency 
                  and education in driving economic and social progress."


############## Description Tab5 ######################################################################################

intro_tab5 <- "The objective of this section is to highlight the disparity in 
                data utilization across different regions, using visualization 
                techniques.  It aims to emphasize the potential benefits 
                of optimizing data usage, particularly in developing 
                countries, which predominantly fall within the lower 
                quantile ranges. This analysis seeks to underscore 
                the potential for substantial socio-economic advancements 
                and contributions to global progress."

Manual_tab5 <- '<div style="padding-left: 1em;">
        In this tab, users can observe which set of qualtile range that a countries belong to based on the score collected in the year 2022. This tab consist of the interactive element as follows:<br>
        <div style="padding-left: 1em;">
            1. Users can filter different set of countries by quantile range by click on quantile <b>(Q1, Q2, Q3, Q4)</b>.<br>
            2. Users can hover over the country to see the country name and overal score.<br>
            3. Users can click to see the detail information on a specific country.
        </div>
     </div>'


summary_tab5 <- "The choropleth map reveals that many countries, particularly in 
                the developing world, have not yet maximized their data potential.
                Most countries are in the lower quantile ranges, while those in 
                the 4th quantile are mainly located in North America and Europe,
                with a few in other regions. Asia and Africa display a wide range 
                of scores, but most countries fall into the 1st, 2nd, and 3rd quantiles.
                
                Given that Asia and Africa together make up a significant portion of the global
                population, 59.18% and 18.14% respectively,
                enhancing data utilization in these regions could greatly benefit a 
                large part of the world's population. This could positively impact 
                socio-economic development, stimulate economic growth, and contribute
                to global progress."

############### 4. User Interface ######################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "SPI SCORE"),
  dashboardSidebar(tags$style(HTML("
      .btn-primary {
        background-color: transparent !important;
        border: none !important;
        color: #9ebcda !important; /* Change the text color if needed */
        padding: 5px 10px;
        cursor: pointer;
      }
      .btn-primary:hover {
        background-color: lightgray !important; /* Optional: Add a hover effect */
      }
      .manual-text-style {padding: 1; 
                           width : 100%; 
                           height : 100%; 
                           overflow: hidden;
                           text-align: justify;
                           font-size: 14px;
                            color: #034f84;
      
                           }
      
    ")),
    actionButton("show_into", "Show Introduction", class = "btn-primary"),
    actionButton("show_spi_score", "Show SPI Score", class = "btn-primary"),
    actionButton("show_history", "Show History", class = "btn-primary"),
    actionButton("show_relation", "Show Relation", class = "btn-primary"),
    actionButton("show_map", "Show Map", class = "btn-primary"),
    tags$div(tags$hr(),
             box(width=12,
                 title = "User Manual",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 collapsed = TRUE,
             tags$ul(
               tags$li("Select the above options to access the tab or click on the tab itself."),
               tags$li(HTML("In each tab, click on the &#43; sign on the <b>User Manual</b> to extend the user manual.")),
               tags$li("Follow the instructions in each user manual to access the interactive element of each tab."),
             class = "manual-text-style"))
             ),
    
    tags$div(
      # Add a horizontal line for separation
      tags$hr(),
      style = "padding: 2px; text-align: center; position: absolute; bottom: 0; font-size: 7px; font-family: Arial;",
      HTML("<b>Data Source</b><br>"),
      HTML("World Bank. (2024, January 8). Statistical Performance Indicators (SPI) | DataBank. Databank.worldbank.org. https://datacatalog.worldbank.org/search/dataset/0037996/Statistical-Performance-Indicators")
      
    )
    
  ),
  dashboardBody(
    tabBox(tags$head(
      tags$style(HTML("
      .custom-text-style {padding: 5; 
                           width : 100%; 
                           height : 100%; 
                           overflow: hidden;
                           text-align: justify;
                           font-size: 14px;
                           font-family: Helvetica, sans-serif
                           }
      .custom-title-style {font-weight: bold; 
                          font-size: 25px; 
                          color: #034f84; 
                          font-family: 'Helvetica', sans-serif;
                            }
      .custom-h2-style {font-size: 18px;
                        color: #034f84;
                        font-weight: bold;
                        font-family: Helvetica, sans-serif;
                        }
    "))
    ),
      width = "95vh",
      id = "tabset1",
      tabPanel("INTRODUCTION", 
               id = "tabset1-panel",
               box(
                 title = "INTRODUCTION",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = NULL,
                 tags$h1(title, 
                         class = "custom-title-style"),
                 tags$h2("Motivation", class = "custom-h2-style"),
                 tags$p(motivation_text,class = "custom-text-style"),
                 tags$p(HTML(questions), 
                        style = "margin: 15px;overflow: hidden;
                        text-align: justify;
                        font-size: 14px;
                        font-family: Helvetica, sans-serif")
               ),
  box(title = "DETAIL DESCRIPTION",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      tags$p(
        "The description of each pillar used to calculate the SPI score is the description adapted from ",
        tags$a(href = "https://www.worldbank.org/en/programs/statistical-performance-indicators", "World Bank")
      ),
      DTOutput("pillarTable"),
      class = "custom-text-style")
  
      ),
      tabPanel("SPI SCORE", 
               fluidRow(
                 column(1,),
                 column(10, 
                        box(width=12,
                            tags$h2("OBJECTIVE", 
                                    class = "custom-h2-style"),
                            textOutput("into_tap2_desc"),
                            class = "custom-text-style"
                        ),
                        box(width=12,
                            title = "User Manual",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            tags$p(HTML(manual_tab2), style = "margin: 15px"),
                            class = "custom-text-style"),
                        
                        box(width=12, 
                            plotlyOutput("plt01_tap2"),
                            textOutput("plt01_tap2_desc"),
                            class = "custom-text-style")
                        )
                
                 ),
               fluidRow(
                 column(1,),
                   column(10, 
                          box(width=12,
                              plotlyOutput("plt02_tap2"),
                              textOutput("plt02_tap2_desc"),
                              class = "custom-text-style")
                          
                   )
                 ),
               fluidRow(
                 column(1,),
                 column(10,
                        box(width=10,
                            Height= 4,
                            selectInput("incomeFilter", 
                                        "Select Income Level", 
                                        choices = unique(vis2_df$income), 
                                        selected = "High income"),
                            plotOutput("plt03_tap3"),
                            class = "custom-text-style"
                            ),
                        box(width=12,
                            textOutput("plt03_tap2_desc"),
                            class = "custom-text-style"
                        )
                      )
                 
                 ),
               fluidRow(column(1,),
                        column(10,box(width =12, 
                                      tags$h2("SUMMARY", 
                                              class = "custom-h2-style"),
                                      textOutput("summary_tap2_desc"),
                                      class = "custom-text-style"))
                       )
               
               ),
      tabPanel("HISTORY",
               fluidRow(column(1,),
                        column(10,
                               box(width=12,
                                   tags$h2("OBJECTIVE", 
                                           class = "custom-h2-style"),
                                   textOutput("into_tap3_desc"),
                                   class = "custom-text-style"
                               ),
                               box(width=12,
                                     title = "User Manual",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     collapsed = TRUE,
                                     tags$p(HTML(manual_tab3)),
                                     class = "custom-text-style"),
                               box(width=12, 
                                   column(12,plotlyOutput("plt01_tap3")),
                                   column(12,tags$p(HTML(plt01_tab3_des)),
                                          class = "custom-text-style")),
                               box(width=12,
                                   column(12,
                                          selectInput("pillarFilter", 
                                           "Select Pillar Type", 
                                           choices = unique(pillars), 
                                           selected = "Data Use Pillar"),
                                          plotOutput("plt02_tap3")
                               )),box(width = 12, tags$h2("Description", 
                                              class = "custom-h2-style"),
                                      textOutput("plt02_tap3_desc"),
                                      class = "custom-text-style", 
                                      style = "background-color: #f0f8ff; padding: 10px;"),
                               box(width = 12 ,
                                   column(12,
                                          tags$h2("SUMMARY", 
                                                  class = "custom-h2-style"),
                                          textOutput("summary_tap3_desc"),
                                          class = "custom-text-style")
                               )
                               )
                               )
                        )
               ,
      tabPanel("RELATION", 
               fluidRow(column(1,),
                        column(10, 
                               box(width=12,
                                   tags$h2("OBJECTIVE", 
                                           class = "custom-h2-style"),
                                   textOutput("into_tap4_desc"),
                                   class = "custom-text-style"
                               ),
                               box(width=12,
                                     title = "User Manual",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     collapsed = TRUE,
                                     tags$p(HTML(manual_tab4)),
                                     class = "custom-text-style"))),
               fluidRow(
                 column(1,),
                 column(10,
                        box(width = 12,
                          column(6, 
                                 selectInput("indicatorFilter",
                                                "Select Indicatore Type",
                                                choices = unique(wdi_df$Indicator.Name),
                                                selected = "GDP per capita (current US$)")),
                          column(6,
                                 selectInput("regType",
                                               "Select Regression Type:",
                                               choices = c("Linear", "Logarithmic"))
                           ) )),
                 
                 ), 
               fluidRow(
                 column(1,),
                 column(10,
                        box(width = 12 ,column(12,
                                                  tags$h2("Statistical Summary", 
                                                          class = "custom-h2-style"),
                                                  textOutput("stat_tap3_desc"),
                                                  class = "custom-text-style"),
                            style = "background-color: #f0f8ff; padding: 10px;"), 
                        box(width = 12 ,column(12,
                        plotlyOutput("plt_tap4")
                        )), 
                        box(width = 12 ,
                            column(12,
                                   tags$h2("SUMMARY", 
                                           class = "custom-h2-style"),
                                   textOutput("plt_tap4_desc"),
                                   class = "custom-text-style")
                        ))
                        
                        )
                 )
               ,
      tabPanel("MAP", 
               fluidRow(
                 column(12, 
                        box(width=12,
                            tags$h2("OBJECTIVE", 
                                    class = "custom-h2-style"),
                            textOutput("into_tap5_desc"),
                            class = "custom-text-style"
                        ),
                        box(width=12,
                            title = "User Manual",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            tags$p(HTML(Manual_tab5)),
                            class = "custom-text-style")),
                 box(width = 12, 
                     column(12,tags$h3("Classification of the Country by the SPI Score Quantile", 
                                       style = "font-size: 18px;
                                       font-weight: bold;
                                       font-family: Helvetica, sans-serif;
                                       text-align: center;"), 
                            leafletOutput("plt01_tap5")))
                 ),
               fluidRow(column(12,box(width =12, 
                                      tags$h2("SUMMARY", 
                                              class = "custom-h2-style"),
                                      textOutput("summary_tab5"),
                                      class = "custom-text-style"))
               )
               )
  ),
    tags$div(id="back-to-top", "↑ Top"),
    tags$head(
      
      tags$style(HTML("
        /* Change the font size and type of the dashboard header */
        .main-header .logo {
          font-size: 12px;        /* Increasing font size */
          font-family: 'Arial', sans-serif; /* Changing font type */
        }
        .main-header .logo-lg {
          font-size: 12px;
          font-family: 'Arial', sans-serif;
        }
        
        #tabset1 {
          font-size: 15px;
        }
        
        /* Specific targeting of the h1 within the tab panel */
    #tabset1 .tab-content #tabset1-panel h1 {
      color: #92a8d1;
    }
  "
      )),tags$style(HTML("
      #back-to-top {
        position: fixed;
        bottom: 20px;
        right: 20px;
        z-index: 9999;
        width: 50px;
        height: 50px;
        font-size: 16px;
        line-height: 50px;
        color: #fff;
        background: #007BFF;
        text-align: center;
        border-radius: 5px;
        cursor: pointer;
        display: none;
      }
    ")),
      tags$script(HTML("
      $(document).ready(function() {
        $(window).scroll(function () {
          if ($(this).scrollTop() > 50) {
            $('#back-to-top').fadeIn();
          } else {
            $('#back-to-top').fadeOut();
          }
        });
        $('#back-to-top').click(function () {
          $('body,html').animate({
            scrollTop: 0
          }, 400);
          return false;
        });
      });
    ")),
      tags$style(HTML("
    #plt_tap4 .plotly {
      height: 600px !important;  /* Adjust height */
      width: 100% !important;    /* Adjust width if necessary */
    }
  "))
    )
    
    
  )
)

############## 5. Server ######################################################################################

server <- function(input, output, session) {
  
  # When the 'show_into' event is triggered, switch the displayed tab to 'INTRODUCTION'
  observeEvent(input$show_into, {
    updateTabsetPanel(session, "tabset1", selected = "INTRODUCTION")
  })
  
  # When the 'show_spi_score' event is triggered, switch the displayed tab to 'SPI SCORE'
  observeEvent(input$show_spi_score, {
    updateTabsetPanel(session, "tabset1", selected = "SPI SCORE")
  })
  
  # When the 'show_history' event is triggered, switch the displayed tab to 'HISTORY'
  observeEvent(input$show_history, {
    updateTabsetPanel(session, "tabset1", selected = "HISTORY")
  })
  
  # When the 'show_relation' event is triggered, switch the displayed tab to 'RELATION'
  observeEvent(input$show_relation, {
    updateTabsetPanel(session, "tabset1", selected = "RELATION")
  })
  
  # When the 'show_map' event is triggered, switch the displayed tab to 'MAP'
  observeEvent(input$show_map, {
    updateTabsetPanel(session, "tabset1", selected = "MAP")
  })
  
  ############Tap1############
  
  # Render the table for display the pillar description
  output$pillarTable <- renderDT({
    datatable(pillar_data, 
              options = list(pageLength = 5, autoWidth = TRUE, dom = 't'))
  })
    
  ############Tap2############
  
  # Render description text for tab 2 introduction
  output$into_tap2_desc <- renderText(into_tab2_des)
  
  # Render description text for tab 2 manual section
  output$manual_tap2_desc <- renderText(manual_tab2)
  
  # Render Plot 1 for tab 2
  output$plt01_tap2 <- renderPlotly({
    plt1_tab2
  })
  
  # Render description text for Plot 1 in tab 2
  output$plt01_tap2_desc <- renderText(plt01_tab2_des)
  
  # Render Plot 2 for tab 2
  output$plt02_tap2 <- renderPlotly({
    plt2_tab2
  })
  
  # Render description text for Plot 2 in tab 2
  output$plt02_tap2_desc <- renderText(plt02_tab2_des)
  
  # Render Plot 3 for tab 3
  output$plt03_tap3 <- renderPlot({
    # Filter data based on the selected income filter from input
    filtered_data <- vis4_df_long %>% 
      filter(income == input$incomeFilter)
    plt3_tab2(filtered_data)
  })
  
  # Render description text for Plot 3 in tab 2
  output$plt03_tap2_desc <- renderText(plt03_tab2_des)
  
  # Render summary description text for tab 2
  output$summary_tap2_desc <- renderText(summary_tab2)
  
  ############Tap3############

  # Render description text for tab 3 introduction
  output$into_tap3_desc <- renderText(into_tab3_des)
  
  # Render Plot 1 for tab 3 
  output$plt01_tap3 <- renderPlotly({
    plt01_tab3
  })
  
  # Render Plot 2 for tab 3
  output$plt02_tap3 <- renderPlot({
   
    pillar <- input$pillarFilter
    
    # Filter the dataset based on the selected pillar
    vis1_tab3_filtered <- vis1_df_log %>% filter(vis1_df_log$Pillar == pillar)
    
    # Generate the plot for tab 3 with the filtered data
    plt02_tap3(pillar, vis1_tab3_filtered)
  })
  
  # Render description text for Plot 2 in tab 3 based on the selected pillar
  output$plt02_tap3_desc <- renderText({
    pillar <- input$pillarFilter
    
    # Check if the selected pillar has a description
    if (pillar %in% names(pillar_desc)) {
      pillar_desc[[pillar]]
    } else {
      "Invalid input"
    }
  })
  
  # Render summary description text for tab 3
  output$summary_tap3_desc <- renderText(summary_tab3)
  
  
  ############Tap4############
  
  output$into_tap4_desc <- renderText(into_tab4_des)
  
    
  output$plt_tap4 <- renderPlotly({
    
    indicator <- input$indicatorFilter
    
    vis4_filtered <- filter_tap4(indicator, wdi_df, map_df)
    
    # Clean the data
    clean_data <- na.omit(vis4_filtered, subset = c("Mean_Across_Years", "SPI_Overall_10Y_mean"))
    
    # Base ggplot
    p <- ggplot(clean_data, aes(x = Mean_Across_Years, y = SPI_Overall_10Y_mean, color = income)) +
      geom_point() +  # Add points for scatter plot
      labs(color = "Income", title = "SPI Overall Score vs Development Indicator", x = indicator, y = "SPI Overall 10-Year Mean") +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),  # Title font size
        axis.title.x = element_text(size = 10),  # X-axis title font size
        axis.title.y = element_text(size = 10),  # Y-axis title font size
        legend.title = element_text(size = 8),  # Legend title font size
        legend.text = element_text(size = 8)    # Legend text font size
      )
    
    # Determine the type of regression line to add and calculate model statistics
    if (input$regType == "Linear") {
      
      model <- lm(SPI_Overall_10Y_mean ~ Mean_Across_Years, data = clean_data)
      p_value <- summary(model)$coefficients[2, 4]
      r_squared <- summary(model)$r.squared
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "blue", size = 0.3)
      
    } else if (input$regType == "Logarithmic") {
      
      model <- lm(SPI_Overall_10Y_mean ~ log(Mean_Across_Years), data = clean_data)
      p_value <- summary(model)$coefficients[2, 4]
      r_squared <- summary(model)$r.squared
      p <- p + geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "blue", size = 0.3)
    
    }
    
    # Create the dynamic text for statistical summary
    stat_text <- paste(
      "The regression model shows a moderate fit with an R-squared of", format(r_squared, digits = 3), 
      ", and a p-value of", format(p_value, digits = 3), 
      "suggesting the model is statistically significant, explaining", 
      format(r_squared * 100, digits = 2), "percent of the variance in the dependent variable."
    )
    
    # Render the dynamic text.
    output$stat_tap3_desc <- renderText(stat_text)
    
    ggplotly(p)
  })
  
  # Render the summary of tab4
  output$plt_tap4_desc <- renderText(plt_tap4_des)

  ############Tap5############
  
  # Render the introduction of the Tab5
  output$into_tap5_desc <- renderText(intro_tab5)
  
  # Render the leaflet for the map.
  output$plt01_tap5 <- renderLeaflet({

    leaflet_map
    
  }) 
  
  # Render the summary for the map.
  output$summary_tab5 <- renderText(summary_tab5)
  
}

# Run the application
shinyApp(ui = ui, server = server)


# Examine and explore specific areas of focus to enhance the statistical capacity of poor nations, leveraging The World Bank's Statistical Performance Indicators (SPI) dataset.

## 1. Introduction 
Many developing nations need reliable and accurate statistical data for a diverse range of purposes, such as poverty reduction monitoring and reporting to international aid or donors. However, their limited ability often makes them struggle to produce outstanding and compelling statistical outputs (World Bank, 2021). This project will examine the Word Bank’s Statistical Performance Indicators, to identify what are the strengths and weaknesses of developing countries, those including Low-income, Lower-middle-income, and Upper-middle-income. The insights might assist policymakers, non-profit organizations, and regulators seeking to improve their statistical performance, not only for the above purposes, but also for improving decision-making, driving innovation, and solving complex problems

The motivation of this project is to identify the area that needs a specific focus area to accelerate and use the data at its full potential. By doing so, we aim to uncover the strengths and weaknesses of their capabilities. To achieve this objective,  Data exploratory analysis and visualization are used to answer the following questions.

## 2	Design Process 
## 2.1	Aspects of Munzner’s what-why-how framework
Before the design took form, we applied Munzner's framework, which typically goes through the following steps:
## 2.1.1	Define the Data (What)
Our data sources are the Statistical Performance Index (SPI)The World Development Index (WDI), and the Natural Earth spatial dataset. The data categorization of the dataset is that we have SPI and WDI in the form of tabular data, whereas the Natural Earth Dataset is the spatial dataset. 
 
SPI and WDI data consist of numerical data (like score, population, GDP), nominal (such as names of countries and pillars), categorical data (groupings like income levels, and regions), ordinal data (ranked data such as effective government index and human development index). The Natural Earth data set contains some data like the other two, in addition, it consists of spatial coordinates of the countries’ boundary. 
#### Data Sources  
This project uses the following datasets:  

1. **[Statistical Performance Indicators (SPI) Dataset](https://datacatalog.worldbank.org/search/dataset/0037996/Statistical-Performance-Indicators)**  
   - Provided by the **World Bank**, this dataset includes statistical performance indicators for various countries.  

2. **[World Development Indicators (WDI) Dataset](https://databank.worldbank.org/source/world-development-indicators)**  
   - A comprehensive collection of development data, including economic, social, and environmental indicators.  

3. **Geospatial Data from the R Natural Earth Library**  
   - This dataset provides geospatial data such as country boundaries, landforms, and population distribution.  
   - More information: [Natural Earth](https://www.naturalearthdata.com/)  

In terms of their structures, those three datasets share a common key, the country's name. Their relationships allow us to apply join and merge operations which will enrich our visualization.
## 2.1.2	Determine the Tasks (Why)
In this visualization project, the specific tasks and objectives for the users include:

#### Q1.	Analyze Statistical Performance:
   - Objective: Understand how statistical performance varies across different income brackets.
   - Questions: How do SPI scores compare between high-income, upper-middle-income, lower-middle-income, and low-income countries?

#### Q2.	Identify Areas for Improvement:
   - Objective: Pinpoint strengths and weaknesses of each pillar in different income groups.
   - Questions: What are the common strong and weak areas that contribute to the SPI of those countries?

#### Q3.	Track Historical Temporal Trends:
   - Objective: Examine changes in SPI scores and related indicators over time.
   - Questions: What is the trend of each area over the observed period of these interesting countries?

#### Q4.	Relationship of SPI with Development Indicators:
   - Objective: Explore the relationship between SPI scores and development indicators such as GDP per capita, Government Effectiveness, and the Human Capital Index (HCI).
   - Questions: Is the SPI score influenced by other development indicators (such as GDP per capita, Government Effectiveness, and the Human Capital Index (HCI))? What is its relationship?

#### Q5.	Highlight Regional Inefficiencies:
   - Objective: Identify regions with notable inefficiencies in data handling and utilization.
   - Questions: Which regions show significant inefficiencies? How can optimizing data utilization in these regions benefit global development?
## 2.1.3	Design the Visualization (How)
To effectively communicate the findings and support the identified tasks, we will choose appropriate visual representation and interaction techniques:
#### 2.1.3.1.	Visual Representations:
   - Violin Plot:  To show the SPI score distribution of the different income brackets.
   - Stacked Bar Charts: To show how each pillar contributes to the overall performance in terms of the percentage of each income country group. This aims to highlight areas of strengths and weaknesses.
   - Tree Plot: To visualize the actual score of each pillar of each income country group.
   - Line Graphs: To show trends in SPI scores over time across different income brackets.
   - Temporal Box Plot:  To visualize the performance trend of each pillar of all income brackets. 
   - Scatter Plots with regression line: To illustrate the relationship between SPI scores and development indicators such as GDP per capita, Government Effectiveness, and the Human Capital Index (HCI).
   - Choropleth Maps: To display geographical variations in SPI scores and highlight regional inefficiencies based on the quantile range.

#### 2.1.3.2.	Interaction Elements:
   - Hover Tooltips: Provide detailed information on specific data points when users hover over them, offering additional context without cluttering the visualization.
   - Filters and Drop-Down Menus: Allow users to select specific countries, regions, income brackets, and regression lines to customize the view according to their interests.
   - Zoom and Pan: Enable users to explore data in greater detail by zooming into specific regions or indicators.
   - Interactive Legends: Allow users to toggle different indicators or income groups on and off to compare data more easily.

#### 2.1.3.3	User Guidance:
   - Simple Explanations: Include straightforward descriptions and explanations to make the visualizations accessible to a general audience.
   - Dynamic Narratives: Use storytelling elements to guide users through the data, highlighting key insights and findings.
   - Tooltips and Annotations: Offer contextual information and clarify complex statistical concepts as users interact with the visualization.

## Dashboard  
🔗 [View Live Dashboard](https://chandarakhvan.shinyapps.io/SPI_dashboard/)

Note: The raw dataset are available to download publicly on World Bank website. This visualization project do not include the data cleaning process. The data presented in this project are already cleaned and created for this specific project only. 


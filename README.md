# Discription of the App and its purpose
The App has four pages: About, Data Exploration, Modeling and Data. 

The purpose of the app is to explore breastfeeding data, which allow users to:
* Do data exploration
* Download datasets
* Fit three supervised learning models
* Use one of the models for prediction

# List of Packages used
```
install.packages(c("shiny", 
                   "shinydashboard", 
                   "DT", 
                   "plotly", 
                   "knitr", 
                   "caret", 
                   "tree", 
                   "randomForest", 
                   "tidyverse"))
                   
library(shinydashboard)
library(shiny)
library(DT)
library(plotly)
library(knitr)
library(caret)
library(tree)
library(randomForest)
library(tidyverse)

```

# Run the App
```
shiny::runGitHub(repo = "project-3", username = "xinceciliaw")
```

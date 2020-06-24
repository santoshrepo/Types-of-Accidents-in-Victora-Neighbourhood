## Types-of-Accidents-in-Victora-Neighbourhood

Link to the app: https://sandytom.shinyapps.io/RshinyApp/

The shiny app aims to correlate the types of accidents happening around Victoria with the different types of houses in Victoria. The target audience is VIC roads management, real estate brokers, and people who are keen to know about the accidents in the area while driving or traveling with family. 

- Scenario 1:
In Victoria, there are different types of accidents happening in certain areas. For instance, the Suburbs with freeway or highway will most likely have speed-related accidents because of the speed limit compared to the residential suburbs which will have less speed limit.

- Scenario 2:
The chances of colliding with pedestrians in Melbourne CBD are more likely high than compared to suburbs with freeway or highway. We could determine the likelihood of accidents in the suburbs using the type of accidents that occurred in that area.


Make sure all the libraries are installed
```
library(shiny)
library(leaflet)
library(plotly)
library(shinythemes)
library(htmltools)
library(leaflet.extras)
library(plotly)
library(shinyalert)
```

Files Descriptions:

- app.R contains the UI and Server function inbulid 
- Download the Subseted CSV to run the the shiny app
- Styles contains all the CSS styling


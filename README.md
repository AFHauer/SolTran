# SolTran
***Solute Transport Modeling in R Shiny Application***

This is a current work in progress for my MSBA Capstone Project at the University of Montana. 
The primary idea is to develop a web application which allows for the uploading of hydrologic, 
nutrient cycling, and limnologic repertory functions, and runs solute transportation models based on those data. 

This work largely works off models developed by Robert Runkel at the USGS in Denver, CO, 
and Robert Hensley and Matthew Cohen at the University of Florida.

This web application is developed in R shiny.

---

To use the Reactive Solute Transport Dashboard (SolTran), clone the SolTran repo and run the app.R Shiny Application in R. Libraries required include:

* library(shiny)
* library(shinyjs)
* library(htmlwidgets)
* library(bs4Dash)
* library(shinyTime)
* library(tidyverse)
* library(lubridate)
* library(DT)
* library(readr)

---

## Model Parameters and Data Input

### Templates

Data is input into the solute transport application in a series of templates available
from the application.

**Data Inputs**

_Conservative Tracer_

|Variable|Data Explanation|Data type|Example|
|--------|----------------|---------|-------|
|datetime|date and time of observation|datetime| yyyy-mm-dd hh:mm:ss|
|time_min|change in time for each observation|integer|1|
|us_sensor_obs|upstream sensor observation|float|1.0|
|ds_sensor_obs|downstream sensor observation|float|1.0|

_Dissolved Oxygen_

|Variable|Data Explanation|Data type|Example|
|--------|----------------|---------|-------|
|datetime|date and time of observation|datetime| yyyy-mm-dd hh:mm:ss|
|time_min|change in time for each observation|integer|1|
|us_station_obs|upstream sensor observation|float|1.0|
|ds_station_obs|downstream sensor observation|float|1.0|
|par|PAR estimate or observation|float|1.0|
|avgtemp|average temperature|float|1.0|

_Nitrate_

|Variable|Data Explanation|Data type|Example|
|--------|----------------|---------|-------|
|datetime|date and time of observation|datetime| yyyy-mm-dd hh:mm:ss|
|time_min|change in time for each observation|integer|1|
|us_station_obs|upstream sensor observation|float|1.0|
|ds_station_obs|downstream sensor observation|float|1.0|


**Model Inputs**

_Conservative Tracer Model_

|Variable|Data Explanation|Data type|Example|
|--------|----------------|---------|-------|
|datetime|date and time of observation|datetime| yyyy-mm-dd hh:mm:ss|
|time_min|change in time for each observation|integer|1|
|Loc_*n*m|Conserve Trace Model at each model location where n = distance from US observation|float|1.0|


_DO Model_

|Variable|Data Explanation|Data type|Example|
|--------|----------------|---------|-------|
|datetime|date and time of observation|datetime| yyyy-mm-dd hh:mm:ss|
|time_min|change in time for each observation|integer|1|
|Loc_*n*m|DO model at each model location where n = distance from US observation|float|1.0|

_Nitrate Model_

|Variable|Data Explanation|Data type|Example|
|--------|----------------|---------|-------|
|datetime|date and time of observation|datetime| yyyy-mm-dd hh:mm:ss|
|time_min|change in time for each observation|integer|1|
|Loc_*n*m|nitrate model at each model location where n = distance from US observation|float|1.0|

### Template Upload
Template formatted data is uploaded into each of its respective data input location.

**Example Data Sets**

The Upper Clark Fork River Datasets from a 2020 study between Cattle Road sensors and
Garrison sensors is available in this repository.

_Directories_

* example data templates: contains example data from the 2020 study for the data input
* example model data: contains example data model from each of the same 2020 study locations.

### Model Parameters

**Model Grid**

This section is for future development. The section allows users to set the distance 
between upstream and downstream observations. Users can then set the quantity of 
modeled observations between the upstream and downstream observation. 

*Distances are given in meters (m)*

**Measured Model Parameters**

These are model parameters which are measured in the field. Used for all models:

* Conservative Tracer
* Dissolved Oxygen
* Nitrate


*Average Depth is in meters (m)*

*Discharge is in cubic meters per second ($m^3/s$)*

**Calculated Model Parameters**

These are model estimated parameters. Used for all models:

* Conservative Tracer
* Dissolved Oxygen
* Nitrate


*Channel Area in square meters (m U+00B2)*

*Storage Area in square meters ($m^2$)*

*Dispersion in square meters per second ($m^2/s$)*

*Exchange Coefficient in seconds (s)*

---

## Conservative Tracer

### Model Outputs

Select the model location. Visualization shows upstream and downstream curves. Model
will update based on model distance selected. Model summary statistics is output below the model selection.

### Conservative Tracer Model Prediction Statistics

Model prediction can be determined by setting the model to the downstream distance. This can be visualized on the Model Observed by Predicted visualization. Model Prediction Statistics are output to the right of the visualization.

## Conservative Tracer Model Table

The combined model input and observation values are output in a table.

*Note: All visualizations and tables can be downloaded.* 
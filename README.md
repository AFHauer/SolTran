# SolTran Web Application
***Solute Transport Modeling R Shiny Application***

This is a current work in progress for my MSBA Capstone Project at the University of Montana. 
The primary idea is to develop a web application which allows for the uploading of hydrologic, 
nutrient cycling, and limnologic respiratory functions, and runs solute transportation models based on those data. 

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

*Conservative Tracer* 

|Model Parameters|Value|
|----------------|-----|
|Segment distance (x)|86.40 m|
|Time (t)|1 min|

UCFR Conditions from Cattle Road to Garrison September 25, 2020, until September 30, 2020

*General Hydraulic Parameters*

|Hydraulic Parameter|Value|
|-------------------|-----|
|Reach Length (L)|4320 m|
|Discharge (Q)|6.1 m<sup>3</sup>/s|
|Ave. Depth (d)|0.6 m|
|Channel Area (A)|8.82 m<sup>2</sup>|
|Storage Area (A<sub>s</sub>)|0.53 m<sup>2</sup>|
|Dispersion (D)|30.32 m<sup>2</sup>/s|
|Exchange Coeff (/alpha)|0.00004 1/s|

*DO Reactive Parameters*

|Reactive Parameter|Value|
|------------------|-----|
|Primary Production Rate Constant (k<sub>pp</sub>|0.000189 mg-O/J|
|Respiration Rate Constant (k<sub>ER</sub>|0.0017 mg-O/m<sup>-2</sup>/d<sup>-1</sup>)|
|Respiration Coeff (n<sub>ER</sub>)|0.2703 mg O<sup>2</sup>|/m<sup>2</sup>/d|
|k|0.000042 1/s|
|DO<sub>SAT</sub>|11.14 mg/L|

*Nitrate Reactive Parameters*

|Reactive Parameter|Value|
|------------------|-----|
|NPP:GPP|0.34 mol C: mol N|
|C:N|6.63 mol C: mol N|
|k<sub>D</sub>|0.00016|
|n<sub>D</sub>|1.78|

_Directories_

* example data templates: contains example data from the 2020 study for the data input
* example model data: contains example data model from each of the same 2020 study locations.

### Model Parameters

**Model Grid**

This section is for future development. The section allows users to set the distance 
between upstream and downstream observations. Users can then set the quantity of 
modeled observations between the upstream and downstream observation. 

**Measured Model Parameters**

These are model parameters which are measured in the field. Used for all models:

* Conservative Tracer
* Dissolved Oxygen
* Nitrate


*Average Depth is in meters (m)*

*Discharge is in cubic meters per second (m<sup>3</sup>/s)*

**Calculated Model Parameters**

These are model estimated parameters. Used for all models:

* Conservative Tracer
* Dissolved Oxygen
* Nitrate


*Channel Area in square meters (m<sup>2</sup>)*

*Storage Area in square meters (m<sup>2</sup>)*

*Dispersion in square meters per second (m<sup>2</sup>/s)*

*Exchange Coefficient in seconds (s)*

---

## Conservative Tracer

*Note: All visualizations and tables can be downloaded.* 

### Model Outputs

Select the model location. Visualization shows upstream and downstream curves. Model
will update based on model distance selected. Model summary statistics is output below the model selection.

### Conservative Tracer Model Prediction Statistics

Model prediction can be determined by setting the model to the downstream distance. This can be visualized on the Model Observed by Predicted visualization. Model Prediction Statistics are output to the right of the visualization.

### Conservative Tracer Model Table

The combined model input and observation values are output in a table.

---

## Dissolved Oxygen

*Note: All visualizations and tables can be downloaded.* 

### Reactive Parameters

Refresh Page data when new data is loaded into the SolTran Application. This refresh will update all data inputs to the current data input. 

Input the k<sub>pp</sub> range and adjust the slider to enter the Primary Production Rate Constant. *This is an exploratory feature for future model data entry.*

Input the Respiration Rate Constant (k<sub>ER</sub>). Average Respiration is calculate using the respiration rate constant, average depth, storage area, and area. 

*Average Respiration = (k<sub>ER</sub> x depth x 1000) x (A<sub>s</sub>/A) x 24000*

*numeric values, 1000 & 24000, for unit conversion*

### Photosynthetically Active Radiation (PAR)

PAR is input on the Dissolved Oxygen data input, template available on the Model Parameters page. Adjust the PAR slider input to see PAR Statistics and Detail Visualization for datetime between input slider range. Red vertical lines on the overall PAR visualization will adjust to display datetime range.

### Gross Primary Production (GPP)



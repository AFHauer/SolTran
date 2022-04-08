# SolTran
***Solute Transport Modeling in R Shiny Application***

This is a current work in progress for my MSBA Capstone Project at the University of Montana. 
The primary idea is to develop a web application which allows for the uploading of hydrologic, 
nutrient cycling, and limnologic repertory functions, and runs solute transportation models based on those data. 

This work largely works off models developed by Robert Runkel at the USGS in Denver, CO, 
and Robert Hensley and Matthew Cohen at the University of Florida.

This web application is developed in R shiny.

---

## Model Parameters and Data Input

###Templates

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


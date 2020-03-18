# SEIR_COVID19_Dev

(Developer's version) SEIR model for COVID-19 infection, including different clinical trajectories of infection, interventions to reduce transmission, and comparisons to healthcare capacity.

The code that produces the interface and functionality of the Shiny App is in files
* **server.R**
* **ui.R**

Files used in the explanatory sections of the app are
* **SEIR.Rmd**
* **www/Parameters.nb.html**

All the functions that actually run the model and process the parameters are in the **code/functions.R** file

If you want to run the code to produce the same outputs as Shiny but without dealing with the app structure, you can use the R scripts
* **runSpread.R**
* **runInterventions.R**
* **runCapacity.R**

When trying out new model structures or plots, it is much easier to work with scripts instead of directly with the app. Once troubleshooting is done, it can be integrated with the app


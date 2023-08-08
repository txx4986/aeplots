# aeplots
R package to create tables and plots for analysing adverse event data in clinical trials

## How to install
You can install `aeplots` with:
```
# install.packages("devtools")
devtools::install_github("https://github.com/txx4986/aeplots.git")
```

Please refer to the [devtools documentation](https://www.r-project.org/nosvn/pandoc/devtools.html) for instructions on how to set up a working development environment and install `devtools` depending on your platform.

## Overview
Functions available in the package:

-   `aetable`: Plots a table of AE summary by body system class
-   `aeseverity`: Plots a table of AE summary by severity
-   `aedot`: Plots a dot plot with proportions alongside treatment effect estimates (IRR) with accompanying 95% confidence interval to give an overview of the harm profile
-   `aestacked`: Plots a stacked bar chart to present the proportions of participants with each event by arm and by maximum severity
-   `aebar`: Plots a bar chart to present the number of events reported per participant

To see more detailed documentation of each function:
```
help(aetable)
```
or
```
?aetable
```

## Sample dataset

|id|arm|adverse_event|body_system_class|severity|date_rand|last_visit|variable1|variable2|
|---|---|---|---|---|---|---|---|---|
|2001|Placebo|Cold|Respiratory|Moderate|2015-10-28|2016-08-31|2|910.785|
|2001|Placebo|Anemia|Blood and lymphatic|Severe|2015-10-28|2016-08-31|2|910.785|
|2001|Placebo|Anemia|Blood and lymphatic|Moderate|2015-10-28|2016-08-31|2|910.785|
|2001|Placebo|Leukocytosis|Blood and lymphatic|Severe|2015-10-28|2016-08-31|2|910.785|
|2001|Placebo|Nausea|Gastrointestinal|Severe|2015-10-28|2016-08-31|2|910.785|
|2002|Intervention|Other|Blood and lymphatic|Moderate|2015-08-18|2016-07-29|2|736.752|
|2002|Intervention|Toothache|Other|Moderate|2015-08-18|2016-07-29|2|736.752|
|2002|Intervention|Accident|Other|Moderate|2015-08-18|2016-07-29|2|736.752|
|2004|Placebo|Cystitis|Renal and urinary|Mild|2015-04-26|2016-08-11|1|307.814|
|2004|Placebo|Vomiting|Gastrointestinal|Severe|2015-04-26|2016-08-11|1|307.814|
|2004|Placebo|Vomiting|Gastrointestinal|Mild|2015-04-26|2016-08-11|1|307.814|
|2004|Placebo|Vomiting|Gastrointestinal|Mild|2015-04-26|2016-08-11|1|307.814|
|2004|Placebo|Anemia|Blood and lymphatic|Moderate|2015-04-26|2016-08-11|1|307.814|
|2004|Placebo|Dry skin|Dermatological|Moderate|2015-04-26|2016-08-11|1|307.814|
|2008|Intervention|Other|Blood and lymphatic|Moderate|2015-12-04|2016-04-28|4|380.890|
|2008|Intervention|Anemia|Blood and lymphatic|Mild|2015-12-04|2016-04-28|4|380.890|
|2008|Intervention|Anemia|Blood and lymphatic|Mild|2015-12-04|2016-04-28|4|380.890|
|2008|Intervention|Nausea|Gastrointestinal|Moderate|2015-12-04|2016-04-28|4|380.890|
|2014|Intervention|Headache|Neuruological|Mild|2015-10-16|2016-01-19|3|389.012|
|2014|Intervention|Headache|Neuruological|Mild|2015-10-16|2016-01-19|3|389.012|

## Variable description for sample dataset
|Variable name|Variable description|Variable type|
|---|---|---|
|id|Participant ID|Factor/Character/Numeric|
|arm|Treatment arm of participant (2, 3 or 4 arms)|Factor/Character/Numeric|
|adverse_event|Adverse event preferred term|Character|
|body_system_class|Body system class of adverse event|Factor|
|severity|Severity of adverse event|Factor|
|date_rand|Randomisation date|Date|
|last_visit|Date of last visit|Date|
|variable|Variables to be included in generalised linear models with Poisson function and log link for estimation of IRRs and 95% CIs|Factor/Character/Numeric|

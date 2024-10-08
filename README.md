# aeplots
R package to create tables and plots for analysing adverse event data in clinical trials

## How to install
You can install `aeplots` with:
```
# install.packages("devtools")
devtools::install_github("https://github.com/txx4986/aeplots.git")
```

Please refer to the [devtools documentation](https://www.r-project.org/nosvn/pandoc/devtools.html) for instructions on how to set up a working development environment and install `devtools` depending on your platform.

## Adverse Event Data
Functions available in the package:

-   `aetable`: Plots a table of AE summary by body system class
-   `aeseverity`: Plots a table of AE summary by severity and body system class
-   `aedot`: Plots a dot plot with proportions alongside treatment effect estimates with accompanying 95% confidence interval to give an overview of the harm profile
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

### Sample dataset

|id|arm|adverse_event|body_system_class|severity|date_rand|date_ae|last_visit|variable1|variable2|
|---|---|---|---|---|---|---|---|---|---|
|2001|Placebo|Cold|Respiratory|Moderate|2015-10-28|2016-04-24|2016-08-31|2|910.785|
|2001|Placebo|Anemia|Blood and lymphatic|Severe|2015-10-28|2015-11-11|2016-08-31|2|910.785|
|2001|Placebo|Anemia|Blood and lymphatic|Moderate|2015-10-28|2016-05-10|2016-08-31|2|910.785|
|2001|Placebo|Leukocytosis|Blood and lymphatic|Severe|2015-10-28|2016-08-29|2016-08-31|2|910.785|
|2001|Placebo|Nausea|Gastrointestinal|Severe|2015-10-28|2016-02-23|2016-08-31|2|910.785|
|2002|Intervention|Other|Blood and lymphatic|Moderate|2015-08-18|2015-11-17|2016-07-29|2|736.752|
|2002|Intervention|Toothache|Other|Moderate|2015-08-18|2016-04-30|2016-07-29|2|736.752|
|2002|Intervention|Accident|Other|Moderate|2015-08-18|2016-03-02|2016-07-29|2|736.752|
|2004|Placebo|Cystitis|Renal and urinary|Mild|2015-04-26|2015-07-06|2016-08-11|1|307.814|
|2004|Placebo|Vomiting|Gastrointestinal|Severe|2015-04-26|2016-06-20|2016-08-11|1|307.814|
|2004|Placebo|Vomiting|Gastrointestinal|Mild|2015-04-26|2016-03-19|2016-08-11|1|307.814|
|2004|Placebo|Vomiting|Gastrointestinal|Mild|2015-04-26|2015-08-21|2016-08-11|1|307.814|
|2004|Placebo|Anemia|Blood and lymphatic|Moderate|2015-04-26|2016-07-26|2016-08-11|1|307.814|
|2004|Placebo|Dry skin|Dermatological|Moderate|2015-04-26|2016-03-26|2016-08-11|1|307.814|
|2008|Intervention|Other|Blood and lymphatic|Moderate|2015-12-04|2016-01-07|2016-04-28|4|380.890|
|2008|Intervention|Anemia|Blood and lymphatic|Mild|2015-12-04|2016-02-11|2016-04-28|4|380.890|
|2008|Intervention|Anemia|Blood and lymphatic|Mild|2015-12-04|2016-02-05|2016-04-28|4|380.890|
|2008|Intervention|Nausea|Gastrointestinal|Moderate|2015-12-04|2016-04-19|2016-04-28|4|380.890|
|2014|Intervention|Headache|Neuruological|Mild|2015-10-16|2016-01-18|2016-01-19|3|389.012|
|2014|Intervention|Headache|Neuruological|Mild|2015-10-16|2015-12-27|2016-01-19|3|389.012|

Note that for each participant who did not experience any adverse events, a row should be included in the input dataset stating his/her `id`, `arm`, `date_rand`, `last_visit` and `variables` to be included in the model. `adverse_event`, `body_system_class`, `severity` and `date_ae` column should be specified as `NA`. For example:

|id|arm|adverse_event|body_system_class|severity|date_rand|date_ae|last_visit|variable1|variable2|
|---|---|---|---|---|---|---|---|---|---|
|2032|Intervention|NA|NA|NA|2015-05-22|NA|2016-04-22|2|1500|
|2033|Placebo|NA|NA|NA|2015-04-20|NA|2016-03-20|1|777|

### Variable description for sample dataset

|Variable name|Variable description|Variable type|
|---|---|---|
|id|Participant ID|Factor/Character/Numeric|
|arm|Treatment arm of participant (2, 3 or 4 arms)|Factor/Character/Numeric|
|adverse_event|Adverse event preferred term|Character|
|body_system_class|Body system class of adverse event|Factor|
|severity|Severity of adverse event (2, 3, 4 or 5 severity levels)|Factor|
|date_rand|Randomisation date|Date|
|date_ae|Date of adverse event|Date|
|last_visit|Date of last visit|Date|
|variable|Variables to be included in model for estimation of treatment effect estimates and 95% CIs|Factor/Character/Numeric|

## Laboratory Data
Functions available in the package:

-   `labtable`: Plots a table that summarises laboratory values with continuous outcomes for baseline and each post-baseline timepoint by treatment arm

To see more detailed documentation of each function:
```
help(labtable)
```
or
```
?labtable
```

### Sample dataset

|id|arm|visit|lab_test|base|aval|region|strat|time|
|---|---|---|---|---|---|---|---|---|
|1|Placebo|Week 0|Alanine Aminotransferase (IU/L)|55.626891|55.626891|Europe|>=1 year|0|
|1|Placebo|Week 4|Alanine Aminotransferase (IU/L)|55.626891|11.948207|Europe|>=1 year|4|
|1|Placebo|Week 8|Alanine Aminotransferase (IU/L)|55.626891|79.998432|Europe|>=1 year|8|
|1|Placebo|Week 0|Lymphocytes (GI/L)|5.646724|5.646724|Europe|>=1 year|0|
|1|Placebo|Week 4|Lymphocytes (GI/L)|5.646724|1.900644|Europe|>=1 year|4|
|1|Placebo|Week 8|Lymphocytes (GI/L)|5.646724|3.525286|Europe|>=1 year|8|
|2|Intervention|Week 0|Alanine Aminotransferase (IU/L)|85.6607224|85.6607224|Other|>=1 year|0|
|2|Intervention|Week 4|Alanine Aminotransferase (IU/L)|85.6607224|13.0007082|Other|>=1 year|4|
|2|Intervention|Week 8|Alanine Aminotransferase (IU/L)|85.6607224|41.6314992|Other|>=1 year|8|
|2|Intervention|Week 0|Lymphocytes (GI/L)|1.4738713|1.4738713|Other|>=1 year|0|
|2|Intervention|Week 4|Lymphocytes (GI/L)|1.4738713|4.8144682|Other|>=1 year|4|
|2|Intervention|Week 8|Lymphocytes (GI/L)|1.4738713|4.2066078|Other|>=1 year|8|
|3|Placebo|Week 0|Alanine Aminotransferase (IU/L)|12.6528213|12.6528213|Other|>=1 year|0|
|3|Placebo|Week 4|Alanine Aminotransferase (IU/L)|12.6528213|69.3338961|Other|>=1 year|4|
|3|Placebo|Week 8|Alanine Aminotransferase (IU/L)|12.6528213|20.6919330|Other|>=1 year|8|
|3|Placebo|Week 0|Lymphocytes (GI/L)|1.1591932|1.1591932|Other|>=1 year|0|
|3|Placebo|Week 4|Lymphocytes (GI/L)|1.1591932|3.0689368|Other|>=1 year|4|
|3|Placebo|Week 8|Lymphocytes (GI/L)|1.1591932|5.2134919|Other|>=1 year|8|

### Variable description for sample dataset

|Variable name|Variable description|Variable type|
|---|---|---|
|id|Participant ID|Factor/Character/Numeric|
|arm|Treatment arm of participant (2, 3 or 4 arms)|Factor/Character/Numeric|
|visit|Timepoint of laboratory test taken|Ordered factor|
|lab_test|Laboratory test|Factor/Character|
|aval|Laboratory measurement value|Numeric|
|base / strat / region / time|Variables to be included in model for estimation of treatment effect estimates and 95% CIs|Factor/Character/Numeric|


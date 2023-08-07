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
To see the full list of exported functions:
```
library("aeplots")
ls("package:aeplots")
```
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

## Sample dataset used to produce table to summarise AEs
<table style="width: 595px;" border="1" cellspacing="0" cellpadding="0">
<tbody>
<tr>
<td valign="top" width="110">
<p><strong>adverse_event</strong></p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p><strong>body_system_class</strong></p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p><strong>id</strong></p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p><strong>arm</strong></p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p><strong>date_rand</strong></p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p><strong>last_visit</strong></p>
</td>
<td valign="top" width="59">
<p><strong>variable1</strong></p>
</td>
<td valign="top" width="64">
<p><strong>variable2</strong></p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Runny nose</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Eyes, ear, nose, throat</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Eczema aggravated</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Eczema aggravated</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Cold sore</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Upper respiratory tract infection</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Fractured wrist</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Musculo-skeletal</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Headache</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Neurological</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Skin infection</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1002</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-22</p>
</td>
<td valign="top" width="59">
<p>0</p>
</td>
<td valign="top" width="64">
<p>789</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Exacerbation of eczema</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Other</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1002</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-22</p>
</td>
<td valign="top" width="59">
<p>0</p>
</td>
<td valign="top" width="64">
<p>789</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Toothache</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Other</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1002</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-22</p>
</td>
<td valign="top" width="59">
<p>0</p>
</td>
<td valign="top" width="64">
<p>789</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Aching in limb</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Musculo-skeletal</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-14</p>
</td>
<td valign="top" width="59">
<p>0</p>
</td>
<td valign="top" width="64">
<p>456</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Cold symptoms</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-14</p>
</td>
<td valign="top" width="59">
<p>0</p>
</td>
<td valign="top" width="64">
<p>456</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Nausea</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Gastro-intestinal</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-14</p>
</td>
<td valign="top" width="59">
<p>0</p>
</td>
<td valign="top" width="64">
<p>456</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Coryzal symptoms</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-14</p>
</td>
<td valign="top" width="59">
<p>0</p>
</td>
<td valign="top" width="64">
<p>456</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Eczema herpeticum</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1005</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-02-18</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-20</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>999</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Infected eczema</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1005</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-02-18</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-20</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>999</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Allergic reaction (pollen)</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Allergies</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1007</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-02-12</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-18</p>
</td>
<td valign="top" width="59">
<p>0</p>
</td>
<td valign="top" width="64">
<p>101</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Urticaria</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1008</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-02-06</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-04</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>567</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Wheeze</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1008</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-02-06</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-04</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>567</p>
</td>
</tr>
<tr>
<td valign="top" width="110">
<p>Urticaria</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="39">
<p>1008</p>
</td>
<td valign="top" nowrap="nowrap" width="34">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="84">
<p>2015-02-06</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-04</p>
</td>
<td valign="top" width="59">
<p>1</p>
</td>
<td valign="top" width="64">
<p>567</p>
</td>
</tr>
</tbody>
</table>
<p>&nbsp;</p>

## Variable description for sample dataset
<table border="1" cellspacing="0" cellpadding="0">
<tbody>
<tr>
<td valign="top" width="121">
<p><strong>Variable name</strong></p>
</td>
<td valign="top" width="400">
<p><strong>Variable description</strong></p>
</td>
<td valign="top" width="237">
<p><strong>Variable type</strong></p>
</td>
</tr>
<tr>
<td valign="top" width="121">
<p>adverse_event</p>
</td>
<td valign="top" width="400">
<p>Adverse event name</p>
</td>
<td valign="top" width="237">
<p>Character</p>
</td>
</tr>
<tr>
<td valign="top" width="121">
<p>body_system_class</p>
</td>
<td valign="top" width="400">
<p>Body system class of adverse event</p>
</td>
<td valign="top" width="237">
<p>Factor</p>
</td>
</tr>
<tr>
<td valign="top" width="121">
<p>id</p>
</td>
<td valign="top" width="400">
<p>Participant ID</p>
</td>
<td valign="top" width="237">
<p>Factor/Character/Numeric</p>
</td>
</tr>
<tr>
<td valign="top" width="121">
<p>arm</p>
</td>
<td valign="top" width="400">
<p>Treatment arm of participant (2, 3 or 4 arms)</p>
</td>
<td valign="top" width="237">
<p>Factor/Character/Numeric</p>
</td>
</tr>
<tr>
<td valign="top" width="121">
<p>date_rand</p>
</td>
<td valign="top" width="400">
<p>Randomisation date</p>
</td>
<td valign="top" width="237">
<p>Date</p>
</td>
</tr>
<tr>
<td valign="top" width="121">
<p>last_visit</p>
</td>
<td valign="top" width="400">
<p>Date of last visit</p>
</td>
<td valign="top" width="237">
<p>Date</p>
</td>
</tr>
<tr>
<td valign="top" width="121">
<p>variable</p>
</td>
<td valign="top" width="400">
<p>Variables to be included in generalised linear models with Poisson function and log link for estimation of IRRs and 95% CIs</p>
</td>
<td valign="top" width="237">
<p>Factor/Character/Numeric</p>
</td>
</tr>
</tbody>
</table>

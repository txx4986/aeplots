# aeplots
R package to create tables and plots for analysing adverse event data in clinical trials

## How to install
```
devtools::install_github("https://github.com/txx4986/aeplots.git")
```

## Overview
To see the full list of exported functions:
```
library("aeplots")
ls("package:aeplots")
```
Functions available in the package:

-   `aetable`: Plots a table of AE summary by body system class
-   `aeseverity`: Plots a table of AE summary by severity
-   `aedot`:Plots a dot plot with proportions alongside treatment effect estimates (IRR) with accompanying 95% confidence interval to give an overview of the harm profile
-   `aestacked`: Plots a stacked bar chart to present the proportions of participants with each event by arm and by maximum severity
-   `aebar`: Plots a bar chart to present the number of events reported per participant

## Sample dataset used to produce table to summarise AEs
<table style="width: 609px;" border="1" cellspacing="0" cellpadding="0">
<tbody>
<tr>
<td valign="top" width="117">
<p>adverse_event</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>body_system_class</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>id</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>arm</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>date_rand</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>last_visit</p>
</td>
<td valign="top" width="65">
<p>variable1</p>
</td>
<td valign="top" width="59">
<p>variable2</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Runny nose</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Eyes, ear, nose, throat</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Eczema aggravated</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Eczema aggravated</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Cold sore</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Upper respiratory tract infection</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Fractured wrist</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Musculo-skeletal</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Headache</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Neurological</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-23</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
<p>234</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Skin infection</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1002</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-22</p>
</td>
<td valign="top" width="65">
<p>0</p>
</td>
<td valign="top" width="59">
<p>789</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Exacerbation of eczema</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Other</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1002</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-22</p>
</td>
<td valign="top" width="65">
<p>0</p>
</td>
<td valign="top" width="59">
<p>789</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Toothache</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Other</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1002</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-22</p>
</td>
<td valign="top" width="65">
<p>0</p>
</td>
<td valign="top" width="59">
<p>789</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Aching in limb</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Musculo-skeletal</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-14</p>
</td>
<td valign="top" width="65">
<p>0</p>
</td>
<td valign="top" width="59">
<p>456</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Cold symptoms</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-14</p>
</td>
<td valign="top" width="65">
<p>0</p>
</td>
<td valign="top" width="59">
<p>456</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Nausea</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Gastro-intestinal</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-14</p>
</td>
<td valign="top" width="65">
<p>0</p>
</td>
<td valign="top" width="59">
<p>456</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Coryzal symptoms</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-12-14</p>
</td>
<td valign="top" width="65">
<p>0</p>
</td>
<td valign="top" width="59">
<p>456</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Eczema herpeticum</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1005</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-02-18</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-20</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
<p>999</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Infected eczema</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1005</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-02-18</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-20</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
<p>999</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Allergic reaction (pollen)</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Allergies</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1007</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-02-12</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-18</p>
</td>
<td valign="top" width="65">
<p>0</p>
</td>
<td valign="top" width="59">
<p>101</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Urticaria</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1008</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-02-06</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-04</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
<p>567</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Wheeze</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1008</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-02-06</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-04</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
<p>567</p>
</td>
</tr>
<tr>
<td valign="top" width="117">
<p>Urticaria</p>
</td>
<td valign="top" nowrap="nowrap" width="129">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="45">
<p>1008</p>
</td>
<td valign="top" nowrap="nowrap" width="40">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2015-02-06</p>
</td>
<td valign="top" nowrap="nowrap" width="76">
<p>2016-01-04</p>
</td>
<td valign="top" width="65">
<p>1</p>
</td>
<td valign="top" width="59">
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
<td valign="top" width="300">
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
<td valign="top" width="300">
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
<td valign="top" width="300">
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
<td valign="top" width="300">
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
<td valign="top" width="300">
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
<td valign="top" width="300">
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
<td valign="top" width="300">
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
<td valign="top" width="300">
<p>Variables to be included in generalised linear models with Poisson function and log link for estimation of IRRs and 95% CIs</p>
</td>
<td valign="top" width="237">
<p>Factor/Character/Numeric</p>
</td>
</tr>
</tbody>
</table>

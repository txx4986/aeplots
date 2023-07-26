# aeplots
Package to create tables and plots for analysing adverse event data in clinical trials

## Sample dataset used to produce table to summarise AEs
<table style="width: 587px;" border="1" cellspacing="0" cellpadding="0">
<tbody>
<tr>
<td valign="top" width="151">
<p><strong>adverse_event</strong></p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p><strong>body_system_class</strong></p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p><strong>id</strong></p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p><strong>arm</strong></p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p><strong>date_rand</strong></p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p><strong>last_visit</strong></p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Runny nose</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Eyes, ear, nose, throat</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-23</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Eczema aggravated</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-23</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Eczema aggravated</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-23</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Cold sore</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-23</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Upper respiratory tract infection</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-23</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Fractured wrist</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Musculo-skeletal</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-23</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Headache</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Neurological</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1001</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-23</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Skin infection</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1002</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-22</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Exacerbation of eczema</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Other</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1002</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-22</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Toothache</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Other</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1002</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-15</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-22</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Aching in limb</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Musculo-skeletal</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-14</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Cold symptoms</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-14</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Nausea</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Gastro-intestinal</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-14</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Coryzal symptoms</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1004</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-01-16</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2015-12-14</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Eczema herpeticum</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1005</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-02-18</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2016-01-20</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Infected eczema</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1005</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-02-18</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2016-01-20</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Allergic reaction (pollen)</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Allergies</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1007</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>C</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-02-12</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2016-01-18</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Urticaria</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1008</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-02-06</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2016-01-04</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Wheeze</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Respiratory</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1008</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-02-06</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2016-01-04</p>
</td>
</tr>
<tr>
<td valign="top" width="151">
<p>Urticaria</p>
</td>
<td valign="top" nowrap="nowrap" width="132">
<p>Dermatological</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>1008</p>
</td>
<td valign="top" nowrap="nowrap" width="57">
<p>I</p>
</td>
<td valign="top" nowrap="nowrap" width="94">
<p>2015-02-06</p>
</td>
<td valign="top" nowrap="nowrap" width="95">
<p>2016-01-04</p>
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
<td valign="top" width="243">
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
<td valign="top" width="243">
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
<td valign="top" width="243">
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
<td valign="top" width="243">
<p>Participant ID</p>
</td>
<td valign="top" width="237">
<p>Numeric</p>
</td>
</tr>
<tr>
<td valign="top" width="121">
<p>arm</p>
</td>
<td valign="top" width="243">
<p>Treatment arm of participant</p>
</td>
<td valign="top" width="237">
<p>Factor (I: Intervention, C: Control)</p>
</td>
</tr>
<tr>
<td valign="top" width="121">
<p>date_rand</p>
</td>
<td valign="top" width="243">
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
<td valign="top" width="243">
<p>Date of last visit</p>
</td>
<td valign="top" width="237">
<p>Date</p>
</td>
</tr>
</tbody>
</table>

---
layout: post  
title: Manteniment Predictiu
 
author: Josep Espluga  
published: true
status: publish
draft: false  
tags:   
---
 

 
Adaptació de la Guia de modelat per Manteniment Predictiu de Azure AI Gallery.  
 
### Descripció problema  
 
Predicció de problemes de funcionament en maquinària, instal.lacions i equips per tal d'actuar de forma proactiva abans que succeixin i minimitzar el cost degut a les aturades intempestives.  
 
### Llibreries  
 

{% highlight r %}
library(tidyverse)
library(readr)
library(funModeling)
library(skimr)
library(lubridate)
library(zoo)
library(data.table)
library(gbm)
library(caret)
library(gt)
{% endhighlight %}
 
## DADES
 
### Telemetria  
 
Dades: voltatge, velocitat, presió i vibració  
Freqüència de registre: registre horari durant l'any 2015  
Nº màquines: 100
 

{% highlight r %}
telemetria <- read_csv("Data/telemetry.csv")
telemetria$datetime <- mdy_hms(telemetria$datetime)
 
telemetria %>% 
  head() %>% 
  gt() %>% 
  fmt_number(columns = 3:6, decimals = 2)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#prahefdaef .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#prahefdaef .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#prahefdaef .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#prahefdaef .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#prahefdaef .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#prahefdaef .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#prahefdaef .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#prahefdaef .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#prahefdaef .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#prahefdaef .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#prahefdaef .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#prahefdaef .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#prahefdaef .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#prahefdaef .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#prahefdaef .gt_from_md > :first-child {
  margin-top: 0;
}

#prahefdaef .gt_from_md > :last-child {
  margin-bottom: 0;
}

#prahefdaef .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#prahefdaef .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#prahefdaef .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#prahefdaef .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#prahefdaef .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#prahefdaef .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#prahefdaef .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#prahefdaef .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#prahefdaef .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#prahefdaef .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#prahefdaef .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#prahefdaef .gt_left {
  text-align: left;
}

#prahefdaef .gt_center {
  text-align: center;
}

#prahefdaef .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#prahefdaef .gt_font_normal {
  font-weight: normal;
}

#prahefdaef .gt_font_bold {
  font-weight: bold;
}

#prahefdaef .gt_font_italic {
  font-style: italic;
}

#prahefdaef .gt_super {
  font-size: 65%;
}

#prahefdaef .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="prahefdaef" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">volt</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotate</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressure</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibration</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-01 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">176.22</td>
      <td class="gt_row gt_right">418.50</td>
      <td class="gt_row gt_right">113.08</td>
      <td class="gt_row gt_right">45.09</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 07:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">162.88</td>
      <td class="gt_row gt_right">402.75</td>
      <td class="gt_row gt_right">95.46</td>
      <td class="gt_row gt_right">43.41</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">170.99</td>
      <td class="gt_row gt_right">527.35</td>
      <td class="gt_row gt_right">75.24</td>
      <td class="gt_row gt_right">34.18</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 09:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">162.46</td>
      <td class="gt_row gt_right">346.15</td>
      <td class="gt_row gt_right">109.25</td>
      <td class="gt_row gt_right">41.12</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 10:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">157.61</td>
      <td class="gt_row gt_right">435.38</td>
      <td class="gt_row gt_right">111.89</td>
      <td class="gt_row gt_right">25.99</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">172.50</td>
      <td class="gt_row gt_right">430.32</td>
      <td class="gt_row gt_right">95.93</td>
      <td class="gt_row gt_right">35.66</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Descripció de les variables:  
 

{% highlight r %}
skim(telemetria)
{% endhighlight %}


|                         |           |
|:------------------------|:----------|
|Name                     |telemetria |
|Number of rows           |876100     |
|Number of columns        |6          |
|_______________________  |           |
|Column type frequency:   |           |
|numeric                  |5          |
|POSIXct                  |1          |
|________________________ |           |
|Group variables          |None       |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|   mean|    sd|     p0|    p25|    p50|    p75|   p100|hist  |
|:-------------|---------:|-------------:|------:|-----:|------:|------:|------:|------:|------:|:-----|
|machineID     |         0|             1|  50.50| 28.87|   1.00|  25.75|  50.50|  75.25| 100.00|▇▇▇▇▇ |
|volt          |         0|             1| 170.78| 15.51|  97.33| 160.30| 170.61| 181.00| 255.12|▁▃▇▁▁ |
|rotate        |         0|             1| 446.61| 52.67| 138.43| 412.31| 447.56| 482.18| 695.02|▁▁▇▃▁ |
|pressure      |         0|             1| 100.86| 11.05|  51.24|  93.50| 100.43| 107.56| 185.95|▁▇▃▁▁ |
|vibration     |         0|             1|  40.39|  5.37|  14.88|  36.78|  40.24|  43.78|  76.79|▁▇▇▁▁ |


**Variable type: POSIXct**

|skim_variable | n_missing| complete_rate|min                 |max                 |median              | n_unique|
|:-------------|---------:|-------------:|:-------------------|:-------------------|:-------------------|--------:|
|datetime      |         0|             1|2015-01-01 06:00:00 |2016-01-01 06:00:00 |2015-07-02 18:00:00 |     8761|
 

{% highlight r %}
telemetria %>% 
  filter(machineID %in% 1:2, 
         datetime > as.POSIXct("2015-01-01"),
         datetime < as.POSIXct("2015-02-01")) %>% 
  ggplot(aes(x = datetime, y = volt, col = factor(machineID)))+
  geom_line(alpha = 0.5) +
  labs(y = "voltage", color = "machineID", title = "Evolució voltatge màquina ID 1-2") +
  facet_wrap(~machineID, ncol=1)
{% endhighlight %}

![plot of chunk pre1](/figures/pre1-1.png)
 
### Errors  
 
Registre d'errors on la màquina encara és operacional (no son averies)  
Aproximants a la hora en punt més propera  
 

{% highlight r %}
errors <- read_csv("Data/errors.csv")
errors$datetime <- mdy_hms(errors$datetime)
errors$errorID <- as.factor(errors$errorID)
 
gt(head(errors))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#gausdtarcp .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gausdtarcp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gausdtarcp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gausdtarcp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gausdtarcp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gausdtarcp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gausdtarcp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gausdtarcp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gausdtarcp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gausdtarcp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gausdtarcp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gausdtarcp .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#gausdtarcp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#gausdtarcp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gausdtarcp .gt_from_md > :first-child {
  margin-top: 0;
}

#gausdtarcp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gausdtarcp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gausdtarcp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#gausdtarcp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gausdtarcp .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#gausdtarcp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gausdtarcp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gausdtarcp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gausdtarcp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gausdtarcp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#gausdtarcp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gausdtarcp .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#gausdtarcp .gt_left {
  text-align: left;
}

#gausdtarcp .gt_center {
  text-align: center;
}

#gausdtarcp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gausdtarcp .gt_font_normal {
  font-weight: normal;
}

#gausdtarcp .gt_font_bold {
  font-weight: bold;
}

#gausdtarcp .gt_font_italic {
  font-style: italic;
}

#gausdtarcp .gt_super {
  font-size: 65%;
}

#gausdtarcp .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="gausdtarcp" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">errorID</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-03 07:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-03 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error3</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-04 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error5</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-10 15:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-22 10:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-25 15:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error4</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Descripció de les variables:  
 

{% highlight r %}
skim(errors)
{% endhighlight %}


|                         |       |
|:------------------------|:------|
|Name                     |errors |
|Number of rows           |3919   |
|Number of columns        |3      |
|_______________________  |       |
|Column type frequency:   |       |
|factor                   |1      |
|numeric                  |1      |
|POSIXct                  |1      |
|________________________ |       |
|Group variables          |None   |


**Variable type: factor**

|skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts                              |
|:-------------|---------:|-------------:|:-------|--------:|:---------------------------------------|
|errorID       |         0|             1|FALSE   |        5|err: 1010, err: 988, err: 838, err: 727 |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|  mean|    sd| p0| p25| p50| p75| p100|hist  |
|:-------------|---------:|-------------:|-----:|-----:|--:|---:|---:|---:|----:|:-----|
|machineID     |         0|             1| 51.04| 28.95|  1|  25|  51|  77|  100|▇▇▇▇▇ |


**Variable type: POSIXct**

|skim_variable | n_missing| complete_rate|min                 |max                 |median              | n_unique|
|:-------------|---------:|-------------:|:-------------------|:-------------------|:-------------------|--------:|
|datetime      |         0|             1|2015-01-01 06:00:00 |2016-01-01 05:00:00 |2015-07-01 06:00:00 |     2720|
 
 
Quantitat d'errors per tipus
 

{% highlight r %}
errors %>% 
  mutate(errorID = as.factor(errorID)) %>% 
  ggplot(aes(x = errorID, fill = errorID)) + 
  geom_bar(show.legend = FALSE) + 
  labs(title = "Errors per tipus", x = "tipus d'error")
{% endhighlight %}

![plot of chunk pre2](/figures/pre2-1.png)
 
Registre errors màquina 4
 

{% highlight r %}
errors %>% 
  filter(machineID == 4) %>%
  ggplot(aes(y = errorID, x = datetime)) + 
    geom_point(alpha = 0.5) + 
    labs(title = "MachineID 4: LOG Errors", x = "Data", y = "tipus d'error")
{% endhighlight %}

![plot of chunk pre3](/figures/pre3-1.png)
 
### Manteniments
 
Registre d'actuacions de manteniment.  
Actuacions previstes/rutinaries i imprevistes per averies.  
Dades dels anys 2014 i 2015  
 

{% highlight r %}
mant <- read_csv("Data/maint.csv")
mant$comp <- as.factor(mant$comp)
 
gt(head(mant))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hiwmhfdsgc .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hiwmhfdsgc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hiwmhfdsgc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hiwmhfdsgc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hiwmhfdsgc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hiwmhfdsgc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hiwmhfdsgc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hiwmhfdsgc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hiwmhfdsgc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hiwmhfdsgc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hiwmhfdsgc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hiwmhfdsgc .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#hiwmhfdsgc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hiwmhfdsgc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hiwmhfdsgc .gt_from_md > :first-child {
  margin-top: 0;
}

#hiwmhfdsgc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hiwmhfdsgc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hiwmhfdsgc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#hiwmhfdsgc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hiwmhfdsgc .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#hiwmhfdsgc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hiwmhfdsgc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hiwmhfdsgc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hiwmhfdsgc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hiwmhfdsgc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#hiwmhfdsgc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hiwmhfdsgc .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#hiwmhfdsgc .gt_left {
  text-align: left;
}

#hiwmhfdsgc .gt_center {
  text-align: center;
}

#hiwmhfdsgc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hiwmhfdsgc .gt_font_normal {
  font-weight: normal;
}

#hiwmhfdsgc .gt_font_bold {
  font-weight: bold;
}

#hiwmhfdsgc .gt_font_italic {
  font-style: italic;
}

#hiwmhfdsgc .gt_super {
  font-size: 65%;
}

#hiwmhfdsgc .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="hiwmhfdsgc" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">comp</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2014-06-01 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp2</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2014-07-16 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2014-07-31 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp3</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2014-12-13 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-05 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-05 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp1</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Descripció de les variables:  
 

{% highlight r %}
skim(mant)
{% endhighlight %}


|                         |     |
|:------------------------|:----|
|Name                     |mant |
|Number of rows           |3286 |
|Number of columns        |3    |
|_______________________  |     |
|Column type frequency:   |     |
|factor                   |1    |
|numeric                  |1    |
|POSIXct                  |1    |
|________________________ |     |
|Group variables          |None |


**Variable type: factor**

|skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts                             |
|:-------------|---------:|-------------:|:-------|--------:|:--------------------------------------|
|comp          |         0|             1|FALSE   |        4|com: 863, com: 811, com: 808, com: 804 |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|  mean|    sd| p0|   p25| p50| p75| p100|hist  |
|:-------------|---------:|-------------:|-----:|-----:|--:|-----:|---:|---:|----:|:-----|
|machineID     |         0|             1| 50.28| 28.91|  1| 25.25|  50|  75|  100|▇▇▇▇▇ |


**Variable type: POSIXct**

|skim_variable | n_missing| complete_rate|min                 |max                 |median              | n_unique|
|:-------------|---------:|-------------:|:-------------------|:-------------------|:-------------------|--------:|
|datetime      |         0|             1|2014-06-01 06:00:00 |2016-01-01 06:00:00 |2015-06-13 06:00:00 |      374|
 

{% highlight r %}
mant %>% 
  mutate(comp = as.factor(comp)) %>% 
  ggplot(aes(x = comp, fill = comp)) + 
  geom_bar(show.legend = FALSE) + 
  labs(title = "Actuacions per component", x = "component averiat", y = "total")
{% endhighlight %}

![plot of chunk pre4](/figures/pre4-1.png)
 
### Màquines  
 
Model màquina i anys de servei.  
 

{% highlight r %}
maquines <- read_csv("Data/machines.csv")
maquines$model <- as.factor(maquines$model)
 
gt(head(maquines))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ereglpvcty .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ereglpvcty .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ereglpvcty .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ereglpvcty .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ereglpvcty .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ereglpvcty .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ereglpvcty .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ereglpvcty .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ereglpvcty .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ereglpvcty .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ereglpvcty .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ereglpvcty .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ereglpvcty .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ereglpvcty .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ereglpvcty .gt_from_md > :first-child {
  margin-top: 0;
}

#ereglpvcty .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ereglpvcty .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ereglpvcty .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ereglpvcty .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ereglpvcty .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ereglpvcty .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ereglpvcty .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ereglpvcty .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ereglpvcty .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ereglpvcty .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ereglpvcty .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ereglpvcty .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ereglpvcty .gt_left {
  text-align: left;
}

#ereglpvcty .gt_center {
  text-align: center;
}

#ereglpvcty .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ereglpvcty .gt_font_normal {
  font-weight: normal;
}

#ereglpvcty .gt_font_bold {
  font-weight: bold;
}

#ereglpvcty .gt_font_italic {
  font-style: italic;
}

#ereglpvcty .gt_super {
  font-size: 65%;
}

#ereglpvcty .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="ereglpvcty" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">age</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2</td>
      <td class="gt_row gt_center">model4</td>
      <td class="gt_row gt_right">7</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">3</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">8</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">4</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">7</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">5</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">2</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">6</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">7</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Descripció de les variables:  
 

{% highlight r %}
skim(maquines)
{% endhighlight %}


|                         |         |
|:------------------------|:--------|
|Name                     |maquines |
|Number of rows           |100      |
|Number of columns        |3        |
|_______________________  |         |
|Column type frequency:   |         |
|factor                   |1        |
|numeric                  |2        |
|________________________ |         |
|Group variables          |None     |


**Variable type: factor**

|skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts                         |
|:-------------|---------:|-------------:|:-------|--------:|:----------------------------------|
|model         |         0|             1|FALSE   |        4|mod: 35, mod: 32, mod: 17, mod: 16 |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|  mean|    sd| p0|   p25|  p50|   p75| p100|hist  |
|:-------------|---------:|-------------:|-----:|-----:|--:|-----:|----:|-----:|----:|:-----|
|machineID     |         0|             1| 50.50| 29.01|  1| 25.75| 50.5| 75.25|  100|▇▇▇▇▇ |
|age           |         0|             1| 11.33|  5.86|  0|  6.75| 12.0| 16.00|   20|▆▅▆▇▇ |
 
 
Antiguitat del parc de màquines: taula
 

{% highlight r %}
table(maquines$age)
{% endhighlight %}



{% highlight text %}
## 
##  0  1  2  3  4  5  6  7  8  9 10 11 12 14 15 16 17 18 19 20 
##  1  3  6  4  3  4  4  6  1  5 10  2  2 14  6  5  7  6  4  7
{% endhighlight %}
 
Antiguitat del parc de màquines: histograma
 

{% highlight r %}
maquines %>% 
  ggplot(aes(x=age))+
  geom_histogram(bins = 20)
{% endhighlight %}

![plot of chunk pre5](/figures/pre5-1.png)
 
### Averies
 
Registre de substitució de components a causa d'averies
 

{% highlight r %}
averies<- read_csv("Data/failures2.csv")
averies$failure <- as.factor(averies$failure)
 
gt(head(averies))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#zdxpvjgoak .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zdxpvjgoak .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zdxpvjgoak .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zdxpvjgoak .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zdxpvjgoak .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zdxpvjgoak .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zdxpvjgoak .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zdxpvjgoak .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zdxpvjgoak .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zdxpvjgoak .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zdxpvjgoak .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zdxpvjgoak .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#zdxpvjgoak .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zdxpvjgoak .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zdxpvjgoak .gt_from_md > :first-child {
  margin-top: 0;
}

#zdxpvjgoak .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zdxpvjgoak .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zdxpvjgoak .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#zdxpvjgoak .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zdxpvjgoak .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#zdxpvjgoak .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zdxpvjgoak .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zdxpvjgoak .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zdxpvjgoak .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zdxpvjgoak .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#zdxpvjgoak .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zdxpvjgoak .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#zdxpvjgoak .gt_left {
  text-align: left;
}

#zdxpvjgoak .gt_center {
  text-align: center;
}

#zdxpvjgoak .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zdxpvjgoak .gt_font_normal {
  font-weight: normal;
}

#zdxpvjgoak .gt_font_bold {
  font-weight: bold;
}

#zdxpvjgoak .gt_font_italic {
  font-style: italic;
}

#zdxpvjgoak .gt_super {
  font-size: 65%;
}

#zdxpvjgoak .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="zdxpvjgoak" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">failure</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-05 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-03-06 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-04-20 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp2</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-06-19 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-09-02 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-10-17 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp2</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
 
Descripció de les variables:  
 

{% highlight r %}
skim(averies)
{% endhighlight %}


|                         |        |
|:------------------------|:-------|
|Name                     |averies |
|Number of rows           |761     |
|Number of columns        |3       |
|_______________________  |        |
|Column type frequency:   |        |
|factor                   |1       |
|numeric                  |1       |
|POSIXct                  |1       |
|________________________ |        |
|Group variables          |None    |


**Variable type: factor**

|skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts                             |
|:-------------|---------:|-------------:|:-------|--------:|:--------------------------------------|
|failure       |         0|             1|FALSE   |        4|com: 259, com: 192, com: 179, com: 131 |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|  mean|    sd| p0| p25| p50| p75| p100|hist  |
|:-------------|---------:|-------------:|-----:|-----:|--:|---:|---:|---:|----:|:-----|
|machineID     |         0|             1| 51.91| 29.52|  1|  24|  51|  79|  100|▇▇▆▆▇ |


**Variable type: POSIXct**

|skim_variable | n_missing| complete_rate|min                 |max                 |median              | n_unique|
|:-------------|---------:|-------------:|:-------------------|:-------------------|:-------------------|--------:|
|datetime      |         0|             1|2015-01-02 03:00:00 |2015-12-31 06:00:00 |2015-06-24 06:00:00 |      302|
 
 
Freqüència d'averies per component
 

{% highlight r %}
averies %>% 
  mutate(failure = as.factor(failure)) %>% 
  ggplot(aes(x = failure, fill = failure)) + 
  geom_bar() + 
  labs(title = "Averies per component", x = "Component", y= "nÂº d'averies")
{% endhighlight %}

![plot of chunk pre6](/figures/pre6-1.png)
 
Distribució averies màquines Id 1,2,3  
 

{% highlight r %}
averies %>% 
  filter(machineID < 4) %>% 
  ggplot(aes(x = failure, fill = factor(machineID))) + 
  geom_bar() + 
  labs(title = "Distribucio averies màquines 1-2-3", x = "component", fill = "MachineID") +
  facet_wrap(~machineID, ncol=1)
{% endhighlight %}

![plot of chunk pre7](/figures/pre7-1.png)
 
Màquines amb més averies  
 

{% highlight r %}
averies %>% 
  group_by(machineID) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(machineID = as.factor(machineID),
         machineID = fct_reorder(machineID, n)) %>% 
  ggplot(aes(x = machineID, y = n, color = factor(machineID))) + 
  geom_point(show.legend = FALSE, size = 3) + 
  geom_segment(aes(y=0, yend=n, x=machineID, xend=machineID), show.legend = FALSE)+
  labs(title = "10 Màquines amb més averies", x = "machineID", y = "Total averies")+
  coord_flip()+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank())+
  ylim(0,20)
{% endhighlight %}

![plot of chunk pre8](/figures/pre8-1.png)
 
 
## FEATURE ENGINEERING  
 
Convertir les dades registrades a característiques que descriguin el millor possible 
les condicions de salut de la màquina en un moment donat.  
Les dades amb registre temporal poden calcular-se considerant intervals de temps.  
Prenem una finestra temporal i es calculen els agregats continus com la mitjana i la desviació per representar-la.  
 
Les dades agregades continues ens proporcionaran:  
- mesura de la tendencia central al llarg del temps (mean, median)  
- mesura de la volatibilidad al llarg del temps (sd, var)  
- detectar canvis de tendència (promitjos ràpids vs. lents)  
- mesurar relacions entre dues sèries temporals (cor, cov)  
 
Un moving average ens ha de permetre visualitzar com una mitjana canvia al llarg del temps i d'aquesta forma diferenciar la tendència 
del soroll.  
Podem modificar la sensibilitat varinat la durada de la finestra temporal.  
Combinant la rolling average amb la rolling standard deviation podem detectar zones d'anormal volatilitat i consolidació que permetin confirmar canvis de tendència.   
 
![-Moving Average-](Images/movingAverage.png)
 
### Telemetria
 
Calculem la mitjana i sd continua  (rolling average) d'una finestra de les últimes 3 hores per cada màquina
 

{% highlight r %}
telemetriamean <- 
  telemetria %>%
  arrange(machineID, datetime) %>% 
  group_by(machineID) %>%
  mutate(voltmean = rollapply(volt, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         rotatemean = rollapply(rotate, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         pressuremean = rollapply(pressure, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         vibrationmean = rollapply(vibration, width = 3, FUN = mean, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltmean, rotatemean, pressuremean, vibrationmean) %>%
  filter(!is.na(voltmean))%>% 
  ungroup()
 
telemetriamean %>% 
  head() %>% 
  gt() %>% 
  fmt_number(columns = 3:6, decimals = 2)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#glupucdhrd .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#glupucdhrd .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#glupucdhrd .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#glupucdhrd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#glupucdhrd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#glupucdhrd .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#glupucdhrd .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#glupucdhrd .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#glupucdhrd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#glupucdhrd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#glupucdhrd .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#glupucdhrd .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#glupucdhrd .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#glupucdhrd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#glupucdhrd .gt_from_md > :first-child {
  margin-top: 0;
}

#glupucdhrd .gt_from_md > :last-child {
  margin-bottom: 0;
}

#glupucdhrd .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#glupucdhrd .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#glupucdhrd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#glupucdhrd .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#glupucdhrd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#glupucdhrd .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#glupucdhrd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#glupucdhrd .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#glupucdhrd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#glupucdhrd .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#glupucdhrd .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#glupucdhrd .gt_left {
  text-align: left;
}

#glupucdhrd .gt_center {
  text-align: center;
}

#glupucdhrd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#glupucdhrd .gt_font_normal {
  font-weight: normal;
}

#glupucdhrd .gt_font_bold {
  font-weight: bold;
}

#glupucdhrd .gt_font_italic {
  font-style: italic;
}

#glupucdhrd .gt_super {
  font-size: 65%;
}

#glupucdhrd .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="glupucdhrd" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-01 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">170.03</td>
      <td class="gt_row gt_right">449.53</td>
      <td class="gt_row gt_right">94.59</td>
      <td class="gt_row gt_right">40.89</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">164.19</td>
      <td class="gt_row gt_right">403.95</td>
      <td class="gt_row gt_right">105.69</td>
      <td class="gt_row gt_right">34.26</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">168.13</td>
      <td class="gt_row gt_right">435.78</td>
      <td class="gt_row gt_right">107.79</td>
      <td class="gt_row gt_right">41.24</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">165.51</td>
      <td class="gt_row gt_right">430.47</td>
      <td class="gt_row gt_right">101.70</td>
      <td class="gt_row gt_right">40.37</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">168.81</td>
      <td class="gt_row gt_right">437.11</td>
      <td class="gt_row gt_right">90.91</td>
      <td class="gt_row gt_right">41.74</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 23:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">168.78</td>
      <td class="gt_row gt_right">486.24</td>
      <td class="gt_row gt_right">90.45</td>
      <td class="gt_row gt_right">41.80</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 

{% highlight r %}
telemetriasd <- 
  telemetria %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd = rollapply(volt, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         rotatesd = rollapply(rotate, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         pressuresd = rollapply(pressure, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         vibrationsd = rollapply(vibration, width = 3, FUN = sd, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltsd, rotatesd, pressuresd, vibrationsd) %>%
  filter(!is.na(voltsd)) %>%
  ungroup()
 
telemetriasd %>% 
  head() %>% 
  gt() %>% 
  fmt_number(columns = 3:6, decimals = 2)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#fqggdgqfjq .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#fqggdgqfjq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fqggdgqfjq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fqggdgqfjq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fqggdgqfjq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fqggdgqfjq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fqggdgqfjq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#fqggdgqfjq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#fqggdgqfjq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fqggdgqfjq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fqggdgqfjq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#fqggdgqfjq .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#fqggdgqfjq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#fqggdgqfjq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fqggdgqfjq .gt_from_md > :first-child {
  margin-top: 0;
}

#fqggdgqfjq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fqggdgqfjq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#fqggdgqfjq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#fqggdgqfjq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fqggdgqfjq .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#fqggdgqfjq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fqggdgqfjq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fqggdgqfjq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fqggdgqfjq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fqggdgqfjq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#fqggdgqfjq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fqggdgqfjq .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#fqggdgqfjq .gt_left {
  text-align: left;
}

#fqggdgqfjq .gt_center {
  text-align: center;
}

#fqggdgqfjq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fqggdgqfjq .gt_font_normal {
  font-weight: normal;
}

#fqggdgqfjq .gt_font_bold {
  font-weight: bold;
}

#fqggdgqfjq .gt_font_italic {
  font-style: italic;
}

#fqggdgqfjq .gt_super {
  font-size: 65%;
}

#fqggdgqfjq .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="fqggdgqfjq" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-01 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">6.72</td>
      <td class="gt_row gt_right">67.85</td>
      <td class="gt_row gt_right">18.93</td>
      <td class="gt_row gt_right">5.87</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">7.60</td>
      <td class="gt_row gt_right">50.12</td>
      <td class="gt_row gt_right">8.56</td>
      <td class="gt_row gt_right">7.66</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">10.12</td>
      <td class="gt_row gt_right">55.08</td>
      <td class="gt_row gt_right">5.91</td>
      <td class="gt_row gt_right">5.17</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">4.67</td>
      <td class="gt_row gt_right">42.05</td>
      <td class="gt_row gt_right">4.55</td>
      <td class="gt_row gt_right">2.11</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">14.75</td>
      <td class="gt_row gt_right">47.05</td>
      <td class="gt_row gt_right">4.24</td>
      <td class="gt_row gt_right">2.21</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 23:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">15.90</td>
      <td class="gt_row gt_right">36.13</td>
      <td class="gt_row gt_right">4.31</td>
      <td class="gt_row gt_right">9.39</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Calculem la mitjana i sd continua (rolling average) d'una finestra de les últimes 24 hores
cada 3 hores per cada màquina  
 

{% highlight r %}
telemetriamean_24hrs <- 
  telemetria %>%
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltmean_24hrs = rollapply(volt, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         rotatemean_24hrs = rollapply(rotate, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         pressuremean_24hrs = rollapply(pressure, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         vibrationmean_24hrs = rollapply(vibration, width = 24, FUN = mean, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltmean_24hrs, rotatemean_24hrs, pressuremean_24hrs, vibrationmean_24hrs) %>%
  filter(!is.na(voltmean_24hrs)) %>% 
  ungroup()
 
telemetriamean_24hrs %>% 
  head() %>% 
  gt() %>% 
  fmt_number(columns = 3:6, decimals = 2)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ucpwouvcjj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ucpwouvcjj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ucpwouvcjj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ucpwouvcjj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ucpwouvcjj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ucpwouvcjj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ucpwouvcjj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ucpwouvcjj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ucpwouvcjj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ucpwouvcjj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ucpwouvcjj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ucpwouvcjj .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ucpwouvcjj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ucpwouvcjj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ucpwouvcjj .gt_from_md > :first-child {
  margin-top: 0;
}

#ucpwouvcjj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ucpwouvcjj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ucpwouvcjj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ucpwouvcjj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ucpwouvcjj .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ucpwouvcjj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ucpwouvcjj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ucpwouvcjj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ucpwouvcjj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ucpwouvcjj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ucpwouvcjj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ucpwouvcjj .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ucpwouvcjj .gt_left {
  text-align: left;
}

#ucpwouvcjj .gt_center {
  text-align: center;
}

#ucpwouvcjj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ucpwouvcjj .gt_font_normal {
  font-weight: normal;
}

#ucpwouvcjj .gt_font_bold {
  font-weight: bold;
}

#ucpwouvcjj .gt_font_italic {
  font-style: italic;
}

#ucpwouvcjj .gt_super {
  font-size: 65%;
}

#ucpwouvcjj .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="ucpwouvcjj" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean_24hrs</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">169.73</td>
      <td class="gt_row gt_right">445.18</td>
      <td class="gt_row gt_right">96.80</td>
      <td class="gt_row gt_right">40.39</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">170.53</td>
      <td class="gt_row gt_right">443.91</td>
      <td class="gt_row gt_right">97.67</td>
      <td class="gt_row gt_right">39.79</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">170.05</td>
      <td class="gt_row gt_right">446.46</td>
      <td class="gt_row gt_right">96.91</td>
      <td class="gt_row gt_right">40.02</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">170.34</td>
      <td class="gt_row gt_right">447.36</td>
      <td class="gt_row gt_right">96.23</td>
      <td class="gt_row gt_right">39.92</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">170.06</td>
      <td class="gt_row gt_right">452.16</td>
      <td class="gt_row gt_right">96.36</td>
      <td class="gt_row gt_right">39.99</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">169.37</td>
      <td class="gt_row gt_right">453.34</td>
      <td class="gt_row gt_right">98.04</td>
      <td class="gt_row gt_right">39.53</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 

{% highlight r %}
telemetriasd_24hrs <- 
  telemetria %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd_24hrs = rollapply(volt, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         rotatesd_24hrs = rollapply(rotate, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         pressuresd_24hrs = rollapply(pressure, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         vibrationsd_24hrs = rollapply(vibration, width = 24, FUN = sd, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltsd_24hrs, rotatesd_24hrs, pressuresd_24hrs, vibrationsd_24hrs) %>%
  filter(!is.na(voltsd_24hrs)) %>%
  ungroup()
 
telemetriasd_24hrs %>% 
  head() %>% 
  gt() %>% 
  fmt_number(columns = 3:6, decimals = 2)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#umcirxbjcc .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#umcirxbjcc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#umcirxbjcc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#umcirxbjcc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#umcirxbjcc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umcirxbjcc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#umcirxbjcc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#umcirxbjcc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#umcirxbjcc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#umcirxbjcc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#umcirxbjcc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#umcirxbjcc .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#umcirxbjcc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#umcirxbjcc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#umcirxbjcc .gt_from_md > :first-child {
  margin-top: 0;
}

#umcirxbjcc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#umcirxbjcc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#umcirxbjcc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#umcirxbjcc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#umcirxbjcc .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#umcirxbjcc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#umcirxbjcc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#umcirxbjcc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umcirxbjcc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#umcirxbjcc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#umcirxbjcc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#umcirxbjcc .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#umcirxbjcc .gt_left {
  text-align: left;
}

#umcirxbjcc .gt_center {
  text-align: center;
}

#umcirxbjcc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#umcirxbjcc .gt_font_normal {
  font-weight: normal;
}

#umcirxbjcc .gt_font_bold {
  font-weight: bold;
}

#umcirxbjcc .gt_font_italic {
  font-style: italic;
}

#umcirxbjcc .gt_super {
  font-size: 65%;
}

#umcirxbjcc .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="umcirxbjcc" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd_24hrs</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">11.23</td>
      <td class="gt_row gt_right">48.72</td>
      <td class="gt_row gt_right">10.08</td>
      <td class="gt_row gt_right">5.85</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">12.59</td>
      <td class="gt_row gt_right">46.93</td>
      <td class="gt_row gt_right">9.41</td>
      <td class="gt_row gt_right">6.10</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">13.28</td>
      <td class="gt_row gt_right">42.84</td>
      <td class="gt_row gt_right">9.07</td>
      <td class="gt_row gt_right">5.48</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">13.82</td>
      <td class="gt_row gt_right">42.81</td>
      <td class="gt_row gt_right">8.26</td>
      <td class="gt_row gt_right">5.86</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">14.79</td>
      <td class="gt_row gt_right">42.53</td>
      <td class="gt_row gt_right">8.67</td>
      <td class="gt_row gt_right">5.91</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">15.67</td>
      <td class="gt_row gt_right">41.69</td>
      <td class="gt_row gt_right">10.61</td>
      <td class="gt_row gt_right">6.21</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Join de tots els data frames de lags de telemetria  
 

{% highlight r %}
# 3 hores: mean + sd
telemetriafeat <- 
  telemetriamean %>% 
  left_join(telemetriasd, by = c("datetime", "machineID"))
 
# 24 hores: mean +sd
telemetriafeat_24hrs <- 
  telemetriamean_24hrs %>% 
  left_join(telemetriasd_24hrs, by = c("datetime", "machineID"))
 
# tot
telemetriafeat <- 
  telemetriafeat %>%
  left_join(telemetriafeat_24hrs, by = c("datetime", "machineID")) %>%
  filter(!is.na(voltmean_24hrs)) %>% 
  ungroup()
 
telemetriafeat %>% 
  head() %>% 
  gt() %>% 
  fmt_number(columns = 3:18, decimals = 2)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#dfjlioiukx .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#dfjlioiukx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dfjlioiukx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#dfjlioiukx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#dfjlioiukx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dfjlioiukx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dfjlioiukx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#dfjlioiukx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#dfjlioiukx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dfjlioiukx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dfjlioiukx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#dfjlioiukx .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#dfjlioiukx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#dfjlioiukx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dfjlioiukx .gt_from_md > :first-child {
  margin-top: 0;
}

#dfjlioiukx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dfjlioiukx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#dfjlioiukx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#dfjlioiukx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dfjlioiukx .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#dfjlioiukx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dfjlioiukx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dfjlioiukx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dfjlioiukx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dfjlioiukx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#dfjlioiukx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dfjlioiukx .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#dfjlioiukx .gt_left {
  text-align: left;
}

#dfjlioiukx .gt_center {
  text-align: center;
}

#dfjlioiukx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dfjlioiukx .gt_font_normal {
  font-weight: normal;
}

#dfjlioiukx .gt_font_bold {
  font-weight: bold;
}

#dfjlioiukx .gt_font_italic {
  font-style: italic;
}

#dfjlioiukx .gt_super {
  font-size: 65%;
}

#dfjlioiukx .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="dfjlioiukx" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd_24hrs</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">180.13</td>
      <td class="gt_row gt_right">440.61</td>
      <td class="gt_row gt_right">94.14</td>
      <td class="gt_row gt_right">41.55</td>
      <td class="gt_row gt_right">21.32</td>
      <td class="gt_row gt_right">48.77</td>
      <td class="gt_row gt_right">2.14</td>
      <td class="gt_row gt_right">10.04</td>
      <td class="gt_row gt_right">169.73</td>
      <td class="gt_row gt_right">445.18</td>
      <td class="gt_row gt_right">96.80</td>
      <td class="gt_row gt_right">40.39</td>
      <td class="gt_row gt_right">11.23</td>
      <td class="gt_row gt_right">48.72</td>
      <td class="gt_row gt_right">10.08</td>
      <td class="gt_row gt_right">5.85</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">176.36</td>
      <td class="gt_row gt_right">439.35</td>
      <td class="gt_row gt_right">101.55</td>
      <td class="gt_row gt_right">36.11</td>
      <td class="gt_row gt_right">18.95</td>
      <td class="gt_row gt_right">51.33</td>
      <td class="gt_row gt_right">13.79</td>
      <td class="gt_row gt_right">6.74</td>
      <td class="gt_row gt_right">170.53</td>
      <td class="gt_row gt_right">443.91</td>
      <td class="gt_row gt_right">97.67</td>
      <td class="gt_row gt_right">39.79</td>
      <td class="gt_row gt_right">12.59</td>
      <td class="gt_row gt_right">46.93</td>
      <td class="gt_row gt_right">9.41</td>
      <td class="gt_row gt_right">6.10</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">160.38</td>
      <td class="gt_row gt_right">424.39</td>
      <td class="gt_row gt_right">99.60</td>
      <td class="gt_row gt_right">36.09</td>
      <td class="gt_row gt_right">13.05</td>
      <td class="gt_row gt_right">13.70</td>
      <td class="gt_row gt_right">9.99</td>
      <td class="gt_row gt_right">1.64</td>
      <td class="gt_row gt_right">170.05</td>
      <td class="gt_row gt_right">446.46</td>
      <td class="gt_row gt_right">96.91</td>
      <td class="gt_row gt_right">40.02</td>
      <td class="gt_row gt_right">13.28</td>
      <td class="gt_row gt_right">42.84</td>
      <td class="gt_row gt_right">9.07</td>
      <td class="gt_row gt_right">5.48</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">170.47</td>
      <td class="gt_row gt_right">442.93</td>
      <td class="gt_row gt_right">102.38</td>
      <td class="gt_row gt_right">40.48</td>
      <td class="gt_row gt_right">16.64</td>
      <td class="gt_row gt_right">56.29</td>
      <td class="gt_row gt_right">3.31</td>
      <td class="gt_row gt_right">8.85</td>
      <td class="gt_row gt_right">170.34</td>
      <td class="gt_row gt_right">447.36</td>
      <td class="gt_row gt_right">96.23</td>
      <td class="gt_row gt_right">39.92</td>
      <td class="gt_row gt_right">13.82</td>
      <td class="gt_row gt_right">42.81</td>
      <td class="gt_row gt_right">8.26</td>
      <td class="gt_row gt_right">5.86</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">163.26</td>
      <td class="gt_row gt_right">468.94</td>
      <td class="gt_row gt_right">102.73</td>
      <td class="gt_row gt_right">40.92</td>
      <td class="gt_row gt_right">17.42</td>
      <td class="gt_row gt_right">38.68</td>
      <td class="gt_row gt_right">9.11</td>
      <td class="gt_row gt_right">3.06</td>
      <td class="gt_row gt_right">170.06</td>
      <td class="gt_row gt_right">452.16</td>
      <td class="gt_row gt_right">96.36</td>
      <td class="gt_row gt_right">39.99</td>
      <td class="gt_row gt_right">14.79</td>
      <td class="gt_row gt_right">42.53</td>
      <td class="gt_row gt_right">8.67</td>
      <td class="gt_row gt_right">5.91</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">163.28</td>
      <td class="gt_row gt_right">446.49</td>
      <td class="gt_row gt_right">104.39</td>
      <td class="gt_row gt_right">38.07</td>
      <td class="gt_row gt_right">21.58</td>
      <td class="gt_row gt_right">41.38</td>
      <td class="gt_row gt_right">20.73</td>
      <td class="gt_row gt_right">6.93</td>
      <td class="gt_row gt_right">169.37</td>
      <td class="gt_row gt_right">453.34</td>
      <td class="gt_row gt_right">98.04</td>
      <td class="gt_row gt_right">39.53</td>
      <td class="gt_row gt_right">15.67</td>
      <td class="gt_row gt_right">41.69</td>
      <td class="gt_row gt_right">10.61</td>
      <td class="gt_row gt_right">6.21</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Descripció de les variables:  
 

{% highlight r %}
skim(telemetriafeat)
{% endhighlight %}


|                         |               |
|:------------------------|:--------------|
|Name                     |telemetriafeat |
|Number of rows           |291300         |
|Number of columns        |18             |
|_______________________  |               |
|Column type frequency:   |               |
|numeric                  |17             |
|POSIXct                  |1              |
|________________________ |               |
|Group variables          |None           |


**Variable type: numeric**

|skim_variable       | n_missing| complete_rate|   mean|    sd|     p0|    p25|    p50|    p75|   p100|hist  |
|:-------------------|---------:|-------------:|------:|-----:|------:|------:|------:|------:|------:|:-----|
|machineID           |         0|             1|  50.50| 28.87|   1.00|  25.75|  50.50|  75.25| 100.00|▇▇▇▇▇ |
|voltmean            |         0|             1| 170.77|  9.50| 125.53| 164.45| 170.43| 176.61| 241.42|▁▇▆▁▁ |
|rotatemean          |         0|             1| 446.61| 33.12| 211.81| 427.57| 448.39| 468.45| 586.68|▁▁▅▇▁ |
|pressuremean        |         0|             1| 100.86|  7.41|  72.12|  96.24| 100.23| 104.40| 162.31|▁▇▁▁▁ |
|vibrationmean       |         0|             1|  40.38|  3.48|  26.57|  38.15|  40.15|  42.23|  69.31|▁▇▁▁▁ |
|voltsd              |         0|             1|  13.30|  6.97|   0.03|   8.03|  12.50|  17.69|  58.44|▇▇▂▁▁ |
|rotatesd            |         0|             1|  44.46| 23.22|   0.08|  26.90|  41.80|  59.10| 179.90|▇▇▂▁▁ |
|pressuresd          |         0|             1|   8.89|  4.66|   0.03|   5.37|   8.35|  11.79|  35.66|▇▇▂▁▁ |
|vibrationsd         |         0|             1|   4.44|  2.32|   0.02|   2.68|   4.17|   5.90|  18.31|▇▇▂▁▁ |
|voltmean_24hrs      |         0|             1| 170.77|  4.72| 156.28| 168.07| 170.21| 172.45| 220.57|▅▇▁▁▁ |
|rotatemean_24hrs    |         0|             1| 446.61| 18.07| 267.01| 441.57| 449.20| 456.37| 499.29|▁▁▁▇▅ |
|pressuremean_24hrs  |         0|             1| 100.85|  4.73|  90.35|  98.67| 100.10| 101.61| 152.66|▇▁▁▁▁ |
|vibrationmean_24hrs |         0|             1|  40.38|  2.06|  35.25|  39.36|  40.07|  40.83|  61.85|▇▃▁▁▁ |
|voltsd_24hrs        |         0|             1|  14.92|  2.26|   6.50|  13.36|  14.86|  16.40|  27.91|▁▇▇▁▁ |
|rotatesd_24hrs      |         0|             1|  49.95|  7.69|  19.84|  44.67|  49.61|  54.80| 105.33|▁▇▃▁▁ |
|pressuresd_24hrs    |         0|             1|  10.05|  1.71|   4.43|   8.92|   9.92|  10.98|  28.87|▅▇▁▁▁ |
|vibrationsd_24hrs   |         0|             1|   5.00|  0.80|   2.11|   4.46|   4.96|   5.48|  12.61|▂▇▁▁▁ |


**Variable type: POSIXct**

|skim_variable | n_missing| complete_rate|min                 |max                 |median              | n_unique|
|:-------------|---------:|-------------:|:-------------------|:-------------------|:-------------------|--------:|
|datetime      |         0|             1|2015-01-02 05:00:00 |2016-01-01 05:00:00 |2015-07-03 05:00:00 |     2913|
 
 
Evolució de la mitjana i de la sd de la vibració en finestres de 24h.  
 

{% highlight r %}
telemetriafeat %>% 
  filter(machineID == 1, 
         datetime > as.POSIXct("2015-01-01"), datetime < as.POSIXct("2015-02-01")) %>% 
  ggplot() +
  geom_line(aes(x = datetime, y = vibrationmean_24hrs, col = factor(machineID)), alpha = 0.5) +
  labs(y = "vibration", color = "machineID", title = "Vibracio: mean 24h")+
  theme_minimal()
{% endhighlight %}

![plot of chunk pre9](/figures/pre9-1.png)

{% highlight r %}
telemetriafeat %>% 
  filter(machineID == 1, 
         datetime > as.POSIXct("2015-01-01"), datetime < as.POSIXct("2015-02-01")) %>% 
  ggplot() +
  geom_line(aes(x = datetime, y = vibrationsd_24hrs, col = factor(machineID)), alpha = 0.5, color ="blue") +
  labs(y = "vibration", color = "machineID", title = "Vibracio: sd 24h")+
  theme_minimal()
{% endhighlight %}

![plot of chunk pre9](/figures/pre9-2.png)
 
 
### Errors
 
Registre temporal d'una variable categòrica (diferents classes d'error).  
Totalitzarem els erros que succeeixen en una finestra de temps.  
 

{% highlight r %}
gt(head(errors))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#tdcohiqaht .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#tdcohiqaht .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tdcohiqaht .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#tdcohiqaht .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#tdcohiqaht .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tdcohiqaht .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tdcohiqaht .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#tdcohiqaht .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#tdcohiqaht .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tdcohiqaht .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tdcohiqaht .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#tdcohiqaht .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#tdcohiqaht .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#tdcohiqaht .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tdcohiqaht .gt_from_md > :first-child {
  margin-top: 0;
}

#tdcohiqaht .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tdcohiqaht .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#tdcohiqaht .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#tdcohiqaht .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tdcohiqaht .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#tdcohiqaht .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tdcohiqaht .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tdcohiqaht .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tdcohiqaht .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tdcohiqaht .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#tdcohiqaht .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tdcohiqaht .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#tdcohiqaht .gt_left {
  text-align: left;
}

#tdcohiqaht .gt_center {
  text-align: center;
}

#tdcohiqaht .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tdcohiqaht .gt_font_normal {
  font-weight: normal;
}

#tdcohiqaht .gt_font_bold {
  font-weight: bold;
}

#tdcohiqaht .gt_font_italic {
  font-style: italic;
}

#tdcohiqaht .gt_super {
  font-size: 65%;
}

#tdcohiqaht .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="tdcohiqaht" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">errorID</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-03 07:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-03 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error3</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-04 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error5</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-10 15:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-22 10:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-25 15:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">error4</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Calculem els diferents tipus d'errors per cada registre de temps
 

{% highlight r %}
cnt_errors <- 
  errors %>% 
    group_by(machineID, datetime) %>% 
    count(errorID) %>% 
    pivot_wider(names_from = "errorID", values_from = "n") %>% 
    ungroup() %>% 
    mutate_if(is.numeric, replace_na,0) %>% 
    rename_at(vars(starts_with("error")), ~(paste(., "_sum", sep="")))
 
gt(head(cnt_errors))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#qnqgdalbxt .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qnqgdalbxt .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qnqgdalbxt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qnqgdalbxt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qnqgdalbxt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qnqgdalbxt .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qnqgdalbxt .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qnqgdalbxt .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qnqgdalbxt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qnqgdalbxt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qnqgdalbxt .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qnqgdalbxt .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#qnqgdalbxt .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qnqgdalbxt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qnqgdalbxt .gt_from_md > :first-child {
  margin-top: 0;
}

#qnqgdalbxt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qnqgdalbxt .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qnqgdalbxt .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#qnqgdalbxt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qnqgdalbxt .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#qnqgdalbxt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qnqgdalbxt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qnqgdalbxt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qnqgdalbxt .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qnqgdalbxt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#qnqgdalbxt .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qnqgdalbxt .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#qnqgdalbxt .gt_left {
  text-align: left;
}

#qnqgdalbxt .gt_center {
  text-align: center;
}

#qnqgdalbxt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qnqgdalbxt .gt_font_normal {
  font-weight: normal;
}

#qnqgdalbxt .gt_font_bold {
  font-weight: bold;
}

#qnqgdalbxt .gt_font_italic {
  font-style: italic;
}

#qnqgdalbxt .gt_super {
  font-size: 65%;
}

#qnqgdalbxt .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="qnqgdalbxt" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error1_sum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error3_sum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error5_sum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error4_sum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error2_sum</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-03 07:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-03 20:00:00</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-04 06:00:00</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-10 15:00:00</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-22 10:00:00</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-25 15:00:00</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Agreguem la totalització d'errors al dataframe de telemetria  
 

{% highlight r %}
errorfeat <- 
  telemetria %>% 
  select(datetime, machineID) %>%
  left_join(cnt_errors, by = c("datetime", "machineID")) %>% 
  mutate_if(is.numeric, replace_na,0)
 
gt(head(errorfeat))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#usdukptzsr .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#usdukptzsr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#usdukptzsr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#usdukptzsr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#usdukptzsr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#usdukptzsr .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#usdukptzsr .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#usdukptzsr .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#usdukptzsr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#usdukptzsr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#usdukptzsr .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#usdukptzsr .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#usdukptzsr .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#usdukptzsr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#usdukptzsr .gt_from_md > :first-child {
  margin-top: 0;
}

#usdukptzsr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#usdukptzsr .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#usdukptzsr .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#usdukptzsr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#usdukptzsr .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#usdukptzsr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#usdukptzsr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#usdukptzsr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#usdukptzsr .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#usdukptzsr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#usdukptzsr .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#usdukptzsr .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#usdukptzsr .gt_left {
  text-align: left;
}

#usdukptzsr .gt_center {
  text-align: center;
}

#usdukptzsr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#usdukptzsr .gt_font_normal {
  font-weight: normal;
}

#usdukptzsr .gt_font_bold {
  font-weight: bold;
}

#usdukptzsr .gt_font_italic {
  font-style: italic;
}

#usdukptzsr .gt_super {
  font-size: 65%;
}

#usdukptzsr .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="usdukptzsr" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error1_sum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error3_sum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error5_sum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error4_sum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error2_sum</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-01 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 07:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 09:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 10:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-01 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Descripció de les variables:  
 

{% highlight r %}
skim(errorfeat)
{% endhighlight %}


|                         |          |
|:------------------------|:---------|
|Name                     |errorfeat |
|Number of rows           |876100    |
|Number of columns        |7         |
|_______________________  |          |
|Column type frequency:   |          |
|numeric                  |6         |
|POSIXct                  |1         |
|________________________ |          |
|Group variables          |None      |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate| mean|    sd| p0|   p25|  p50|   p75| p100|hist  |
|:-------------|---------:|-------------:|----:|-----:|--:|-----:|----:|-----:|----:|:-----|
|machineID     |         0|             1| 50.5| 28.87|  1| 25.75| 50.5| 75.25|  100|▇▇▇▇▇ |
|error1_sum    |         0|             1|  0.0|  0.03|  0|  0.00|  0.0|  0.00|    1|▇▁▁▁▁ |
|error3_sum    |         0|             1|  0.0|  0.03|  0|  0.00|  0.0|  0.00|    1|▇▁▁▁▁ |
|error5_sum    |         0|             1|  0.0|  0.02|  0|  0.00|  0.0|  0.00|    1|▇▁▁▁▁ |
|error4_sum    |         0|             1|  0.0|  0.03|  0|  0.00|  0.0|  0.00|    1|▇▁▁▁▁ |
|error2_sum    |         0|             1|  0.0|  0.03|  0|  0.00|  0.0|  0.00|    1|▇▁▁▁▁ |


**Variable type: POSIXct**

|skim_variable | n_missing| complete_rate|min                 |max                 |median              | n_unique|
|:-------------|---------:|-------------:|:-------------------|:-------------------|:-------------------|--------:|
|datetime      |         0|             1|2015-01-01 06:00:00 |2016-01-01 06:00:00 |2015-07-02 18:00:00 |     8761|
 
 
Calculem el total d'errors d'una finetra temporal de 24h, cada 3 hores per cada màquina.  
 

{% highlight r %}
errorfeat <- 
  errorfeat %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(error1count = rollapply(error1_sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error2count = rollapply(error2_sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error3count = rollapply(error3_sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error4count = rollapply(error4_sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error5count = rollapply(error5_sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, error1count, error2count, error3count, error4count, error5count) %>%
  filter(!is.na(error1count)) %>% 
  ungroup()
 
gt(head(errorfeat))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#rxnrlfukth .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#rxnrlfukth .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rxnrlfukth .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rxnrlfukth .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rxnrlfukth .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rxnrlfukth .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rxnrlfukth .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#rxnrlfukth .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rxnrlfukth .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rxnrlfukth .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rxnrlfukth .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rxnrlfukth .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#rxnrlfukth .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rxnrlfukth .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rxnrlfukth .gt_from_md > :first-child {
  margin-top: 0;
}

#rxnrlfukth .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rxnrlfukth .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#rxnrlfukth .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#rxnrlfukth .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rxnrlfukth .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#rxnrlfukth .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rxnrlfukth .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rxnrlfukth .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rxnrlfukth .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rxnrlfukth .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#rxnrlfukth .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rxnrlfukth .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#rxnrlfukth .gt_left {
  text-align: left;
}

#rxnrlfukth .gt_center {
  text-align: center;
}

#rxnrlfukth .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rxnrlfukth .gt_font_normal {
  font-weight: normal;
}

#rxnrlfukth .gt_font_bold {
  font-weight: bold;
}

#rxnrlfukth .gt_font_italic {
  font-style: italic;
}

#rxnrlfukth .gt_super {
  font-size: 65%;
}

#rxnrlfukth .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="rxnrlfukth" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error1count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error2count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error3count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error4count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error5count</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Descripció de les variables:  
 

{% highlight r %}
skim(errorfeat)
{% endhighlight %}


|                         |          |
|:------------------------|:---------|
|Name                     |errorfeat |
|Number of rows           |291300    |
|Number of columns        |7         |
|_______________________  |          |
|Column type frequency:   |          |
|numeric                  |6         |
|POSIXct                  |1         |
|________________________ |          |
|Group variables          |None      |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|  mean|    sd| p0|   p25|  p50|   p75| p100|hist  |
|:-------------|---------:|-------------:|-----:|-----:|--:|-----:|----:|-----:|----:|:-----|
|machineID     |         0|             1| 50.50| 28.87|  1| 25.75| 50.5| 75.25|  100|▇▇▇▇▇ |
|error1count   |         0|             1|  0.03|  0.17|  0|  0.00|  0.0|  0.00|    2|▇▁▁▁▁ |
|error2count   |         0|             1|  0.03|  0.16|  0|  0.00|  0.0|  0.00|    2|▇▁▁▁▁ |
|error3count   |         0|             1|  0.02|  0.15|  0|  0.00|  0.0|  0.00|    2|▇▁▁▁▁ |
|error4count   |         0|             1|  0.02|  0.14|  0|  0.00|  0.0|  0.00|    2|▇▁▁▁▁ |
|error5count   |         0|             1|  0.01|  0.10|  0|  0.00|  0.0|  0.00|    2|▇▁▁▁▁ |


**Variable type: POSIXct**

|skim_variable | n_missing| complete_rate|min                 |max                 |median              | n_unique|
|:-------------|---------:|-------------:|:-------------------|:-------------------|:-------------------|--------:|
|datetime      |         0|             1|2015-01-02 05:00:00 |2016-01-01 05:00:00 |2015-07-03 05:00:00 |     2913|
 
 
### Registres de Manteniment  
 
Dies passats des de que un component ha estat substituit per última vegada.  
 
Taula binaria per cada component. 1 ha estat canviat, 0 no ha estat canviat.  
 

{% highlight r %}
comprep <- mant %>% 
    select(datetime, machineID, comp) %>% 
    mutate(comp1 = as.integer(comp == "comp1"), 
           comp2 = as.integer(comp == "comp2"),
           comp3 = as.integer(comp == "comp3"),
           comp4 = as.integer(comp == "comp4")) %>%
    select(-comp)
 
gt(head(comprep))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ixpssoyczl .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ixpssoyczl .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ixpssoyczl .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ixpssoyczl .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ixpssoyczl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ixpssoyczl .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ixpssoyczl .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ixpssoyczl .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ixpssoyczl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ixpssoyczl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ixpssoyczl .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ixpssoyczl .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ixpssoyczl .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ixpssoyczl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ixpssoyczl .gt_from_md > :first-child {
  margin-top: 0;
}

#ixpssoyczl .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ixpssoyczl .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ixpssoyczl .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ixpssoyczl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ixpssoyczl .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ixpssoyczl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ixpssoyczl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ixpssoyczl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ixpssoyczl .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ixpssoyczl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ixpssoyczl .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ixpssoyczl .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ixpssoyczl .gt_left {
  text-align: left;
}

#ixpssoyczl .gt_center {
  text-align: center;
}

#ixpssoyczl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ixpssoyczl .gt_font_normal {
  font-weight: normal;
}

#ixpssoyczl .gt_font_bold {
  font-weight: bold;
}

#ixpssoyczl .gt_font_italic {
  font-style: italic;
}

#ixpssoyczl .gt_super {
  font-size: 65%;
}

#ixpssoyczl .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="ixpssoyczl" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">comp1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">comp2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">comp3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">comp4</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2014-06-01 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2014-07-16 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2014-07-31 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2014-12-13 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-05 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-05 06:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Converitr en data.table i assignar index a la taula
 

{% highlight r %}
comprep <- as.data.table(comprep) #convertir en data.table
setkey(comprep, machineID, datetime) # assignar index
{% endhighlight %}
 
Separar els diferents canvis de component en diferents taules.
Dupliquem la data en una nova variable. 
 

{% highlight r %}
comp1rep <- comprep[comp1 == 1, .(machineID, datetime, lastrepcomp1 = datetime)]
comp2rep <- comprep[comp2 == 1, .(machineID, datetime, lastrepcomp2 = datetime)]
comp3rep <- comprep[comp3 == 1, .(machineID, datetime, lastrepcomp3 = datetime)]
comp4rep <- comprep[comp4 == 1, .(machineID, datetime, lastrepcomp4 = datetime)]
 
gt(head(comp4rep))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#swgaiayqsz .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#swgaiayqsz .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#swgaiayqsz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#swgaiayqsz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#swgaiayqsz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#swgaiayqsz .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#swgaiayqsz .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#swgaiayqsz .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#swgaiayqsz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#swgaiayqsz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#swgaiayqsz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#swgaiayqsz .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#swgaiayqsz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#swgaiayqsz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#swgaiayqsz .gt_from_md > :first-child {
  margin-top: 0;
}

#swgaiayqsz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#swgaiayqsz .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#swgaiayqsz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#swgaiayqsz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#swgaiayqsz .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#swgaiayqsz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#swgaiayqsz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#swgaiayqsz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#swgaiayqsz .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#swgaiayqsz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#swgaiayqsz .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#swgaiayqsz .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#swgaiayqsz .gt_left {
  text-align: left;
}

#swgaiayqsz .gt_center {
  text-align: center;
}

#swgaiayqsz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#swgaiayqsz .gt_font_normal {
  font-weight: normal;
}

#swgaiayqsz .gt_font_bold {
  font-weight: bold;
}

#swgaiayqsz .gt_font_italic {
  font-style: italic;
}

#swgaiayqsz .gt_super {
  font-size: 65%;
}

#swgaiayqsz .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="swgaiayqsz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">lastrepcomp4</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2014-07-16 06:00:00</td>
      <td class="gt_row gt_left">2014-07-16 06:00:00</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-05 06:00:00</td>
      <td class="gt_row gt_left">2015-01-05 06:00:00</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-02-04 06:00:00</td>
      <td class="gt_row gt_left">2015-02-04 06:00:00</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-06-19 06:00:00</td>
      <td class="gt_row gt_left">2015-06-19 06:00:00</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-09-02 06:00:00</td>
      <td class="gt_row gt_left">2015-09-02 06:00:00</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-10-02 06:00:00</td>
      <td class="gt_row gt_left">2015-10-02 06:00:00</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Ultilitzar el timestamp del dataframe de telemetria  
 

{% highlight r %}
compdate <- as.data.table(telemetriafeat[,c(1:2)]) 
setkey(compdate, machineID, datetime)
{% endhighlight %}
 
Join roll del timesatmap de telemetriafeat amb les dates de l'últim error.  
Lliga la data del telemetriafeat amb la data que correspont a l'anterior error del component.  
 

{% highlight r %}
comp1feat <- comp1rep[compdate[,.(machineID, datetime)],roll = TRUE]
comp2feat <- comp2rep[compdate[,.(machineID, datetime)],roll = TRUE]
comp3feat <- comp3rep[compdate[,.(machineID, datetime)],roll = TRUE]
comp4feat <- comp4rep[compdate[,.(machineID, datetime)],roll = TRUE]
 
gt(head(comp4feat))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uanwshcdam .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uanwshcdam .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uanwshcdam .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uanwshcdam .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uanwshcdam .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uanwshcdam .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uanwshcdam .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uanwshcdam .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uanwshcdam .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uanwshcdam .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uanwshcdam .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uanwshcdam .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#uanwshcdam .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uanwshcdam .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uanwshcdam .gt_from_md > :first-child {
  margin-top: 0;
}

#uanwshcdam .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uanwshcdam .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uanwshcdam .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#uanwshcdam .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uanwshcdam .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#uanwshcdam .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uanwshcdam .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uanwshcdam .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uanwshcdam .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uanwshcdam .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#uanwshcdam .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uanwshcdam .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#uanwshcdam .gt_left {
  text-align: left;
}

#uanwshcdam .gt_center {
  text-align: center;
}

#uanwshcdam .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uanwshcdam .gt_font_normal {
  font-weight: normal;
}

#uanwshcdam .gt_font_bold {
  font-weight: bold;
}

#uanwshcdam .gt_font_italic {
  font-style: italic;
}

#uanwshcdam .gt_super {
  font-size: 65%;
}

#uanwshcdam .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="uanwshcdam" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">lastrepcomp4</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_left">2014-07-16 06:00:00</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_left">2014-07-16 06:00:00</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_left">2014-07-16 06:00:00</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_left">2014-07-16 06:00:00</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_left">2014-07-16 06:00:00</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_left">2014-07-16 06:00:00</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Amb les dues dates, calcula els dies passats.  
 

{% highlight r %}
comp1feat$sincelastcomp1 <- as.numeric(difftime(comp1feat$datetime, comp1feat$lastrepcomp1, units = "days"))
comp2feat$sincelastcomp2 <- as.numeric(difftime(comp2feat$datetime, comp2feat$lastrepcomp2, units = "days"))
comp3feat$sincelastcomp3 <- as.numeric(difftime(comp3feat$datetime, comp3feat$lastrepcomp3, units = "days"))
comp4feat$sincelastcomp4 <- as.numeric(difftime(comp4feat$datetime, comp4feat$lastrepcomp4, units = "days"))
{% endhighlight %}
 
Agregar tots el componenets.   
 

{% highlight r %}
compfeat <- 
 bind_cols(compdate, comp1feat, comp2feat, comp3feat, comp4feat) %>% 
 select(datetime, machineID, starts_with("since"))
 
compfeat %>% 
  head() %>% 
  gt() %>% 
  fmt_number(columns = 3:6, decimals = 2)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#oozywztmzk .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#oozywztmzk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#oozywztmzk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#oozywztmzk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#oozywztmzk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oozywztmzk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#oozywztmzk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#oozywztmzk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#oozywztmzk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#oozywztmzk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#oozywztmzk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#oozywztmzk .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#oozywztmzk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#oozywztmzk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#oozywztmzk .gt_from_md > :first-child {
  margin-top: 0;
}

#oozywztmzk .gt_from_md > :last-child {
  margin-bottom: 0;
}

#oozywztmzk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#oozywztmzk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#oozywztmzk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oozywztmzk .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#oozywztmzk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oozywztmzk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#oozywztmzk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oozywztmzk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#oozywztmzk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#oozywztmzk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#oozywztmzk .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#oozywztmzk .gt_left {
  text-align: left;
}

#oozywztmzk .gt_center {
  text-align: center;
}

#oozywztmzk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#oozywztmzk .gt_font_normal {
  font-weight: normal;
}

#oozywztmzk .gt_font_bold {
  font-weight: bold;
}

#oozywztmzk .gt_font_italic {
  font-style: italic;
}

#oozywztmzk .gt_super {
  font-size: 65%;
}

#oozywztmzk .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="oozywztmzk" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp4</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">19.96</td>
      <td class="gt_row gt_right">214.96</td>
      <td class="gt_row gt_right">154.96</td>
      <td class="gt_row gt_right">169.96</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">20.08</td>
      <td class="gt_row gt_right">215.08</td>
      <td class="gt_row gt_right">155.08</td>
      <td class="gt_row gt_right">170.08</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">20.21</td>
      <td class="gt_row gt_right">215.21</td>
      <td class="gt_row gt_right">155.21</td>
      <td class="gt_row gt_right">170.21</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">20.33</td>
      <td class="gt_row gt_right">215.33</td>
      <td class="gt_row gt_right">155.33</td>
      <td class="gt_row gt_right">170.33</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">20.46</td>
      <td class="gt_row gt_right">215.46</td>
      <td class="gt_row gt_right">155.46</td>
      <td class="gt_row gt_right">170.46</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">20.58</td>
      <td class="gt_row gt_right">215.58</td>
      <td class="gt_row gt_right">155.58</td>
      <td class="gt_row gt_right">170.58</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
### Agregar totes les dades.  
 

{% highlight r %}
 # telemetriaemtria amb mitjanes, sd, i 24 hores
gt(head(telemetriafeat))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#rrcxjgqmon .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#rrcxjgqmon .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rrcxjgqmon .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rrcxjgqmon .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rrcxjgqmon .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rrcxjgqmon .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rrcxjgqmon .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#rrcxjgqmon .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rrcxjgqmon .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rrcxjgqmon .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rrcxjgqmon .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rrcxjgqmon .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#rrcxjgqmon .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rrcxjgqmon .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rrcxjgqmon .gt_from_md > :first-child {
  margin-top: 0;
}

#rrcxjgqmon .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rrcxjgqmon .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#rrcxjgqmon .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#rrcxjgqmon .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rrcxjgqmon .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#rrcxjgqmon .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rrcxjgqmon .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rrcxjgqmon .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rrcxjgqmon .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rrcxjgqmon .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#rrcxjgqmon .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rrcxjgqmon .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#rrcxjgqmon .gt_left {
  text-align: left;
}

#rrcxjgqmon .gt_center {
  text-align: center;
}

#rrcxjgqmon .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rrcxjgqmon .gt_font_normal {
  font-weight: normal;
}

#rrcxjgqmon .gt_font_bold {
  font-weight: bold;
}

#rrcxjgqmon .gt_font_italic {
  font-style: italic;
}

#rrcxjgqmon .gt_super {
  font-size: 65%;
}

#rrcxjgqmon .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="rrcxjgqmon" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd_24hrs</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">180.1338</td>
      <td class="gt_row gt_right">440.6083</td>
      <td class="gt_row gt_right">94.13797</td>
      <td class="gt_row gt_right">41.55154</td>
      <td class="gt_row gt_right">21.32273</td>
      <td class="gt_row gt_right">48.77051</td>
      <td class="gt_row gt_right">2.135684</td>
      <td class="gt_row gt_right">10.037208</td>
      <td class="gt_row gt_right">169.7338</td>
      <td class="gt_row gt_right">445.1799</td>
      <td class="gt_row gt_right">96.79711</td>
      <td class="gt_row gt_right">40.38516</td>
      <td class="gt_row gt_right">11.23312</td>
      <td class="gt_row gt_right">48.71739</td>
      <td class="gt_row gt_right">10.079880</td>
      <td class="gt_row gt_right">5.853209</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">176.3643</td>
      <td class="gt_row gt_right">439.3497</td>
      <td class="gt_row gt_right">101.55321</td>
      <td class="gt_row gt_right">36.10558</td>
      <td class="gt_row gt_right">18.95221</td>
      <td class="gt_row gt_right">51.32964</td>
      <td class="gt_row gt_right">13.789279</td>
      <td class="gt_row gt_right">6.737739</td>
      <td class="gt_row gt_right">170.5257</td>
      <td class="gt_row gt_right">443.9068</td>
      <td class="gt_row gt_right">97.66725</td>
      <td class="gt_row gt_right">39.78667</td>
      <td class="gt_row gt_right">12.59195</td>
      <td class="gt_row gt_right">46.93028</td>
      <td class="gt_row gt_right">9.406795</td>
      <td class="gt_row gt_right">6.098173</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">160.3846</td>
      <td class="gt_row gt_right">424.3853</td>
      <td class="gt_row gt_right">99.59872</td>
      <td class="gt_row gt_right">36.09464</td>
      <td class="gt_row gt_right">13.04708</td>
      <td class="gt_row gt_right">13.70250</td>
      <td class="gt_row gt_right">9.988609</td>
      <td class="gt_row gt_right">1.639962</td>
      <td class="gt_row gt_right">170.0497</td>
      <td class="gt_row gt_right">446.4613</td>
      <td class="gt_row gt_right">96.90616</td>
      <td class="gt_row gt_right">40.01651</td>
      <td class="gt_row gt_right">13.27734</td>
      <td class="gt_row gt_right">42.83678</td>
      <td class="gt_row gt_right">9.071472</td>
      <td class="gt_row gt_right">5.481724</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">170.4725</td>
      <td class="gt_row gt_right">442.9340</td>
      <td class="gt_row gt_right">102.38059</td>
      <td class="gt_row gt_right">40.48300</td>
      <td class="gt_row gt_right">16.64235</td>
      <td class="gt_row gt_right">56.29045</td>
      <td class="gt_row gt_right">3.305739</td>
      <td class="gt_row gt_right">8.854145</td>
      <td class="gt_row gt_right">170.3420</td>
      <td class="gt_row gt_right">447.3553</td>
      <td class="gt_row gt_right">96.22952</td>
      <td class="gt_row gt_right">39.92196</td>
      <td class="gt_row gt_right">13.81716</td>
      <td class="gt_row gt_right">42.80863</td>
      <td class="gt_row gt_right">8.256794</td>
      <td class="gt_row gt_right">5.862312</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">163.2638</td>
      <td class="gt_row gt_right">468.9376</td>
      <td class="gt_row gt_right">102.72665</td>
      <td class="gt_row gt_right">40.92180</td>
      <td class="gt_row gt_right">17.42469</td>
      <td class="gt_row gt_right">38.68038</td>
      <td class="gt_row gt_right">9.105775</td>
      <td class="gt_row gt_right">3.060781</td>
      <td class="gt_row gt_right">170.0606</td>
      <td class="gt_row gt_right">452.1634</td>
      <td class="gt_row gt_right">96.35744</td>
      <td class="gt_row gt_right">39.99047</td>
      <td class="gt_row gt_right">14.79287</td>
      <td class="gt_row gt_right">42.52529</td>
      <td class="gt_row gt_right">8.669605</td>
      <td class="gt_row gt_right">5.907157</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">163.2785</td>
      <td class="gt_row gt_right">446.4932</td>
      <td class="gt_row gt_right">104.38758</td>
      <td class="gt_row gt_right">38.06812</td>
      <td class="gt_row gt_right">21.58049</td>
      <td class="gt_row gt_right">41.38096</td>
      <td class="gt_row gt_right">20.725597</td>
      <td class="gt_row gt_right">6.932127</td>
      <td class="gt_row gt_right">169.3693</td>
      <td class="gt_row gt_right">453.3362</td>
      <td class="gt_row gt_right">98.04201</td>
      <td class="gt_row gt_right">39.53167</td>
      <td class="gt_row gt_right">15.67479</td>
      <td class="gt_row gt_right">41.68962</td>
      <td class="gt_row gt_right">10.607947</td>
      <td class="gt_row gt_right">6.205887</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

{% highlight r %}
# contador d'errors
gt(head(errorfeat)) 
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#tbgdnqpzpg .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#tbgdnqpzpg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tbgdnqpzpg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#tbgdnqpzpg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#tbgdnqpzpg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tbgdnqpzpg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tbgdnqpzpg .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#tbgdnqpzpg .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#tbgdnqpzpg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tbgdnqpzpg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tbgdnqpzpg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#tbgdnqpzpg .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#tbgdnqpzpg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#tbgdnqpzpg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tbgdnqpzpg .gt_from_md > :first-child {
  margin-top: 0;
}

#tbgdnqpzpg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tbgdnqpzpg .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#tbgdnqpzpg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#tbgdnqpzpg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tbgdnqpzpg .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#tbgdnqpzpg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tbgdnqpzpg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tbgdnqpzpg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tbgdnqpzpg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tbgdnqpzpg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#tbgdnqpzpg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tbgdnqpzpg .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#tbgdnqpzpg .gt_left {
  text-align: left;
}

#tbgdnqpzpg .gt_center {
  text-align: center;
}

#tbgdnqpzpg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tbgdnqpzpg .gt_font_normal {
  font-weight: normal;
}

#tbgdnqpzpg .gt_font_bold {
  font-weight: bold;
}

#tbgdnqpzpg .gt_font_italic {
  font-style: italic;
}

#tbgdnqpzpg .gt_super {
  font-size: 65%;
}

#tbgdnqpzpg .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="tbgdnqpzpg" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error1count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error2count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error3count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error4count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error5count</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

{% highlight r %}
 # dies des de últim error
gt(head(compfeat))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#clambuxnlp .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#clambuxnlp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#clambuxnlp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#clambuxnlp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#clambuxnlp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#clambuxnlp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#clambuxnlp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#clambuxnlp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#clambuxnlp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#clambuxnlp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#clambuxnlp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#clambuxnlp .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#clambuxnlp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#clambuxnlp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#clambuxnlp .gt_from_md > :first-child {
  margin-top: 0;
}

#clambuxnlp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#clambuxnlp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#clambuxnlp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#clambuxnlp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#clambuxnlp .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#clambuxnlp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#clambuxnlp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#clambuxnlp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#clambuxnlp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#clambuxnlp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#clambuxnlp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#clambuxnlp .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#clambuxnlp .gt_left {
  text-align: left;
}

#clambuxnlp .gt_center {
  text-align: center;
}

#clambuxnlp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#clambuxnlp .gt_font_normal {
  font-weight: normal;
}

#clambuxnlp .gt_font_bold {
  font-weight: bold;
}

#clambuxnlp .gt_font_italic {
  font-style: italic;
}

#clambuxnlp .gt_super {
  font-size: 65%;
}

#clambuxnlp .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="clambuxnlp" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp4</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">19.95833</td>
      <td class="gt_row gt_right">214.9583</td>
      <td class="gt_row gt_right">154.9583</td>
      <td class="gt_row gt_right">169.9583</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">20.08333</td>
      <td class="gt_row gt_right">215.0833</td>
      <td class="gt_row gt_right">155.0833</td>
      <td class="gt_row gt_right">170.0833</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">20.20833</td>
      <td class="gt_row gt_right">215.2083</td>
      <td class="gt_row gt_right">155.2083</td>
      <td class="gt_row gt_right">170.2083</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">20.33333</td>
      <td class="gt_row gt_right">215.3333</td>
      <td class="gt_row gt_right">155.3333</td>
      <td class="gt_row gt_right">170.3333</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">20.45833</td>
      <td class="gt_row gt_right">215.4583</td>
      <td class="gt_row gt_right">155.4583</td>
      <td class="gt_row gt_right">170.4583</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">20.58333</td>
      <td class="gt_row gt_right">215.5833</td>
      <td class="gt_row gt_right">155.5833</td>
      <td class="gt_row gt_right">170.5833</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

{% highlight r %}
 # model de mÃ quina i tenps de servei
gt(head(maquines))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#grpiqrssxk .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#grpiqrssxk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#grpiqrssxk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#grpiqrssxk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#grpiqrssxk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#grpiqrssxk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#grpiqrssxk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#grpiqrssxk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#grpiqrssxk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#grpiqrssxk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#grpiqrssxk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#grpiqrssxk .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#grpiqrssxk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#grpiqrssxk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#grpiqrssxk .gt_from_md > :first-child {
  margin-top: 0;
}

#grpiqrssxk .gt_from_md > :last-child {
  margin-bottom: 0;
}

#grpiqrssxk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#grpiqrssxk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#grpiqrssxk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#grpiqrssxk .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#grpiqrssxk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#grpiqrssxk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#grpiqrssxk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#grpiqrssxk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#grpiqrssxk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#grpiqrssxk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#grpiqrssxk .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#grpiqrssxk .gt_left {
  text-align: left;
}

#grpiqrssxk .gt_center {
  text-align: center;
}

#grpiqrssxk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#grpiqrssxk .gt_font_normal {
  font-weight: normal;
}

#grpiqrssxk .gt_font_bold {
  font-weight: bold;
}

#grpiqrssxk .gt_font_italic {
  font-style: italic;
}

#grpiqrssxk .gt_super {
  font-size: 65%;
}

#grpiqrssxk .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="grpiqrssxk" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">age</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2</td>
      <td class="gt_row gt_center">model4</td>
      <td class="gt_row gt_right">7</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">3</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">8</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">4</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">7</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">5</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">2</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">6</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">7</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

{% highlight r %}
finalfeat <- 
  telemetriafeat %>% 
    left_join(errorfeat, by = c("datetime","machineID")) %>% 
    left_join(compfeat, by = c("datetime","machineID")) %>% 
    left_join(maquines, by = c("machineID"))
 
 
finalfeat %>% 
  head() %>% 
  gt() %>% 
  fmt_number(columns = 3:18, decimals = 2) %>%
  fmt_number(columns = 24:27, decimals = 2)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#qeadopxuzm .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qeadopxuzm .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qeadopxuzm .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qeadopxuzm .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qeadopxuzm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qeadopxuzm .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qeadopxuzm .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qeadopxuzm .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qeadopxuzm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qeadopxuzm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qeadopxuzm .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qeadopxuzm .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#qeadopxuzm .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qeadopxuzm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qeadopxuzm .gt_from_md > :first-child {
  margin-top: 0;
}

#qeadopxuzm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qeadopxuzm .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qeadopxuzm .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#qeadopxuzm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qeadopxuzm .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#qeadopxuzm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qeadopxuzm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qeadopxuzm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qeadopxuzm .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qeadopxuzm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#qeadopxuzm .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qeadopxuzm .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#qeadopxuzm .gt_left {
  text-align: left;
}

#qeadopxuzm .gt_center {
  text-align: center;
}

#qeadopxuzm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qeadopxuzm .gt_font_normal {
  font-weight: normal;
}

#qeadopxuzm .gt_font_bold {
  font-weight: bold;
}

#qeadopxuzm .gt_font_italic {
  font-style: italic;
}

#qeadopxuzm .gt_super {
  font-size: 65%;
}

#qeadopxuzm .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="qeadopxuzm" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error1count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error2count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error3count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error4count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error5count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp4</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">age</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">180.13</td>
      <td class="gt_row gt_right">440.61</td>
      <td class="gt_row gt_right">94.14</td>
      <td class="gt_row gt_right">41.55</td>
      <td class="gt_row gt_right">21.32</td>
      <td class="gt_row gt_right">48.77</td>
      <td class="gt_row gt_right">2.14</td>
      <td class="gt_row gt_right">10.04</td>
      <td class="gt_row gt_right">169.73</td>
      <td class="gt_row gt_right">445.18</td>
      <td class="gt_row gt_right">96.80</td>
      <td class="gt_row gt_right">40.39</td>
      <td class="gt_row gt_right">11.23</td>
      <td class="gt_row gt_right">48.72</td>
      <td class="gt_row gt_right">10.08</td>
      <td class="gt_row gt_right">5.85</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">19.96</td>
      <td class="gt_row gt_right">214.96</td>
      <td class="gt_row gt_right">154.96</td>
      <td class="gt_row gt_right">169.96</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">176.36</td>
      <td class="gt_row gt_right">439.35</td>
      <td class="gt_row gt_right">101.55</td>
      <td class="gt_row gt_right">36.11</td>
      <td class="gt_row gt_right">18.95</td>
      <td class="gt_row gt_right">51.33</td>
      <td class="gt_row gt_right">13.79</td>
      <td class="gt_row gt_right">6.74</td>
      <td class="gt_row gt_right">170.53</td>
      <td class="gt_row gt_right">443.91</td>
      <td class="gt_row gt_right">97.67</td>
      <td class="gt_row gt_right">39.79</td>
      <td class="gt_row gt_right">12.59</td>
      <td class="gt_row gt_right">46.93</td>
      <td class="gt_row gt_right">9.41</td>
      <td class="gt_row gt_right">6.10</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.08</td>
      <td class="gt_row gt_right">215.08</td>
      <td class="gt_row gt_right">155.08</td>
      <td class="gt_row gt_right">170.08</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">160.38</td>
      <td class="gt_row gt_right">424.39</td>
      <td class="gt_row gt_right">99.60</td>
      <td class="gt_row gt_right">36.09</td>
      <td class="gt_row gt_right">13.05</td>
      <td class="gt_row gt_right">13.70</td>
      <td class="gt_row gt_right">9.99</td>
      <td class="gt_row gt_right">1.64</td>
      <td class="gt_row gt_right">170.05</td>
      <td class="gt_row gt_right">446.46</td>
      <td class="gt_row gt_right">96.91</td>
      <td class="gt_row gt_right">40.02</td>
      <td class="gt_row gt_right">13.28</td>
      <td class="gt_row gt_right">42.84</td>
      <td class="gt_row gt_right">9.07</td>
      <td class="gt_row gt_right">5.48</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.21</td>
      <td class="gt_row gt_right">215.21</td>
      <td class="gt_row gt_right">155.21</td>
      <td class="gt_row gt_right">170.21</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">170.47</td>
      <td class="gt_row gt_right">442.93</td>
      <td class="gt_row gt_right">102.38</td>
      <td class="gt_row gt_right">40.48</td>
      <td class="gt_row gt_right">16.64</td>
      <td class="gt_row gt_right">56.29</td>
      <td class="gt_row gt_right">3.31</td>
      <td class="gt_row gt_right">8.85</td>
      <td class="gt_row gt_right">170.34</td>
      <td class="gt_row gt_right">447.36</td>
      <td class="gt_row gt_right">96.23</td>
      <td class="gt_row gt_right">39.92</td>
      <td class="gt_row gt_right">13.82</td>
      <td class="gt_row gt_right">42.81</td>
      <td class="gt_row gt_right">8.26</td>
      <td class="gt_row gt_right">5.86</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.33</td>
      <td class="gt_row gt_right">215.33</td>
      <td class="gt_row gt_right">155.33</td>
      <td class="gt_row gt_right">170.33</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">163.26</td>
      <td class="gt_row gt_right">468.94</td>
      <td class="gt_row gt_right">102.73</td>
      <td class="gt_row gt_right">40.92</td>
      <td class="gt_row gt_right">17.42</td>
      <td class="gt_row gt_right">38.68</td>
      <td class="gt_row gt_right">9.11</td>
      <td class="gt_row gt_right">3.06</td>
      <td class="gt_row gt_right">170.06</td>
      <td class="gt_row gt_right">452.16</td>
      <td class="gt_row gt_right">96.36</td>
      <td class="gt_row gt_right">39.99</td>
      <td class="gt_row gt_right">14.79</td>
      <td class="gt_row gt_right">42.53</td>
      <td class="gt_row gt_right">8.67</td>
      <td class="gt_row gt_right">5.91</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.46</td>
      <td class="gt_row gt_right">215.46</td>
      <td class="gt_row gt_right">155.46</td>
      <td class="gt_row gt_right">170.46</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">163.28</td>
      <td class="gt_row gt_right">446.49</td>
      <td class="gt_row gt_right">104.39</td>
      <td class="gt_row gt_right">38.07</td>
      <td class="gt_row gt_right">21.58</td>
      <td class="gt_row gt_right">41.38</td>
      <td class="gt_row gt_right">20.73</td>
      <td class="gt_row gt_right">6.93</td>
      <td class="gt_row gt_right">169.37</td>
      <td class="gt_row gt_right">453.34</td>
      <td class="gt_row gt_right">98.04</td>
      <td class="gt_row gt_right">39.53</td>
      <td class="gt_row gt_right">15.67</td>
      <td class="gt_row gt_right">41.69</td>
      <td class="gt_row gt_right">10.61</td>
      <td class="gt_row gt_right">6.21</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.58</td>
      <td class="gt_row gt_right">215.58</td>
      <td class="gt_row gt_right">155.58</td>
      <td class="gt_row gt_right">170.58</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
## LABELING
 
OBJECTIU: calcular les probabilitats que una màquina falli en les pròximes 24h per qualsevol dels components.    
Labeling de totes les variables en una finestra de 24h abans no falla qualsevol dels components.
 
Join del dataframe fallades amb un filtrat de quines estan dins una finestra de 24h
 

{% highlight r %}
labeled <- 
  finalfeat %>% 
  left_join(averies, by = "machineID") %>% 
  mutate(datediff = difftime(datetime.y, datetime.x, units = "hours")) %>%
  filter(datediff <= 24, datediff >= 0) %>% 
  select(datetime.x, machineID, failure)
 
gt(head(labeled))
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uwooabanvn .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uwooabanvn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uwooabanvn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uwooabanvn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uwooabanvn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uwooabanvn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uwooabanvn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uwooabanvn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uwooabanvn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uwooabanvn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uwooabanvn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uwooabanvn .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#uwooabanvn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uwooabanvn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uwooabanvn .gt_from_md > :first-child {
  margin-top: 0;
}

#uwooabanvn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uwooabanvn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uwooabanvn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#uwooabanvn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uwooabanvn .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#uwooabanvn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uwooabanvn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uwooabanvn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uwooabanvn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uwooabanvn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#uwooabanvn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uwooabanvn .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#uwooabanvn .gt_left {
  text-align: left;
}

#uwooabanvn .gt_center {
  text-align: center;
}

#uwooabanvn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uwooabanvn .gt_font_normal {
  font-weight: normal;
}

#uwooabanvn .gt_font_bold {
  font-weight: bold;
}

#uwooabanvn .gt_font_italic {
  font-style: italic;
}

#uwooabanvn .gt_super {
  font-size: 65%;
}

#uwooabanvn .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="uwooabanvn" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime.x</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">failure</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-04 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-04 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-04 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-04 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-04 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp4</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-04 23:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_center">comp4</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
Etiquetar el dataframe finalfeat.  
 

{% highlight r %}
labeledfeatures <- 
  finalfeat %>% 
  left_join(labeled, by = c("datetime" = "datetime.x", "machineID")) %>%
  arrange(machineID,datetime)
 
# afegim el level "none"
levels(labeledfeatures$failure) <- c(levels(labeledfeatures$failure), "none")
 
# substituir NA per la etiqueta "none"
labeledfeatures <- 
  labeledfeatures %>% 
  mutate(failure = replace_na(failure, "none"))
 
labeledfeatures %>% 
  head() %>% 
  gt() %>% 
  fmt_number(columns = 3:18, decimals = 2) %>% 
  fmt_number(columns = 24:27, decimals = 2)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uwmlezpqie .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uwmlezpqie .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uwmlezpqie .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uwmlezpqie .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uwmlezpqie .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uwmlezpqie .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uwmlezpqie .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uwmlezpqie .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uwmlezpqie .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uwmlezpqie .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uwmlezpqie .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uwmlezpqie .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#uwmlezpqie .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uwmlezpqie .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uwmlezpqie .gt_from_md > :first-child {
  margin-top: 0;
}

#uwmlezpqie .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uwmlezpqie .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uwmlezpqie .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#uwmlezpqie .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uwmlezpqie .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#uwmlezpqie .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uwmlezpqie .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uwmlezpqie .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uwmlezpqie .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uwmlezpqie .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#uwmlezpqie .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uwmlezpqie .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#uwmlezpqie .gt_left {
  text-align: left;
}

#uwmlezpqie .gt_center {
  text-align: center;
}

#uwmlezpqie .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uwmlezpqie .gt_font_normal {
  font-weight: normal;
}

#uwmlezpqie .gt_font_bold {
  font-weight: bold;
}

#uwmlezpqie .gt_font_italic {
  font-style: italic;
}

#uwmlezpqie .gt_super {
  font-size: 65%;
}

#uwmlezpqie .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="uwmlezpqie" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error1count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error2count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error3count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error4count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error5count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp4</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">age</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">failure</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">180.13</td>
      <td class="gt_row gt_right">440.61</td>
      <td class="gt_row gt_right">94.14</td>
      <td class="gt_row gt_right">41.55</td>
      <td class="gt_row gt_right">21.32</td>
      <td class="gt_row gt_right">48.77</td>
      <td class="gt_row gt_right">2.14</td>
      <td class="gt_row gt_right">10.04</td>
      <td class="gt_row gt_right">169.73</td>
      <td class="gt_row gt_right">445.18</td>
      <td class="gt_row gt_right">96.80</td>
      <td class="gt_row gt_right">40.39</td>
      <td class="gt_row gt_right">11.23</td>
      <td class="gt_row gt_right">48.72</td>
      <td class="gt_row gt_right">10.08</td>
      <td class="gt_row gt_right">5.85</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">19.96</td>
      <td class="gt_row gt_right">214.96</td>
      <td class="gt_row gt_right">154.96</td>
      <td class="gt_row gt_right">169.96</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">176.36</td>
      <td class="gt_row gt_right">439.35</td>
      <td class="gt_row gt_right">101.55</td>
      <td class="gt_row gt_right">36.11</td>
      <td class="gt_row gt_right">18.95</td>
      <td class="gt_row gt_right">51.33</td>
      <td class="gt_row gt_right">13.79</td>
      <td class="gt_row gt_right">6.74</td>
      <td class="gt_row gt_right">170.53</td>
      <td class="gt_row gt_right">443.91</td>
      <td class="gt_row gt_right">97.67</td>
      <td class="gt_row gt_right">39.79</td>
      <td class="gt_row gt_right">12.59</td>
      <td class="gt_row gt_right">46.93</td>
      <td class="gt_row gt_right">9.41</td>
      <td class="gt_row gt_right">6.10</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.08</td>
      <td class="gt_row gt_right">215.08</td>
      <td class="gt_row gt_right">155.08</td>
      <td class="gt_row gt_right">170.08</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">160.38</td>
      <td class="gt_row gt_right">424.39</td>
      <td class="gt_row gt_right">99.60</td>
      <td class="gt_row gt_right">36.09</td>
      <td class="gt_row gt_right">13.05</td>
      <td class="gt_row gt_right">13.70</td>
      <td class="gt_row gt_right">9.99</td>
      <td class="gt_row gt_right">1.64</td>
      <td class="gt_row gt_right">170.05</td>
      <td class="gt_row gt_right">446.46</td>
      <td class="gt_row gt_right">96.91</td>
      <td class="gt_row gt_right">40.02</td>
      <td class="gt_row gt_right">13.28</td>
      <td class="gt_row gt_right">42.84</td>
      <td class="gt_row gt_right">9.07</td>
      <td class="gt_row gt_right">5.48</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.21</td>
      <td class="gt_row gt_right">215.21</td>
      <td class="gt_row gt_right">155.21</td>
      <td class="gt_row gt_right">170.21</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">170.47</td>
      <td class="gt_row gt_right">442.93</td>
      <td class="gt_row gt_right">102.38</td>
      <td class="gt_row gt_right">40.48</td>
      <td class="gt_row gt_right">16.64</td>
      <td class="gt_row gt_right">56.29</td>
      <td class="gt_row gt_right">3.31</td>
      <td class="gt_row gt_right">8.85</td>
      <td class="gt_row gt_right">170.34</td>
      <td class="gt_row gt_right">447.36</td>
      <td class="gt_row gt_right">96.23</td>
      <td class="gt_row gt_right">39.92</td>
      <td class="gt_row gt_right">13.82</td>
      <td class="gt_row gt_right">42.81</td>
      <td class="gt_row gt_right">8.26</td>
      <td class="gt_row gt_right">5.86</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.33</td>
      <td class="gt_row gt_right">215.33</td>
      <td class="gt_row gt_right">155.33</td>
      <td class="gt_row gt_right">170.33</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">163.26</td>
      <td class="gt_row gt_right">468.94</td>
      <td class="gt_row gt_right">102.73</td>
      <td class="gt_row gt_right">40.92</td>
      <td class="gt_row gt_right">17.42</td>
      <td class="gt_row gt_right">38.68</td>
      <td class="gt_row gt_right">9.11</td>
      <td class="gt_row gt_right">3.06</td>
      <td class="gt_row gt_right">170.06</td>
      <td class="gt_row gt_right">452.16</td>
      <td class="gt_row gt_right">96.36</td>
      <td class="gt_row gt_right">39.99</td>
      <td class="gt_row gt_right">14.79</td>
      <td class="gt_row gt_right">42.53</td>
      <td class="gt_row gt_right">8.67</td>
      <td class="gt_row gt_right">5.91</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.46</td>
      <td class="gt_row gt_right">215.46</td>
      <td class="gt_row gt_right">155.46</td>
      <td class="gt_row gt_right">170.46</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">163.28</td>
      <td class="gt_row gt_right">446.49</td>
      <td class="gt_row gt_right">104.39</td>
      <td class="gt_row gt_right">38.07</td>
      <td class="gt_row gt_right">21.58</td>
      <td class="gt_row gt_right">41.38</td>
      <td class="gt_row gt_right">20.73</td>
      <td class="gt_row gt_right">6.93</td>
      <td class="gt_row gt_right">169.37</td>
      <td class="gt_row gt_right">453.34</td>
      <td class="gt_row gt_right">98.04</td>
      <td class="gt_row gt_right">39.53</td>
      <td class="gt_row gt_right">15.67</td>
      <td class="gt_row gt_right">41.69</td>
      <td class="gt_row gt_right">10.61</td>
      <td class="gt_row gt_right">6.21</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.58</td>
      <td class="gt_row gt_right">215.58</td>
      <td class="gt_row gt_right">155.58</td>
      <td class="gt_row gt_right">170.58</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
## TRAINING, VALIDATION & TESTING
 
- dades provinents de registre temporals
- no utilitzar random splitting
- no utilitzar k-fold cross validation (també és random)
- train amb dades històriques i test amb dades futures
- escollim un punt temporal per fer el tall train/test
 
### Imbalanced Distribution
 
En el cas d'una distribució del target molt descompensada en quant a observacions 
hem de tenir en compte que encara que el paràmetre accuracy sigui molt alt no
garantirà un model adeqüat.
Tenir un baix recall (sensitivity) si el cost de les falses alarmes és molt alt 
suposarà un problema econòmic.  
 

{% highlight r %}
options(scipen = 999)
ggplot(labeledfeatures, aes(x = failure, fill =failure))+
  geom_bar()
{% endhighlight %}

![plot of chunk pre10](/figures/pre10-1.png)

{% highlight r %}
table(labeledfeatures$failure)
{% endhighlight %}



{% highlight text %}
## 
##  comp1  comp2  comp3  comp4   none 
##   1450   2048   1017   1408 285705
{% endhighlight %}
 
### Split train-test.  
 
Train: primers 10 mesos del 2015  
Test: darrers 2 mesos del 2015 

 

{% highlight r %}
trainingdata1 <- labeledfeatures[labeledfeatures$datetime < "2015-09-30 01:00:00",] 
testingdata1 <- labeledfeatures[labeledfeatures$datetime > "2015-10-01 01:00:00",]
 
trainingdata1 %>% head() %>% 
  gt() %>% 
  fmt_number(columns = 3:18, decimals = 2) %>% 
  fmt_number(columns = 24:27, decimals = 2)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#txxzuodajz .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#txxzuodajz .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#txxzuodajz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#txxzuodajz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#txxzuodajz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#txxzuodajz .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#txxzuodajz .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#txxzuodajz .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#txxzuodajz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#txxzuodajz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#txxzuodajz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#txxzuodajz .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#txxzuodajz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#txxzuodajz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#txxzuodajz .gt_from_md > :first-child {
  margin-top: 0;
}

#txxzuodajz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#txxzuodajz .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#txxzuodajz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#txxzuodajz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#txxzuodajz .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#txxzuodajz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#txxzuodajz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#txxzuodajz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#txxzuodajz .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#txxzuodajz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#txxzuodajz .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#txxzuodajz .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#txxzuodajz .gt_left {
  text-align: left;
}

#txxzuodajz .gt_center {
  text-align: center;
}

#txxzuodajz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#txxzuodajz .gt_font_normal {
  font-weight: normal;
}

#txxzuodajz .gt_font_bold {
  font-weight: bold;
}

#txxzuodajz .gt_font_italic {
  font-style: italic;
}

#txxzuodajz .gt_super {
  font-size: 65%;
}

#txxzuodajz .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="txxzuodajz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error1count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error2count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error3count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error4count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error5count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp4</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">age</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">failure</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-01-02 05:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">180.13</td>
      <td class="gt_row gt_right">440.61</td>
      <td class="gt_row gt_right">94.14</td>
      <td class="gt_row gt_right">41.55</td>
      <td class="gt_row gt_right">21.32</td>
      <td class="gt_row gt_right">48.77</td>
      <td class="gt_row gt_right">2.14</td>
      <td class="gt_row gt_right">10.04</td>
      <td class="gt_row gt_right">169.73</td>
      <td class="gt_row gt_right">445.18</td>
      <td class="gt_row gt_right">96.80</td>
      <td class="gt_row gt_right">40.39</td>
      <td class="gt_row gt_right">11.23</td>
      <td class="gt_row gt_right">48.72</td>
      <td class="gt_row gt_right">10.08</td>
      <td class="gt_row gt_right">5.85</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">19.96</td>
      <td class="gt_row gt_right">214.96</td>
      <td class="gt_row gt_right">154.96</td>
      <td class="gt_row gt_right">169.96</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 08:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">176.36</td>
      <td class="gt_row gt_right">439.35</td>
      <td class="gt_row gt_right">101.55</td>
      <td class="gt_row gt_right">36.11</td>
      <td class="gt_row gt_right">18.95</td>
      <td class="gt_row gt_right">51.33</td>
      <td class="gt_row gt_right">13.79</td>
      <td class="gt_row gt_right">6.74</td>
      <td class="gt_row gt_right">170.53</td>
      <td class="gt_row gt_right">443.91</td>
      <td class="gt_row gt_right">97.67</td>
      <td class="gt_row gt_right">39.79</td>
      <td class="gt_row gt_right">12.59</td>
      <td class="gt_row gt_right">46.93</td>
      <td class="gt_row gt_right">9.41</td>
      <td class="gt_row gt_right">6.10</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.08</td>
      <td class="gt_row gt_right">215.08</td>
      <td class="gt_row gt_right">155.08</td>
      <td class="gt_row gt_right">170.08</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 11:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">160.38</td>
      <td class="gt_row gt_right">424.39</td>
      <td class="gt_row gt_right">99.60</td>
      <td class="gt_row gt_right">36.09</td>
      <td class="gt_row gt_right">13.05</td>
      <td class="gt_row gt_right">13.70</td>
      <td class="gt_row gt_right">9.99</td>
      <td class="gt_row gt_right">1.64</td>
      <td class="gt_row gt_right">170.05</td>
      <td class="gt_row gt_right">446.46</td>
      <td class="gt_row gt_right">96.91</td>
      <td class="gt_row gt_right">40.02</td>
      <td class="gt_row gt_right">13.28</td>
      <td class="gt_row gt_right">42.84</td>
      <td class="gt_row gt_right">9.07</td>
      <td class="gt_row gt_right">5.48</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.21</td>
      <td class="gt_row gt_right">215.21</td>
      <td class="gt_row gt_right">155.21</td>
      <td class="gt_row gt_right">170.21</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 14:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">170.47</td>
      <td class="gt_row gt_right">442.93</td>
      <td class="gt_row gt_right">102.38</td>
      <td class="gt_row gt_right">40.48</td>
      <td class="gt_row gt_right">16.64</td>
      <td class="gt_row gt_right">56.29</td>
      <td class="gt_row gt_right">3.31</td>
      <td class="gt_row gt_right">8.85</td>
      <td class="gt_row gt_right">170.34</td>
      <td class="gt_row gt_right">447.36</td>
      <td class="gt_row gt_right">96.23</td>
      <td class="gt_row gt_right">39.92</td>
      <td class="gt_row gt_right">13.82</td>
      <td class="gt_row gt_right">42.81</td>
      <td class="gt_row gt_right">8.26</td>
      <td class="gt_row gt_right">5.86</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.33</td>
      <td class="gt_row gt_right">215.33</td>
      <td class="gt_row gt_right">155.33</td>
      <td class="gt_row gt_right">170.33</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 17:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">163.26</td>
      <td class="gt_row gt_right">468.94</td>
      <td class="gt_row gt_right">102.73</td>
      <td class="gt_row gt_right">40.92</td>
      <td class="gt_row gt_right">17.42</td>
      <td class="gt_row gt_right">38.68</td>
      <td class="gt_row gt_right">9.11</td>
      <td class="gt_row gt_right">3.06</td>
      <td class="gt_row gt_right">170.06</td>
      <td class="gt_row gt_right">452.16</td>
      <td class="gt_row gt_right">96.36</td>
      <td class="gt_row gt_right">39.99</td>
      <td class="gt_row gt_right">14.79</td>
      <td class="gt_row gt_right">42.53</td>
      <td class="gt_row gt_right">8.67</td>
      <td class="gt_row gt_right">5.91</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.46</td>
      <td class="gt_row gt_right">215.46</td>
      <td class="gt_row gt_right">155.46</td>
      <td class="gt_row gt_right">170.46</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-01-02 20:00:00</td>
      <td class="gt_row gt_right">1</td>
      <td class="gt_row gt_right">163.28</td>
      <td class="gt_row gt_right">446.49</td>
      <td class="gt_row gt_right">104.39</td>
      <td class="gt_row gt_right">38.07</td>
      <td class="gt_row gt_right">21.58</td>
      <td class="gt_row gt_right">41.38</td>
      <td class="gt_row gt_right">20.73</td>
      <td class="gt_row gt_right">6.93</td>
      <td class="gt_row gt_right">169.37</td>
      <td class="gt_row gt_right">453.34</td>
      <td class="gt_row gt_right">98.04</td>
      <td class="gt_row gt_right">39.53</td>
      <td class="gt_row gt_right">15.67</td>
      <td class="gt_row gt_right">41.69</td>
      <td class="gt_row gt_right">10.61</td>
      <td class="gt_row gt_right">6.21</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">20.58</td>
      <td class="gt_row gt_right">215.58</td>
      <td class="gt_row gt_right">155.58</td>
      <td class="gt_row gt_right">170.58</td>
      <td class="gt_row gt_center">model3</td>
      <td class="gt_row gt_right">18</td>
      <td class="gt_row gt_center">none</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

{% highlight r %}
trainingdata1 %>% tail() %>% 
  gt() %>% 
  fmt_number(columns = 3:18, decimals = 2) %>% 
  fmt_number(columns = 24:27, decimals = 2)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#lzsbpsijva .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#lzsbpsijva .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lzsbpsijva .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lzsbpsijva .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lzsbpsijva .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lzsbpsijva .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lzsbpsijva .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#lzsbpsijva .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#lzsbpsijva .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lzsbpsijva .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lzsbpsijva .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lzsbpsijva .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#lzsbpsijva .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#lzsbpsijva .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lzsbpsijva .gt_from_md > :first-child {
  margin-top: 0;
}

#lzsbpsijva .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lzsbpsijva .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#lzsbpsijva .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#lzsbpsijva .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lzsbpsijva .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#lzsbpsijva .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lzsbpsijva .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lzsbpsijva .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lzsbpsijva .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lzsbpsijva .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#lzsbpsijva .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lzsbpsijva .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#lzsbpsijva .gt_left {
  text-align: left;
}

#lzsbpsijva .gt_center {
  text-align: center;
}

#lzsbpsijva .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lzsbpsijva .gt_font_normal {
  font-weight: normal;
}

#lzsbpsijva .gt_font_bold {
  font-weight: bold;
}

#lzsbpsijva .gt_font_italic {
  font-style: italic;
}

#lzsbpsijva .gt_super {
  font-size: 65%;
}

#lzsbpsijva .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="lzsbpsijva" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">datetime</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">machineID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatemean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuremean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationmean_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">voltsd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rotatesd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pressuresd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">vibrationsd_24hrs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error1count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error2count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error3count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error4count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">error5count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sincelastcomp4</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">age</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">failure</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">2015-09-29 05:00:00</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_right">170.63</td>
      <td class="gt_row gt_right">465.89</td>
      <td class="gt_row gt_right">97.06</td>
      <td class="gt_row gt_right">39.50</td>
      <td class="gt_row gt_right">13.33</td>
      <td class="gt_row gt_right">32.15</td>
      <td class="gt_row gt_right">15.06</td>
      <td class="gt_row gt_right">10.73</td>
      <td class="gt_row gt_right">171.54</td>
      <td class="gt_row gt_right">448.50</td>
      <td class="gt_row gt_right">100.61</td>
      <td class="gt_row gt_right">38.70</td>
      <td class="gt_row gt_right">14.78</td>
      <td class="gt_row gt_right">39.42</td>
      <td class="gt_row gt_right">9.83</td>
      <td class="gt_row gt_right">5.53</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">3.96</td>
      <td class="gt_row gt_right">63.96</td>
      <td class="gt_row gt_right">48.96</td>
      <td class="gt_row gt_right">33.96</td>
      <td class="gt_row gt_center">model4</td>
      <td class="gt_row gt_right">5</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-09-29 08:00:00</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_right">163.86</td>
      <td class="gt_row gt_right">393.57</td>
      <td class="gt_row gt_right">98.99</td>
      <td class="gt_row gt_right">40.90</td>
      <td class="gt_row gt_right">1.93</td>
      <td class="gt_row gt_right">27.35</td>
      <td class="gt_row gt_right">2.61</td>
      <td class="gt_row gt_right">2.51</td>
      <td class="gt_row gt_right">170.22</td>
      <td class="gt_row gt_right">443.20</td>
      <td class="gt_row gt_right">99.69</td>
      <td class="gt_row gt_right">38.80</td>
      <td class="gt_row gt_right">14.48</td>
      <td class="gt_row gt_right">42.54</td>
      <td class="gt_row gt_right">9.34</td>
      <td class="gt_row gt_right">5.49</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">4.08</td>
      <td class="gt_row gt_right">64.08</td>
      <td class="gt_row gt_right">49.08</td>
      <td class="gt_row gt_right">34.08</td>
      <td class="gt_row gt_center">model4</td>
      <td class="gt_row gt_right">5</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-09-29 11:00:00</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_right">155.08</td>
      <td class="gt_row gt_right">450.19</td>
      <td class="gt_row gt_right">101.84</td>
      <td class="gt_row gt_right">44.11</td>
      <td class="gt_row gt_right">23.90</td>
      <td class="gt_row gt_right">26.39</td>
      <td class="gt_row gt_right">10.18</td>
      <td class="gt_row gt_right">7.43</td>
      <td class="gt_row gt_right">168.65</td>
      <td class="gt_row gt_right">449.18</td>
      <td class="gt_row gt_right">99.25</td>
      <td class="gt_row gt_right">40.02</td>
      <td class="gt_row gt_right">16.71</td>
      <td class="gt_row gt_right">39.97</td>
      <td class="gt_row gt_right">8.18</td>
      <td class="gt_row gt_right">5.79</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">4.21</td>
      <td class="gt_row gt_right">64.21</td>
      <td class="gt_row gt_right">49.21</td>
      <td class="gt_row gt_right">34.21</td>
      <td class="gt_row gt_center">model4</td>
      <td class="gt_row gt_right">5</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-09-29 14:00:00</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_right">185.39</td>
      <td class="gt_row gt_right">435.26</td>
      <td class="gt_row gt_right">92.83</td>
      <td class="gt_row gt_right">43.53</td>
      <td class="gt_row gt_right">18.81</td>
      <td class="gt_row gt_right">78.60</td>
      <td class="gt_row gt_right">10.68</td>
      <td class="gt_row gt_right">0.88</td>
      <td class="gt_row gt_right">170.19</td>
      <td class="gt_row gt_right">445.05</td>
      <td class="gt_row gt_right">98.94</td>
      <td class="gt_row gt_right">40.84</td>
      <td class="gt_row gt_right">18.33</td>
      <td class="gt_row gt_right">45.61</td>
      <td class="gt_row gt_right">8.74</td>
      <td class="gt_row gt_right">5.49</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">4.33</td>
      <td class="gt_row gt_right">64.33</td>
      <td class="gt_row gt_right">49.33</td>
      <td class="gt_row gt_right">34.33</td>
      <td class="gt_row gt_center">model4</td>
      <td class="gt_row gt_right">5</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-09-29 17:00:00</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_right">166.11</td>
      <td class="gt_row gt_right">444.16</td>
      <td class="gt_row gt_right">103.05</td>
      <td class="gt_row gt_right">38.29</td>
      <td class="gt_row gt_right">13.24</td>
      <td class="gt_row gt_right">34.51</td>
      <td class="gt_row gt_right">7.65</td>
      <td class="gt_row gt_right">3.35</td>
      <td class="gt_row gt_right">170.48</td>
      <td class="gt_row gt_right">439.88</td>
      <td class="gt_row gt_right">98.54</td>
      <td class="gt_row gt_right">40.30</td>
      <td class="gt_row gt_right">18.16</td>
      <td class="gt_row gt_right">42.00</td>
      <td class="gt_row gt_right">8.35</td>
      <td class="gt_row gt_right">5.57</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">4.46</td>
      <td class="gt_row gt_right">64.46</td>
      <td class="gt_row gt_right">49.46</td>
      <td class="gt_row gt_right">34.46</td>
      <td class="gt_row gt_center">model4</td>
      <td class="gt_row gt_right">5</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">2015-09-29 20:00:00</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_right">157.02</td>
      <td class="gt_row gt_right">468.56</td>
      <td class="gt_row gt_right">106.29</td>
      <td class="gt_row gt_right">42.70</td>
      <td class="gt_row gt_right">13.35</td>
      <td class="gt_row gt_right">79.29</td>
      <td class="gt_row gt_right">7.68</td>
      <td class="gt_row gt_right">6.91</td>
      <td class="gt_row gt_right">167.33</td>
      <td class="gt_row gt_right">441.64</td>
      <td class="gt_row gt_right">99.16</td>
      <td class="gt_row gt_right">40.46</td>
      <td class="gt_row gt_right">17.76</td>
      <td class="gt_row gt_right">47.56</td>
      <td class="gt_row gt_right">9.00</td>
      <td class="gt_row gt_right">5.68</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_right">4.58</td>
      <td class="gt_row gt_right">64.58</td>
      <td class="gt_row gt_right">49.58</td>
      <td class="gt_row gt_right">34.58</td>
      <td class="gt_row gt_center">model4</td>
      <td class="gt_row gt_right">5</td>
      <td class="gt_row gt_center">none</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 
### Model
 
Model Gradient Boosting Machine
 

{% highlight r %}
set.seed(1234)
gbm_model1 <- gbm(failure ~ voltmean + rotatemean + pressuremean + vibrationmean + 
                            voltsd + rotatesd + pressuresd + vibrationsd + voltmean_24hrs + 
                            rotatemean_24hrs + pressuremean_24hrs + vibrationmean_24hrs + 
                            voltsd_24hrs + rotatesd_24hrs + pressuresd_24hrs + vibrationsd_24hrs + 
                            error1count + error2count + error3count + error4count + error5count + 
                            sincelastcomp1 + sincelastcomp2 + sincelastcomp3 + sincelastcomp4 + 
                            model + age,
                  data = trainingdata1, 
                  distribution = "multinomial", n.trees = 50,
                  interaction.depth = 5, shrinkage = 0.1)
{% endhighlight %}
 
Resultat Model
 

{% highlight r %}
summary(gbm_model1)
{% endhighlight %}

![plot of chunk unnamed-chunk-37](/figures/unnamed-chunk-37-1.png)

{% highlight text %}
##                                     var      rel.inf
## error2count                 error2count 17.569923493
## voltmean_24hrs           voltmean_24hrs 15.868091978
## error5count                 error5count 14.357060428
## vibrationmean_24hrs vibrationmean_24hrs 12.459134379
## pressuremean_24hrs   pressuremean_24hrs 11.093165403
## error3count                 error3count 10.320978552
## rotatemean_24hrs       rotatemean_24hrs  5.312248762
## error1count                 error1count  4.848048246
## error4count                 error4count  4.397328332
## sincelastcomp1           sincelastcomp1  1.602141578
## sincelastcomp4           sincelastcomp4  0.430785208
## model                             model  0.380656487
## sincelastcomp3           sincelastcomp3  0.376817760
## sincelastcomp2           sincelastcomp2  0.210338294
## vibrationmean             vibrationmean  0.188257051
## rotatemean                   rotatemean  0.158779631
## pressuremean               pressuremean  0.140194064
## voltmean                       voltmean  0.068497002
## vibrationsd_24hrs     vibrationsd_24hrs  0.063243685
## pressuresd                   pressuresd  0.036233865
## rotatesd_24hrs           rotatesd_24hrs  0.025779034
## pressuresd_24hrs       pressuresd_24hrs  0.024906740
## voltsd_24hrs               voltsd_24hrs  0.024766552
## voltsd                           voltsd  0.015349676
## rotatesd                       rotatesd  0.013135993
## vibrationsd                 vibrationsd  0.010121759
## age                                 age  0.004016047
{% endhighlight %}



{% highlight r %}
gbm_model1
{% endhighlight %}



{% highlight text %}
## gbm(formula = failure ~ voltmean + rotatemean + pressuremean + 
##     vibrationmean + voltsd + rotatesd + pressuresd + vibrationsd + 
##     voltmean_24hrs + rotatemean_24hrs + pressuremean_24hrs + 
##     vibrationmean_24hrs + voltsd_24hrs + rotatesd_24hrs + pressuresd_24hrs + 
##     vibrationsd_24hrs + error1count + error2count + error3count + 
##     error4count + error5count + sincelastcomp1 + sincelastcomp2 + 
##     sincelastcomp3 + sincelastcomp4 + model + age, distribution = "multinomial", 
##     data = trainingdata1, n.trees = 50, interaction.depth = 5, 
##     shrinkage = 0.1)
## A gradient boosted model with multinomial loss function.
## 50 iterations were performed.
## There were 27 predictors of which 27 had non-zero influence.
{% endhighlight %}
 
### Predicció
 

{% highlight r %}
pred_gbm1 <- as.data.frame(predict(gbm_model1, testingdata1, 
                                   n.trees = 50,type = "response"))
 
pred_gbm1 %>% head() %>% 
  gt() %>% 
  fmt_number(columns = 1:5, decimals = 4)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hfvweixlku .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hfvweixlku .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hfvweixlku .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hfvweixlku .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hfvweixlku .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hfvweixlku .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hfvweixlku .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hfvweixlku .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hfvweixlku .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hfvweixlku .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hfvweixlku .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hfvweixlku .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#hfvweixlku .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hfvweixlku .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hfvweixlku .gt_from_md > :first-child {
  margin-top: 0;
}

#hfvweixlku .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hfvweixlku .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hfvweixlku .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#hfvweixlku .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hfvweixlku .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#hfvweixlku .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hfvweixlku .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hfvweixlku .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hfvweixlku .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hfvweixlku .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#hfvweixlku .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hfvweixlku .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#hfvweixlku .gt_left {
  text-align: left;
}

#hfvweixlku .gt_center {
  text-align: center;
}

#hfvweixlku .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hfvweixlku .gt_font_normal {
  font-weight: normal;
}

#hfvweixlku .gt_font_bold {
  font-weight: bold;
}

#hfvweixlku .gt_font_italic {
  font-style: italic;
}

#hfvweixlku .gt_super {
  font-size: 65%;
}

#hfvweixlku .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="hfvweixlku" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">comp1.50</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">comp2.50</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">comp3.50</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">comp4.50</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">none.50</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
 

{% highlight r %}
pred <- 
pred_gbm1 %>% 
  mutate(fallada = case_when(
                  comp1.50 > 0.5 ~ "comp1",
                  comp2.50 > 0.5 ~ "comp2",
                  comp3.50 > 0.5 ~ "comp3",
                  comp4.50 > 0.5 ~ "comp4",
                  none.50 > 0.5 ~ "none"))
 
pred$fallada <- factor(pred$fallada, levels = c("comp1","comp2","comp3","comp4","none"))
 
 
pred %>% head() %>% 
  gt() %>% 
  fmt_number(columns = 1:5, decimals = 4)
{% endhighlight %}

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#wkiqvijhvn .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#wkiqvijhvn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wkiqvijhvn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wkiqvijhvn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wkiqvijhvn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wkiqvijhvn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wkiqvijhvn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#wkiqvijhvn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#wkiqvijhvn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wkiqvijhvn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wkiqvijhvn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#wkiqvijhvn .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#wkiqvijhvn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#wkiqvijhvn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wkiqvijhvn .gt_from_md > :first-child {
  margin-top: 0;
}

#wkiqvijhvn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wkiqvijhvn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#wkiqvijhvn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#wkiqvijhvn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wkiqvijhvn .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#wkiqvijhvn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wkiqvijhvn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wkiqvijhvn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wkiqvijhvn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wkiqvijhvn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#wkiqvijhvn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wkiqvijhvn .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#wkiqvijhvn .gt_left {
  text-align: left;
}

#wkiqvijhvn .gt_center {
  text-align: center;
}

#wkiqvijhvn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wkiqvijhvn .gt_font_normal {
  font-weight: normal;
}

#wkiqvijhvn .gt_font_bold {
  font-weight: bold;
}

#wkiqvijhvn .gt_font_italic {
  font-style: italic;
}

#wkiqvijhvn .gt_super {
  font-size: 65%;
}

#wkiqvijhvn .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="wkiqvijhvn" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">comp1.50</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">comp2.50</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">comp3.50</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">comp4.50</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">none.50</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">fallada</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
      <td class="gt_row gt_center">none</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.0001</td>
      <td class="gt_row gt_right">0.9998</td>
      <td class="gt_row gt_center">none</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

{% highlight r %}
table(pred$fallada)
{% endhighlight %}



{% highlight text %}
## 
## comp1 comp2 comp3 comp4  none 
##   303   590   216   320 72441
{% endhighlight %}
 
Matriu de confusió.  
 

{% highlight r %}
cm_gbd <- confusionMatrix(testingdata1$failure, pred$fallada)
cm_gbd
{% endhighlight %}



{% highlight text %}
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction comp1 comp2 comp3 comp4  none
##      comp1   280    14     0    10     2
##      comp2     2   560     0     8     0
##      comp3     8     0   212     3     0
##      comp4     6    16     4   294     1
##      none      7     0     0     5 72438
## 
## Overall Statistics
##                                                
##                Accuracy : 0.9988               
##                  95% CI : (0.9986, 0.9991)     
##     No Information Rate : 0.9807               
##     P-Value [Acc > NIR] : < 0.00000000000000022
##                                                
##                   Kappa : 0.9694               
##                                                
##  Mcnemar's Test P-Value : NA                   
## 
## Statistics by Class:
## 
##                      Class: comp1 Class: comp2 Class: comp3 Class: comp4 Class: none
## Sensitivity              0.924092     0.949153     0.981481     0.918750      1.0000
## Specificity              0.999647     0.999864     0.999851     0.999633      0.9916
## Pos Pred Value           0.915033     0.982456     0.950673     0.915888      0.9998
## Neg Pred Value           0.999687     0.999591     0.999946     0.999646      0.9979
## Prevalence               0.004102     0.007987     0.002924     0.004332      0.9807
## Detection Rate           0.003790     0.007581     0.002870     0.003980      0.9806
## Detection Prevalence     0.004142     0.007716     0.003019     0.004345      0.9808
## Balanced Accuracy        0.961869     0.974508     0.990666     0.959191      0.9958
{% endhighlight %}
 

{% highlight r %}
cm_gbd$table %>% 
  as_tibble() %>% 
  ggplot(aes(Prediction, Reference, fill =n)) +
  geom_tile(show.legend = FALSE, color = "white") +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 6)+
  labs(title = "Confusion Matrix" )+
  theme_minimal()+
  theme(panel.grid.major = element_blank())+
  scale_fill_gradient(low="gray", high="orange", na.value="black", name="")
{% endhighlight %}

![plot of chunk pre11](/figures/pre11-1.png)
 
 
Com bé captem les fallades queda indicat pel recall/sensivity.  
 
### Exemple predicció  
 
A les 18:00 passem pel model les dades del dia i com a resultat detectem la predicció de fallada del component 4 a la màquina 1.  
Al torn de nit podem fer el manteniment oportú.  
 

{% highlight r %}
pred$datetime <- testingdata1$datetime
pred$machineID <- testingdata1$machineID
 
pred_tall <- 
  pred %>% 
  filter(datetime > "2015-12-15 06:00:00", datetime < "2015-12-15 18:00:00") %>% 
  filter(!fallada %in% "none") %>% 
  filter(machineID == 1)
 
fail_tall <- 
  averies %>% 
  filter(datetime > "2015-12-15 06:00:00") %>% 
  filter(!failure %in% "none") %>% 
  filter(machineID == 1)
 
lims <- as.POSIXct(strptime(c("2015-12-15 06:00:00","2015-12-15 19:00:00"), format = "%Y-%m-%d %H:%M:%S"))    
 
ggplot()+
  geom_point(data=pred_tall, aes(datetime, fallada), color = "orange", size = 3)+
  geom_vline(aes(xintercept = as.numeric(as_datetime("2015-12-15 18:00:00"))), linetype ="dashed", color = "orange")+
   geom_vline(aes(xintercept = as.numeric(as_datetime("2015-12-15 06:00:00"))), linetype ="dashed", color = "orange")+
  scale_x_datetime(date_breaks  ="1 hour", limits = lims)+
  coord_flip()+
  theme_minimal()
{% endhighlight %}

![plot of chunk pre13](/figures/pre13-1.png)
 
Si comprovem amb les dades reals en 24h realment tindriem una averia.    
 

{% highlight r %}
lims1 <- as.POSIXct(strptime(c("2015-12-15 06:00:00","2015-12-16 07:00:00"), format = "%Y-%m-%d %H:%M:%S"))    
 
ggplot()+
  geom_point(data=pred_tall, aes(datetime, fallada), color = "orange", sixe = 3)+
  geom_point(data=fail_tall, aes(datetime, failure), color = "red", size = 5, shape = 17)+
  geom_vline(aes(xintercept = as.numeric(as_datetime("2015-12-16 06:00:00"))), linetype ="dashed", color = "red" )+
  geom_vline(aes(xintercept = as.numeric(as_datetime("2015-12-15 18:00:00"))), linetype ="dashed", color = "orange" )+
   geom_vline(aes(xintercept = as.numeric(as_datetime("2015-12-15 06:00:00"))), linetype ="dashed", color = "orange" )+
  scale_x_datetime(date_breaks  ="1 hour", limits = lims1)+
  coord_flip()+
  theme_minimal()
{% endhighlight %}

![plot of chunk pre14](/figures/pre14-1.png)
 
 

---
layout: post  
title: Classifiació 
author: Josep Espluga  
published: true
status: publish
draft: false  
tags: classificació 
---

Anàlisi de classficació, base per crear eines de treball de Exploratory Data Analysis (EDA).  
Test del paquet de modelat i anàlisi 'tidymodels' amb les llibreries rsample, recipes, parnsnip i yardstick.  
Predicció d'ingressos superiors/inferiors a $50k/any en funció de les dades del cens.
 
***
 

 
# **Llibreries**
 

{% highlight r %}
library(tidyverse)
library(funModeling)
library(gmodels)
library(readr)
library(mice) # NA
library(ggthemes) #tableau
library(vcd) # assocstats Cramer-V
library(cowplot) # plot_grid
library(Rmisc) # CI
library(scales) # percent
library(skimr)
library(knitr)
library(kableExtra)
library(corrplot)
library(RColorBrewer)
library(minerva)
library(GGally) # plot_grid()
library(plotly)
library(writexl)
library(readxl)
library(caret)
library(tidymodels)
library(pROC)
{% endhighlight %}
 
# **Dades**
 

{% highlight r %}
adult <- read_csv("adult.csv", na="?")
{% endhighlight %}



{% highlight text %}
('C:/Users/jespl/Documents/R/PROJECTES/jespluga.github.io').
{% endhighlight %}



{% highlight r %}
kable(head(adult)) %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> age </th>
   <th style="text-align:left;"> workclass </th>
   <th style="text-align:right;"> fnlwgt </th>
   <th style="text-align:left;"> education </th>
   <th style="text-align:right;"> education.num </th>
   <th style="text-align:left;"> marital.status </th>
   <th style="text-align:left;"> occupation </th>
   <th style="text-align:left;"> relationship </th>
   <th style="text-align:left;"> race </th>
   <th style="text-align:left;"> sex </th>
   <th style="text-align:right;"> capital.gain </th>
   <th style="text-align:right;"> capital.loss </th>
   <th style="text-align:right;"> hours.per.week </th>
   <th style="text-align:left;"> native.country </th>
   <th style="text-align:left;"> income </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 77053 </td>
   <td style="text-align:left;"> HS-grad </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> Widowed </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Not-in-family </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4356 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> United-States </td>
   <td style="text-align:left;"> &lt;=50K </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:left;"> Private </td>
   <td style="text-align:right;"> 132870 </td>
   <td style="text-align:left;"> HS-grad </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> Widowed </td>
   <td style="text-align:left;"> Exec-managerial </td>
   <td style="text-align:left;"> Not-in-family </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4356 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> United-States </td>
   <td style="text-align:left;"> &lt;=50K </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 186061 </td>
   <td style="text-align:left;"> Some-college </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Widowed </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Unmarried </td>
   <td style="text-align:left;"> Black </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4356 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> United-States </td>
   <td style="text-align:left;"> &lt;=50K </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> Private </td>
   <td style="text-align:right;"> 140359 </td>
   <td style="text-align:left;"> 7th-8th </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> Divorced </td>
   <td style="text-align:left;"> Machine-op-inspct </td>
   <td style="text-align:left;"> Unmarried </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3900 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> United-States </td>
   <td style="text-align:left;"> &lt;=50K </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:left;"> Private </td>
   <td style="text-align:right;"> 264663 </td>
   <td style="text-align:left;"> Some-college </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Separated </td>
   <td style="text-align:left;"> Prof-specialty </td>
   <td style="text-align:left;"> Own-child </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3900 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> United-States </td>
   <td style="text-align:left;"> &lt;=50K </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:left;"> Private </td>
   <td style="text-align:right;"> 216864 </td>
   <td style="text-align:left;"> HS-grad </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> Divorced </td>
   <td style="text-align:left;"> Other-service </td>
   <td style="text-align:left;"> Unmarried </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3770 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> United-States </td>
   <td style="text-align:left;"> &lt;=50K </td>
  </tr>
</tbody>
</table>
 
 

{% highlight r %}
df_x <- adult
 
df_x %>% 
  # keep(is.numeric) %>% 
  map_df(~(data.frame(unics= n_distinct(.x),
                      classe= class(.x),
                      NA_q= sum(is.na(.x), na.rm = T),
                      zero_q= sum(.x == 0, na.rm = T)
  )
  ),
  .id= "variable") %>% 
  mutate(NA_p = percent(NA_q / nrow(df_x)),
         zero_p = percent(zero_q / nrow(df_x)),
         NA_q = cell_spec(NA_q, "html", color = ifelse(NA_q > 0, "white", "black"), background = ifelse(NA_q > 0, "red", "")),
         NA_p = cell_spec(NA_p, "html", color = ifelse(NA_p > 0.001, "white", "black"), background = ifelse(NA_p > 0.001, "red", "")),
         zero_q = cell_spec(zero_q, "html", color = ifelse(zero_q > 0.001, "white", "black"), background = ifelse(zero_q > 0.001, "orange", "")),
         zero_p = cell_spec(zero_p, "html", color = ifelse(zero_p > 0.001, "white", "black"), background = ifelse(zero_p > 0.001, "orange", "")),
         classe = cell_spec(classe, "html", color= ifelse(classe == "character", "blue", "")),
         unics = cell_spec(unics, align = "c")) %>% 
  select(1,3,2,4,6,5,7) %>% 
  arrange(classe) %>% 
  kable(format = "html", escape = F) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>% 
  add_header_above(c(" " = 1, " " = 1," " = 1, "NA" = 2, "ZERO" = 2))
{% endhighlight %}

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">NA</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">ZERO</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:left;"> classe </th>
   <th style="text-align:left;"> unics </th>
   <th style="text-align:left;"> NA_q </th>
   <th style="text-align:left;"> NA_p </th>
   <th style="text-align:left;"> zero_q </th>
   <th style="text-align:left;"> zero_p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> age </td>
   <td style="text-align:left;"> <span style="     color:  !important;">numeric</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">73</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fnlwgt </td>
   <td style="text-align:left;"> <span style="     color:  !important;">numeric</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">21648</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> education.num </td>
   <td style="text-align:left;"> <span style="     color:  !important;">numeric</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">16</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.gain </td>
   <td style="text-align:left;"> <span style="     color:  !important;">numeric</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">119</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: orange !important;">29849</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: orange !important;">91.7%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.loss </td>
   <td style="text-align:left;"> <span style="     color:  !important;">numeric</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">92</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: orange !important;">31042</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: orange !important;">95.3%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hours.per.week </td>
   <td style="text-align:left;"> <span style="     color:  !important;">numeric</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">94</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> workclass </td>
   <td style="text-align:left;"> <span style="     color: blue !important;">character</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">9</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: red !important;">1836</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: red !important;">5.64%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> education </td>
   <td style="text-align:left;"> <span style="     color: blue !important;">character</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">16</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> marital.status </td>
   <td style="text-align:left;"> <span style="     color: blue !important;">character</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">7</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> occupation </td>
   <td style="text-align:left;"> <span style="     color: blue !important;">character</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">15</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: red !important;">1843</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: red !important;">5.66%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> relationship </td>
   <td style="text-align:left;"> <span style="     color: blue !important;">character</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">6</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> race </td>
   <td style="text-align:left;"> <span style="     color: blue !important;">character</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">5</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sex </td>
   <td style="text-align:left;"> <span style="     color: blue !important;">character</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">2</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> native.country </td>
   <td style="text-align:left;"> <span style="     color: blue !important;">character</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">42</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: red !important;">583</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: red !important;">1.79%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income </td>
   <td style="text-align:left;"> <span style="     color: blue !important;">character</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">2</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.00%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
</tbody>
</table>
 
NA: workclass, occupation, native.country.  
Dues variables amb elevada freqüència de zeros. Variables caracter possibles factors.  
capital.gain i capital.loss amb elevada freqüècia de zeros.  
fnlwgt: alt valor d'únics.  
native.country: alta cardinalitat.  
education number i education: mateix nombre d'únics.  
Variables numèriques i caràcter.  
 

{% highlight r %}
adult_m <- 
adult %>% 
  mutate_if(is.character, as.factor)
 
adult_n <- 
  adult %>% 
  keep(is.numeric)
 
adult_f <- 
  adult %>% 
  keep(is.factor)
{% endhighlight %}
 
 
# **Anàlisi NA**
 

{% highlight r %}
adult %>% 
  map_df(~sum(is.na(.x))) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "q_na") %>% 
  # gather(key= "variable", value = "q_na") %>% 
  mutate(p_na= percent(q_na/nrow(adult))) %>% 
  arrange(desc(p_na)) %>% 
  filter(q_na > 0) %>% 
  kable(caption = "NA's") %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>NA's</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:right;"> q_na </th>
   <th style="text-align:left;"> p_na </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> occupation </td>
   <td style="text-align:right;"> 1843 </td>
   <td style="text-align:left;"> 5.66% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> workclass </td>
   <td style="text-align:right;"> 1836 </td>
   <td style="text-align:left;"> 5.64% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> native.country </td>
   <td style="text-align:right;"> 583 </td>
   <td style="text-align:left;"> 1.79% </td>
  </tr>
</tbody>
</table>
 
Occupation i workclass amb el mateix nombre d'únics .  
Comprovem els NA de occupation que tenen workclass que son. 
1783 - 1863 = 7 individus.  
Tots 7 son "never-worked"  
 

{% highlight r %}
adult %>% 
  filter( is.na(occupation) & !is.na(workclass)) %>% 
  select(workclass)
{% endhighlight %}



{% highlight text %}
## # A tibble: 7 x 1
##   workclass   
##   <chr>       
## 1 Never-worked
## 2 Never-worked
## 3 Never-worked
## 4 Never-worked
## 5 Never-worked
## 6 Never-worked
## 7 Never-worked
{% endhighlight %}
 
Converisió NA:
NA occupation: Altres.  
NA workclass: Altres.  
NA native.conutry: Altres.  
 

{% highlight r %}
adult_m <- 
  adult_m %>% 
  mutate(occupation = fct_explicit_na(occupation, na_level = "Altres"),
         workclass = fct_explicit_na(workclass, na_level = "Altres"),
         native.country = fct_explicit_na(native.country, na_level = "Altres"))
{% endhighlight %}
 
# **Anàlisi Univariant** 
 
## **Numèriques**
 
Estadística Descriptiva:  
 

{% highlight r %}
options(scipen = 999)
adult %>% 
  keep(is.numeric) %>% 
  map_df(~(data.frame(mean = mean(.x),
                      sd= sd(.x),
                      mediana= median(.x),
                      max= max(.x),
                      min= min(.x),
                      zero_q= sum(.x==0, na.rm = T))),
        .id="variable") %>% 
  mutate(zero_p = percent(zero_q / nrow(df_x)),
         zero_q = cell_spec(zero_q, "html", color = ifelse(zero_q > 0.001, "white", "black"), background = ifelse(zero_q > 0.001, "orange", "")),
         zero_p = cell_spec(zero_p, "html", color = ifelse(zero_p > 0.001, "white", "black"), background = ifelse(zero_p > 0.001, "orange", ""))) %>% 
  # select(1,3,4,6,5,7,2) %>% 
  mutate_if(is.numeric, format, digits = 3, nsmall = 0, big.mark=',', small.interval = 3) %>% 
  kable(format = "html", escape = F, caption = "Estadistica Descriptiva") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
{% endhighlight %}

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Estadistica Descriptiva</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:left;"> mean </th>
   <th style="text-align:left;"> sd </th>
   <th style="text-align:left;"> mediana </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> zero_q </th>
   <th style="text-align:left;"> zero_p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> age </td>
   <td style="text-align:left;"> 38.6 </td>
   <td style="text-align:left;"> 13.64 </td>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 90 </td>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fnlwgt </td>
   <td style="text-align:left;"> 189,778.4 </td>
   <td style="text-align:left;"> 105,549.98 </td>
   <td style="text-align:left;"> 178,356 </td>
   <td style="text-align:left;"> 1,484,705 </td>
   <td style="text-align:left;"> 12,285 </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> education.num </td>
   <td style="text-align:left;"> 10.1 </td>
   <td style="text-align:left;"> 2.57 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.gain </td>
   <td style="text-align:left;"> 1,077.6 </td>
   <td style="text-align:left;"> 7,385.29 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 99,999 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: orange !important;">29849</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: orange !important;">91.7%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.loss </td>
   <td style="text-align:left;"> 87.3 </td>
   <td style="text-align:left;"> 402.96 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 4,356 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: orange !important;">31042</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: orange !important;">95.3%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hours.per.week </td>
   <td style="text-align:left;"> 40.4 </td>
   <td style="text-align:left;"> 12.35 </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> 99 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
</tbody>
</table>
 
Histogràmes:
 

{% highlight r %}
adult_n %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>% 
  # gather() %>% 
  ggplot(aes(valor))+
    geom_histogram(bins = 20, fill = "steelblue3", color = "white")+
    theme_minimal()+
    facet_wrap(~variable, scales = 'free')+
    labs(x = "", y = "")
{% endhighlight %}

<img src="/figures/census1-1.png" title="plot of chunk census1" alt="plot of chunk census1" style="display: block; margin: auto;" />
 
Grafics de densitat:
 

{% highlight r %}
adult_n %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>% 
  # gather() %>%  
  ggplot(aes(x= valor, fill= variable))+
  #geom_histogram(aes( y= ..density..), fill = "steelblue3", color="white")+
  geom_density(fill = "steelblue3", alpha= 0.4)+
   theme_minimal()+
  facet_wrap(~variable, scales = "free")+
  labs(title = "", x="", y="")
{% endhighlight %}

<img src="/figures/census2-1.png" title="plot of chunk census2" alt="plot of chunk census2" style="display: block; margin: auto;" />
 
 
### **Outliers**
 
Anàlisi gràfic: 
 

{% highlight r %}
adult_n %>% 
  gather() %>% 
  ggplot(aes(factor(key), value))+
  geom_boxplot(fill = "steelblue3", alpha = 0.5)+
  theme_minimal()+
  facet_wrap(~key, scales = 'free')+
  labs(x = "", y = "")
{% endhighlight %}

<img src="/figures/census3-1.png" title="plot of chunk census3" alt="plot of chunk census3" style="display: block; margin: auto;" />
 
 
Distribució capital.gain i capital.loss conseqüència elevat n. zeros.  
Capital.gain te un outlier diferenciat.  
Education.num te observacions amb baix nivell d'educació.  
 
Descriptius:  
 

{% highlight r %}
options(scipen = 999)
adult %>% 
  keep(is.numeric) %>% 
  map_df(~(data.frame(q_25= quantile(.x, probs = 0.25),
                      q_75= quantile(.x, probs = 0.75),
                      iqr= IQR(.x),
                      n_out= length(boxplot(.x, plot = FALSE)$out))),
         .id="variable") %>% 
  mutate(p_out = percent(n_out / nrow(adult)),
         lo_wi= q_25 - (iqr * 1.5),
         up_wi= q_75 + (iqr * 1.5),
         lo_wi= ifelse(lo_wi < 0, 0, lo_wi)) %>% 
  mutate(p_out = cell_spec(p_out, "html", color = "red")) %>% 
  # select(1,3,4,6,5,7,2) %>% 
  kable(format = "html", escape = F, caption = "Descriptius Outliers") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
{% endhighlight %}

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Descriptius Outliers</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:right;"> q_25 </th>
   <th style="text-align:right;"> q_75 </th>
   <th style="text-align:right;"> iqr </th>
   <th style="text-align:right;"> n_out </th>
   <th style="text-align:left;"> p_out </th>
   <th style="text-align:right;"> lo_wi </th>
   <th style="text-align:right;"> up_wi </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> age </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 143 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.4%</span> </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 78.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fnlwgt </td>
   <td style="text-align:right;"> 117827 </td>
   <td style="text-align:right;"> 237051 </td>
   <td style="text-align:right;"> 119224 </td>
   <td style="text-align:right;"> 992 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">3.0%</span> </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 415887.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> education.num </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1198 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">3.7%</span> </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 16.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.gain </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2712 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">8.3%</span> </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.loss </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1519 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">4.7%</span> </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hours.per.week </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 9008 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">27.7%</span> </td>
   <td style="text-align:right;"> 32.5 </td>
   <td style="text-align:right;"> 52.5 </td>
  </tr>
</tbody>
</table>
 
 
### **education number**
 
Hi ha 16 únics de education.num i 16 unics de education.  
Convertim la variable a factor.  
 

{% highlight r %}
adult_m <- 
  adult_m %>% 
  mutate(education.num = as.factor(education.num))
 
adult_m %>% 
  group_by(education.num, education) %>% 
  summarise() %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> education.num </th>
   <th style="text-align:left;"> education </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Preschool </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1st-4th </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 5th-6th </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 7th-8th </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 9th </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 10th </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 11th </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 12th </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> HS-grad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> Some-college </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Assoc-voc </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> Assoc-acdm </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Bachelors </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Masters </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Prof-school </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Doctorate </td>
  </tr>
</tbody>
</table>
 

{% highlight r %}
t_edu <- table(adult_m$education.num, adult_m$education)
chisq.test(t_edu)
{% endhighlight %}



{% highlight text %}
## 
## 	Pearson's Chi-squared test
## 
## data:  t_edu
## X-squared = 488420, df = 225, p-value < 0.00000000000000022
{% endhighlight %}



{% highlight r %}
asso <- assocstats(t_edu)
{% endhighlight %}
 
Ho: varibles independents.  
p << 0.05 resufem hipòtesi nula.  
Son dos variables depenents.  
 
Cramer-V = **1**, intensitat de la dependencia 100%.  
 
### **fnlwgt**
 
Comprovem com s'agrupa.  
 

{% highlight r %}
adult %>% 
  group_by(fnlwgt) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  top_n(5)
{% endhighlight %}



{% highlight text %}
## # A tibble: 7 x 2
##   fnlwgt     n
##    <dbl> <int>
## 1 123011    13
## 2 164190    13
## 3 203488    13
## 4 113364    12
## 5 121124    12
## 6 126675    12
## 7 148995    12
{% endhighlight %}
 
Agrupacio màxima de 13 observacions.  
Desconeixem forma d'agrupar-los en sectors mes grans.  
Descartem com a variable d'analisi.  
 
## **Categoriques**
 
Variable Depenent
 

{% highlight r %}
adult_m %>% 
    ggplot(aes(income, fill= income))+
    geom_bar(alpha = 1)+
    theme_light()+
    coord_flip(ylim = c(0, nrow(adult)))+
    geom_text(stat='count', aes(label= paste((..count..), "/", round((..prop..)*100,2), "%")  , group= 1),
              hjust=-0.1, position = position_stack(), color= "black", size=3.5)+
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())+
    scale_fill_manual(values=c("gray70","steelblue3"))+
    geom_hline(yintercept = nrow(adult_m), linetype = "dashed", color = "blue", size = 1)
{% endhighlight %}

<img src="/figures/census4-1.png" title="plot of chunk census4" alt="plot of chunk census4" style="display: block; margin: auto;" />
 
 

{% highlight r %}
Bad_rate <- mean(adult$income == ">50K")
{% endhighlight %}
 
Percentatge >50K: **0.2408096**  
 
Variables independents
 

{% highlight r %}
adult_m %>% 
    keep(is.factor) %>% 
    select(-income) %>% 
    gather() %>% 
    ggplot(aes(value))+
    geom_bar(alpha = 1, fill= "steelblue3")+
    theme_light()+
    coord_flip(ylim = c(0, nrow(adult)))+
    facet_wrap(~key, scales = 'free')+
    geom_text(stat='count', aes(label= paste((..count..), "/", round((..prop..)*100,2), "%")  , group= 1),
              hjust=-0.1, position = position_stack(), color= "black", size=3.5)+
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())+
    geom_hline(yintercept = nrow(adult_m), linetype = "dashed", color = "blue", size = 1)
{% endhighlight %}

<img src="/figures/census5-1.png" title="plot of chunk census5" alt="plot of chunk census5" style="display: block; margin: auto;" />
 
Possibles agrupacions a estudiar:  
- education: unir de preescolar a 12th  
- mariral: married + separated + no married + widow  
- country: USA + Eur + SthA + SthOM + China + exBrit  
- work: Na-others+never+nopay, self + gov_local `gov_nac  
- race: other  
- occupation: administracio + services + manual   
 
# **Anàlisi Bivariant**
 
## **Numèriques**
 
Relació de les variables predictores amb la variable depenent.  
 

{% highlight r %}
adult_n %>% 
    mutate(income = adult$income) %>% 
    gather(-income, key = "variable", value = "valor") %>%  
    ggplot( aes(valor, fill= income))+
      geom_density(alpha = 0.5)+
      theme_minimal()+
      facet_wrap(~variable, scales = 'free')+
      theme(legend.position="none")+
      scale_fill_manual(values=c("gray70","steelblue3"))
{% endhighlight %}

<img src="/figures/census6-1.png" title="plot of chunk census6" alt="plot of chunk census6" style="display: block; margin: auto;" />
 
Age, education.num, hours per week: diferencies en funció de la sortida.
 

{% highlight r %}
adult_n %>% 
    mutate(income = adult$income) %>% 
    gather(-income, key = "variable", value = "valor") %>% 
    ggplot(aes(factor(variable), valor, fill= income))+
      geom_boxplot()+
      theme_grey()+
      facet_wrap(~variable, scales = 'free')+
      scale_fill_manual(values=c("gray70","steelblue3"))
{% endhighlight %}

<img src="/figures/census7-1.png" title="plot of chunk census7" alt="plot of chunk census7" style="display: block; margin: auto;" />
 
### **Age**  
 
Separem per edats de mes i menys de 50k/any i calculem mitjanes:  
 

{% highlight r %}
income_h <- adult$age[adult$income == "<=50K"]
income_l <- adult$age[adult$income == ">50K"]
 
m1 <- mean(income_h, na.rm = T)
m2 <- mean(income_l, na.rm = T)
{% endhighlight %}
 
Mitjana edat <=50k: **36.7837379**  
Mitjana edat  >50k: **44.2498406**  
 
* Test de normalitat (alternativa al saphiro.test si n>5000):
 
Menys de 50k:  
 

{% highlight r %}
ks.test(income_l, y = 'pnorm', alternative = 'two.sided' )
{% endhighlight %}



{% highlight text %}
## 
## 	One-sample Kolmogorov-Smirnov test
## 
## data:  income_l
## D = 1, p-value < 0.00000000000000022
## alternative hypothesis: two-sided
{% endhighlight %}
 
Mes de 50k:  
 

{% highlight r %}
ks.test(income_h, y = 'pnorm', alternative = 'two.sided' )
{% endhighlight %}



{% highlight text %}
## 
## 	One-sample Kolmogorov-Smirnov test
## 
## data:  income_h
## D = 1, p-value < 0.00000000000000022
## alternative hypothesis: two-sided
{% endhighlight %}
 
Ho: distribucio normal.  
p << 0.05 refusem la hipòtesi nula: distribució no normal.  
 
* Mitjanes estadisticament significativament diferents:
 
Aplicarem el t.test amb la opció per distribucions NO normals wilcox.test.  
 

{% highlight r %}
wilcox.test(income_h, income_l, alternative = 'two.sided')
{% endhighlight %}



{% highlight text %}
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  income_h and income_l
## W = 61203000, p-value < 0.00000000000000022
## alternative hypothesis: true location shift is not equal to 0
{% endhighlight %}

Ho: les mitjanes son iguals.  
p << 0.5, podem refusar hipòtesi nula --> mitjanes DIFERENTS.  
 
* Intervals de confianca.  
 

{% highlight r %}
ci <- summarySE(data= adult, measurevar = 'age', groupvars = 'income', na.rm = T)
ci
{% endhighlight %}



{% highlight text %}
##   income     N      age       sd         se        ci
## 1  <=50K 24720 36.78374 14.02009 0.08917159 0.1747817
## 2   >50K  7841 44.24984 10.51903 0.11879273 0.2328654
{% endhighlight %}
 

{% highlight r %}
ggplot(ci, aes(x=income, y=age, color = income))+
  geom_point(size= 3)+
  geom_errorbar(aes(ymin= age-ci, ymax= age+ci), width=0.2, size= 1.5)+
  theme_minimal()+
  scale_color_manual(values=c("gray70","steelblue3"))
{% endhighlight %}

<img src="/figures/census8-1.png" title="plot of chunk census8" alt="plot of chunk census8" style="display: block; margin: auto;" />
 
### **Education.num**
 

{% highlight r %}
edu_h <- adult$education.num[adult$income == "<=50K"]
edu_l <- adult$education.num[adult$income == ">50K"]
 
m3 <- mean(edu_h, na.rm = T)
m4 <- mean(edu_l, na.rm = T)
{% endhighlight %}
 
Mitjana education <=50k: **9.5950647**  
Mitjana education  >50k: **11.6116567**  
 

{% highlight r %}
ci_edu <- summarySE(data= adult, measurevar = 'education.num', groupvars = 'income', na.rm = T)
ci_edu
{% endhighlight %}



{% highlight text %}
##   income     N education.num       sd         se         ci
## 1  <=50K 24720      9.595065 2.436147 0.01549456 0.03037026
## 2   >50K  7841     11.611657 2.385129 0.02693556 0.05280089
{% endhighlight %}
 

{% highlight r %}
ggplot(ci_edu, aes(x=income, y=education.num, color = income))+
  geom_point(size= 3)+
  geom_errorbar(aes(ymin= education.num - ci, ymax= education.num + ci), width=0.2, size= 1.5)+
  theme_minimal()+
  scale_color_manual(values=c("gray70","steelblue3"))
{% endhighlight %}

<img src="/figures/census9-1.png" title="plot of chunk census9" alt="plot of chunk census9" style="display: block; margin: auto;" />
 

{% highlight r %}
wilcox.test(edu_h, edu_l, alternative = 'two.sided')
{% endhighlight %}



{% highlight text %}
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  edu_h and edu_l
## W = 54854000, p-value < 0.00000000000000022
## alternative hypothesis: true location shift is not equal to 0
{% endhighlight %}
 
Ho: les mitjanes son iguals.  
p << 0.5, podem refusar hipotesi nula --> mitjanes DIFERENTS. 
 
### **Hours.per.week**
 
 

{% highlight r %}
hw_h <- adult$hours.per.week[adult$income == "<=50K"]
hw_l <- adult$hours.per.week[adult$income == ">50K"]
 
m5 <- mean(hw_h, na.rm = T)
m6 <- mean(hw_l, na.rm = T)
{% endhighlight %}
 
Mitjana hours.week <=50k: **38.8402104**  
Mitjana hours.week  >50k: **45.4730264**  
 

{% highlight r %}
ci_hw <- summarySE(data= adult, measurevar = 'hours.per.week', groupvars = 'income', na.rm = T)
ci_hw
{% endhighlight %}



{% highlight text %}
##   income     N hours.per.week       sd         se        ci
## 1  <=50K 24720       38.84021 12.31899 0.07835217 0.1535750
## 2   >50K  7841       45.47303 11.01297 0.12437090 0.2438001
{% endhighlight %}
 

{% highlight r %}
ggplot(ci_hw, aes(x=income, y=hours.per.week, color = income))+
  geom_point(size= 3)+
  geom_errorbar(aes(ymin= hours.per.week - ci, ymax= hours.per.week + ci), width=0.2, size= 1.5)+
  theme_minimal()+
  scale_color_manual(values=c("gray70","steelblue3"))
{% endhighlight %}

<img src="/figures/census10-1.png" title="plot of chunk census10" alt="plot of chunk census10" style="display: block; margin: auto;" />
 

{% highlight r %}
wilcox.test(hw_h, hw_l, alternative = 'two.sided')
{% endhighlight %}



{% highlight text %}
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  hw_h and hw_l
## W = 63569000, p-value < 0.00000000000000022
## alternative hypothesis: true location shift is not equal to 0
{% endhighlight %}
 
Ho: les mitjanes son iguals.  
p << 0.5, podem refusar hipotesi nula --> mitjanes DIFERENTS. 
 
### **capital.gain / loss**
 
Presencia d'un 91,7% i 95.33% de zeros.  
Filtrem tots els valors = zero.
 

{% highlight r %}
cap_gain <- 
  adult %>% 
  filter(capital.gain > 0) %>% 
  select(capital.gain, income)
 
cap_loss <- 
  adult %>% 
  filter(capital.loss > 0) %>% 
  select(capital.loss, income)
{% endhighlight %}
 

{% highlight r %}
p_gain <- 
  cap_gain %>% 
            ggplot(aes(y=capital.gain, fill= income))+
              geom_boxplot()+
              theme_minimal()+
              coord_flip()+
              scale_fill_manual(values=c("gray70","steelblue3"))
p_loss <- 
  cap_loss %>% 
            ggplot(aes(y=capital.loss, fill= income))+
              geom_boxplot()+
              theme_minimal()+
              coord_flip()+
              scale_fill_manual(values=c("gray70","steelblue3"))
 
plot_grid(p_gain, p_loss, nrow = 2)
{% endhighlight %}

<img src="/figures/census11-1.png" title="plot of chunk census11" alt="plot of chunk census11" style="display: block; margin: auto;" />
 
 
Pels valors que no son zero, gràficament podriem parlar d'una diferencia en income pels de mes capital.gain.  
Pel que fa capital.loss no es diferencien.  
 
Calculem les mitjanes per grup per tenir una aproximació de possibles discretitzacions
 
* Capital.gain:  

{% highlight r %}
ci_capgain <- summarySE(data= cap_gain, measurevar = 'capital.gain', groupvars = 'income', na.rm = T)
ci_capgain
{% endhighlight %}



{% highlight text %}
##   income    N capital.gain        sd        se        ci
## 1  <=50K 1035     3552.813  3173.419  98.64096  193.5593
## 2   >50K 1677    18731.165 26778.676 653.91691 1282.5798
{% endhighlight %}
 
* Capital.loss:  

{% highlight r %}
ci_caploss <- summarySE(data= cap_loss, measurevar = 'capital.loss', groupvars = 'income', na.rm = T)
ci_caploss
{% endhighlight %}



{% highlight text %}
##   income   N capital.loss       sd        se       ci
## 1  <=50K 746     1760.983 438.9062 16.069496 31.54689
## 2   >50K 773     1978.017 264.1436  9.500586 18.65005
{% endhighlight %}
 
Capital.gain presenta outliers.  
 

{% highlight r %}
adult_n %>% 
  skim() %>%  
  select(-value) %>% 
  group_by(variable) %>% 
  spread(stat,formatted) %>% 
  select(variable, type, n, complete, missing, mean, sd, 
         Min = p0,
         Max = p100,
         p25,
         median= p50,
         p75,
         -level, -hist) %>% 
  filter(variable %in% c("capital.gain", "capital.loss")) %>% 
  kable() %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)
{% endhighlight %}

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:left;"> type </th>
   <th style="text-align:left;"> n </th>
   <th style="text-align:left;"> complete </th>
   <th style="text-align:left;"> missing </th>
   <th style="text-align:left;"> mean </th>
   <th style="text-align:left;"> sd </th>
   <th style="text-align:left;"> Min </th>
   <th style="text-align:left;"> Max </th>
   <th style="text-align:left;"> p25 </th>
   <th style="text-align:left;"> median </th>
   <th style="text-align:left;"> p75 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> capital.gain </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 32561 </td>
   <td style="text-align:left;"> 32561 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1077.65 </td>
   <td style="text-align:left;"> 7385.29 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 99999 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.loss </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 32561 </td>
   <td style="text-align:left;"> 32561 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 87.3 </td>
   <td style="text-align:left;"> 402.96 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 4356 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
</tbody>
</table>
 

{% highlight r %}
options(scipen = 999)
adult %>% 
  select(capital.gain, capital.loss) %>% 
  map_df(~(data.frame(q_25= quantile(.x, probs = 0.25),
                      q_75= quantile(.x, probs = 0.75),
                      iqr= IQR(.x),
                      n_out= length(boxplot(.x, plot = FALSE)$out))),
         .id="variable") %>% 
  mutate(p_out = percent(n_out / nrow(adult)),
         lo_wi= q_25 - (iqr * 1.5),
         up_wi= q_75 + (iqr * 1.5),
         lo_wi= ifelse(lo_wi < 0, 0, lo_wi)) %>% 
  mutate(p_out = cell_spec(p_out, "html", color = "red")) %>% 
  # select(1,3,4,6,5,7,2) %>% 
  kable(format = "html", escape = F, caption = "Descriptius Outliers") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
{% endhighlight %}

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Descriptius Outliers</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:right;"> q_25 </th>
   <th style="text-align:right;"> q_75 </th>
   <th style="text-align:right;"> iqr </th>
   <th style="text-align:right;"> n_out </th>
   <th style="text-align:left;"> p_out </th>
   <th style="text-align:right;"> lo_wi </th>
   <th style="text-align:right;"> up_wi </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> capital.gain </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2712 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">8.33%</span> </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.loss </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1519 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">4.67%</span> </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>
 

{% highlight r %}
adult %>% 
  group_by(capital.gain) %>% 
  dplyr::summarise(total = n()) %>% 
  arrange(desc(capital.gain)) %>% 
  head()
{% endhighlight %}



{% highlight text %}
## # A tibble: 6 x 2
##   capital.gain total
##          <dbl> <int>
## 1        99999   159
## 2        41310     2
## 3        34095     5
## 4        27828    34
## 5        25236    11
## 6        25124     4
{% endhighlight %}
 
159 observacions amb el valor 99999.  
L'anterior valor es 41310.     
Semblan errors. 
Tambe podria ser que no hi havia mes digits i eren valors mes alts.  
Pero no s'entendria el gap entre 43310 i mes de 99999.  
 

{% highlight r %}
adult %>% 
  group_by(capital.gain) %>% 
  dplyr::summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  head()
{% endhighlight %}



{% highlight text %}
## # A tibble: 6 x 2
##   capital.gain total
##          <dbl> <int>
## 1            0 29849
## 2        15024   347
## 3         7688   284
## 4         7298   246
## 5        99999   159
## 6         3103    97
{% endhighlight %}
 
Freqü+encies de repeticio del valor 9999.   
 
### **Correlacions lineals**
 

{% highlight r %}
adult_n <- 
  adult %>% 
  keep(is.numeric)
 
kor <- cor(adult_n, use = "pairwise.complete.obs")
 
corrplot(kor, method = "color", order = "hclust", addCoef.col = "black", tl.col = "black",
         type="upper", diag = FALSE,
         outline= T, addgrid.col= "darkgray")
{% endhighlight %}

![plot of chunk unnamed-chunk-32](/figures/unnamed-chunk-32-1.png)
 
No hi ha correlació lineal entre variables independents.  
 
### **Correlacions no lineal**
 
MINERVA: MIC Relacio funcional (lineal + no lineal)
 

{% highlight r %}
df_mic <- mine(kor)
 
kable(round(df_mic$MIC, 2), caption = "MIC") %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>MIC</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> age </th>
   <th style="text-align:right;"> fnlwgt </th>
   <th style="text-align:right;"> education.num </th>
   <th style="text-align:right;"> capital.gain </th>
   <th style="text-align:right;"> capital.loss </th>
   <th style="text-align:right;"> hours.per.week </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> age </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.46 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fnlwgt </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.46 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> education.num </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.gain </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.loss </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hours.per.week </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
</tbody>
</table>
 
MINERVA: MICR2 Relació no lineal
 

{% highlight r %}
kable(round(df_mic$MICR2, 2),caption = "MICR2" ) %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>MICR2</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> age </th>
   <th style="text-align:right;"> fnlwgt </th>
   <th style="text-align:right;"> education.num </th>
   <th style="text-align:right;"> capital.gain </th>
   <th style="text-align:right;"> capital.loss </th>
   <th style="text-align:right;"> hours.per.week </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> age </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 0.45 </td>
   <td style="text-align:right;"> 0.44 </td>
   <td style="text-align:right;"> 0.44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fnlwgt </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.34 </td>
   <td style="text-align:right;"> 0.41 </td>
   <td style="text-align:right;"> 0.41 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> education.num </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 0.34 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.gain </td>
   <td style="text-align:right;"> 0.45 </td>
   <td style="text-align:right;"> 0.41 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> 0.98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> capital.loss </td>
   <td style="text-align:right;"> 0.44 </td>
   <td style="text-align:right;"> 0.41 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hours.per.week </td>
   <td style="text-align:right;"> 0.44 </td>
   <td style="text-align:right;"> 0.37 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.98 </td>
   <td style="text-align:right;"> 0.16 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
</tbody>
</table>
 

{% highlight r %}
corrplot(df_mic$MIC, method = "color", order = "hclust", addCoef.col = "black", tl.col = "black",
            type="upper", diag = FALSE,
            outline= T, addgrid.col= "darkgray")
{% endhighlight %}

<img src="/figures/census12-1.png" title="plot of chunk census12" alt="plot of chunk census12" style="display: block; margin: auto;" />
 
Valor molt elevat entre:  
- capital.gain i hours.per.week  
- education.num i hours.per.week
- education.num i capital.gain
 
Representem gràficament filtrant els valors mes grans de zero i mes petits de 99999 de la variable capital.gain
 

{% highlight r %}
cap_gain <- 
adult %>% 
  filter(capital.gain > 0 & capital.gain < 99999) %>% 
  ggplot(aes(hours.per.week, capital.gain, color= income, 
                        text= paste(
                                    # "relationship:", relationship, "\n",
                                    "marital:", marital.status, "\n",
                                    "workclass:", workclass, "\n",
                                    "occupation:", occupation, "\n",
                                    "education:", education, "\n",
                                    "education.num:", education.num, "\n",
                                    "race:", race, "\n",
                                    "sex:", sex, "\n",
                                    "Age:", age, "\n",
                                    "country:", native.country, "\n"
                                    )))+
      geom_point(size= 3)+
      coord_trans(x="log10", y="log10")
 
ggplotly(cap_gain) %>% layout(yaxis = list(type="log", autorange=TRUE), xaxis = list(type="log", autorange=TRUE))
{% endhighlight %}



{% highlight text %}
## Error in file(con, "rb"): cannot open the connection
{% endhighlight %}
 

{% highlight r %}
educa <- 
adult %>% 
  filter(capital.gain > 0 & capital.gain < 99999) %>% 
  ggplot(aes(capital.gain, education.num,  color= income, 
                        text= paste(
                                    "relationship:", relationship, "\n",
                                    "marital:", marital.status, "\n",
                                    "workclass:", workclass, "\n",
                                    "occupation:", occupation, "\n",
                                    "education:", education, "\n",
                                    "education.num:", education.num, "\n",
                                    "race:", race, "\n",
                                    "sex:", sex, "\n"
                                    )))+
      geom_point(size= 3)+
      coord_trans(x="log10", y="log10")
 
ggplotly(educa) %>% layout(yaxis = list(type="log", autorange=TRUE), xaxis = list(type="log", autorange=TRUE))
{% endhighlight %}



{% highlight text %}
## Error in file(con, "rb"): cannot open the connection
{% endhighlight %}
 
## **Categòriques**
 

{% highlight r %}
adult_m %>% 
  keep(is.factor) %>% 
  names()
{% endhighlight %}



{% highlight text %}
##  [1] "workclass"      "education"      "education.num"  "marital.status"
##  [5] "occupation"     "relationship"   "race"           "sex"           
##  [9] "native.country" "income"
{% endhighlight %}
 
 

{% highlight r %}
tema1 <- 
  theme_minimal()+
  theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5))
 
tema2 <- 
  theme_light()+
  theme(
    legend.position = "none",
    panel.grid.major.x= element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 10, angle =45, hjust = 1))
 
tema3 <- 
  theme_gray()+
  theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 0 ),
        axis.text.y = element_text(size = 10, angle = 45),
        plot.title = element_text(size = 16, hjust = 0.5))
{% endhighlight %}
 

{% highlight r %}
binom_stats <- function(x,... ) {
  x <- x$income[!is.na(x$income)]
  res <- prop.test(x = sum(x == ">50K"), n = length(x), ...)
  data.frame(Proportion  = unname(res$estimate), 
             Lower = res$conf.int[1],
             Upper = res$conf.int[2])
}
{% endhighlight %}
 
### **Sex**
 

{% highlight r %}
pr_sex1 <- 
  adult_m %>% 
  ggplot(aes(x= sex, y= (..count..), fill= income))+
      geom_bar()+
        geom_text(aes(y= (..count..), label=   ifelse(((..count..)/sum(..count..)) >0.01 , (percent((..count..)/sum(..count..))) , "") ), 
                  stat="count", position = 'stack', vjust=-0.5, color= "black", size=3.5)+
      scale_fill_manual("income", values=c("gray70","steelblue3"))+ 
      labs(x="", y="", title = "Sex - Income")+
      tema1
 
pr_sex2 <- 
adult_m %>% 
  group_by(sex) %>%
  do(binom_stats(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>% 
  mutate(sex = reorder(factor(sex), Proportion)) %>% 
  ggplot(aes(x= sex, y=Proportion))+
    geom_point(size=3, color="steelblue3")+  
    geom_segment(aes(x= sex, xend=sex, y=0, yend=Proportion), color="gray70")+
    geom_hline(yintercept = Bad_rate, linetype= "dashed", color= "steelblue3")+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(0, Bad_rate, label= paste(round(100*Bad_rate,2),"%") ), vjust= -1, hjust= -0.5, color="steelblue3")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper, width=0.2), size= 1.5, color="steelblue3")+
    labs(x="", y="Proporci? >50K", title = "")+
    tema2
 
tt <- table(adult_m$sex,adult$income)
xisq <- chisq.test(tt)
# percentatge de contribuci? amb signe
prop_xi <-100*xisq$residuals^2 / xisq$statistic * sign(xisq$residuals)
 
pr_sex3 <-
prop_xi %>%
  as.data.frame() %>%
  ggplot(aes(x = Var2, y = Var1))+
    geom_tile(aes(fill = Freq), color = "grey70")+
    scale_fill_gradient2(midpoint = 0, low = "firebrick4", mid = "white", high = "blue4", space = "Lab")+
    geom_text(aes(label = ifelse(round(Freq, 0) != 0, paste(round(Freq, 0), '%'), "")), color = "cyan3")+
    labs(x = "", y = "Chiq.test % Contribuci?")+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    coord_fixed(ratio= 0.9)+
    tema3
    
 
plot_grid(pr_sex1, pr_sex2, pr_sex3, nrow= 1, ncol= 3, rel_widths = c(2,2,0.8), axis= "b", align = "hv")
{% endhighlight %}

<img src="/figures/census15-1.png" title="plot of chunk census15" alt="plot of chunk census15" style="display: block; margin: auto;" />
 
 
### **Race**
 

{% highlight r %}
pr_race1 <- 
  adult_m %>% 
  ggplot(aes(x= race, y= (..count..), fill= income))+
      geom_bar()+
        geom_text(aes(y= (..count..), label=   ifelse(((..count..)/sum(..count..)) >0.01 , (percent((..count..)/sum(..count..))) , "")), 
                           stat="count", position = 'stack', vjust=-0.5, color= "black", size=3.5)+
      scale_fill_manual("income", values=c("gray70","steelblue3"))+ 
      labs(x="", y="", title = "Race - Income")+
      tema1
 
 
pr_race2 <- 
adult_m %>%
  group_by(race) %>%
  do(binom_stats(.)) %>%
  ungroup() %>% 
  mutate(race = reorder(factor(race), Proportion)) %>% 
  ggplot(aes(x= race, y=Proportion))+
    geom_point(size=3, color="steelblue3")+  
    geom_segment(aes(x= race, xend= race, y=0, yend=Proportion), color="gray70")+
    geom_hline(yintercept = Bad_rate, linetype= "dashed", color= "steelblue3")+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(0, Bad_rate, label= paste(round(100*Bad_rate,2),"%") ), vjust= -1, hjust= -0.5, color="steelblue3")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper, width=0.2), size= 1.5, color="steelblue3")+
    labs(x="", y="Proporci? >50K", title = "")+
    tema2
 
tt <- table(adult_m$race,adult$income)
xisq <- chisq.test(tt)
prop_xi <-100*xisq$residuals^2 / xisq$statistic * sign(xisq$residuals)
 
pr_race3 <-
prop_xi %>%
  as.data.frame() %>%
  ggplot(aes(x = Var2, y = Var1))+
    geom_tile(aes(fill = Freq), color = "grey70")+
    scale_fill_gradient2(midpoint = 0, low = "firebrick4", mid = "white", high = "blue4", space = "Lab")+
    geom_text(aes(label = ifelse(round(Freq, 0) != 0, paste(round(Freq, 0), '%'), "")), color = "cyan3")+
    labs(x = "", y = "Chiq.test % Contribuci?")+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    coord_fixed(ratio= 0.9)+
    tema3
 
 
plot_grid(pr_race1, pr_race2, pr_race3, nrow= 1, ncol= 3, rel_widths = c(2,2,0.8), axis= "b", align = "hv")
{% endhighlight %}

<img src="/figures/census16-1.png" title="plot of chunk census16" alt="plot of chunk census16" style="display: block; margin: auto;" />
 
 
### **Relationship**
 

{% highlight r %}
pr_rel1 <- 
  adult_m %>% 
  ggplot(aes(x= relationship, y= (..count..), fill= income))+
      geom_bar()+
        geom_text(aes(y= (..count..), label=   ifelse(((..count..)/sum(..count..)) >0.00 , (percent((..count..)/sum(..count..))) , "")     ), 
                           stat="count", position = 'stack', vjust=-0.5, color= "black", size=3.5)+
      scale_fill_manual("income", values=c("gray70","steelblue3"))+ 
      labs(x="", y="", title = "Relationship - Income")+
      tema1
 
 
pr_rel2 <- 
adult_m %>%
  group_by(relationship) %>%
  do(binom_stats(.)) %>%
  ungroup() %>% 
  mutate(relationship = reorder(factor(relationship), Proportion)) %>% 
  ggplot(aes(x= relationship, y=Proportion))+
    geom_point(size=3, color="steelblue3")+  
    geom_segment(aes(x= relationship, xend= relationship, y=0, yend=Proportion), color="grey70")+
    geom_hline(yintercept = Bad_rate, linetype= "dashed", color= "steelblue3")+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(0, Bad_rate, label= paste(round(100*Bad_rate,2),"%") ), vjust= -1, hjust= -0.5, color="steelblue3")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper, width=0.2), size= 1.5, color="steelblue3")+
    labs(x="", y="Proporci? >50K", title = "")+
    tema2
 
tt <- table(adult_m$relationship,adult$income)
xisq <- chisq.test(tt)
prop_xi <-100*xisq$residuals^2 / xisq$statistic * sign(xisq$residuals)
 
pr_rel3 <-
prop_xi %>%
  as.data.frame() %>%
  ggplot(aes(x = Var2, y = Var1))+
    geom_tile(aes(fill = Freq), color = "grey70")+
    scale_fill_gradient2(midpoint = 0, low = "firebrick4", mid = "white", high = "blue4", space = "Lab")+
      geom_text(aes(label = ifelse(round(Freq, 0) != 0, paste(round(Freq, 0), '%'), "")), color = "cyan3")+
    labs(x = "", y = "Chiq.test % Contribuci?")+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    coord_fixed(ratio= 0.9)+
    tema3
 
plot_grid(pr_rel1, pr_rel2, pr_rel3, nrow= 1, ncol= 3, rel_widths = c(2,2,0.8), axis= "b", align = "hv")
{% endhighlight %}

<img src="/figures/census17-1.png" title="plot of chunk census17" alt="plot of chunk census17" style="display: block; margin: auto;" />
 
### **Occupation**
 

{% highlight r %}
pr_occ1 <- 
  adult_m %>% 
  ggplot(aes(x= occupation, y= (..count..), fill= income))+
      geom_bar()+
        geom_text(aes(y= (..count..), label=   ifelse(((..count..)/sum(..count..)) >0.00 , (percent((..count..)/sum(..count..))) , "")     ), 
                           stat="count", position = 'stack', vjust=-0.5, color= "black", size=3.5)+
      scale_fill_manual("income", values=c("gray70","steelblue3"))+ 
      labs(x="", y="", title = "Occupation - Income")+
      tema1
 
 
pr_occ2 <- 
adult_m %>%
  group_by(occupation) %>%
  do(binom_stats(.)) %>%
  ungroup() %>% 
  mutate(occupation = reorder(factor(occupation), Proportion)) %>% 
  ggplot(aes(x= occupation, y=Proportion))+
    geom_point(size=3, color="steelblue3")+  
    geom_segment(aes(x= occupation, xend= occupation, y=0, yend=Proportion), color="grey70")+
    geom_hline(yintercept = Bad_rate, linetype= "dashed", color= "steelblue3")+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(0, Bad_rate, label= paste(round(100*Bad_rate,2),"%") ), vjust= -1, hjust= -0.5, color="steelblue3")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper, width=0.2), size= 1.5, color="steelblue3")+
    labs(x="", y="Proporci? >50K", title = "")+
    tema2
 
tt <- table(adult_m$occupation,adult$income)
xisq <- chisq.test(tt)
prop_xi <-100*xisq$residuals^2 / xisq$statistic * sign(xisq$residuals)
 
pr_occ3 <-
prop_xi %>%
  as.data.frame() %>%
  ggplot(aes(x = Var2, y = Var1))+
    geom_tile(aes(fill = Freq), color = "grey70")+
    scale_fill_gradient2(midpoint = 0, low = "firebrick4", mid = "white", high = "blue4", space = "Lab")+
      geom_text(aes(label = ifelse(round(Freq, 0) != 0, paste(round(Freq, 0), '%'), "")), color = "cyan3")+
    labs(x = "", y = "Chiq.test % Contribuci?")+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    coord_fixed(ratio= 0.9)+
    tema3
 
 
plot_grid(pr_occ1, pr_occ2, pr_occ3, nrow= 1, ncol= 3, rel_widths = c(2,2,0.8), axis= "b", align = "hv")
{% endhighlight %}

<img src="/figures/census18-1.png" title="plot of chunk census18" alt="plot of chunk census18" style="display: block; margin: auto;" />
 
### **Workclass**
 

{% highlight r %}
pr_wok1 <- 
  adult_m %>% 
  ggplot(aes(x= workclass, y= (..count..), fill= income))+
      geom_bar()+
        geom_text(aes(y= (..count..), label=   ifelse(((..count..)/sum(..count..)) >0.00 , (percent((..count..)/sum(..count..))) , "")     ), 
                           stat="count", position = 'stack', vjust=-0.5, color= "black", size=3.5)+
      scale_fill_manual("income", values=c("gray70","steelblue3"))+ 
      labs(x="", y="", title = "Workclass - Income")+
      tema1
 
 
pr_wok2 <- 
adult_m %>%
  group_by(workclass) %>%
  do(binom_stats(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>% 
  mutate(workclass = reorder(factor(workclass), Proportion)) %>% 
  ggplot(aes(x= workclass, y=Proportion))+
    geom_point(size=3, color="steelblue3")+  
    geom_segment(aes(x= workclass, xend= workclass, y=0, yend=Proportion), color="grey70")+
    geom_hline(yintercept = Bad_rate, linetype= "dashed", color= "steelblue3")+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(0, Bad_rate, label= paste(round(100*Bad_rate,2),"%") ), vjust= -1, hjust= -0.5, color="steelblue3")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper, width=0.2), size= 1.5, color="steelblue3")+
    labs(x="", y="Proporci? >50K", title = "")+
    tema2
 
tt <- table(adult_m$workclass,adult$income)
xisq <- chisq.test(tt)
prop_xi <-100*xisq$residuals^2 / xisq$statistic * sign(xisq$residuals)
 
pr_wok3 <-
prop_xi %>%
  as.data.frame(prop_xi) %>%
  ggplot(aes(x = Var2, y = Var1))+
    geom_tile(aes(fill = Freq), color = "grey70")+
    scale_fill_gradient2(midpoint = 0, low = "firebrick4", mid = "white", high = "blue4", space = "Lab")+
      geom_text(aes(label = ifelse(round(Freq, 0) != 0, paste(round(Freq, 0), '%'), "")), color = "cyan3")+
    labs(x = "", y = "Chiq.test % Contribuci?")+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    coord_fixed(ratio= 0.9)+
    tema3
 
 
plot_grid(pr_wok1, pr_wok2, pr_wok3, nrow= 1, ncol= 3, rel_widths = c(2,2,0.8), axis= "b", align = "hv")
{% endhighlight %}

<img src="/figures/census19-1.png" title="plot of chunk census19" alt="plot of chunk census19" style="display: block; margin: auto;" />
 
### **Marital.Status**
 

{% highlight r %}
pr_mar1 <- 
  adult_m %>% 
  ggplot(aes(x= marital.status, y= (..count..), fill= income))+
      geom_bar()+
        geom_text(aes(y= (..count..), label=   ifelse(((..count..)/sum(..count..)) >0.00 , (percent((..count..)/sum(..count..))) , "")     ), 
                           stat="count", position = 'stack', vjust=-0.5, color= "black", size=3.5)+
      scale_fill_manual("income", values=c("gray70","steelblue3"))+ 
      labs(x="", y="", title = "Marital.Status - Income")+
      tema1
 
 
pr_mar2 <- 
adult_m %>%
  group_by(marital.status) %>%
  do(binom_stats(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>% 
  mutate(marital.status = reorder(factor(marital.status), Proportion)) %>% 
  ggplot(aes(x= marital.status, y=Proportion))+
    geom_point(size=3, color="steelblue3")+  
    geom_segment(aes(x= marital.status, xend= marital.status, y=0, yend=Proportion), color="grey70")+
    geom_hline(yintercept = Bad_rate, linetype= "dashed", color= "steelblue3")+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(0, Bad_rate, label= paste(round(100*Bad_rate,2),"%") ), vjust= -1, hjust= -0.5, color="steelblue3")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper, width=0.2), size= 1.5, color="steelblue3")+
    labs(x="", y="Proporci? >50K", title = "")+
    tema2
 
tt <- table(adult_m$marital.status,adult$income)
xisq <- chisq.test(tt)
prop_xi <-100*xisq$residuals^2 / xisq$statistic * sign(xisq$residuals)
 
pr_mar3 <-
prop_xi %>%
  as.data.frame() %>%
  ggplot(aes(x = Var2, y = Var1))+
    geom_tile(aes(fill = Freq), color = "grey70")+
    scale_fill_gradient2(midpoint = 0, low = "firebrick4", mid = "white", high = "blue4", space = "Lab")+
      geom_text(aes(label = ifelse(round(Freq, 0) != 0, paste(round(Freq, 0), '%'), "")), color = "cyan3")+
    labs(x = "", y = "Chiq.test % Contribuci?")+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    coord_fixed(ratio= 0.9)+
    tema3
 
 
 
plot_grid(pr_mar1, pr_mar2, pr_mar3, nrow= 1, ncol=3, rel_widths = c(2,2,0.8), axis= "b", align = "hv")
{% endhighlight %}

<img src="/figures/census20-1.png" title="plot of chunk census20" alt="plot of chunk census20" style="display: block; margin: auto;" />
 
### **Education**
 

{% highlight r %}
pr_edu1 <- 
  adult_m %>% 
  ggplot(aes(x= education, y= (..count..), fill= income))+
      geom_bar()+
        geom_text(aes(y= (..count..), label=   ifelse(((..count..)/sum(..count..)) >0.01 , (percent((..count..)/sum(..count..))) , "")     ), 
                           stat="count", position = 'stack', vjust=-0.5, color= "black", size=3)+
      scale_fill_manual("income", values=c("gray70","steelblue3"))+ 
      labs(x="", y="", title = "Education - Income")+
      tema1
 
 
pr_edu2 <- 
adult_m %>%
  group_by(education) %>%
  do(binom_stats(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>% 
  mutate(education = reorder(factor(education), Proportion)) %>% 
  ggplot(aes(x= education, y=Proportion))+
    geom_point(size=3, color="steelblue3")+  
    geom_segment(aes(x= education, xend= education, y=0, yend=Proportion), color="grey70")+
    geom_hline(yintercept = Bad_rate, linetype= "dashed", color= "steelblue3")+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(0, Bad_rate, label= paste(round(100*Bad_rate,2),"%") ), vjust= -1, hjust= -0.5, color="steelblue3")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper, width=0.2), size= 1.5, color="steelblue3")+
    labs(x="", y="Proporci? >50K", title = "")+
    tema2
 
tt <- table(adult_m$education,adult$income)
xisq <- chisq.test(tt)
prop_xi <-100*xisq$residuals^2 / xisq$statistic * sign(xisq$residuals)
 
pr_edu3 <-
prop_xi %>%
  as.data.frame(prop_xi) %>%
  ggplot(aes(x = Var2, y = Var1))+
    geom_tile(aes(fill = Freq), color = "grey70")+
    scale_fill_gradient2(midpoint = 0, low = "firebrick4", mid = "white", high = "blue4", space = "Lab")+
      geom_text(aes(label = ifelse(round(Freq, 0) != 0, paste(round(Freq, 0), '%'), "")), color = "cyan3")+
    labs(x = "", y = "Chiq.test % Contribuci?")+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    coord_fixed(ratio= 0.9)+
    tema3
 
 
 
plot_grid(pr_edu1, pr_edu2, pr_edu3, nrow= 1, ncol=3, rel_widths = c(2,2,0.8), axis= "b", align = "hv")
{% endhighlight %}

<img src="/figures/census21-1.png" title="plot of chunk census21" alt="plot of chunk census21" style="display: block; margin: auto;" />
 
### **Country**
 

{% highlight r %}
  adult_m %>% 
  ggplot(aes(x= native.country, y= (..count..), fill= income))+
      geom_bar()+
        geom_text(aes(y= (..count..), label=   ifelse(((..count..)/sum(..count..)) >0.01 , (percent((..count..)/sum(..count..))) , "")     ), 
                           stat="count", position = 'stack', vjust=-0.5, color= "black", size=3.5)+
      scale_fill_manual("income", values=c("gray70","steelblue3"))+ 
      labs(x="", y="", title = "Country - Income")+
      tema1
{% endhighlight %}

<img src="/figures/census22-1.png" title="plot of chunk census22" alt="plot of chunk census22" style="display: block; margin: auto;" />
 
### **Intensitat Relacions Cramer-V**
 

{% highlight r %}
# selecci? categ?riques
adult_f <- 
  adult %>% 
  mutate_if(is.character, as.factor) %>% 
  keep(is.factor)
 
# funci? c?lcul p.value i Cramer-V
xitest <- function(var, var2, data=adult_f){
  tt <- table(data[[var]], data[[var2]])
  ch <- chisq.test(tt)
  cra <- assocstats(tt)
  data.frame(var1= var, 
             var2= var2,
             p.value= ch$p.value, 
             cramer.V = cra$cramer)
}
 
# selecci? noms variables
vars_cat <- 
  adult_f %>% 
  names()
 
# combinacion creuades possibles de la variable
df_cross <- cross2(vars_cat, vars_cat)           # purrr:: llista de combinacions variables sense repetici?
df_cross <- do.call(rbind.data.frame, df_cross)  # genera un dataframe a partir de la llista anterior
names(df_cross) <- c("var1", "var2")  
 
# aplicar funci? atotes les combinacions, convertir a matriu, corrplot
df_cramer <- 
  df_cross %>% 
  mutate(x= map2(as.character(var1), as.character(var2), .f= xitest)) %>% 
  unnest() %>%        # separa elements d'uns camp en diferets capms
  bind_rows() %>% 
  select(-var11, -var21, -p.value) %>% 
  mutate(cramer.V= round(cramer.V, 2)) %>% 
  spread(var2, cramer.V) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  column_to_rownames('var1') %>% 
  as.matrix() %>% 
  corrplot(method = "color", 
           type="upper",
           outline= T, addgrid.col= "darkgray", 
           order = "hclust", addCoef.col = "black", tl.col = "black", diag = FALSE)
{% endhighlight %}

![plot of chunk unnamed-chunk-38](/figures/unnamed-chunk-38-1.png)
 
Les dues variables que més relacio tenen amb Income son marital.status i relationship.  
Aquestes dues estan relacionades entre elles.  
 
 
- Relationship - Sex  
 
El percentatge d'homes es molt elevat respecte a dones.  
Aixo implica que el percentatge de 'marits' es molt mes elevat que el de 'esposes'.  
 

{% highlight r %}
adult_f %>% 
  group_by(sex, relationship) %>%
  dplyr::summarise(n = n()) %>% 
  mutate(prop = percent(n/sum(n))) %>%
  select(-n) %>% 
  spread(sex, prop)%>%
  kable() %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> relationship </th>
   <th style="text-align:left;"> Female </th>
   <th style="text-align:left;"> Male </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Husband </td>
   <td style="text-align:left;"> 0.0% </td>
   <td style="text-align:left;"> 60.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not-in-family </td>
   <td style="text-align:left;"> 36.0% </td>
   <td style="text-align:left;"> 20.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other-relative </td>
   <td style="text-align:left;"> 4.0% </td>
   <td style="text-align:left;"> 2.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Own-child </td>
   <td style="text-align:left;"> 20.8% </td>
   <td style="text-align:left;"> 13.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unmarried </td>
   <td style="text-align:left;"> 24.6% </td>
   <td style="text-align:left;"> 3.6% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wife </td>
   <td style="text-align:left;"> 14.5% </td>
   <td style="text-align:left;"> 0.0% </td>
  </tr>
</tbody>
</table>
 
- Marital.Status - Sex  
 
El percentatge d'homes es molt elevat respecte a dones.  
Homes casats, mot mes gran que dones casades.  
 

{% highlight r %}
adult_f %>% 
  group_by(sex,marital.status) %>%
  dplyr::summarise(n = n()) %>% 
  mutate(prop = percent(n/sum(n))) %>%
  select(-n) %>% 
  spread(sex, prop)%>%
  kable() %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> marital.status </th>
   <th style="text-align:left;"> Female </th>
   <th style="text-align:left;"> Male </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Divorced </td>
   <td style="text-align:left;"> 24.8% </td>
   <td style="text-align:left;"> 8.1% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Married-AF-spouse </td>
   <td style="text-align:left;"> 0.1% </td>
   <td style="text-align:left;"> 0.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Married-civ-spouse </td>
   <td style="text-align:left;"> 15.4% </td>
   <td style="text-align:left;"> 61.1% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Married-spouse-absent </td>
   <td style="text-align:left;"> 1.9% </td>
   <td style="text-align:left;"> 1.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Never-married </td>
   <td style="text-align:left;"> 44.3% </td>
   <td style="text-align:left;"> 27.2% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Separated </td>
   <td style="text-align:left;"> 5.9% </td>
   <td style="text-align:left;"> 1.8% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Widowed </td>
   <td style="text-align:left;"> 7.7% </td>
   <td style="text-align:left;"> 0.8% </td>
  </tr>
</tbody>
</table>
 
 
- Marital.Status - Relationship  
Homes - HOmes Casats - Marits
 
- Native.Country - race  
 
- Sex - Occupation  
Nomes hi ha tres casos on la proporció de dones es superior: Adm-clerical, Other-sevice, Pri-house-serv.  
 

{% highlight r %}
adult_f %>% 
  group_by(sex,occupation) %>%
  dplyr::summarise(n = n()) %>% 
  mutate(prop = percent(n/sum(n))) %>%
  select(-n) %>% 
  spread(sex, prop)%>%
  kable() %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> occupation </th>
   <th style="text-align:left;"> Female </th>
   <th style="text-align:left;"> Male </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Adm-clerical </td>
   <td style="text-align:left;"> 23.6% </td>
   <td style="text-align:left;"> 5.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Armed-Forces </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 0.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Craft-repair </td>
   <td style="text-align:left;"> 2.1% </td>
   <td style="text-align:left;"> 17.8% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Exec-managerial </td>
   <td style="text-align:left;"> 10.8% </td>
   <td style="text-align:left;"> 13.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Farming-fishing </td>
   <td style="text-align:left;"> 0.6% </td>
   <td style="text-align:left;"> 4.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Handlers-cleaners </td>
   <td style="text-align:left;"> 1.5% </td>
   <td style="text-align:left;"> 5.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Machine-op-inspct </td>
   <td style="text-align:left;"> 5.1% </td>
   <td style="text-align:left;"> 6.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other-service </td>
   <td style="text-align:left;"> 16.7% </td>
   <td style="text-align:left;"> 6.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Priv-house-serv </td>
   <td style="text-align:left;"> 1.3% </td>
   <td style="text-align:left;"> 0.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Prof-specialty </td>
   <td style="text-align:left;"> 14.1% </td>
   <td style="text-align:left;"> 12.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Protective-serv </td>
   <td style="text-align:left;"> 0.7% </td>
   <td style="text-align:left;"> 2.6% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sales </td>
   <td style="text-align:left;"> 11.7% </td>
   <td style="text-align:left;"> 11.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tech-support </td>
   <td style="text-align:left;"> 3.2% </td>
   <td style="text-align:left;"> 2.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Transport-moving </td>
   <td style="text-align:left;"> 0.8% </td>
   <td style="text-align:left;"> 6.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 7.8% </td>
   <td style="text-align:left;"> 4.6% </td>
  </tr>
</tbody>
</table>
 
# **Transformacio**
 
## **Race**
 
Agrupem Other + Amer-Indian-Eskimo
 

{% highlight r %}
adult_m$race2 <- fct_collapse(adult_m$race,
                              White = "White",
                              Black = "Black",
                              Asian = "Asian-Pac-Islander",
                              Altres = c("Other", "Amer-Indian-Eskimo"))
 
adult_m %>% 
    group_by(race2, income) %>% 
    tally(wt=NULL) %>% 
    mutate(percentatge= n*100/sum(n)) %>% 
    ggplot(aes(x= race2, y=n, fill= income))+
      geom_bar(stat = 'identity', position = 'stack')+
      geom_text(aes(label= paste(round(percentatge, 1), "%")), position = 'stack', vjust=-0.5, color= "black", size=3.5)+
      coord_flip()+
      theme_light()+
          theme(
              legend.position = "none",
               panel.grid.major.x= element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_blank())+
      scale_fill_manual(values=c("gray70","steelblue3")) 
{% endhighlight %}

<img src="/figures/census23-1.png" title="plot of chunk census23" alt="plot of chunk census23" style="display: block; margin: auto;" />
 
## **Workclass**
 
Agrupem 4 classes: no_work, govern, self, private
 

{% highlight r %}
adult_m$workclass2 <- fct_collapse(adult_m$workclass,
                                   no_work = c("Never-worked", "Without-pay"),
                                   altres = c("Altres"),
                                   govern_loc = c("State-gov", "Local-gov"),
                                   govern_fed = c("Federal-gov"),
                                   self_inc = c("Self-emp-inc"),
                                   self_ninc = c("Self-emp-not-inc"),
                                   private = "Private")
 
adult_m %>% 
    group_by(workclass2, income) %>% 
    tally(wt=NULL) %>% 
    mutate(percentatge= n*100/sum(n)) %>% 
    ggplot(aes(x= workclass2, y=n, fill= income))+
      geom_bar(stat = 'identity', position = 'stack')+
      geom_text(aes(label= paste(round(percentatge, 1), "%")), position = 'stack', vjust=-0.5, color= "black", size=3.5)+
      coord_flip()+
      theme_light()+
          theme(
              legend.position = "none",
              panel.grid.major.x= element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_blank())+
      scale_fill_manual(values=c("gray70","steelblue3")) 
{% endhighlight %}

<img src="/figures/census24-1.png" title="plot of chunk census24" alt="plot of chunk census24" style="display: block; margin: auto;" />
 
 
## **Marital.Status**
 
Agrupem: casats, separats, no_casats
 

{% highlight r %}
adult_m$marital2 <- fct_collapse(adult$marital.status,
                         casats = c("Married-civ-spouse"),
                         casats_b = c("Married-spouse-absent", "Married-AF-spouse"),
                         separats = c("Divorced", "Separated", "Widowed"),
                         no_casats = "Never-married")
adult_m %>% 
    group_by(marital2, income) %>% 
    tally(wt=NULL) %>% 
    mutate(percentatge= n*100/sum(n)) %>% 
    ggplot(aes(x= marital2, y=n, fill= income))+
      geom_bar(stat = 'identity', position = 'stack')+
      geom_text(aes(label= paste(round(percentatge, 1), "%")), position = 'stack', vjust=-0.5, color= "black", size=3.5)+
      coord_flip()+
      theme_light()+
          theme(
              legend.position = "none",
              panel.grid.major.x= element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_blank())+
      scale_fill_manual(values=c("gray70","steelblue3"))
{% endhighlight %}

<img src="/figures/census25-1.png" title="plot of chunk census25" alt="plot of chunk census25" style="display: block; margin: auto;" />
 
 
## **Education**
 
Agrupem tots els de bàsica
 

{% highlight r %}
adult_m$education2 <- fct_collapse(adult$education,
                           Basica = c("10th", "11th", "12th", "1st-4th", "5th-6th", "7th-8th", "9th", "Preschool"),
                           SomeCollege = "Some-college",
                           ProfSchool = "Prof-school",
                           Masters = "Masters",
                           HSgrad = "HS-grad", 
                           Doctorate = "Doctorate",
                           Bachelors = "Bachelors",
                           Assoc = c("Assoc-voc", "Assoc-acdm"))
adult_m %>% 
        group_by(education2, income) %>% 
        tally(wt=NULL) %>% 
        mutate(percentatge= n*100/sum(n)) %>% 
        ggplot(aes(x= education2, y=n, fill= income))+
          geom_bar(stat = 'identity', position = 'stack')+
          geom_text(aes(label= paste(round(percentatge, 1), "%")), position = 'stack', vjust=-0.5, color= "black", size=3.5)+
          coord_flip()+
          theme_light()+
              theme(
                  legend.position = "none",
                  panel.grid.major.x= element_blank(),
                  panel.border = element_blank(),
                  axis.ticks.x = element_blank())+
          scale_fill_manual(values=c("gray70","steelblue3"))
{% endhighlight %}

<img src="/figures/census26-1.png" title="plot of chunk census26" alt="plot of chunk census26" style="display: block; margin: auto;" />
 
 
## **Country**
 
Agrupem per continent
 

{% highlight r %}
adult_m$country2 <- fct_collapse(adult_m$native.country,
                                 EU = c("England", "France", "Germany", "Greece", "Holand-Netherlands","Scotland",
                                         "Portugal", "Ireland","Italy", "Poland", "Yugoslavia", "Hungary" ),
                                 SAme = c("Columbia", "Cuba", "Guatemala", "Mexico", "Honduras", "Nicaragua", "Puerto-Rico",
                                           "El-Salvador" , "Ecuador","Dominican-Republic", "Puerto-Rico", "Peru", "Haiti",
                                           "Trinadad&Tobago", "Jamaica"),
                                 Sasia = c("Cambodia", "Vietnam", "Laos", "Thailand"),
                                 Asia = c("Japan", "South", "Philippines", "Taiwan"),
                                 NAme = c( "United-States", "Outlying-US(Guam-USVI-etc)", "Canada"),
                                 Asia2 = c("China", "India", "Iran", "Hong"),
                                 Altres = "Altres")
 
adult_m %>% 
    group_by(country2, income) %>% 
    tally(wt=NULL) %>% 
    mutate(percentatge= n*100/sum(n)) %>% 
    ggplot(aes(x= country2, y=n, fill= income))+
      geom_bar(stat = 'identity', position = 'stack')+
      geom_text(aes(label= paste(round(percentatge, 1), "%")), position = 'stack', vjust=-0.5, color= "black", size=3.5)+
      coord_flip()+
      theme_light()+
          theme(
              legend.position = "none",
              panel.grid.major.x= element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_blank())+
      scale_fill_manual(values=c("gray70","steelblue3"))
{% endhighlight %}

<img src="/figures/census27-1.png" title="plot of chunk census27" alt="plot of chunk census27" style="display: block; margin: auto;" />
 
## **Age**
 
Discretització de la variable per grups d'edat.  
 

{% highlight r %}
adult_m$age2 <-  cut(adult$age, breaks = c(-Inf,20,25,30,35,40,45,50,55,60,Inf), labels = c("m20","25","30","35","40","45","50","55","60","M60"))
 
adult_m$age2 %>% 
  fct_count(sort = T, prop = T) %>% 
  mutate(p = percent(p)) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> f </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:left;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:right;"> 4353 </td>
   <td style="text-align:left;"> 13.37% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:right;"> 4193 </td>
   <td style="text-align:left;"> 12.88% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:right;"> 4161 </td>
   <td style="text-align:left;"> 12.78% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:right;"> 4001 </td>
   <td style="text-align:left;"> 12.29% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:right;"> 3816 </td>
   <td style="text-align:left;"> 11.72% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:right;"> 3167 </td>
   <td style="text-align:left;"> 9.73% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m20 </td>
   <td style="text-align:right;"> 2410 </td>
   <td style="text-align:left;"> 7.40% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:right;"> 2371 </td>
   <td style="text-align:left;"> 7.28% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> M60 </td>
   <td style="text-align:right;"> 2332 </td>
   <td style="text-align:left;"> 7.16% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:right;"> 1757 </td>
   <td style="text-align:left;"> 5.40% </td>
  </tr>
</tbody>
</table>
 
## **Hours.per.week**  
 

{% highlight r %}
adult_m$hours2 <-  cut(adult$hours.per.week, breaks = c(-Inf,25,35,45,55,Inf), labels = c("m25","35","45","55","M55"))
 
adult_m$hours2 %>% 
  fct_count(sort = T, prop = T) %>% 
  mutate(p = percent(p)) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> f </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:left;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:right;"> 18542 </td>
   <td style="text-align:left;"> 56.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:right;"> 4407 </td>
   <td style="text-align:left;"> 13.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m25 </td>
   <td style="text-align:right;"> 3943 </td>
   <td style="text-align:left;"> 12.1% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:right;"> 2937 </td>
   <td style="text-align:left;"> 9.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> M55 </td>
   <td style="text-align:right;"> 2732 </td>
   <td style="text-align:left;"> 8.4% </td>
  </tr>
</tbody>
</table>
 
## **Capital.gain** 
 

{% highlight r %}
adult_m$gain <-  cut(adult$capital.gain, breaks = c(-Inf,3000,4300,Inf), labels = c("m30","35-43","M43"))
 
adult_m$gain %>% 
  fct_count(sort = T, prop = T) %>% 
  mutate(p = percent(p)) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> f </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:left;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> m30 </td>
   <td style="text-align:right;"> 30321 </td>
   <td style="text-align:left;"> 93.1% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> M43 </td>
   <td style="text-align:right;"> 1834 </td>
   <td style="text-align:left;"> 5.6% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35-43 </td>
   <td style="text-align:right;"> 406 </td>
   <td style="text-align:left;"> 1.2% </td>
  </tr>
</tbody>
</table>
 
## **Capital.loss**
 

{% highlight r %}
adult_m$loss <-  cut(adult$capital.loss, breaks = c(-Inf,0,1800,Inf), 
                          labels = c("0","0-18","M18")) 
 
adult_m$loss %>% 
  fct_count(sort = T, prop = T) %>% 
  mutate(p = percent(p)) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> f </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:left;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 31042 </td>
   <td style="text-align:left;"> 95.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> M18 </td>
   <td style="text-align:right;"> 993 </td>
   <td style="text-align:left;"> 3.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0-18 </td>
   <td style="text-align:right;"> 526 </td>
   <td style="text-align:left;"> 1.6% </td>
  </tr>
</tbody>
</table>
 
## **Varibles descartades**  
 
* fnlwgt
* education.num
 
## **Data frame final**
 

{% highlight r %}
df_final <- 
  adult_m %>% 
  select(age=age2, workclass=workclass2, education=education2, marital=marital2, 
         occupation, relationship, race=race2, gain, loss, hours=hours2, 
         country=country2, income, sex )
 
df_x <- df_final
df_x %>% 
  # keep(is.numeric) %>% 
  map_df(~(data.frame(unics= n_distinct(.x),
                      classe= class(.x),
                      NA_q= sum(is.na(.x), na.rm = T),
                      zero_q= sum(.x == 0, na.rm = T)
  )
  ),
  .id= "variable") %>% 
  mutate(NA_p = percent(NA_q / nrow(df_x)),
         zero_p = percent(zero_q / nrow(df_x)),
         NA_q = cell_spec(NA_q, "html", color = ifelse(NA_q > 0, "white", "black"), background = ifelse(NA_q > 0, "red", "")),
         NA_p = cell_spec(NA_p, "html", color = ifelse(NA_p > 0.001, "white", "black"), background = ifelse(NA_p > 0.001, "red", "")),
         zero_q = cell_spec(zero_q, "html", color = ifelse(zero_q > 0.001, "white", "black"), background = ifelse(zero_q > 0.001, "orange", "")),
         zero_p = cell_spec(zero_p, "html", color = ifelse(zero_p > 0.001, "white", "black"), background = ifelse(zero_p > 0.001, "orange", "")),
         classe = cell_spec(classe, "html", color= ifelse(classe == "character", "blue", "")),
         unics = cell_spec(unics, align = "c")) %>% 
  select(1,3,2,4,6,5,7) %>% 
  arrange(classe) %>% 
  kable(format = "html", escape = F) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>% 
  add_header_above(c(" " = 1, " " = 1," " = 1, "NA" = 2, "ZERO" = 2))
{% endhighlight %}

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">NA</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">ZERO</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:left;"> classe </th>
   <th style="text-align:left;"> unics </th>
   <th style="text-align:left;"> NA_q </th>
   <th style="text-align:left;"> NA_p </th>
   <th style="text-align:left;"> zero_q </th>
   <th style="text-align:left;"> zero_p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> age </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">10</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> workclass </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">7</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> education </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">8</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> marital </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">4</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> occupation </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">15</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> relationship </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">6</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> race </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">4</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gain </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">3</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> loss </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">3</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: orange !important;">31042</span> </td>
   <td style="text-align:left;"> <span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: orange !important;">95.3%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hours </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">5</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> country </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">7</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">2</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sex </td>
   <td style="text-align:left;"> <span style="     color:  !important;">factor</span> </td>
   <td style="text-align:left;"> <span style="     text-align: c;">2</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0%</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color:  !important;">0.0%</span> </td>
  </tr>
</tbody>
</table>
 

{% highlight r %}
write_xlsx(df_final, "df_eda.xlsx")  
{% endhighlight %}
 
# **Model**
 
## **Train / Test**
 

{% highlight r %}
# split training-test data
set.seed(2019)
i_split <- initial_split(df_final, prop = 0.8)
i_split
{% endhighlight %}



{% highlight text %}
## <26049/6512/32561>
{% endhighlight %}



{% highlight r %}
# df_train, df_test
df_train <- training(i_split)
df_test <-  testing(i_split)
 
# crear recepte sobre conjunt de training
recepte <- 
  df_train %>% 
  recipe(income ~.) %>% 
  step_dummy(all_nominal(), -income)
 
preparada <- 
  recepte %>% 
  prep() #aplica transformacions a les dades de training
 
# transformar dades de test amb la mateixa recepte anterior
test_baked <- bake(preparada, df_test)
# obtenir split de train transformat (no cal fer bake)
train_baked <- juice(preparada)
{% endhighlight %}
 
## **Model Regressió Logística**
 

{% highlight r %}
# model training
set.seed(2019)
mod_glm <-
  logistic_reg(mode = "classification") %>%  # especificaci? del model, quin ?s l'objectiu
  set_engine("glm") %>%
  fit(income ~ ., data = train_baked)
 
# df prediccions: raw + probabilitats + target
predict_glm <- 
  predict(mod_glm, test_baked, type = "prob") %>%  # predicci? probabilitat
  bind_cols(predict(mod_glm, test_baked)) %>%    # predicci? raw
  bind_cols(select(test_baked, income))          # valor real
 
# metriques valoracio model
 
metrics_glm <- 
predict_glm %>% 
  metrics(truth = income, `.pred_>50K`, estimate = .pred_class)
 
# confusion matrix
maco_glm <- 
  predict_glm %>% 
  conf_mat(income, .pred_class, dnn= c("Predicci?", "Real")) 
 
# metriques de valoraci? del model: metriques + matriu confusi?
summary(maco_glm) %>% 
  select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "precision", "recall", "f_meas")) %>% 
  rbind(select(metrics_glm, -.estimator)) %>% 
  filter(!.metric  %in% c("kap", "mn_log_loss")) %>% 
  kable(caption = "Metriques del Model") %>% 
    kable_styling( full_width = F)
{% endhighlight %}

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Metriques del Model</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> .metric </th>
   <th style="text-align:right;"> .estimate </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> accuracy </td>
   <td style="text-align:right;"> 0.8619472 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> precision </td>
   <td style="text-align:right;"> 0.8868648 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> recall </td>
   <td style="text-align:right;"> 0.9373733 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> f_meas </td>
   <td style="text-align:right;"> 0.9114198 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> accuracy </td>
   <td style="text-align:right;"> 0.8619472 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> roc_auc </td>
   <td style="text-align:right;"> 0.9155783 </td>
  </tr>
</tbody>
</table>
 

{% highlight r %}
# plot matriu confusio
predict_glm %>%
  conf_mat(income, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n)) +
  geom_tile(show.legend = FALSE, fill = "blue", color = "white") +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)+
  labs(title = "Confusion Matrix" )
{% endhighlight %}

<img src="/figures/census29-1.png" title="plot of chunk census29" alt="plot of chunk census29" style="display: block; margin: auto;" />
 

{% highlight r %}
# calcul AUC
auc <- 
predict_glm %>% 
  roc_auc(income, `.pred_>50K`)%>% 
  select(.estimate) %>% 
  mutate(.estimate = round(.estimate,2)) %>% 
  as.character()
 
# corba ROC 
predict_glm %>% 
  roc_curve(income, `.pred_>50K`) %>%
  autoplot()+
  ggtitle(paste("Corba ROC  --  AUC", auc))
{% endhighlight %}

<img src="/figures/census30-1.png" title="plot of chunk census30" alt="plot of chunk census30" style="display: block; margin: auto;" />
 
 
## **Cross Validation**  
 

{% highlight r %}
# splits
set.seed(2019)
rsam <- vfold_cv(df_train, v = 10)
 
# receptes desl splits
rsam$receptes <- map(rsam$splits, prepper, recipe = recepte, retain = TRUE)
 
# models dels splits
modelar <- function(split, recepte) {
  test <- bake(recepte, analysis(split))
  modi <- logistic_reg(mode = "classification") %>%  
    set_engine("glm") %>%
    fit(income ~ ., data = (test))
}
 
set.seed(2019)
rsam <- 
  rsam %>% 
  mutate(models = list(splits, receptes) %>% pmap(modelar)) 
 
# prediccio dels splits
predir <- function(split, recepte, model) {
  test <- bake(recepte, assessment(split))
  tibble(
    actual    = test$income,
    predit    = (predict(model, test))[[1]]
  )
}
 
set.seed(2019)
rsam <- 
  rsam %>% 
  mutate(pred = list(splits, receptes,  models) %>% pmap(predir),
         metrics = map(pred, metrics, actual, predit))  #pmap itera sobre n variables
 
# validate
rsam %>% 
  select(metrics) %>% 
  unnest(metrics) %>% 
  filter(.metric == "accuracy") %>% 
  select(.estimate) %>% 
  kable(caption = "Accuracy 10 cv-fold") %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Accuracy 10 cv-fold</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> .estimate </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.8564299 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.8698656 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.8610365 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.8564299 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.8606526 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.8376200 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.8560461 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.8633397 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.8537428 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.8509985 </td>
  </tr>
</tbody>
</table>



{% highlight r %}
rsam %>% 
  select(metrics) %>% 
  unnest(metrics) %>% 
  filter(.metric == "accuracy") %>% 
  summarise(mean_accuracy = mean(.estimate)) %>% 
  kable(caption = "Mitjana 10 cv-fold") %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Mitjana 10 cv-fold</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> mean_accuracy </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.8566162 </td>
  </tr>
</tbody>
</table>
 

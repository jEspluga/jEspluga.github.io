---
layout: post  
title: ClassifiaciÃ³  
author: Josep Espluga  
published: true
status: publish
draft: false  
tags: classificaciÃ³  
---
 
Prediccio d'ingressos superiors/inferiors a $50k/any en funcio de les dades del cens.
 
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
## Error: 'adult.csv' does not exist in current working directory ('C:/Users/jespl/Documents/R/PROJECTES/jespluga.github.io').
{% endhighlight %}



{% highlight r %}
head(adult)
{% endhighlight %}



{% highlight text %}
## # A tibble: 6 x 15
##     age workclass fnlwgt education education.num marital.status occupation
##   <dbl> <chr>      <dbl> <chr>             <dbl> <chr>          <chr>     
## 1    90 <NA>       77053 HS-grad               9 Widowed        <NA>      
## 2    82 Private   132870 HS-grad               9 Widowed        Exec-mana~
## 3    66 <NA>      186061 Some-col~            10 Widowed        <NA>      
## 4    54 Private   140359 7th-8th               4 Divorced       Machine-o~
## 5    41 Private   264663 Some-col~            10 Separated      Prof-spec~
## 6    34 Private   216864 HS-grad               9 Divorced       Other-ser~
## # ... with 8 more variables: relationship <chr>, race <chr>, sex <chr>,
## #   capital.gain <dbl>, capital.loss <dbl>, hours.per.week <dbl>,
## #   native.country <chr>, income <chr>
{% endhighlight %}
 
 

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
Dues variables amb elevada frequencia de zeros.Variables caracter possibles factors.  
capital.gain i capital.loss amb elevada frequencia de zeros.  
fnlwgt: alt valor d'unics.  
native.country: alta cardinalitat.  
education number i education: mateix nombre d'unics.  
Variables numeriques i caracter.  
 

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
 
 
# **Analisi NA**
 

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
 
Occupation i workclass amb el mateix nombre de unics .  
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
 
Converisi? NA:
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
 
# **Analisi Univariant** 
 
## **Numeriques**
 
Estadistica Descriptiva:  
 

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
 
Histogrames:
 

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
 
Analisi grafic: 
 

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
 
 
Distribucio capital.gain i capital.loss consequencia elevat n. zeros.  
Capital.gain te un outlier diferenciat.  
Education.num te observacions amb baix nivell d'educacio.  
 
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
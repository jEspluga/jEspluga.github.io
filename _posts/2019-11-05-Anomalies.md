---
layout: post  
title: Deteció d'Anomalies
author: Josep Espluga  
published: true
status: publish
draft: false  
tags: anomalies
---

*** 

### Introducció

La detecció d'anomalies preten detectar observacions que no concorden amb la resta de les dades, tot i que no tenen perque suposar valors fora dels rangs normals de treball.  
Tot i no disposar de dades etiquetades per tal de poder entrenar un model de classificació, és possible detectar anomalies, utilitzant eines d'aprenentatge no supervisat com el 'clustering'.  
Les dades més similars s'agruparan en 'clusters' i els punts més allunyats dels centres del cluster els considerarem anomalies.

 
### Llibreries
 

{% highlight r %}
library(tidyverse)
library(funModeling)
library(lubridate)
library(weathermetrics)
library(factoextra)
library(corrplot)
library(qgraph)
library(kableExtra)
library(scales)
library(plotly)
library(broom)
library(tidymodels)
{% endhighlight %}
 
### Dades
 

{% highlight r %}
machine <- read_csv("machine_temperature_system_failure.csv")
summary(machine)
{% endhighlight %}



{% highlight text %}
##    timestamp                       value        
##  Min.   :2013-12-02 21:15:00   Min.   :  2.085  
##  1st Qu.:2013-12-22 14:02:30   1st Qu.: 83.080  
##  Median :2014-01-11 05:50:00   Median : 89.408  
##  Mean   :2014-01-11 06:16:49   Mean   : 85.927  
##  3rd Qu.:2014-01-30 22:37:30   3rd Qu.: 94.016  
##  Max.   :2014-02-19 15:25:00   Max.   :108.511
{% endhighlight %}



{% highlight r %}
machine <- 
  machine %>% 
  mutate(temp = fahrenheit.to.celsius(value, round = 2))
{% endhighlight %}
 

{% highlight r %}
ggplot(machine, aes(x = timestamp, y=temp))+
  geom_line(color="grey30")+
  theme_minimal()+
  labs(title = "Registre Temperatura", x=" ", y="Temperatura (?C)")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))
{% endhighlight %}

<img src="/figures/a1-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />
 
### Feature Engineering

Agrupacions horàries diferenciant horari treball/inactiu i caps de setmana.  

{% highlight r %}
machine <- 
  machine %>% 
  mutate(hora   = hour(timestamp),
         torn  = ifelse(hora > 7 & hora < 22, "dia", "nit"),
         diaSet = wday(timestamp, week_start = getOption("lubridate.week.start", 1)),
         jour = date(timestamp),
         wEnd   = ifelse(diaSet %in% c(1,2,3,4,5), "SET", "CAP")) %>% 
  group_by(wEnd, torn) %>% 
  mutate(grup = paste(wEnd, torn, sep = "_")) %>% 
  ungroup() %>% 
  mutate(grup = as_factor(grup))
{% endhighlight %}
 

{% highlight r %}
ggplot(machine, aes(x = timestamp, y=temp, color= grup))+
  geom_line()+
  theme_minimal()+
  labs(title = "Registre Temperatura", x=" ", y="Temperatura (?C)")+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))+
  facet_wrap(~grup)
{% endhighlight %}

<img src="/figures/a2-1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" style="display: block; margin: auto;" />

{% highlight r %}
ggplot(machine, aes(x=temp, fill= grup))+
  geom_histogram(color= "white")+
  theme_minimal()+
  labs(title = "Registre Temperatura", x=" ", y="Temperatura (?C)")+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))+
  facet_wrap(~grup, scales = "free")
{% endhighlight %}

<img src="/figures/a2-2.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" style="display: block; margin: auto;" />

{% highlight r %}
ggplot(machine, aes(y=temp, fill= grup))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Registre Temperatura", x=" ", y="Temperatura (?C)")+
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_blank())
{% endhighlight %}

<img src="/figures/a2-3.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" style="display: block; margin: auto;" />
 
### Transformació Dades

Transformació en dades numèriques.  

{% highlight r %}
machine_num <- 
  machine %>% 
  mutate(torn_n = ifelse(torn == "dia", 1, 0),
         wEnd_n = ifelse(wEnd == "SET", 1, 0)) %>% 
  select(temp, hora, torn_n, wEnd_n, diaSet)
 
head(machine_num)
{% endhighlight %}



{% highlight text %}
## # A tibble: 6 x 5
##    temp  hora torn_n wEnd_n diaSet
##   <dbl> <int>  <dbl>  <dbl>  <dbl>
## 1  23.3    21      1      1      1
## 2  23.8    21      1      1      1
## 3  24.5    21      1      1      1
## 4  25.6    21      1      1      1
## 5  26.3    21      1      1      1
## 6  26.0    21      1      1      1
{% endhighlight %}
 
### Anàlisi Components Principals
 

{% highlight r %}
acp <-prcomp(machine_num, center=T, scale.=T) # PCA + estandatització de variables
 
acp
{% endhighlight %}



{% highlight text %}
## Standard deviations (1, .., p=5):
## [1] 1.3393728 1.2327473 0.9994110 0.6964306 0.4500852
## 
## Rotation (n x k) = (5 x 5):
##                  PC1          PC2         PC3         PC4          PC5
## temp    0.0500410651  0.119989562 -0.98525101  0.07394691  0.083130600
## hora    0.0009793380 -0.698735979 -0.13852758 -0.70177270 -0.009605317
## torn_n  0.0003314771 -0.705195736 -0.03150325  0.70816668  0.014365251
## wEnd_n -0.7047566596  0.008271019 -0.09270506  0.01870399 -0.703068683
## diaSet  0.7076813362  0.001049491 -0.02244710  0.01403727 -0.706034778
{% endhighlight %}



{% highlight r %}
# eigenvalue: valors superiors a 1, varian?a percentual acumulada
get_eigenvalue(acp)
{% endhighlight %}



{% highlight text %}
##       eigenvalue variance.percent cumulative.variance.percent
## Dim.1  1.7939196        35.878392                    35.87839
## Dim.2  1.5196658        30.393316                    66.27171
## Dim.3  0.9988223        19.976447                    86.24815
## Dim.4  0.4850156         9.700312                    95.94847
## Dim.5  0.2025767         4.051533                   100.00000
{% endhighlight %}



{% highlight r %}
# gr?fic varian?a acumulada
fviz_eig(acp, addlabels = TRUE, ylim = c(0, 40))
{% endhighlight %}

<img src="/figures/a4-2.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />

{% highlight r %}
actives <- 
  as.data.frame(acp$x[,1:3]) %>% 
  select('PC1_dia' = 'PC1', 'PC2_hora'= "PC2", 'PC3_temp'= "PC3")
{% endhighlight %}
 
 

{% highlight r %}
plot(acp$x[,1], acp$x[,3], xlab="PC1_dia", ylab="PC3_temp")
abline(h=0,v=0,col="gray60")
{% endhighlight %}

<img src="/figures/a3-1.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />

{% highlight r %}
plot(acp$x[,2], acp$x[,3], xlab="PC2_hora", ylab="PC3_temp")
abline(h=0,v=0,col="gray60")
{% endhighlight %}

<img src="/figures/a3-2.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />
 
### Correlacions
 

{% highlight r %}
kor <- cor(machine_num, acp$x[,1:3])
 
kor %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", full_width= F))
{% endhighlight %}

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> PC1 </th>
   <th style="text-align:right;"> PC2 </th>
   <th style="text-align:right;"> PC3 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> temp </td>
   <td style="text-align:right;"> 0.067 </td>
   <td style="text-align:right;"> 0.147 </td>
   <td style="text-align:right;"> -0.984 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hora </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> -0.861 </td>
   <td style="text-align:right;"> -0.138 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> torn </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> -0.869 </td>
   <td style="text-align:right;"> -0.031 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wEnd </td>
   <td style="text-align:right;"> -0.943 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> -0.092 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> diaSet </td>
   <td style="text-align:right;"> 0.947 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> -0.022 </td>
  </tr>
</tbody>
</table>



{% highlight r %}
corrplot(kor)
{% endhighlight %}

<img src="/figures/a4-1.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" style="display: block; margin: auto;" />

{% highlight r %}
# rotacions
acp_rot<-varimax(kor) 
acp_rot$loadings
{% endhighlight %}



{% highlight text %}
## 
## Loadings:
##        PC1    PC2    PC3   
## temp                 -0.997
## hora          -0.872       
## torn_n        -0.867       
## wEnd_n -0.947              
## diaSet  0.946              
## 
##                  PC1   PC2   PC3
## SS loadings    1.792 1.512 1.008
## Proportion Var 0.358 0.302 0.202
## Cumulative Var 0.358 0.661 0.862
{% endhighlight %}
 
### Dendograma  
 

{% highlight r %}
dd <- dist(actives, method = "euclidean") 
hc.ward <- hclust(dd, method = "ward.D2") 
plot(hc.ward, hang = -1, cex = 0.5)
rect.hclust(hc.ward, k=11, border="red")
{% endhighlight %}

<img src="/figures/a5-1.png" title="plot of chunk unnamed-chunk-10" alt="plot of chunk unnamed-chunk-10" style="display: block; margin: auto;" />
 

{% highlight r %}
k_15 <- cutree(hc.ward, k = 11)
 
# assignació de cluster segons hcust
actives$k <- as.factor(k_15)
 
prop <- prop.table(table(k_15))
round(prop,2)
{% endhighlight %}



{% highlight text %}
## k_15
##    1    2    3    4    5    6    7    8    9   10   11 
## 0.08 0.11 0.13 0.08 0.02 0.11 0.11 0.08 0.08 0.18 0.02
{% endhighlight %}



{% highlight r %}
table(k_15)
{% endhighlight %}



{% highlight text %}
## k_15
##    1    2    3    4    5    6    7    8    9   10   11 
## 1914 2521 2909 1738  376 2587 2579 1735 1920 3984  432
{% endhighlight %}
 
### Gràfic Cluster HCLUST
 

{% highlight r %}
actives %>% 
  mutate(k= as.factor(k)) %>% 
  # filter(k %in% c(1,2,3,4)) %>% 
  ggplot()+
    geom_point(aes(x = PC1_dia, y = PC3_temp, col = k))+
    theme_bw()+
    labs(title = "Clusters", x="PC1_dia", y="PC3_temp")+
    theme(legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          panel.grid.major.x =element_blank(),
          title = element_text(color= "dodgerblue3"),
          plot.title = element_text(hjust=0.5))
{% endhighlight %}

<img src="/figures/a6-1.png" title="plot of chunk unnamed-chunk-12" alt="plot of chunk unnamed-chunk-12" style="display: block; margin: auto;" />

{% highlight r %}
# clusters dendograma
actives %>% 
  mutate(k= as.factor(k)) %>% 
  ggplot()+
  geom_point(aes(x = PC2_hora, y = PC3_temp, col = k))+
  theme_bw()+
  labs(title = "Clusters", x="PC2_hora", y="PC3_temp")+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))
{% endhighlight %}

<img src="/figures/a6-2.png" title="plot of chunk unnamed-chunk-12" alt="plot of chunk unnamed-chunk-12" style="display: block; margin: auto;" />
 
### Centres HCLUST
 

{% highlight r %}
cdg <- 
  actives %>% 
  dplyr::group_by(k) %>% 
  dplyr::summarise(c_dia=mean(PC1_dia),
                   c_hora=mean(PC2_hora),
                   c_temp=mean(PC3_temp)) %>% 
  select(-k)
head(cdg)
{% endhighlight %}



{% highlight text %}
## # A tibble: 6 x 3
##    c_dia c_hora c_temp
##    <dbl>  <dbl>  <dbl>
## 1 -1.21  -0.596  0.210
## 2 -1.11   1.66   0.151
## 3 -0.424 -1.16  -0.568
## 4 -0.918 -1.08   1.77 
## 5 -1.39   1.42   2.07 
## 6 -0.374  1.69  -0.379
{% endhighlight %}
 
### KMEANS
 

{% highlight r %}
k_mean <- kmeans(actives[,1:3], centers = cdg)
 
# assignació de cluster segons kmean
actives$kmean <-as.factor(k_mean$cluster)
machine$kmean <-as.factor(k_mean$cluster)
 
#distribució cluster, size
k_mean %>% 
  tidy() %>% 
  select(cluster, size) %>% 
  kable(caption = "Distribuci? clusters") %>% 
  kable_styling(full_width = F)
{% endhighlight %}

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Distribució clusters</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> cluster </th>
   <th style="text-align:right;"> size </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 1700 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 2201 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 2412 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 1344 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 676 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 2607 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 2870 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 2549 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:right;"> 1919 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:right;"> 3927 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:right;"> 490 </td>
  </tr>
</tbody>
</table>
 
### Comparació K-MEANS / HCLUST
 

{% highlight r %}
c_m <- conf_mat(actives, k, kmean, dnn= c("kmean", "hclust"))
 
c_m %>% 
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(hclust, kmean, alpha = n)) +
  geom_tile(show.legend = FALSE, fill = "blue", color = "white") +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 6)+
  labs(title = "Confusion Matrix hclust-kmean" )+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.major.x =element_blank(),
        panel.grid.major.y =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))
{% endhighlight %}

![plot of chunk unnamed-chunk-15](/figures/a7-1.png)

{% highlight r %}
accu <- 
summary(c_m) %>% 
  select(-.estimator) %>%
  filter(.metric %in% c("accuracy"))
 
percent(accu$.estimate)
{% endhighlight %}



{% highlight text %}
## [1] "84.8%"
{% endhighlight %}



{% highlight r %}
# index d'estabilitat 84.84%
{% endhighlight %}
 
### Gràfic Clusters K-MEANS
 

{% highlight r %}
# plot clusters ggplot
actives %>% 
  mutate(k= as.factor(k)) %>% 
  ggplot()+
    geom_point(aes(x = PC1_dia, y = PC3_temp, col = k))+
    theme_bw()+
    labs(title = "Clusters", x="PC1_dia", y="PC3_temp")+
    theme(legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          panel.grid.major.x =element_blank(),
          title = element_text(color= "dodgerblue3"),
          plot.title = element_text(hjust=0.5))
{% endhighlight %}

<img src="/figures/a8-1.png" title="plot of chunk unnamed-chunk-16" alt="plot of chunk unnamed-chunk-16" style="display: block; margin: auto;" />

{% highlight r %}
actives %>% 
  mutate(k= as.factor(k)) %>% 
  ggplot()+
  geom_point(aes(x = PC2_hora, y = PC3_temp, col = k))+
  theme_bw()+
  labs(title = "Clusters", x="PC2_hora", y="PC3_temp")+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))
{% endhighlight %}

<img src="/figures/a8-2.png" title="plot of chunk unnamed-chunk-16" alt="plot of chunk unnamed-chunk-16" style="display: block; margin: auto;" />

### Gràfic 3-D clusters K-MEANS
 
{% highlight r %}
plot_ly(x=actives$PC1_dia, y=actives$PC2_hora, z=actives$PC3_temp, 
        type="scatter3d", mode="markers", color=actives$kmean)
{% endhighlight %}



<img src="/figures/clusters.png" title="clusters" alt="clusters" style="display: block; margin: auto;" />
 
### Centres K-MEANS
 

{% highlight r %}
centroids <- 
  as.data.frame(fitted(k_mean)) %>%  #resum dels centres
  select('PC1_cen' = 'PC1_dia', 'PC2_cen'= "PC2_hora", 'PC3_cen'= "PC3_temp")
head(centroids)
{% endhighlight %}



{% highlight text %}
##       PC1_cen    PC2_cen   PC3_cen
## X1   -1.05906 -0.7869974 0.5792558
## X1.1 -1.05906 -0.7869974 0.5792558
## X1.2 -1.05906 -0.7869974 0.5792558
## X1.3 -1.05906 -0.7869974 0.5792558
## X1.4 -1.05906 -0.7869974 0.5792558
## X1.5 -1.05906 -0.7869974 0.5792558
{% endhighlight %}



{% highlight r %}
#assignaci? centres de clusters
actives$PC1_cen <-  centroids$PC1_cen
actives$PC2_cen <-  centroids$PC2_cen 
actives$PC3_cen <-  centroids$PC3_cen 
{% endhighlight %}
 

{% highlight r %}
# plot clusters amb centres
actives %>% 
  mutate(kmean= as.factor(kmean)) %>% 
  ggplot()+
    geom_point(aes(x = PC1_dia, y = PC3_temp, col = kmean), alpha = 0.5)+
    geom_point(aes(x = PC1_cen, y = PC3_cen), color = "black", shape = 13, size = 5)+
    theme_bw()+
    labs(title = "Clusters dia-temp", x="PC1_dia", y="PC3_temp")+
    theme(legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          panel.grid.major.x =element_blank(),
          title = element_text(color= "dodgerblue3"),
          plot.title = element_text(hjust=0.5))+
    facet_wrap(~kmean)
{% endhighlight %}

<img src="/figures/a10-1.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" style="display: block; margin: auto;" />
 

{% highlight r %}
actives %>% 
  mutate(kmean= as.factor(kmean)) %>% 
  ggplot()+
  geom_point(aes(x = PC2_hora, y = PC3_temp, col = kmean), alpha = 0.5)+
  geom_point(aes(x = PC2_cen, y = PC3_cen), color = "black", shape = 13, size = 5)+
  theme_bw()+
  labs(title = "Clusters hora-temp", x="PC2_hora", y="PC3_temp")+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))+
  facet_wrap(~kmean)
{% endhighlight %}

<img src="/figures/a11-1.png" title="plot of chunk unnamed-chunk-20" alt="plot of chunk unnamed-chunk-20" style="display: block; margin: auto;" />
 
 
### Distàncies
 

{% highlight r %}
# Distancia Euclidea al Centroid 
actives <- 
  actives %>% 
  mutate(dist_c = sqrt( (PC1_dia - PC1_cen)^2 + 
                        (PC2_hora - PC2_cen)^2 + 
                        (PC3_temp - PC3_cen)^2))
 
# detectar outliers de les distancies
anomalies <- 
  actives %>% 
  dplyr::group_by(kmean) %>% 
  dplyr::mutate(iqr = IQR(dist_c),
                q_75 = quantile(dist_c, probs = 0.75),
                out = ifelse(dist_c > q_75 + iqr * 1.5, 1, 0)) %>% 
          
  ungroup()
 
head(anomalies)
{% endhighlight %}



{% highlight text %}
## # A tibble: 6 x 12
##   PC1_dia PC2_hora PC3_temp k     kmean PC1_cen PC2_cen PC3_cen dist_c
##     <dbl>    <dbl>    <dbl> <fct> <fct>   <dbl>   <dbl>   <dbl>  <dbl>
## 1   -1.53    -1.66    0.615 1     1       -1.06  -0.787   0.579  0.993
## 2   -1.53    -1.65    0.546 1     1       -1.06  -0.787   0.579  0.984
## 3   -1.52    -1.64    0.461 1     1       -1.06  -0.787   0.579  0.979
## 4   -1.52    -1.62    0.317 1     1       -1.06  -0.787   0.579  0.988
## 5   -1.51    -1.61    0.232 1     1       -1.06  -0.787   0.579  1.00 
## 6   -1.51    -1.62    0.276 1     1       -1.06  -0.787   0.579  0.995
## # ... with 3 more variables: iqr <dbl>, q_75 <dbl>, out <dbl>
{% endhighlight %}



{% highlight r %}
# total de outliers per cluster
anomalies %>% 
  dplyr::summarise(Total = sum(out),
                   Percentatge = percent(Total / nrow(actives))) %>% 
  kable(caption = "Total Anomalies") %>% 
  kable_styling(full_width = F)
{% endhighlight %}

Total d'anomalies: 331  
Percentatge:       1.46%

{% highlight r %}
# assignar anomalies a la taula actives
actives$out <- as.factor(anomalies$out)
machine$out <- anomalies$out
{% endhighlight %}
 
### Outliers de les Distàncies
 

{% highlight r %}
# dispersió de clusters i possibles outliers
actives %>% 
  mutate(kmean = as.factor(kmean)) %>% 
  ggplot(aes(x = kmean, y=dist_c, color= as.factor(kmean)))+
  geom_boxplot(outlier.shape = NA, varwidth = TRUE)+
  geom_jitter(data = subset(actives, out == 1), aes(color= as.factor(kmean)), 
              alpha = 0.5)+
  theme_minimal()+
  labs(title = "Ditancia Euclidea al Centroid", x=" ", y="")+
  theme(legend.position = "none",
        panel.grid.major.x =element_blank(),
        panel.grid.major.y =element_blank(),
        panel.grid.minor.y =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5),
        axis.text.y = element_blank())
{% endhighlight %}

<img src="/figures/a12-1.png" title="plot of chunk unnamed-chunk-22" alt="plot of chunk unnamed-chunk-22" style="display: block; margin: auto;" />
 
### Clusters amb outliers
 

{% highlight r %}
# plot clusters amb outlir  
actives %>% 
  ggplot()+
  geom_point(aes(x = PC1_dia, y = PC3_temp, col = out), alpha = 0.5)+
  geom_point(aes(x = PC1_cen, y = PC3_cen), color = "black", shape = 13, size = 5)+
  scale_color_manual(values=c("gray70","red"))+
  theme_bw()+
  labs(title = "Clusters dia-temp", x="PC1_dia", y="PC3_temp")+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))+
  facet_wrap(~kmean)
{% endhighlight %}

<img src="/figures/a13-1.png" title="plot of chunk unnamed-chunk-23" alt="plot of chunk unnamed-chunk-23" style="display: block; margin: auto;" />
 

{% highlight r %}
actives %>% 
  ggplot()+
  geom_point(aes(x = PC2_hora, y = PC3_temp, color = out), alpha = 0.4)+
  geom_point(aes(x = PC2_cen, y = PC3_cen), color = "black", shape = 13, size = 5)+
  scale_color_manual(values=c("gray70","red"))+
  theme_bw()+
  labs(title = "Clusters PC2-PC3", x="PC2_hora", y="PC3_temp")+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))+
  facet_wrap(~kmean)
{% endhighlight %}

<img src="/figures/a14-1.png" title="plot of chunk unnamed-chunk-24" alt="plot of chunk unnamed-chunk-24" style="display: block; margin: auto;" />
 
### Gràfic 3D cluster 1 
 

{% highlight r %}
clusty <- 1
mig <- c(k_mean$centers[clusty, ],clusty,2)
 
cluster_sel <- 
actives %>% 
  filter(kmean == clusty) %>% 
  select(PC1_dia, PC2_hora, PC3_temp, kmean, anomal= out) %>%  
  mutate(anomal = fct_expand(anomal, "2")) %>% 
  rbind(mig) %>% 
  mutate(anomal = fct_recode(anomal, normal = "0",
                                     anomalies = "1",
                                     centre = "2"))
 
colors <-  c('#c6cbcc', '#ba072b', '#46c4eb')
plot_ly(cluster_sel, x= ~PC1_dia, y= ~PC2_hora, z= ~PC3_temp, 
        type="scatter3d", mode="markers", color = ~anomal, colors = colors)
{% endhighlight %}



<img src="/figures/cluster_1.png" title="cluster_1" alt="cluster_1" style="display: block; margin: auto;" />
 
 
### Distribució horària de les anomalies
 

{% highlight r %}
# distribució horària de les anomalies
machine %>% 
  filter(out == 1) %>% 
  ggplot(aes(x=hora, fill= grup))+
    geom_histogram(color= "white")+
    theme_bw()+
    labs(title = "Anomalies x hores", x=" ", y="")+
    theme(legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          panel.grid.major.x =element_blank(),
          title = element_text(color= "dodgerblue3"),
          plot.title = element_text(hjust=0.5))+
    facet_wrap(~grup)
{% endhighlight %}

<img src="/figures/a16-1.png" title="plot of chunk unnamed-chunk-26" alt="plot of chunk unnamed-chunk-26" style="display: block; margin: auto;" />
 
### Distribució diaria de les anomalies
 

{% highlight r %}
# distribuci? diaria de les anomalies
machine %>% 
  filter(out == 1) %>% 
  ggplot(aes(x=diaSet, fill= grup))+
  geom_histogram(color= "white")+
  theme_bw()+
  labs(title = "Aanomalies x dies", x=" ", y="")+
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))
{% endhighlight %}

<img src="/figures/a17-1.png" title="plot of chunk unnamed-chunk-27" alt="plot of chunk unnamed-chunk-27" style="display: block; margin: auto;" />
 
### Linia temporal amb anomalies
 

{% highlight r %}
machine %>%
  ggplot(aes(x = timestamp, y=temp), alpha = 0.4)+
  geom_point(color="grey70")+
  geom_line(color="grey70")+
  geom_point(data = subset(machine, out == 1), color = "red", size = 3)+
  theme_minimal()+
  labs(title = "Registre Temperatura", x=" ", y="Temperatura (?C)")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))
{% endhighlight %}

<img src="/figures/a18-1.png" title="plot of chunk unnamed-chunk-28" alt="plot of chunk unnamed-chunk-28" style="display: block; margin: auto;" />
 
#### Per grups: 
 

{% highlight r %}
# per grups
machine %>% 
  ggplot(aes(x = timestamp, y=temp), alpha = 0.4)+
  geom_point(color="grey70")+
  geom_line(color="grey70")+
  geom_point(data = subset(machine, out == 1), color = "red", size = 3)+
  theme_minimal()+
  labs(title = "Registre Temperatura", x=" ", y="Temperatura (?C)")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust=0.5))+
  facet_wrap(~grup)
{% endhighlight %}

<img src="/figures/a19-1.png" title="plot of chunk unnamed-chunk-29" alt="plot of chunk unnamed-chunk-29" style="display: block; margin: auto;" />
 
#### Per dies:
 

{% highlight r %}
# per dies
machine %>% 
  ggplot(aes(x = timestamp, y=temp), alpha = 0.4)+
    geom_point(color="grey70")+
    # geom_line(color="grey70")+
    geom_point(data = subset(machine, out == 1), color = "red", size = 3)+
    theme_bw()+
    labs(title = "Registre Temperatura", x=" ", y="Temperatura (?C)")+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          panel.grid.major.x =element_blank(),
          title = element_text(color= "dodgerblue3"),
          plot.title = element_text(hjust=0.5))+
  facet_wrap(~wday(timestamp, week_start = getOption("lubridate.week.start", 1)), ncol = 2)
{% endhighlight %}

<img src="/figures/a20-1.png" title="plot of chunk unnamed-chunk-30" alt="plot of chunk unnamed-chunk-30" style="display: block; margin: auto;" />
 
#### Tots els dilluns:
 

{% highlight r %}
sel_dia <- 
  machine %>% 
  filter(diaSet == 1) 
  
plt <- 
sel_dia %>% 
  ggplot(aes(x = timestamp, y=temp, text= paste("dia=", diaSet, "\n")), alpha = 0.4)+
  geom_point(color="grey70")+
  geom_point(data = subset(sel_dia, out == 1), color = "red", size = 3 )+
  theme_minimal()
 
ggplotly(plt)
{% endhighlight %}



<img src="/figures/a21-1.png" title="plot of chunk unnamed-chunk-31" alt="plot of chunk unnamed-chunk-31" style="display: block; margin: auto;" />
 
#### Dilluns 16-12-2013:
 

{% highlight r %}
dia_treball <- 
machine %>% 
  filter(diaSet == 1) %>% 
  # filter(jour == "2013-12-16")
  filter(jour == "2013-12-16")
 
dia_treball %>% 
  ggplot(aes(x = timestamp, y=temp), alpha = 0.4)+
  geom_point(color="grey70")+
  geom_line(color="grey70")+
  geom_point(data = subset(dia_treball, out == 1), color = "red", size = 3 )+
  theme_minimal()
{% endhighlight %}

<img src="/figures/a22-1.png" title="plot of chunk unnamed-chunk-32" alt="plot of chunk unnamed-chunk-32" style="display: block; margin: auto;" />
 
 

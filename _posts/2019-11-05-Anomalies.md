---
layout: post  
title: DetecciÃ³ Anomalies  
author: Josep Espluga  
published: true
status: publish
draft: false  
tags: anomalies
---
 

 
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
machine <- read_csv("Machine-Failure/machine_temperature_system_failure.csv")
{% endhighlight %}



{% highlight text %}
## Error: 'Machine-Failure/machine_temperature_system_failure.csv' does not exist in current working directory ('C:/Users/jespl/Documents/R/PROJECTES/jespluga.github.io').
{% endhighlight %}



{% highlight r %}
summary(machine)
{% endhighlight %}



{% highlight text %}
## Error in summary(machine): object 'machine' not found
{% endhighlight %}



{% highlight r %}
machine <- 
  machine %>% 
  mutate(temp = fahrenheit.to.celsius(value, round = 2))
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'machine' not found
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



{% highlight text %}
## Error in ggplot(machine, aes(x = timestamp, y = temp)): object 'machine' not found
{% endhighlight %}
 
### Feature Engineering
 

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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'machine' not found
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



{% highlight text %}
## Error in ggplot(machine, aes(x = timestamp, y = temp, color = grup)): object 'machine' not found
{% endhighlight %}



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



{% highlight text %}
## Error in ggplot(machine, aes(x = temp, fill = grup)): object 'machine' not found
{% endhighlight %}



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



{% highlight text %}
## Error in ggplot(machine, aes(y = temp, fill = grup)): object 'machine' not found
{% endhighlight %}
 
### Transformaci? Dades
 

{% highlight r %}
machine_num <- 
  machine %>% 
  mutate(torn_n = ifelse(torn == "dia", 1, 0),
         wEnd_n = ifelse(wEnd == "SET", 1, 0)) %>% 
  select(temp, hora, torn_n, wEnd_n, diaSet)
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'machine' not found
{% endhighlight %}



{% highlight r %}
head(machine_num)
{% endhighlight %}



{% highlight text %}
## Error in head(machine_num): object 'machine_num' not found
{% endhighlight %}
 
### Principal Components
 

{% highlight r %}
acp <-prcomp(machine_num, center=T, scale.=T) # PCA + estandatitzaciÃ³ de variables
{% endhighlight %}



{% highlight text %}
## Error in prcomp(machine_num, center = T, scale. = T): object 'machine_num' not found
{% endhighlight %}



{% highlight r %}
acp
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'acp' not found
{% endhighlight %}



{% highlight r %}
# eigenvalue: valors superiors a 1, varian?a percentual acumulada
get_eigenvalue(acp)
{% endhighlight %}



{% highlight text %}
## Error in get_eig(X): object 'acp' not found
{% endhighlight %}



{% highlight r %}
# gr?fic varian?a acumulada
fviz_eig(acp, addlabels = TRUE, ylim = c(0, 40))
{% endhighlight %}



{% highlight text %}
## Error in get_eig(X): object 'acp' not found
{% endhighlight %}



{% highlight r %}
actives <- 
  as.data.frame(acp$x[,1:3]) %>% 
  select('PC1_dia' = 'PC1', 'PC2_hora'= "PC2", 'PC3_temp'= "PC3")
{% endhighlight %}



{% highlight text %}
## Error in as.data.frame(acp$x[, 1:3]): object 'acp' not found
{% endhighlight %}
 
 

{% highlight r %}
plot(acp$x[,1], acp$x[,3], xlab="PC1_dia", ylab="PC3_temp")
{% endhighlight %}



{% highlight text %}
## Error in plot(acp$x[, 1], acp$x[, 3], xlab = "PC1_dia", ylab = "PC3_temp"): object 'acp' not found
{% endhighlight %}



{% highlight r %}
abline(h=0,v=0,col="gray60")
{% endhighlight %}



{% highlight text %}
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): plot.new has not been called yet
{% endhighlight %}



{% highlight r %}
plot(acp$x[,2], acp$x[,3], xlab="PC2_hora", ylab="PC3_temp")
{% endhighlight %}



{% highlight text %}
## Error in plot(acp$x[, 2], acp$x[, 3], xlab = "PC2_hora", ylab = "PC3_temp"): object 'acp' not found
{% endhighlight %}



{% highlight r %}
abline(h=0,v=0,col="gray60")
{% endhighlight %}



{% highlight text %}
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): plot.new has not been called yet
{% endhighlight %}
 
### Correlacions
 

{% highlight r %}
kor <- cor(machine_num, acp$x[,1:3])
{% endhighlight %}



{% highlight text %}
## Error in is.data.frame(y): object 'acp' not found
{% endhighlight %}



{% highlight r %}
kor %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", full_width= F))
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'kor' not found
{% endhighlight %}



{% highlight r %}
corrplot(kor)
{% endhighlight %}



{% highlight text %}
## Error in corrplot(kor): object 'kor' not found
{% endhighlight %}



{% highlight r %}
# rotacions
acp_rot<-varimax(kor) 
{% endhighlight %}



{% highlight text %}
## Error in ncol(x): object 'kor' not found
{% endhighlight %}



{% highlight r %}
acp_rot$loadings
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'acp_rot' not found
{% endhighlight %}
 
### Dendograma  
 

{% highlight r %}
dd <- dist(actives, method = "euclidean") 
{% endhighlight %}



{% highlight text %}
## Error in as.matrix(x): object 'actives' not found
{% endhighlight %}



{% highlight r %}
hc.ward <- hclust(dd, method = "ward.D2") 
{% endhighlight %}



{% highlight text %}
## Error in hclust(dd, method = "ward.D2"): object 'dd' not found
{% endhighlight %}



{% highlight r %}
plot(hc.ward, hang = -1, cex = 0.5)
{% endhighlight %}



{% highlight text %}
## Error in plot(hc.ward, hang = -1, cex = 0.5): object 'hc.ward' not found
{% endhighlight %}



{% highlight r %}
rect.hclust(hc.ward, k=11, border="red")
{% endhighlight %}



{% highlight text %}
## Error in rect.hclust(hc.ward, k = 11, border = "red"): object 'hc.ward' not found
{% endhighlight %}
 

{% highlight r %}
k_15 <- cutree(hc.ward, k = 11)
{% endhighlight %}



{% highlight text %}
## Error in nrow(tree$merge): object 'hc.ward' not found
{% endhighlight %}



{% highlight r %}
# assignaci? de cluster segons hcust
actives$k <- as.factor(k_15)
{% endhighlight %}



{% highlight text %}
## Error in is.factor(x): object 'k_15' not found
{% endhighlight %}



{% highlight r %}
prop <- prop.table(table(k_15))
{% endhighlight %}



{% highlight text %}
## Error in table(k_15): object 'k_15' not found
{% endhighlight %}



{% highlight r %}
round(prop,2)
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'prop' not found
{% endhighlight %}



{% highlight r %}
table(k_15)
{% endhighlight %}



{% highlight text %}
## Error in table(k_15): object 'k_15' not found
{% endhighlight %}
 
### Gr?fic Cluster HCLUST
 

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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}



{% highlight r %}
    #facet_wrap(~k)
 
# clusters dendograma
actives %>% 
  mutate(k= as.factor(k)) %>% 
  # filter(k %in% c(1,2,3,4)) %>% 
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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}
 
### Centres HCLUST
 

{% highlight r %}
cdg <- 
  actives %>% 
  dplyr::group_by(k) %>% 
  dplyr::summarise(c_dia=mean(PC1_dia),
                   c_hora=mean(PC2_hora),
                   c_temp=mean(PC3_temp)) %>% 
  select(-k)
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}



{% highlight r %}
head(cdg)
{% endhighlight %}



{% highlight text %}
## Error in head(cdg): object 'cdg' not found
{% endhighlight %}
 
### KMEANS
 

{% highlight r %}
k_mean <- kmeans(actives[,1:3], centers = cdg)
{% endhighlight %}



{% highlight text %}
## Error in as.matrix(x): object 'actives' not found
{% endhighlight %}



{% highlight r %}
# k_mean
# table(k_mean$cluster)
# k_mean$size
# table(actives$k, k_mean$cluster)
 
# assignaci? de cluster segons kmean
actives$kmean <-as.factor(k_mean$cluster)
{% endhighlight %}



{% highlight text %}
## Error in is.factor(x): object 'k_mean' not found
{% endhighlight %}



{% highlight r %}
machine$kmean <-as.factor(k_mean$cluster)
{% endhighlight %}



{% highlight text %}
## Error in is.factor(x): object 'k_mean' not found
{% endhighlight %}



{% highlight r %}
#distribuci? cluster, size
k_mean %>% 
  tidy() %>% 
  select(cluster, size) %>% 
  kable(caption = "Distribuci? clusters") %>% 
  kable_styling(full_width = F)
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'k_mean' not found
{% endhighlight %}
 
### Comparaci? K-MEANS / HCLUST
 

{% highlight r %}
c_m <- conf_mat(actives, k, kmean, dnn= c("kmean", "hclust"))
{% endhighlight %}



{% highlight text %}
## Error in conf_mat(actives, k, kmean, dnn = c("kmean", "hclust")): object 'actives' not found
{% endhighlight %}



{% highlight r %}
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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'c_m' not found
{% endhighlight %}



{% highlight r %}
accu <- 
summary(c_m) %>% 
  select(-.estimator) %>%
  filter(.metric %in% c("accuracy"))
{% endhighlight %}



{% highlight text %}
## Error in summary(c_m): object 'c_m' not found
{% endhighlight %}



{% highlight r %}
percent(accu$.estimate)
{% endhighlight %}



{% highlight text %}
## Error in number(x = x, accuracy = accuracy, scale = scale, prefix = prefix, : object 'accu' not found
{% endhighlight %}



{% highlight r %}
# index d'estabilitat 84.84%
{% endhighlight %}
 
### Gr?fic Clusters K-MEANS
 

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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}



{% highlight r %}
    #facet_wrap(~k)
 
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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}



{% highlight r %}
#facet_wrap(~k)
{% endhighlight %}
 
 
### Gr?fic 3-D clusters K-MEANS
 

{% highlight r %}
plot_ly(x=actives$PC1_dia, y=actives$PC2_hora, z=actives$PC3_temp, type="scatter3d", mode="markers", color=actives$kmean)
{% endhighlight %}



{% highlight text %}
## Error in plot_ly(x = actives$PC1_dia, y = actives$PC2_hora, z = actives$PC3_temp, : object 'actives' not found
{% endhighlight %}
 
### Centres K-MEANS
 

{% highlight r %}
# tots els centres de cada punt
# k_mean$centers[k_mean$cluster, ]
# fitted(k_mean)
# head(k_mean$centers)
 
 
centroids <- 
  as.data.frame(fitted(k_mean)) %>%  #resum dels centres
  select('PC1_cen' = 'PC1_dia', 'PC2_cen'= "PC2_hora", 'PC3_cen'= "PC3_temp")
{% endhighlight %}



{% highlight text %}
## Error in fitted(k_mean): object 'k_mean' not found
{% endhighlight %}



{% highlight r %}
head(centroids)
{% endhighlight %}



{% highlight text %}
## Error in head(centroids): object 'centroids' not found
{% endhighlight %}



{% highlight r %}
#assignaci? centres de clusters
actives$PC1_cen <-  centroids$PC1_cen
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'centroids' not found
{% endhighlight %}



{% highlight r %}
actives$PC2_cen <-  centroids$PC2_cen 
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'centroids' not found
{% endhighlight %}



{% highlight r %}
actives$PC3_cen <-  centroids$PC3_cen 
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'centroids' not found
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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}
 

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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}
 
 
### Dist?ncies
 

{% highlight r %}
# Distancia Euclidea al Centroid 
actives <- 
  actives %>% 
  mutate(dist_c = sqrt( (PC1_dia - PC1_cen)^2 + (PC2_hora - PC2_cen)^2 + (PC3_temp - PC3_cen)^2))
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}



{% highlight r %}
# detectar outliers de les distancies
anomalies <- 
  actives %>% 
  dplyr::group_by(kmean) %>% 
  dplyr::mutate(iqr = IQR(dist_c),
                q_75 = quantile(dist_c, probs = 0.75),
                out = ifelse(dist_c > q_75 + iqr * 1.5, 1, 0)) %>% 
          
  ungroup()
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}



{% highlight r %}
head(anomalies)
{% endhighlight %}



{% highlight text %}
## Error in head(anomalies): object 'anomalies' not found
{% endhighlight %}



{% highlight r %}
# total de outliers per cluster
anomalies %>% 
  dplyr::summarise(Total = sum(out),
                   Percentatge = percent(Total / nrow(actives))) %>% 
  kable(caption = "Total Anomalies") %>% 
  kable_styling(full_width = F)
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'anomalies' not found
{% endhighlight %}



{% highlight r %}
# assignar anomalies a la taula actives
actives$out <- as.factor(anomalies$out)
{% endhighlight %}



{% highlight text %}
## Error in is.factor(x): object 'anomalies' not found
{% endhighlight %}



{% highlight r %}
machine$out <- anomalies$out
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'anomalies' not found
{% endhighlight %}
 
### Outliers de les Dist?ncies
 

{% highlight r %}
# dispersi? de clusters i possibles outliers
actives %>% 
  mutate(kmean = as.factor(kmean)) %>% 
  ggplot(aes(x = kmean, y=dist_c, color= as.factor(kmean)))+
  geom_boxplot(outlier.shape = NA, varwidth = TRUE)+
  geom_jitter(data = subset(actives, out == 1), aes(color= as.factor(kmean)), alpha = 0.5)+
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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}
 
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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}
 

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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}
 
### Gr?fic 3D cluster 1 
 

{% highlight r %}
clusty <- 1
mig <- c(k_mean$centers[clusty, ],clusty,2)
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'k_mean' not found
{% endhighlight %}



{% highlight r %}
cluster_sel <- 
actives %>% 
  filter(kmean == clusty) %>% 
  select(PC1_dia, PC2_hora, PC3_temp, kmean, anomal= out) %>%  
  mutate(anomal = fct_expand(anomal, "2")) %>% 
  rbind(mig) %>% 
  mutate(anomal = fct_recode(anomal, normal = "0",
                                     anomalies = "1",
                                     centre = "2"))
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'actives' not found
{% endhighlight %}



{% highlight r %}
colors <-  c('#c6cbcc', '#ba072b', '#46c4eb')
plot_ly(cluster_sel, x= ~PC1_dia, y= ~PC2_hora, z= ~PC3_temp, 
        type="scatter3d", mode="markers", color = ~anomal, colors = colors)
{% endhighlight %}



{% highlight text %}
## Error in is.data.frame(data): object 'cluster_sel' not found
{% endhighlight %}
 
 
### Distribuci? horaria de les anomalies
 

{% highlight r %}
# distribuci? horaria de les anomalies
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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'machine' not found
{% endhighlight %}
 
### Distribuci? diaria de les anomalies
 

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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'machine' not found
{% endhighlight %}
 
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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'machine' not found
{% endhighlight %}
 
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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'machine' not found
{% endhighlight %}
 
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



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'machine' not found
{% endhighlight %}
 
#### Tots els dilluns:
 

{% highlight r %}
sel_dia <- 
  machine %>% 
  filter(diaSet == 1) 
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'machine' not found
{% endhighlight %}



{% highlight r %}
plt <- 
sel_dia %>% 
  ggplot(aes(x = timestamp, y=temp, text= paste("dia=", diaSet, "\n")), alpha = 0.4)+
  geom_point(color="grey70")+
  geom_point(data = subset(sel_dia, out == 1), color = "red", size = 3 )+
  theme_minimal()
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'sel_dia' not found
{% endhighlight %}



{% highlight r %}
ggplotly(plt)
{% endhighlight %}



{% highlight text %}
## Error in ggplotly(plt): object 'plt' not found
{% endhighlight %}
 
#### Dilluns 16-12-2013:
 

{% highlight r %}
dia_treball <- 
machine %>% 
  filter(diaSet == 1) %>% 
  # filter(jour == "2013-12-16")
  filter(jour == "2013-12-16")
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'machine' not found
{% endhighlight %}



{% highlight r %}
dia_treball %>% 
  ggplot(aes(x = timestamp, y=temp), alpha = 0.4)+
  geom_point(color="grey70")+
  geom_line(color="grey70")+
  geom_point(data = subset(dia_treball, out == 1), color = "red", size = 3 )+
  theme_minimal()
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'dia_treball' not found
{% endhighlight %}
 

{% highlight r %}
saveRDS(actives, file = "actives.rds")
{% endhighlight %}



{% highlight text %}
## Error in saveRDS(actives, file = "actives.rds"): object 'actives' not found
{% endhighlight %}



{% highlight r %}
saveRDS(k_mean$centers, file = "centres.rds")
{% endhighlight %}



{% highlight text %}
## Error in saveRDS(k_mean$centers, file = "centres.rds"): object 'k_mean' not found
{% endhighlight %}



{% highlight r %}
saveRDS(machine, file = "machine.rds")
{% endhighlight %}



{% highlight text %}
## Error in saveRDS(machine, file = "machine.rds"): object 'machine' not found
{% endhighlight %}
 

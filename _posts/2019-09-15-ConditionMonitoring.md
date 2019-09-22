---
layout: post  
title: Condition Monitoring Analysis  
author: Josep Espluga  
published: true
status: publish
draft: false  
tags: industrial  
---
 

 
***
 
  
 
### **Introducció**
 
La possibilitat de descobrir i facilitar la interpretació de quins son els diferents estats de funcionament d'un equipament, a través de l'anàlisi de les sèries de dades registrades per diferents sensors, ha de permetre obtenir un model no supervisat de segmentació de condicions de treball que ens permeti detectar anomalies i predir l'estat de funcionament per tal de facilitar la programació del manteniment preventiu, minimitzar possibles parades i reduir costos de situacions inesperades de manteniment.  
 
### **Dades**  
 
Disposem d'un equipament elèctric-pneumàtic on es monitoritza el consum i la pressió de l'aire comprimit durant tots els cicles de màquina per tal de detectar canvis de condició de funcionament.  
Cicles de 2.6 segons amb 8 moviments de cilindres pneumàtics de petit tamany i registres cada 0.5 segons de les dues variables, durant 15 minuts.    
Analitzem dues sèreis diferents d'observacions. Una sèrie amb condicions normals de funcinament i una sèrie on simulem una fuita d'aire.    
El format dels registres és el següent:  
 

{% highlight r %}
# Condicions de treball. OK
x <- scan("LOGFILE_health.log",what=character(), skip = 1, skipNul = TRUE)
x1 <- data.frame(x, stringsAsFactors = FALSE)
df_1 <- separate(x1, col = x, into = c("hora","cabal","pressio"), 
                 sep = ",", convert = TRUE)
 
df_ok <- 
df_1 %>% 
  mutate(cabal = ifelse(cabal<0, 0, cabal),
         temps = c(1:nrow(df_1)))
 
 
# Condicions de treball: fuites
y <- scan("LOGFILE_ill.log",what=character(), skip = 1, skipNul = TRUE)
y1 <- data.frame(y, stringsAsFactors = FALSE)
df_2 <- separate(y1, col = y, into = c("hora","cabal","pressio"), 
                 sep = ",", convert = TRUE)
 
df_fug <- 
df_2 %>% 
  mutate(cabal = ifelse(cabal<0, 0, cabal),
         temps = c(1:nrow(df_2)))
 
df_fug %>% 
  select(-temps) %>% 
  rename("pressio" = "pressio") %>% 
  head() %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> hora </th>
   <th style="text-align:center;"> cabal </th>
   <th style="text-align:center;"> pressio </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 10:11:05 </td>
   <td style="text-align:center;"> 0.0 </td>
   <td style="text-align:center;"> 5.8 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 10:11:06 </td>
   <td style="text-align:center;"> 0.0 </td>
   <td style="text-align:center;"> 5.8 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 10:11:06 </td>
   <td style="text-align:center;"> 109 </td>
   <td style="text-align:center;"> 5.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 10:11:07 </td>
   <td style="text-align:center;"> 108 </td>
   <td style="text-align:center;"> 4.9 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 10:11:07 </td>
   <td style="text-align:center;"> 57 </td>
   <td style="text-align:center;"> 4.6 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 10:11:08 </td>
   <td style="text-align:center;"> 55 </td>
   <td style="text-align:center;"> 5.0 </td>
  </tr>
</tbody>
</table>
 
### **Descriptius estadístics**  
 

{% highlight r %}
hist_ok <- 
df_ok %>% 
  select(pressio, cabal) %>% 
  gather() %>%  
  ggplot(aes(x= value))+
  geom_histogram(fill= "dodgerblue3", color="white")+
  facet_wrap(~key, scales = "free")+
  theme_minimal()+
  theme(title = element_text(color= "dodgerblue3"))+
  labs(title = "Condicions Treball: OK", x="", y="")
 
hist_fui <- 
df_fug %>% 
  select(pressio, cabal) %>% 
  gather() %>%  
  ggplot(aes(x= value))+
  geom_histogram(fill= "orange", color="white")+
  facet_wrap(~key, scales = "free")+
  theme_minimal()+
  theme(title = element_text(color= "orange"))+
  labs(title = "Condicions Treball: fuites", x="", y="")
 
plot_grid(hist_ok, hist_fui, labels = "", ncol=1)
{% endhighlight %}

<img src="/figures/unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />
 

{% highlight r %}
p_dens_q <- 
ggplot()+
  geom_density(data = df_ok, aes(x = cabal), fill = "dodgerblue3", alpha= 0.5)+
  geom_density(data = df_fug, aes(x = cabal), fill= "orange", alpha= 0.5)+
  theme_minimal()+
  theme(title = element_text(color= "grey30"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Cabal", x="", y="")
 
p_dens_p <-
ggplot()+
  geom_density(data = df_ok, aes(pressio), fill= "dodgerblue3", alpha= 0.5)+
  geom_density(data = df_fug, aes(pressio), fill= "orange", alpha= 0.5 )+
  theme_minimal()+
  theme(title = element_text(color= "grey30"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Pressio", x="", y="")
 
plot_grid(p_dens_q, p_dens_p, ncol = 2)
{% endhighlight %}

<img src="/figures/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />
 

{% highlight r %}
df_ok %>% 
  slice(1:1742) %>% 
  select(Cabal_OK = cabal, Pressio_OK = pressio) %>% 
  mutate(Cabal_fuites = df_fug$cabal,
         Pressio_fuites = df_fug$pressio) %>% 
  rename("Pressio_OK" = Pressio_OK, "Pressio_fuites" = Pressio_fuites) %>% 
  map_df(~(data.frame(min = min(.x),
                      max = max(.x),
                      mean = mean(.x),
                      sd = sd(.x),
                      med = median(.x))),
         .id= "Variable") %>% 
  arrange(factor(Variable, 
                 levels= c("Cabal_OK", "Cabal_fuites", "Pressio_OK", "Pressio_fuites"))) %>% 
  mutate_if(is.numeric, format, digits= 3) %>% 
  kable(caption = "Estadistics:") %>% 
  kable_styling(bootstrap_options = c("condensed"), full_width = F)
{% endhighlight %}

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Estadistics:</caption>
 <thead>
  <tr>
   <th style="text-align:center;"> Variable </th>
   <th style="text-align:center;"> min </th>
   <th style="text-align:center;"> max </th>
   <th style="text-align:center;"> mean </th>
   <th style="text-align:center;"> sd </th>
   <th style="text-align:center;"> med </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> Cabal_OK </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 203 </td>
   <td style="text-align:center;"> 63 </td>
   <td style="text-align:center;"> 54 </td>
   <td style="text-align:center;"> 48 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Cabal_fuites </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 207 </td>
   <td style="text-align:center;"> 66 </td>
   <td style="text-align:center;"> 54 </td>
   <td style="text-align:center;"> 55 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Pressio_OK </td>
   <td style="text-align:center;"> 4.4 </td>
   <td style="text-align:center;"> 5.8 </td>
   <td style="text-align:center;"> 5.0 </td>
   <td style="text-align:center;"> 0.2 </td>
   <td style="text-align:center;"> 5.0 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Pressio_fuites </td>
   <td style="text-align:center;"> 4.4 </td>
   <td style="text-align:center;"> 5.8 </td>
   <td style="text-align:center;"> 4.9 </td>
   <td style="text-align:center;"> 0.2 </td>
   <td style="text-align:center;"> 4.9 </td>
  </tr>
</tbody>
</table>
 
Les dues séries presenten valors molt poc diferenciats, aspecte que dificulta una segmentació de diferents condicions de funcionament.  
 
### **Séries Temporals** 
 
Convertim les observacions en una série sequencial i ordenada en el temps.   
Aquesta seqüència de dades ordenades i equidistants cronologicament, mostra l'estat de la instal.lació, en referència al consum i pressió observables en diferents cicles de funcionament de màquina.
 
#### Cabal
 

{% highlight r %}
cabal_ok <-  
  ggplot(df_ok, aes(x = temps, y=cabal))+
    geom_line(color="grey30")+
    theme_minimal()+
    labs(title = "CABAL Condicions treball: OK", x="temps", y="cabal (l/min)")+
    theme(#axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          panel.grid.major.x =element_blank(),
          title = element_text(color= "dodgerblue3"))
 
cabal_ko <-  
  ggplot(df_fug, aes(x = temps, y=cabal))+
    geom_line(color="grey30")+
    theme_minimal()+
    labs(title = "CABAL Condicions treball: fuites", x="temps", y="cabal (l/min)")+
    theme(#axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          panel.grid.major.x =element_blank(),
          title = element_text(color= "orange"))
 
plot_grid(cabal_ok,cabal_ko, labels = "", ncol=1)
{% endhighlight %}

<img src="/figures/unnamed-chunk-6-1.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" style="display: block; margin: auto;" />
 
#### Pressió
 

{% highlight r %}
pressio_ok <-  
  ggplot(df_ok, aes(x = temps, y=pressio))+
    geom_line(color="grey30")+
    theme_minimal()+
    labs(title = "PRESSIOâ€œ Condicions treball: OK", x="temps", y="pressio (bar)")+
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          panel.grid.major.x =element_blank(),
          title = element_text(color= "dodgerblue3"))
 
pressio_ko <-  
  ggplot(df_fug, aes(x = temps, y=pressio))+
  geom_line(color="grey30")+
  theme_minimal()+
  labs(title = "PRESSIOâ€œ Condicions treball: fuites", x="temps", y="pressio (bar)")+
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "orange"))
  
plot_grid(pressio_ok,pressio_ko, labels = "", ncol=1)
{% endhighlight %}

<img src="/figures/unnamed-chunk-7-1.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" style="display: block; margin: auto;" />
 
 
### **Descomposició Séries Temporals**   
 
Anàlisi de les séries temporals des del punt de vista de les seves components estructurals:  
<center>  
  
Tendència + Efecte Estacional + Residus  
  
</center>
D'aquesta descomposició ens interessa especialment la informació de la tendència de les variables analitzades.    
 
#### Cabal  
 

{% highlight r %}
ts_qh <- ts(df_ok$cabal, frequency = 120)
 
d_ts_qh <- decompose(ts_qh, type = "additive")
# plot(d_ts_qh)
 
d_ts_qh %>% 
  augment() %>% 
  rename("Estacional" = .seasonal,
         "Trend" = .trend,
         "Random" = .remainder,
         "Data" = .seasadj) %>% 
  gather(variable, valor) %>% 
  mutate(temps = as.numeric(rownames(.))) %>% 
  ggplot(aes(x= temps, y= valor))+
  geom_line(aes(color=as.factor(variable)))+
  theme_minimal()+
  theme(legend.position = "none",
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Condicions Treball: OK", x="", y="")+
  facet_wrap(~variable, scales="free", ncol = 1)
{% endhighlight %}

<img src="/figures/unnamed-chunk-8-1.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />
 
 

{% highlight r %}
ts_qi <- ts(df_fug$cabal, frequency = 120)
 
d_ts_qi <- decompose(ts_qi, type = "additive")
# plot(d_ts_qi)
 
d_ts_qi %>% 
  augment() %>% 
  rename("Estacional" = .seasonal,
         "Trend" = .trend,
         "Random" = .remainder,
         "Data" = .seasadj) %>% 
  gather(variable, valor) %>% 
  mutate(temps = as.numeric(rownames(.))) %>% 
  ggplot(aes(x= temps, y= valor))+
  geom_line(aes(color=as.factor(variable)))+
  theme_minimal()+
  theme(legend.position = "none",
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Condicions Treball: fuites", x="", y="")+
  facet_wrap(~variable, scales="free", ncol = 1)
{% endhighlight %}

<img src="/figures/unnamed-chunk-9-1.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" style="display: block; margin: auto;" />
 
#### Pressió
 

{% highlight r %}
ts_ph <- ts(df_ok$pressio, frequency = 120)
 
d_ts_ph <- decompose(ts_ph, type = "additive")
# plot(d_ts_ph)
 
d_ts_ph %>% 
  augment() %>% 
  rename("Estacional" = .seasonal,
         "Trend" = .trend,
         "Random" = .remainder,
         "Data" = .seasadj) %>% 
  gather(variable, valor) %>% 
  mutate(temps = as.numeric(rownames(.))) %>% 
  ggplot(aes(x= temps, y= valor))+
  geom_line(aes(color=as.factor(variable)))+
  theme_minimal()+
  theme(legend.position = "none",
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Condicions Treball: OK", x="", y="")+
  facet_wrap(~variable, scales="free", ncol = 1)
{% endhighlight %}

<img src="/figures/unnamed-chunk-10-1.png" title="plot of chunk unnamed-chunk-10" alt="plot of chunk unnamed-chunk-10" style="display: block; margin: auto;" />
 
   

{% highlight r %}
ts_pi <- ts(df_fug$pressio, frequency = 120)
 
d_ts_pi <- decompose(ts_pi, type = "additive")
# plot(d_ts_pi)
 
d_ts_pi %>% 
  augment() %>% 
  rename("Estacional" = .seasonal,
         "Trend" = .trend,
         "Random" = .remainder,
         "Data" = .seasadj) %>% 
  gather(variable, valor) %>% 
  mutate(temps = as.numeric(rownames(.))) %>% 
  ggplot(aes(x= temps, y= valor))+
  geom_line(aes(color=as.factor(variable)))+
  theme_minimal()+
  theme(legend.position = "none",
        title = element_text(color= "dodgerblue3"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Condicions Treball: fuites", x="", y="")+
  facet_wrap(~variable, scales="free", ncol = 1)
{% endhighlight %}

<img src="/figures/unnamed-chunk-11-1.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" style="display: block; margin: auto;" />
 
### **Changepoint Detection Algorithms** 
 
Algoritme de detecció de variacions sobtades en una serie temporal.  
Aquests canvis poden representar transicions entre estats de condició de treball.  
 
#### Cabal   
 

{% highlight r %}
penalty_val_q <- 25000
 
cptm_qh <- cpt.mean(ts_qh, penalty = "Manual", 
                           pen.value = penalty_val_q, method = "PELT")
 
cptm_qi <- cpt.mean(ts_qi, penalty = "Manual", 
                           pen.value = penalty_val_q, method = "PELT")
 
par(mfrow = c(2,1))
plot(cptm_qh, col= "grey", main= "Cabal. Monitoritzacio Condicions: OK", 
              col.main= "dodgerblue3")
plot(cptm_qi, col= "grey", main= "Cabal. Monitoritzacio Condicions: fuites", 
              col.main= "orange")
{% endhighlight %}

<img src="/figures/unnamed-chunk-12-1.png" title="plot of chunk unnamed-chunk-12" alt="plot of chunk unnamed-chunk-12" style="display: block; margin: auto;" />
 

 
Punt Canvi CABAL 1: **493**    
Punt Canvi CABAL 2: **1211**  
Mitjanes segments :     **64.5360248, 164.4265375, 72.0762343, 159.8938775, 60.3427641**
 
#### Pressió 
 

{% highlight r %}
penalty_val_p <- 2
cptm_ph <- cpt.mean(ts_ph, penalty = "Manual", 
                           pen.value = penalty_val_p, method = "PELT")
 
cptm_pi <- cpt.mean(ts_pi, penalty = "Manual", 
                           pen.value = penalty_val_p, method = "PELT")
 
par(mfrow = c(2,1))
plot(cptm_ph, col= "grey", main= "PressiÃ³. Monitoritzacio Condicions: OK", 
              col.main= "dodgerblue3")
plot(cptm_pi, col= "grey", main= "PressiÃ³. Monitoritzacio Condicions: fuites", 
              col.main= "orange")
{% endhighlight %}

<img src="/figures/unnamed-chunk-14-1.png" title="plot of chunk unnamed-chunk-14" alt="plot of chunk unnamed-chunk-14" style="display: block; margin: auto;" />
 

 
Punt Canvi PRESSIÓ 1: **583**  
Punt Canvi PRESSIÓ 2: **1367**  
Mitjanes segments   : **5.0419655, 4.8692441, 5.1152011**  
 
### **Anàisi Tendència** 
 

{% highlight r %}
# Decompose: Cabal
ts_qh.mstl <- mstl(ts_qh)
tend_qh <- as.data.frame(ts_qh.mstl)
df_ok$tend_qh <- tend_qh$Trend
 
ts_qi.mstl <- mstl(ts_qi)
tend_qi <- as.data.frame(ts_qi.mstl)
df_fug$tend_qi <- tend_qi$Trend
 
# Decompose: PressiÃ³
ts_ph.mstl <- mstl(ts_ph)
tend_ph <- as.data.frame(ts_ph.mstl)
df_ok$tend_ph <- tend_ph$Trend
 
ts_pi.mstl <- mstl(ts_pi)
tend_pi <- as.data.frame(ts_pi.mstl)
df_fug$tend_pi <- tend_pi$Trend
{% endhighlight %}
 
#### Cabal  
 

{% highlight r %}
ggplot()+
  geom_line(data = df_ok, aes(x = temps, y = tend_qh), color = "#00AFBB")+
  geom_line(data = df_fug, aes(x = temps, y=tend_qi), color = "#E7B800")+
  theme_minimal()+
  labs(title = "CABAL", x="temps", y="cabal (l/min)")+
  theme(#axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x =element_blank(),
    title = element_text(color= "gray30"),
    plot.title = element_text(hjust = 0.5))+
    annotate("text", x= 1525, y= 71, label= "Serie OK", hjust= 0, 
             size= 4, color= "#00AFBB")+
    annotate("text", x= 890, y= 81, label= "Serie fuites", hjust= 0, 
             size= 4, color= "#E7B800")
{% endhighlight %}

<img src="/figures/unnamed-chunk-17-1.png" title="plot of chunk unnamed-chunk-17" alt="plot of chunk unnamed-chunk-17" style="display: block; margin: auto;" />
 
 
#### Pressió   
 

{% highlight r %}
ggplot()+
  geom_line(data = df_ok, aes(x = temps, y = tend_ph), color = "#00AFBB")+
  geom_line(data = df_fug, aes(x = temps, y=tend_pi), color = "#E7B800")+
  theme_minimal()+
  labs(title = "PRESSIO", x="temps", y="pressiÃ³ (bar)")+
  theme(#axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x =element_blank(),
    title = element_text(color= "gray30"),
    plot.title = element_text(hjust = 0.5))+
    annotate("text", x= 725, y= 5.1, label= "Serie OK", hjust= 0, 
            size= 4, color= "#00AFBB")+
    annotate("text", x= 750, y= 4.9, label= "Serie fuites", hjust= 0, 
            size= 4, color= "orange")
{% endhighlight %}

<img src="/figures/unnamed-chunk-18-1.png" title="plot of chunk unnamed-chunk-18" alt="plot of chunk unnamed-chunk-18" style="display: block; margin: auto;" />
 
 
### **Interdependència** 
 
Un canvi de les condicions de treball pot suposar un canvi de les dependències entre els diferents sensors.  
Correlacionar la situació de canvi de les dues variables amplifica els punts d'anomalia a partir de les gràfiques de tendència.  
Gràfiques dels valors escalats de la tendència de pressió i cabal de les dues séries.  
 
#### OK
 

{% highlight r %}
df_ok %>% 
  select(tend_qh, tend_ph) %>% 
  scale() %>% 
  as.data.frame() %>% 
  mutate(t= df_ok$temps) %>%
  ggplot()+
  geom_line(aes(x= t, y= tend_qh), color = "blue") +
  geom_line(aes(x= t, y= tend_ph), color = "cyan3") +
  geom_hline(yintercept=0, linetype="dashed", color="gray30", size= 1)+
  theme_minimal()+
  annotate("text", x= 1450, y= -1, label= "Cabal", hjust= 0, 
        size= 5, color= "blue")+
  annotate("text", x= 1600, y= 0.5, label= "Pressio", hjust= 0, 
        size= 5, color= "cyan3")+
  labs(title = "Condicions Treball: OK", x="", y="")+
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "blue"),
        plot.title = element_text(hjust = 0.5))
{% endhighlight %}

<img src="/figures/unnamed-chunk-19-1.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" style="display: block; margin: auto;" />
 
#### Fuites
 

{% highlight r %}
df_fug %>% 
  select(tend_qi, tend_pi) %>% 
  scale() %>% 
  as.data.frame() %>% 
  mutate(t= df_fug$temps) %>%
  ggplot()+
  geom_line(aes(x= t, y= tend_qi), color = "blue") +
  geom_line(aes(x= t, y= tend_pi), color = "cyan3") +
  geom_hline(yintercept=0, linetype="dashed", color="gray30", size= 1)+
  theme_minimal()+
  annotate("text", x= 1450, y= -0.2, label= "Cabal", hjust= 0, 
        size= 5, color= "blue")+
  annotate("text", x= 1450, y= 1.8, label= "Pressio", hjust= 0, 
        size= 5, color= "cyan3")+
  annotate("text", x= 800, y= 0.5, label= "Anomalia", hjust= 0, 
        size= 5, color= "firebrick3")+
  annotate("rect", xmin = 565, xmax = 1215, ymin = -2, ymax = 2.5, 
        fill= "red", alpha= 0.2)+
  labs(title = "Condicions Treball: fuites", x="", y="")+
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x =element_blank(),
        title = element_text(color= "orange"),
        plot.title = element_text(hjust = 0.5))
{% endhighlight %}

<img src="/figures/unnamed-chunk-20-1.png" title="plot of chunk unnamed-chunk-20" alt="plot of chunk unnamed-chunk-20" style="display: block; margin: auto;" />
 

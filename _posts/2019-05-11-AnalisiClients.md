---
layout: post
title: Anàlisi Clients
author: Josep Espluga
published: true
status: publish
draft: false
tags: myblog
---
 
***  
 
### **Anàlisi Descriptiu**
 
L'anàlisi descriptiu ens ha de permetre conèixer la tipologia actual dels clients.  
Discretitzem les variables numèriques per facilitar l'interpretabilitat.  
  

 

 
 

 

 
 

 

 

 

 
&nbsp;  
 
<img src="jEspluga.github.io/figures/unnamed-chunk-8-1.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />
 
* #### **Sexe**
 
Diferència entre homes i dones del 12% (24 persones d'una mostra de 200). 
 
* #### **Edat**
 
Un 30% dels clients estan entre els 26-35 anys.  
La resta de franges d'edat està molt igualada. Amb un descens natural en la última franja d'edat.   
Entre els 26 i els 55 anys (66,5%) el percentatge de dones és superior a la diferència global homes-dones (81 dones - 52 homes). 
Al voltant dels 50 anys hi ha un augment d'activitat en les dones  i a partir dels 55 anys és millor la participació d'homes.  
  
&nbsp;  
 
<img src="jEspluga.github.io/figures/unnamed-chunk-9-1.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" style="display: block; margin: auto;" />
 
* #### **Ingressos**  
 
La franja més important de nivells d'ingressos es troba entre els  60 i $90.000.  
Entre els 30 i $90.000 acumulem el 73% dels enquestats.
Entre els 0 i $90.000 dolars tenim el 89% dels enquestats.  
Les rendes son molt homogenies entre els dos sexes.  
Les rendes altes son poc significatives.  
 
* #### **Score Despesa**  
 
El 37,5% dels clients estan en un ranking 'average'.  
La segona posició amb un 18% correspon a un ranking 'baix'.  
La resta estan molt equilibrats.  
La suma dels rankings top + alt suposen un 31%.
Podriem parlar d'un 31% de bons clients, un 37% de normals i un 31 de dolents.
Existeix un cert equilibri homes-dones excepte al ranking 'mig-baix' on predominen les dones. 
  
  
***
 
### **Anàlisi Relacional**  
 
&nbsp; 
 
  

 

 
<img src="jEspluga.github.io/figures/unnamed-chunk-12-1.png" title="plot of chunk unnamed-chunk-12" alt="plot of chunk unnamed-chunk-12" style="display: block; margin: auto;" />
 
* #### **Ingressos - Score**  
  
Gràfica que relaciona els ingressos amb la classificació del client.
El tamay del punt ens indica l'edat i el color refereix sexe.  
  
Aquesta classificació ens permet intuir una certa distribució, a la vegada que ens dona dades del perfil de client de cada grup:  
- S' insinuen 5 grups diferenciats  
- No hi ha una clara distinció determinada pel sexe  
- Per ordre de valoració de client:  
    - clients TOP 1: perfil de baix ingrès, els méss joves. Valoració positiva probablement pel volum de compres i no per l'import.  
    - clients TOP 2: perfil ingressos superiors a la mitjana, edat al voltant dels 40 anys  
    - clients PROMIG: clients amb valoració 50 de 100 i centrats segons nivell d'ingressos. Edat al voltant dels 50 anys  
    

 
***  
 
### **Anàlisi Cluster** 
 
Per confirmar la segmentació intuida visualment gràcies a la representació gràfica, realitzem un anàlisi cluster per
agrupar automàticament les observacions de la mostra.  
 
 

 

 
 

 
 
<img src="jEspluga.github.io/figures/unnamed-chunk-17-1.png" title="plot of chunk unnamed-chunk-17" alt="plot of chunk unnamed-chunk-17" style="display: block; margin: auto;" />
 
* #### **Resum per Clusters** 
    

  

  

 
<img src="jEspluga.github.io/figures/unnamed-chunk-21-1.png" title="plot of chunk unnamed-chunk-21" alt="plot of chunk unnamed-chunk-21" style="display: block; margin: auto;" />
 

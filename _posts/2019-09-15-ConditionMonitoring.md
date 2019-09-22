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
 

 
### **IntroducciÃ³**
 
La possibilitat de descobrir i facilitar la interpretaciÃ³ de quins son els diferents estats de funcionament d'un equipament, a travÃ©s de l'anÃ l.lisi de les sÃ¨ries de dades registrades per diferents sensors, ha de permetre obtenir un model no supervisat de segmentaciÃ³ de condicions de treball que ens permeti detectar anomalies i predir l'estat de funcionament per tal de facilitar la programaciÃ³ del manteniment preventiu, minimitzar possibles parades i reduir costos de situacions inesperades de manteniment.  
 
### **Dades**  
 
Disposem d'un equipament elÃ¨ctric-pneumÃ tic on es monitoritza el consum i la pressiÃ³ de l'aire comprimit durant tots els cicles de mÃ quina per tal de detectar canvis de condiciÃ³ de funcionament.  
Cicles de 2.6 segons amb 8 moviments de cilindres pneumÃ tics de petit tamany i registres cada 0.5 segons de les dues variables, durant 15 minuts.    
Analitzem dues sÃ¨reis diferents d'observacions. Una sÃ¨rie amb condicions normals de funcinament i una sÃ¨rie on simulem una fuita d'aire.    
El format dels registres Ã©s el segÃ¼ent:  
 

 
### **Descriptius estadÃ­stics**  
 

 

 

 
Les dues sÃ¨ries presenten valors molt poc diferenciats, aspecte que dificulta una segmentaciÃ³ de diferents condicions de funcionament.  
 
### **SÃ¨ries Temporals** 
 
Convertim les observacions en una sÃ¨rie sequencial i ordenada en el temps.   
Aquesta seqÃ¼Ã¨ncia de dades ordenades i equidistants cronologicament, mostra l'estat de la instal.laciÃ³, en referÃ¨ncia al consum i pressiÃ³ observables en diferents cicles de funcionament de mÃ quina.
 
#### Cabal
 

 
#### PressiÃ³
 

 
 
### **DescomposiciÃ³ SÃ¨ries Temporals**   
 
AnÃ lisi de les sÃ¨ries temporals des del punt de vista de les seves components estructurals:  
<center>  
  
TendÃ¨ncia + Efecte Estacional + Residus  
  
</center>
D'aquesta descomposiciÃ³ ens interessa especialment la informaciÃ³ de la tendÃ¨ncia de les variables analitzades.    
 
#### Cabal  
 

 
 

 
#### PressiÃ³
 

 
   

 
### **Changepoint Detection Algorithms** 
 
Algoritme de detecciÃ³ de variacions sobtades en una serie temporal.  
Aquests canvis poden representar transicions entre estats de condiciÃ³ de treball.  
 
#### Cabal   
 

 

 
Punt Canvi CABAL 1: **493**    
Punt Canvi CABAL 2: **1211**  
Mitjanes segments :     **64.5360248, 164.4265375, 72.0762343, 159.8938775, 60.3427641**
 
#### PressiÃ³  
 

 

 
Punt Canvi PRESSIÃƒâ€œ 1: **583**  
Punt Canvi PRESSIÃƒâ€œ 2: **1367**  
Mitjanes segments   : **5.0419655, 4.8692441, 5.1152011**  
 
### **AnÃ lisi TendÃ¨ncia** 
 

 
#### Cabal  
 

 
 
#### PressiÃ³   
 

 
 
### **InterdependÃ¨ncia** 
 
Un canvi de les condicions de treball pot suposar un canvi de les dependÃ¨ncies entre els diferents sensors.  
Correlacionar la situaciÃ³ de canvi de les dues variables amplifica els punts d'anomalia a partir de les grÃ fiques de tendÃ¨ncia.  
GrÃ fiques dels valors escalats de la tendÃ¨ncia de pressiÃ³ i cabal de les dues sÃ¨ries.  
 
#### OK
 

 
#### Fuites
 

 

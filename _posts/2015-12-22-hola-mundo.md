---
layout: post
title: Hola Mundo
author: Pedro J. Perez
published: true
status: publish
draft: false
tags: myblog
---
 
Hello world,
 
Si lees esto es que has conseguido montar tu jekyll blog hosted in Github!! Guau!! 
 
- Acuerdate que has de modificar el fichero **_config.yml** para poner tus datos (nombre , e-mail etc...). Lo que tienes que cambiar esta seÃ±alado con la marca [AQUI+]   
 
- Si quieres poner tu foto tienes que sustituir el archivo meSmall.jpg que encontraras en la carpeta  [  /images  ]   
 
 
Para subir posts a tu blog solo tienes que aÃ±adir ficheros escritos en markdown (.md) en la carpeta [  _posts  ]    
 
 
Este blog esta muy-muy basado, vamos que es una copia (forkeo) del de Andy South. Lo puedes ver [aquÃ­](http://andysouth.github.io/)    
 
 
 
Ale, a escribir    
 
 
 

{% highlight r %}
PD: recuerda que si quieres escribir con ficheros (.Rmd):   
 
0) Escribe en .Rmd pero el yaml header ha de tener (mira el fichero en la carpeta _rmd):     
 
    - layout: post    
    
    - status: process  
    
1) Haz un source del fichero rmd2md.R Se cargara la funciÃ³n rmd2md() 
 
2) En la consola de RStudio ejecuta:  rmd2md()  
{% endhighlight %}
 
 
Un poco mejor explicado [aquÃ­](http://perezp44.github.io/r/Tutorial-para-crear-Jekyll-blog-hosted-in-Gitbub/)

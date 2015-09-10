# curso Modelos y Datos

Un curso introductorio a métodos de máxima verosimilitud y Bayes usando R y WinBUGS/Jags. Eventualmente incorporaré STAN. Los archivos *.Rmd están configurados para compilarlos con knitr a HTML con estilo bootstrap usando [knitrBootstrap](https://github.com/jimhester/knitrBootstrap). El ecabezado YAML Front-matter es de tipo:

```
    knitrBootstrap::bootstrap_document:
    title: "intro R"
    theme: cerulean
    highlight: HighlightJS
    theme.chooser: FALSE
    highlight.chooser: FALSE
    dev: svg
```

Los archivos Rmd se pueden compilar de todas maneras con el botón knitr HTML en RStudio, pero para que queden como los estoy subiendo a RPubs hay que ejecutar los siguientes comandos: 

```
 library(knitrBootstrap)
 library(rmarkdown)
 render('file.Rmd', 'knitrBootstrap::bootstrap_document')
```
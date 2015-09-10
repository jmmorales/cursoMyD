# curso Modelos y Datos

Un curso introductorio a métodos de máxima verosimilitud y Bayes usando R y WinBUGS/Jags. Eventualmente incorporaré STAN. Los archivos *.Rmd están configurados para compilarlos con knitr a HTML con estilo bootstrap usando [knitrBootstrap](https://github.com/jimhester/knitrBootstrap). El ecabezado YAML Front-matter es de tipo:

```
output:
  knitrBootstrap::bootstrap_document:
    title: "intro"
    theme: cerulean
    highlight: Solarized - Light
    theme.chooser: TRUE
    highlight.chooser: TRUE
```

Los archivos Rmd se pueden compilar con el botón knitr en RStudio.
---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

# causnet

<!-- badges: start -->
<!-- badges: end -->

The goal of causnet is to ...

## Installation

You can install the development version from GitHub with:

```{r installation, eval=FALSE}
require("devtools")

```



``` r
install.packages("causnet")
```

## Example

```{r}
library(CausalNets)

# simulate data
set.seed(1234)
mydata = simdat(300,5,1)
# run Causnet
links.s = sfun(mydata, alpha = 0.5, surdata=NULL, scoreFn = "bic", pheno = FALSE, alpha1 = 0.01, alpha2 = 0.01, pp = NULL)



netplot_jm(links.s)
```





## Using base bioconductor image

For the pending release of bioc3.11, using their `` container,
there are some packages that won't get auto installed when
doing "the easy" gitub install for `multiGSEA.shiny@develop`,
so run this after the bioconductor docker container starts up:

```r
BiocManager::install(update = TRUE, ask = FALSE)
BiocManager::install("remotes")

BiocManager::install(c(
    "BiasedUrn",
    "BiocStyle",
    "DESeq2",
    "fgsea",
    "GO.db",
    "goseq",
    "msigdbr",
    "knitr",
    "org.Hs.eg.db",
    "org.Mm.eg.db",
    "PANTHER.db",
    "reactome.db",
    "statmod",
    "webshot"))

BiocManager::install("lianos/multiGSEA.shiny")
```

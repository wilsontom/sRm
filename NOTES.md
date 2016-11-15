### NOTES

- `Rcpp`, `gtable`, `scale`, `munsell`, `colorspace`, `plyr` are all listed as an Imports,although they are imported _via_ other dependancies. This was a fix in order to get the AppVeyor CI builds to stop failing. This caues a `NOTE` in `R CMD check`.



# Modified version of the FBMS branch

# Installations
To install and load the package
```
library(devtools)
install_github("ylvast/GMJMCMC@FBMSY")
library(FBMS)
```

# Updates
The package is used the same way as before; the syntax should be the same. One new file is added, **`R/feature_helper.R`**, which is a helper script that contains functions used to calculate feature complexity and perform the transformations.

To add new transformations (or remove), this is done by adding a transformation-function in **`R/feature_generation.R`** (or removing the unwanted transformation-function), and updating **`gen.feature`** in the same file. Furthermore, **`gen.params.mjmcmc`** in **`R/arguments.R`** must be updated with the new transformation probabilities. 





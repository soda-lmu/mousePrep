# preprocessingmouse package 

### When to use preprocessingmouse package? 

...


### Installing package from source (when the package is not available on CRAN or when the repository is not public)

#### Required Packages 

```R
library(haven)
library(devtools)
```
#### Steps 
Clone the repository into local system (possibly in documents folder). Open the .Rproj file as a project (on R studio)
To build the package, do these steps

```R
devtools::document()
devtools::install()
```
That will install the package into the R library. Now the package can be called via library() from any R session. 
```R
library(preprocessingmouse)
```

### Example of preprocessing steps from the preprocessingmouse package

#### Remove touch devices

Remove all participants who used touch devices using the rm_cases() function, and furthermore use the 
mouse_class() function to remove the cases with less than 50 data points per session. 

```R
rm_cases(data, column_rm = "userAgent_is_touch_capable", factor_rm = TRUE, criteria = 1)
```



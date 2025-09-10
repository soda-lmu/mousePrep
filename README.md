# NAME FOR PACKAGE? 

### When to use "PACKAGE_NAME" ? 

...


### Installation 

#### Required Packages 

```R
library(haven)
```

### Preprocessing Steps 

#### Remove touch devices

Remove all participants who used touch devices using the rm_cases() function, and furthermore use the 
mouse_class() function to remove the cases with less than 50 data points per session. 

```R
rm_cases(data, column_rm = "userAgent_is_touch_capable", factor_rm = TRUE, criteria = 1)
```

#### Remove cases where the window was resized

... 
```R
rm_cases(data, factor_rm = "resize", criteria = 1)
```

sjPlot - data visualization
------------------------------------------------------------------------------
Collection of several plotting / table output functions for visualizing data.

### Installation

#### Latest development build

To install the latest development snapshot (see latest changes below), type following command in the R console:

```r
devtools::install_github("devel", "sjPlot")
```

#### Officiale, stable release
To install the latest stable release from CRAN, type following command in the R console:

```r
install.packages("sjPlot")
```

### Changelog of current development build

#### Changes to functions:
* Added parameter "axisLimits.y" to function "sjp.emm.int"
* Added parameter "axisLimits.y" to function "sjp.lm.int"

#### General:
* Removed extracted single functions from other packages and added imports for those functions. sjPlot now also imports "psych" and "cluster" package.


### References and documentation

- [RPubs documentaton](http://rpubs.com/sjPlot/)
- [Weblog](http://strengejacke.wordpress.com/sjplot-r-package/)

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

#### Changes to functions
* Added `themr` option to `theme` paramater of all sjp-plotting function, so the sjPlot package can be used with the [ggthemr-package](https://github.com/cttobin/ggthemr)
* Added parameter `axisLimits.y` to function `sjp.emm.int`
* Added parameter `axisLimits.y` to function `sjp.lm.int`
* `sjp.lm` now shows adjusted r-square in model summary.

#### General
* Removed extracted single functions from other packages and added imports for those functions. sjPlot now also imports "psych" and "cluster" package.
* Removed packages `cluster`, `coin`, `lsmeans` and `lmtest` from required imports and moved them to suggested packages.

#### Bug fixes
* Parameter "theme" was ignored in "sjp.scatter" - fixed.


### References and documentation

- [RPubs documentaton](http://rpubs.com/sjPlot/)
- [Weblog](http://strengejacke.wordpress.com/sjplot-r-package/)


### Citation

In case you want / have to cite my package, please use `citation('sjPlot')` for citation information. Since this package makes heavy use of the [ggplot-package](http://cran.r-project.org/web/packages/ggplot2/index.html), consider citing this package as well.

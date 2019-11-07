# xlerate

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/HealthEconomicsHackathon/xlerate.svg?branch=master)](https://travis-ci.org/HealthEconomicsHackathon/xlerate)
[![codecov.io](https://codecov.io/github/HealthEconomicsHackathon/xlerate/coverage.svg?branch=master)](https://codecov.io/github/HealthEconomicsHackathon/xlerate?branch=master)
![works?](https://img.shields.io/badge/works-on%20my%20machine-pink)

An experiment in turning an Excel file into an R program.  It takes as inputs an excel file (in xlsx format) a set of cell references as inputs and a set of cell references as outputs and returns a function that can be used to "execute" the excel workbook from R.  Currently only very basic excel operations are supported.



For example, [this excel workbook](inst/example/tree.xlsx) implements a tree model with inputs on the first sheet and outputs on the second.


```r
path <- system.file("example/tree.xlsx", package = "xlerate")
```

The inputs are on the first tab, making up a column with a few gaps, and with labels in the column to its left:

```r
inputs <- xlerate::xlerate_ref(
  c("D3:D13", "D15:D16", "D18:D21"),
  1, list(col = -1))
```

The outputs are all over the second sheet and have labels two rows below


```r
outputs <- xlerate::xlerate_ref(
  c("C40", "E34", "E50", "G29", "G38", "G45", "G54"),
  2, list(row = 2))
```

Then create a function that can generate these outputs


```r
tree <- xlerate::xlerate(path, inputs, outputs)
tree
```

```
## <an xlerate object>
```


```r
tree(c("pTST_pos" = 0.01))
```

```
##        TST        pos        neg       LTBI  LTBI free       LTBI
##   71.90691  500.00000   67.58273 1342.00000    0.00000 1342.00000
##  LTBI free
##    0.00000
```

## License

MIT © Imperial College of Science, Technology and Medicine

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

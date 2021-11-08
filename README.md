### Description

**Plot physiological parameters measured using the Li-COR biosystems photosystem (optimized for the Li-6800).**
\
\
This package uses Excel sheet files (**.xlsx**) created by the Li-6800 system.
\
\
**How to use the licornetics package**
\
First, you need to download and install the package in R or R studio.
You can install it with the following code:
```yaml
if (!require(devtools)) install.packages('devtools')
library(devtools)
devtools::install_github("lbmountain/licornetics")
library(licornetics)
```
\
\
The licornetics function included in the package can then be executed by using **`licornetics(identifier = "filename.xlsx")`**.
\
If you want to plot averaged data from more than one file, simply change `"filename.xlsx"` to a string of letters that are common to all the files you want to include such as `"plantline1"`. Showing data from different files within the same plot is possible by executing **`licornetics(identifier = c("plantline1", "plantline2"))`**.
\
\
By changing the `type` argument, the following parameters can be plotted:
\
**`type = "gsw"`** for **absolute stomatal conductance** (_gsw_ in the excel files). This is the default plot.
\
**`type = "relgsw"`** for **relative stomatal conductance** (_gsw/max(gsw)_).
\
**`type = "A"`** for **carbon assimilation** (_A_ in the excel files).
\
**`type = "WUE"`** for **water-use efficiency** (_A/gsw_).
\
\
\
Further options listed below can be used to modify the plot and plotted data:
\
**`area_correction`** corrects the parameters by the measured leaf area. Divide the Li-COR chamber size by the average leaf area of the samples (e.g. with a 2cm<sup>2</sup> chamber and an average leaf area of 0.64cm<sup>2</sup> it would be 2/0.64=3.125, so `area_correction = 3.125`).
\
\
**`timestamps`** adds vertical dotted lines to the plot (e.g. `timestamps = c(20, 40, 60)`).
\
\
**`observations`** limits the range of the x axis. E.g. by setting `observations = c(16:70)` only observations (_obs_) from 16 to 70 would be shown.
\
\
**`legend_title`** allows to change the legend title. Default title is "Genotype".
\
\
**`legend_labels`** allows to modify the names of items within the legend. The labels given here should follow the order of the respective keywords in `identifier`. Default is set to the input names given in `identifier`.
\
\
**`remove_outliers`** will remove outliers from the plot if set to `remove_outliers = "yes"`. These are determined by excluding boxplot outliers based on the water-use efficiency (WUE) column of the data.

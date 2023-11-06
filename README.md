### How to download and install the package in R or R Studio
You can install the most recent version (v2.1.1) of licornetics from github with the following code:
```yaml
if (!require(devtools)) install.packages('devtools')
devtools::install_github("lbmountain/licornetics")
```

Next, load the package library (this needs to be done every time you restart your R session).
```yaml
library(licornetics)
```

<br /> 
<br />

### Licorplots (name changed from version 2.0.0 of licornetics onwards)

**Plot physiological parameters measured using the Li-COR biosystems photosystem.**

This function enables easy visualization of physiological parameters measured with a Li-COR portable photosystem.

It can plot absolute or relative stomatal conductance (_g<sub>SW</sub>_), carbon assimilation (_A_), intrinsic water-use efficiency (iWUE), intercellular CO<sub>2</sub> (_C_<sub>i</sub>) and ambient CO<sub>2</sub> (_C_<sub>a</sub>). Also allows to plot other values from version 2.0.1 onward. Data from single files can be plotted as well as averaged data across several input files (with standard error or deviation bars) and different genotypes, species or conditions can be shown in one plot.

Licorplots is optimized for the use of Excel sheet files (**.xlsx**) created by the Li-6800 system.

As output, a customizable plot is generated using the `ggplot2` package which can therefore be used downstream with ggplot-based packages such as `ggpubr`. Plot colours are added based on the "Isfahan1" palette from the `MetBrewer` package (https://github.com/BlakeRMills/MetBrewer) as a default but can also be customized with your own colour palette.


#### Updates included in version 2.0.0
- additional plotting option for Intercellular CO<sub>2</sub> and Ambient CO<sub>2</sub>
- **values can now be normalised by stomatal density with the `stomden` function**
- colours are more customizable now
- x axis depicts actual time in minutes calculated from measured timepoint now rather than based on the _obs_ column
- outlier removal is based on _A_ not iWUE now
- customizable text labels now support markdown formatting (e.g. italics or bold)

#### Updates included in version 2.0.1
- The "type" argument now also allows other inputs. It is important that the input is a column of the Li-COR excel output data files (e.g. "Fs" or "Qamb_out").
- y axis label text can be changed now with the "axis_label" argument








#### How to use licorplots
##### **1. Basic usage**
The `licorplots` function included in the package can then be executed by using the following code:
```yaml
licorplots(identifier = "filename.xlsx")
```
![plot1_basic](images/plot1_base.png)

Make sure that your files are in the folder you are currently working in (check with `getwd()`) or specify the path to where they are. If all files are in one specific folder you can also use `setwd("path/to/folder")` to go there before you use `licorplots`.

If the filename you are referring to has a unique identifier (such as "test" in the filename "testfile.xlsx"), you can also use this instead of the whole filename:
```yaml
licorplots(identifier = "test")
```
![plot2_basic2](images/plot2_base2.png)




##### **2. Different plot types**
While the default setting of licorplots will yield a plot of data for absolute stomatal conductance, there are also other options available (including intrinsic water-use efficiency and relative stomatal conductance (not included in the Li-6800 output files)) which can be selected with the `type` argument:

1. **Absolute stomatal conductance** (`"gsw"`, _gsw_ in the excel files)

2. **Relative stomatal conductance** (`"relgsw"`), absolute stomatal conductance divided by the maximum value of the whole measurement

3. **Carbon assimilation rate** (`"A"`, _A_ in the excel files)

4. **Intrinsic Water-use efficiency** (`"WUE"`), Carbon assimilation rate divided by absolute stomatal conductance

5. **Intercellular CO<sub>2</sub>** (`"Ci"`, _Ci_ in the excel files) 

6. **Ambient Co<sub>2</sub>** (`"Ca"`, _Ca_ in the excel files)

7. **Data column of your choice** (refer to a column name in the excel files)
```yaml
licorplots(identifier = "test", type = "gsw")
licorplots(identifier = "test", type = "relgsw")
licorplots(identifier = "test", type = "A")
licorplots(identifier = "test", type = "WUE")
licorplots(identifier = "test", type = "Ci")
licorplots(identifier = "test", type = "Ca")
licorplots(identifier = "test", type = "Fs")
```
![plot3_types](images/plot3_types.png)




##### **3. Merge data from different files**
If you want to plot averaged data from more than one file (with error bars (standard error)), simply change the `identifier` argument to a string of letters that are common to all the files you want to include such as "plantline1". Showing data from different file sets is also possible by using `c("plantline1", "plantline2")`.
```yaml
licorplots(identifier = "plantline1")
licorplots(identifier = c("plantline1", "plantline2"))
```
![plot4_lines](images/plot4_lines.png)




##### **4. Choose error bar type**
The default error bars shown in the plots are standard error (`"se"`) bars. If you want to change them to display standard deviation instead, set the `errorbars` argument to `"sd"`.




##### **5. Data correction by average leaf area and/or stomatal density**
When the leaf is not big enough to fill the entire LI-COR system chamber, gas exchange measurements should be corrected by leaf area. This can be done manually in the excel files or by using the `area_correction` argument of licorplots. Simply divide the chamber size by the average measured leaf area (e.g. 2cm<sup>2</sup>/0.8cm<sup>2</sup>=2.5) and add this value to the code:
```yaml
licorplots(identifier = "plantline1")
licorplots(identifier = "plantline1", area_correction = 2.5)
```
![plot5_areacorrection](images/plot5_areacorrection.png)

It is also possible to normalise your data by stomatal density to show your data on a per stoma basis. For this you need to give the stomatal density [stoma/mm<sup>-2</sup>] to the `stomden` argument. If you specified more than one identifier, you need to input the stomatal density for each identifier
in the same order as in the `identifier` argument.
```yaml
licorplots(identifier = c("plantline1", "plantline2"))
licorplots(identifier = c("plantline1", plantline2""), stomden = c(60, 40))
```
![plot5.2_stomden](images/plot5.2_stomden.png)




##### **6. Change axis limits**
If you want to modify the range of values displayed on the y axis, use the `y_axis_limits` argument:
```yaml
licorplots(identifier = "plantline1", y_axis_limits = c(0, 0.7))
licorplots(identifier = "plantline1", area_correction = 2.5, y_axis_limits = c(0, 0.7))
```
![plot6_yaxis](images/plot6_yaxis.png)


To change which x axis values are included, use `timeframe`:
```yaml
licorplots(identifier = "plantline1", timeframe = 25:65)
```
![plot7_xaxis](images/plot7_xaxis.png)




##### **7. Remove outliers**
If the `remove_outliers` argument is set to `"yes"`, licorplots will remove outliers based on boxplot outliers of the _A_ values (carbon assimilation).





##### **8. Further plot modifications**
Some more arguments are available to edit the final plot:

`timestamps` adds **dotted lines** based on the x axis values it is given.
```yaml
licorplots(identifier = "plantline1", timestamps = c(20, 40, 60))
```
![plot8_timestamps](images/plot8_timestamps.png)


`legend_title` allows to change the legend **title**. Default title is `"Genotype"`.

The **labels** of the legend can be modified by using `legend_labels`. The order of labels should follow the order of data given in the `identifier` argument. If `legend_labels` is not changed, the names used in `identifier` are displayed. Both `legend_title` and `legend_labels` support markdown formatting so that you can for example format your text in _italics_ or **bold**.

The **y axis label** can be customized by using the `axis_label` argument. This argument supports usage of markdown language formatting.
```yaml
licorplots(identifier = c("plantline1", "plantline2"), legend_title = "**Species**"", legend_labels = c("*Plant x*", "Plant y"), axis_label = "**g<sub>SW</sub>**")
```
![plot9_labels](images/plot9_labels.png)


Licorplots uses the colour palette "Isfahan1" of the `MetBrewer` package as default colour palette (https://github.com/BlakeRMills/MetBrewer). You can
also use your own selection of colours or use another palette from `MetBrewer` or a similar package.
```yaml
licorplots(identifier = c("L1", "L2", "L3", "L4"))
licorplots(identifier = c("L1", "L2", "L3", "L4"), colours = met.brewer("Derain"))
licorplots(identifier = c("L1", "L2", "L3", "L4"), colours = c("blue", "green"))
licorplots(identifier = c("L1", "L2", "L3", "L4"), colours = c("#228B22", "#800080"))

```
![plot10_colours](images/plot10_colours.png)






#### Troubleshooting, tips and tricks for licorplots

We found that sometimes excel files were not read properly into R, leading to empty plots. In those cases it helped to open the excel file outside R Studio and save them again. Afterwards, R read in the data just fine.
Similarly, data could not be read into R when the file is still open in Excel so make sure to close the files before using the package.

It is highly recommended to keep the LI-COR input files in a folder that does not contain other files. If you did not specify a full file name as input but rather chose a fragment of it as identifier, licorplots will try to open all the files with this fragment that are in the folder and if there is a 
file with this fragment that is not a LI-COR excel output, it will cause an error.

If installation via `devtools` does not work, try replacing `devtools` in the code with `remotes`.







<br />
<br />
<br />






### Licorvalues (new in version 2.0.0 of licornetics)

**Calculate steady state values and kinetics parameters from data measured using the Li-COR photosystem.**

This function calculates steady state values along with kinetics parameters based on Li-COR excel files.
It also visualises absolute stomatal conductance and plots the fitted curve onto the data.
Modeling and calculation of kinetics parameters is based on the model describing the change of stomatal conductance over time introduced in [Vialet-Chabrand et al. (2013)](#1) and [McAusland et al. (2016)](#2).

The following values will be calculated:

1. **Steady state absolute stomatal conductance ("gsw")** averaged over the last 5 time points of a specified time frame with standard deviation and error.
2. **Steady state carbon assimilation ("A")** averaged over the last 5 time points of a specified time frame with standard deviation and error.
3. **Intrinsic water-use efficiency ("iWUE")** averaged over the last 5 time points of a specified time frame with standard deviation and error.
4. **Lag-time (λ)** of the curve in response to a change in irradiance (in s and min) with standard error.
5. **Time constant ("k")** of opening or closing (in s<sup>-1</sup> and min<sup>-1</sup>) with standard error.
6. **Maximum slope (Slmax)** of the curve in response to a change in irradiance with standard error.

Licorvalues is optimized for the use of Excel sheet files (**.xlsx**) created by the Li-6800 system.

As output, a data frame/table containing the values is generated and the data with curves are depicted as plots.




<br />


#### How to use licorvalues
##### **1. Basic Usage**
Let's imagine you have ran a Li-COR program on several individuals of two genotypes in which you changed the light intensity each 20 minutes. Your final
plot that you created using the `licorplots` function might now look like this:
![plotv_1_example](images/plotv_1_example.png)

Now you want to have a closer look at the second opening transition at 41 to 58 minutes. You can use `licorvalues` to calculate steady state values
for the last 5 minutes of this time frame and it will also show you what the exponential decay curve looks like that was fitted over your data.
Similar to the `licorplots` function you name identifiers, here `"plantline1"` and `"plantline2"`, and then you give a list object to the `transition` argument that specifies the time frame that you want to calculate steady state and kinetics values for.

```yaml
licorvalues(identifier = c("plantline1", "plantline2"), transition = list(c(41:58)))
```
The output will then be a plot showing the data with the fitted curves:
![plotv_2_curves](images/plotv_2_curves.png)

and a table with the values that is printed to the console:
```
    ID transition_zone       gsw      gsw_sd       gsw_se        A       A_sd       A_se     iWUE   iWUE_sd   iWUE_se
  LB14           41:58 0.1985915 0.002009115 0.0008985037 16.39369 0.03759949 0.01681500 86.42987 0.9092028 0.4066079
    wt           41:58 0.2533792 0.002537768 0.0011349245 17.67942 0.06860583 0.03068146 70.92771 0.7266602 0.3249723
    
     lambda se_lambda lambda_min se_lambda_min        k     se_k    k_min  se_k_min     Slmax   se_Slmax
  142.63662  44.24771   2.377277     0.7374618 109.2266 25.97587 1.820443 0.4329312 0.1906983 0.09819969
   89.62661  15.81718   1.493777     0.2636196 121.2167 10.65192 2.020278 0.1775320 0.4465352 0.03579890
```

If you want to save the table later, you can also save the output to an R object which will then include the table but not the plots:
```yaml
new_data <- licorvalues(identifier = c("plantline1", "plantline2"), transition = list(c(41:58)))
```

<br />


##### **2. Data normalisation by stomatal density**
It is also possible to normalise your data by stomatal density to show your data on a per stoma basis. For this you need to give the stomatal density [stoma/mm<sup>-2</sup>] to the `stomden` argument. If you specified more than one identifier, you need to input the stomatal density for each identifier
in the same order as in the `identifier` argument.
```yaml
licorvalues(identifier = "plantline1")
```

```
  ID transition_zone       gsw      gsw_sd      gsw_se        A       A_sd       A_se     iWUE   iWUE_sd
  wt           41:58 0.2533792 0.002537768 0.001134924 17.67942 0.06860583 0.03068146 70.92771 0.7266602

    iWUE_se   lambda se_lambda lambda_min se_lambda_min        k     se_k    k_min se_k_min     Slmax
  0.3249723 89.62661  15.81718   1.493777     0.2636196 121.2167 10.65192 2.020278 0.177532 0.4465352

   se_Slmax
  0.0357989
```

<br />

```yaml
licorvalues(identifier = "plantline1", stomden = 60)
```

```
  ID transition_zone          gsw       gsw_sd       gsw_se            A        A_sd         A_se        iWUE    iWUE_sd
  wt           41:58 4.222987e-06 4.229614e-08 1.891541e-08 0.0002946569 1.14343e-06 5.113577e-07 0.001182129 1.2111e-05
       iWUE_se   lambda se_lambda lambda_min se_lambda_min        k     se_k    k_min  se_k_min        Slmax     se_Slmax
  5.416205e-06 50.03848  12.55972  0.8339747     0.2093287 149.7999 11.72112 2.496665 0.1953521 5.805185e-06 5.120071e-07
```


<br />

##### **3. Outlier removal**
If `remove_outliers = "yes"`, licorvalues will remove outliers from the data based on boxplot outliers of the _A_ (carbon assimilation) column.



<br />

##### **4. Colours**
Per default, plots are depicted in black but you can also customise this by adding a colour palette via the `colours` argument that assigns a colour to each identifier.
```yaml
licorvalues(identifier = c("plantline1", "plantline2"), transition = list(c(41:58), c(61:75)), colours = met.brewer("Derain"))
```

```
     genotype transition_zone        gsw      gsw_sd       gsw_se          A
2  plantline1           41:59 0.25002099 0.002745064 0.0012276297 17.5336849
1  plantline1           61:75 0.02693562 0.004860572 0.0021737137 -0.9172865
11 plantline2           41:59 0.19859154 0.002009115 0.0008985037 16.3936853
12 plantline2           61:75 0.02550996 0.005165314 0.0023099984 -1.0230697

         A_sd       A_se       iWUE     iWUE_sd     iWUE_se rate_constant      T50
2  0.06999676 0.03130350   71.43659   0.8333434   0.3726825     0.1667535 4.156717
1  0.04142863 0.01852745 -180.21156 238.4259830 106.6273411     0.2266650 3.058025
11 0.03759949 0.01681500   86.42987   0.9092028   0.4066079     0.1117941 6.200213
12 0.08053374 0.03601579  -53.34118  11.4081815   5.1018938     0.2182721 3.175611
```


![plotv_5_curves](images/plotv_5_curves.png)





##### **5. Different plotting options**
Licorvalues will usually create a folder called "GraphModel" and further folders within that folder labelled with the specified time frames. These folder contain plots for individual files and averaged plots for each identifier. Within R (Studio), a plot will be displayed in the Plots Viewer panel that shows the plots for each identifier as default (see above plots). If you want this to display the plots for each single file instead, set 'show_individuals =T'.







### References
<a id="1">[1]</a> 
McAusland, L., Vialet-Chabrand, S., Davey, P., Baker, N.R., Brendel, O., and Lawson, T. (2016). 
"Effects of Kinetics of Light-Induced Stomatal Responses on Photosynthesis and Water-Use Efficiency." 
The New Phytologist 211 (4): 1209–20.

<a id="2">[2]</a>
Vialet-Chabrand, S., Dreyer, E., and Brendel, O. 2013. “Performance of a New Dynamic Model for Predicting Diurnal Time Courses of Stomatal Conductance at the Leaf Level.” Plant, Cell & Environment 36 (8): 1529–46.

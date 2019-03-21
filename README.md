# RAR
R Package for Rest Activity Rhythm analysis

This package is intended for easy analysis of rest activity rhythm (RAR) analysis collected using actigraphy data that has ben condensed into epochs (e.g. activity counts within 30 second, 60 second, etc epochs). This package utilizes three main approaches for analysis: 
* Parametric Approach: Sigmoidally transformed extended cosine models
* Residual Spectrum Analysis: Spectral analysis based on residuals of fitted cosine models
* Localized Measures: Mean, standard devation, and relative activity measures within user-specified time bins.

## Installing RAR
```
install_github("JessLGraves/RAR")
```

## Example Data
This package includes several example datasets of RAR data collected in 60 second epocs. There are three main types of data files:
* Example activity data for a single participant (`rar_data`, `rar_data2`, `rar_data3`)
* Example activity for multiple participants (`rar_data_multi`, `rar_data_multi_wake`)
* Example data outputted from other `RAR` functions (`df_predicted`)

You can access these datasets by using `data()`. For example:
```
data(rar_data3) 
```
The first few observations of `rar_data3` look like:

| time  | act |
| ----- |---- |
| 19:00:00  | 26  |
| 19:01:00  | 4  |
| 19:02:00  | 125|

## Example of Use
### Parametric Approach
The user can specify one of three sigmoidal transformations: Anti-Logistic, Arctangent, Hill Function. Details of these transformations can be found in Marler et al. 2006.

```
cos = RAR_ExCosine(df = rar_data3, act_column = act, time_column = time, transform = "antilogit", plot=TRUE) 
cos$plot_log.act
```

![rar_data3 activity](/rar_data3_plot.png)

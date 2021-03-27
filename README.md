# RAR
R Package for Rest Activity Rhythm analysis

This package is intended for easy extraction of features of rest activity rhythms (RARs) collected using actigraphy data that has ben condensed into epochs (e.g. activity counts within 30 second, 60 second, etc epochs). This package utilizes three main approaches for analysis: 
* Parametric Approach: Sigmoidally transformed extended cosine models
* Residual Circadian Spectrum Analysis: Spectral analysis based on residuals of fitted cosine models
* Localized Measures: Mean, standard devation, and relative activity measures within user-specified time bins.

__Example RAR pattern estimated using anti-logistic extended cosine model__
<img src="https://github.com/JessLGraves/RAR/blob/master/param_ex1.png" width="400">

See <a href = "https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.2466">Marler et al. 2006</a> for a summary of the model structure and parameters estimated from the models. 

__Example Residual Circadian Spectrum Analysis__

Spectral analysis is performed on the residuals of derived from the anti-logistic extended cosine model. From there, a user-specified specified band (a, c) for which the area under the smoothed spectrum is estimated. 

See <a href = "https://link.springer.com/article/10.1007/s12561-018-09230-2"> Krafty et al. 2019 </a> for methodology underlying residual circadian spectrum analysis and frequency band estimation.

## Installing RAR
```
install_github("JessLGraves/RAR")
```

## Publications which have used RAR

- Graves, Jessica L., et al. "Profiles of Accelerometry-Derived Physical Activity Are Related to Perceived Physical Fatigability in Older Adults." Sensors 21.5 (2021): 1718. <a href = "https://www.mdpi.com/1424-8220/21/5/1718"> link </a>
- Smagula, Stephen F., et al. "Activity patterns related to depression symptoms in stressed dementia caregivers." International psychogeriatrics (2019): 1-8. <a href = "https://www.cambridge.org/core/journals/international-psychogeriatrics/article/abs/activity-patterns-related-to-depression-symptoms-in-stressed-dementia-caregivers/B241BD2B09FEF44A715F2424F7B62294"> link </a>

##
See manual.pdf for more information.



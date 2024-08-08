# The EPoCH study

Exploring Prenatal influences on Childhood Health

![](https://cpb-eu-w2.wpmucdn.com/blogs.bristol.ac.uk/dist/c/500/files/2018/11/Untitled-26hzp4l.png)

The Exploring Prenatal influences on Childhood Health, or EPoCH project, investigates how parents’ lifestyles in the important prenatal period might affect the health of their children.

The main analysts for the EPoCH study are Gemma Sharp (PI) and Kayleigh Easey.

## EPoCH involves four main stages:

### 1. Data preparation and summarising stage

Preparing and harmonising data between the main EPoCH cohorts: ALSPAC, Born in Bradford, Millenium Cohort Study, MoBa.

Code for this stage is found in the data_prep folder.

### 2. Cohort analysis

For each cohort, we run multivariable regression analyses of the association between all combinations of exposures and outcomes. What we end up with is a sort of mini phenome-wide association study (pheWAS) for each exposure of interest.

This stage involves:

* Making a pheWAS "key" to describe the covariates that should be adjusted for for each combination of exposure and outcome. The code for this is found in the making_key folder.
* Running the pheWAS. The code for this is found in the phewas folder.
* Running some QC on the results. The code for this is found in the cohort_qc folder.

### 3. Meta-analysis and combining results

We then run fixed effects meta-analysis on the results from each cohort. The code is found in the meta_analysis folder.
Results are then combined (with a final bit of QC) using the combine_clean_meta_results script.

### 4. Triangulation

Evidence from different analyses is 'triangulated' (combined) to assess the degree of confidence in an exposure-outcome association reflecting a causal relationship.

## Where to find more information

The full analysis plan is provided in this repository (https://github.com/gcsharp/EPoCH_analysis/blob/main/analysis_plan/analysis_plan.md).

The [EPoCH website](https://gcsharp.github.io/EPoCH_website/) also has some additional information. 


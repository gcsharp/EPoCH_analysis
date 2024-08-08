# EPoCH analysis plan

![](https://cpb-eu-w2.wpmucdn.com/blogs.bristol.ac.uk/dist/c/500/files/2018/11/Untitled-26hzp4l.png)

## Background

We urgently need better evidence about how our experiences before birth might influence our long-term health. Most research in this area has focused on the lifestyles of pregnant mothers, but the evidence is patchy and health advice offered to pregnant women can be confusing and inconsistent. More recent research suggests that a father's behaviour can influence the health of his unborn children, but very little public health advice is currently offered to fathers-to-be. EPoCH is addressing the urgent need for better quality scientific evidence on how the health behaviours of both mums and dads in the prenatal period might affect the health of their children. We aim to contextualise the effects of these individual behaviours relative to the effects of the social determinants of health.

## 1. Data preparation and summarising stage

We are using data from multiple European cohorts:

* In the hypothesis-generating pheWAS stage:
	* ALSPAC
	* Millenium Cohort Study
	* MoBa
	* Born in Bradford

### Data preparation and harmonisation

We created harmonised variables for each exposure, outcome and covariates. The process is described in the ALSPAC, BiB and MCS data prep files. Basically, we had a good idea of the general concepts we wanted to capture (e.g. "smoking in the first trimester") and then were led by the data in terms of how these could be defined (e.g. ordinal categories).

Exposures are broadly grouped into the following classes:

* Smoking
* Alcohol consumption
* Caffeine consumption

Outcomes are broadly grouped into the following domains:

* Body size and adiposity
* Psychosocial and cognitive
* Immunological
* Serum biomarkers
* Blood pressure

Outcomes are measured during the following stages. Where there are multiple measures within one of these stages, we have taken the oldest available measure:

* At birth or in first year of life (>0 to <1)
* Childhood stage 1 (around age 2; >=1 to <3)
* Childhood stage 2 (around age 4; >=3 to <5)
* Childhood stage 3 (around age 6; >=5 to <8)
* Childhood stage 4 (around age 9; >=8 to <11)

Within each cohort, we perform checks on the data (to check for weird distributions etc). Through this, we can identify any potential coding errors in the data preparation scripts and address these. These data checks are performed using the SUMMARISE_DATA.R script. It produces barcharts summarising counts of categorical variables and histograms of continuous variables. At the end of every cohort prep file (e.g. alspac_derive.r) we create tables summarising counts, means, ranges, etc. These are tidied up and formatted using the 'make table ones.R' script.

### Create a 'key'

For each cohort, we create a pheWAS "key" to translate the variable names to more understandable terms and to describe the covariates that should be included in each model. The code for this is found in the making_key folder.

## Systematic analysis (PheWAS)

We are exploring prenatal influences on childhood health using a semi-hypothesis-free systematic exploration approach that is similar to a Phenome-Wide Association Study (PheWAS) approach. PheWAS analyse many phenotypic outcomes in relation to a single exposure or genetic variant/score. They allow researchers to identify a subset of outcomes that may be (causally) related to an exposure. The approach can be applied to any richly phenotyped dataset. 

We are conducting two sets of PheWAS (observational and Mendelian randomization; MR) using individual cohort data to explore a wide range of offspring health outcomes in relation to parental health behaviours in the prenatal period.

The pheWASs are conducted using data from four European birth cohorts:

Summary statistics will be compared between cohorts and meta-analysed to improve power.

Code for running pheWAS is found in the phewas folder.

## Meta-analysis

We conduct a fixed effects meta-analysis of all observational and MR pheWAS results across all cohorts. We choose fixed-effects as all cohorts are of European origin and we therefore assume they are estimating effects from the same underlying populations. Also there are too feww studies to run random effects meta-analysis.

Inter-study heterogeneity is assessed using the I^2 statistic, heterogeneity Q test, and by generating and observing forest plots.

meta_analysis folder.

## Models

Directed Acyclic Graphs (DAGs) were used to select the covariates for adjustment. These are described under analysis_plan > planning_models

## Cleaning and preparing results

Some cleaning and preparation of results is necessary. This is conducted using the cohort_qc and combine_clean_meta_results scripts.

## Triangulation

Evidence from different analyses is 'triangulated' (combined) to assess the degree of confidence in an exposure-outcome association reflecting a causal relationship. This is done using the triangulation script.

## EPoCH Explorer (R Shiny app)

We will generate a great many more results than we can possibly report on in scientific papers. Therefore, we created a web tool (R Shiny app) to make these results publicly available and develop an app to explore both the pheWAS and follow-up analysis results. This the code for the tool is available in a separate repository.

## Organisation of code and data

All prepared data (for ALSPAC, BiB and MCS) is stored in the EPoCH folder on the RDSF at the University of Bristol, except MoBa data which is stored and analysed on the servers at the Norwegian Institute for Public Health in Oslo.

All code is in this GitHub repository.

Most stages in EPoCH (all except the cohort-specific data preparation) can be run using a single bash script (`bash_master.sh`), which takes two arguments: `cohort` and `scriptname`. Before and after running this script (as a job on blue pebble), it is necessary to run two other scripts (`before.sh` and `after.sh`). This is explained in the file [pipeline.md](https://github.com/gcsharp/EPoCH/blob/main/analysis_plan/pipeline.md).




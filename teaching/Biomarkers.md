Statistical Analysis of Biomarkers
========================================================
author: Alejandro Cáceres, PhD
date: Almirall, 10th of June 2020
autosize: true




Presentation
========================================================
- <b>Senior statistician</b> in the Bioinformatics group at Barcelona Institute of Global Health <https://www.isglobal.org/>

- Adjunct <b>lecturer</b> in statustics at the Universitat Politectica de Catalunya <https://eebe.upc.edu/es>

- Over 13 years of experience analysing biomedical data
- Develop novel analysis methods for <b>biomarker discovery</b>
- High dimensional data including imaging and omic data: genomic, transcriptomic, exposomic, etc.
- I write scientific articles and implement methods in software packages (R/Matlab).


Presentation
========================================================
<br />
You can find me at: 

- [linkedin](https://es.linkedin.com/in/alejandro-caceres-dominguez-7449aa176)

- [google scholar](https://scholar.google.es/citations?user=s1D-6WAAAAAJ&hl=es)

- [gitHub](https://github.com/alejandro-isglobal)

- [my blog](https://alejandro-isglobal.github.io/)



Presentation
========================================================
<br />

Analytical validation of a biomarker: Functional magnetic resonance imaging

<img src="./Biomarkers-figure/fmri.png" style="width:75%"  align="center">



Presentation
========================================================
<br />

Discovery of severity Biomarker: Chromsome Y function and risk of disease in men 

<img src="./Biomarkers-figure/jnci.png" style="width:75%"  align="center">


Summary
========================================================
- Introduction: 
  - Biomarkers definition
  - Guidance for qualification and reporting
- Analysis methods for biomarkers:
  - Diagnostic matrix
  - Regression analyeses (stratified and with interactions)
  - Statistical power
  - Multiple Biomarkers
- Examples:   
  - Prediction of drug effects (depresion)
  - Response to a therapeutic intervention (psoriasis)



Introduction
========================================================
<br />
Definition:
<br />
- A biomarker is a <b>biological measurement</b>.

Properties:
<br />

- Source material or matrix 
- Method of measurement

Introduction
========================================================

Source material or matrix:

- specific analyte (e.g., cholesterol)
- anatomic feature (e.g., joint angle)
- physiological characteristic (e.g., blood pressure) 

when they are composite
- how the compoenents are interrelated (e.g. genomic data)
- how is the process of obtaining the biomarker (e.g. algorithm, score)


Introduction
========================================================

Method of measurement:

- molecular
- histologic
- radiographic
- physiologic characteristic

New biomarkers are continously created as novel methods of measurements and their analysis are constantly developed for adressing outstanding disease-related or treatment-related <b>needs</b>.



Introduction
========================================================

Context of use (COU):
- diagnostic biomarker
- monitoring biomarker
- pharmacodynamic/response biomarker (e.g., clinical trial endpoints, including surrogate endpoints)
- predictive biomarker
- prognostic biomarker
- safety biomarker
-susceptibility/risk biomarker

See FDA's Biomarker Qualification 2018 [doc](https://alejandro-isglobal.github.io/teaching/docs/fdabiomarker.pdf)

Introduction
========================================================
| Role        | Description   | 
| ----------  | ------------- | 
| Diagnosis of a disease | To make a diagnosis more reliably, more rapidly, or more inexpensively than available methods |
| Severity assessment | To identify subgroup of patients with a severe form of a disease associated with an increased probability of death or severe outcome
| Risk assessment | To identify subgroup of patients who may experience better (or worse) outcome when expose to an intervention
| Prediction of drug effects | To identify the pharmacological response of a patient exposed to a drug (efficacy, toxicity, and pharmacokinetics) 
| Monitoring | To assess the response to a therapeutic intervention

Analysis methods for Biomarker development
========================================================
<br />
<b>Is the test reliable?</b>

Analytical considerations: 
<br />
- validation of the Biomarkers test’s technical performance
- cost-effectives, feasability
- assessment of measurement error 

To which extent individuals obtain the same measurement, if we were to repeat it?

<b>test-retest reliability studies </b>

Statistics:
- intra-class correlation coefficients
- Cohen's kappa
- correlation coefficients

Analysis methods for Biomarker usefullness
========================================================

<b>Is the test a clinically useful biomarker?</b>

<br />
Establish the relationship between a biomarker and an outcome of interest:

- Randomized controlled trial
- Single-arm/historical control trial
- Cohort study
- Case-control study (including nested)
- Cross-sectional study
- Case series or case reports
- Registry information
- Meta-analysis

Strongest evidence comes from <b>prospective studies</b> that are specifically designed but data from studies conducted for <b>other purposes</b> can be used to support biomarker qualification.


Analysis methods: Diagnostic Matrix
========================================================

Supose we have gathered data for reponse/no-reponse (case/control) subjects and positive/negative test for each subject

The Respose measurement has to events:
- yes 
- no

The Test measurement has to events:
- positive
- negative

Analysis methods: Diagnostic Matrix
========================================================

Each individual has two measurements: (Response, Test)

| -- Subject -- | -- Reponse -- | -- Test -- |
| ------------- | ------------- | ---------- |
| $s_1$         |   yes        | positive |
| $s_2$         |   no         | negative |
| $s_3$         |   yes        | positive |
|...            |   ...        | ...      |
| $s_i$         |   no         | positive* |
|...            |   ...        | ...      |
|...            |   ...        | ...      |
| $s_3$         |   yes        | negative* |
|...            |   ...        | ...      |


Analysis methods: Diagnostic Matrix
========================================================

Let's think first in terms of the response

<br />
Within those who responded (yes), how many the test was positive?

- <b>Sensitivity</b> (true positive rate)

$$fr(positive|yes)=\frac{n_{positive|yes}}{n_{negative|yes}+n_{negative|yes}}$$


Analysis methods: Diagnostic Matrix
========================================================

Let's think first in terms of the response

<br />
Within those who did not respond (no), how many the test was negative?

- <b>Especificity</b> (True negative rate)

$$fr(negative|no)=\frac{n_{negative|no}}{n_{positive|no}+n_{negative|no}}$$


Analysis methods: Diagnostic Matrix
========================================================

|  | -Response: Yes -- | -- Response: No -- |
| --------- | --------- | -------- |
| -<b>Test: positive</b>- | $fr(positive|yes)$ | $fr(positive|no)$ | 
| -<b>Test: negative</b>- | $fr(negative|yes)$ | $fr(positive|no)$ | 
| -<b>sum</b>-      | 1                | 1               |


We can use model:
$$E[Test|Response]= \beta_0 +\beta*Response$$
or in more familiar terms
$$y_i = \beta_0 +\beta*x_i + \epsilon_i$$
a <b>logistic</b> regression for the observed values of $y_i=Test_i$ and $x_i=Response_i$  

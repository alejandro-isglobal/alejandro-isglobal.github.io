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

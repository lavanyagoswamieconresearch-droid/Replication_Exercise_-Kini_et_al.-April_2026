This paper replicates Kini et al (2022), investigating the causal impact of labour unions on 
production quality ─ measured by product recalls. I adopt a two-part Difference-in-differences 
(DiD) model with fixed-effects predicting the intensive and extensive margin effects of 
unionisation on recalls. I probe the validity of the parallel-trends assumption graphically. 
Then I conduct an RDD which measures the difference in product recalls around the 
unionisation threshold of 50% votes using the two-part model.  
Finally, for robustness, I test for reverse causality using lead and lag values of the 
unionisation variable in the DiD setting. In the RDD setting, I verify whether there is any 
discontinuity in the outcome variables associated with firm characteristics at the unionisation 
cutoff. 
Authored By: Lavanya Goswami
Guided by: Prof. Steve Pischke, Class Teacher: Covadonga Machicado Alvarez
Course: Econometrics II - 2025-26

The STATA code achieves the following:
1. Cleans and preps data (including winsorisation and re-labelling)
2. Constructs lags and leads of the dependent variable
3. Performs panel data regressions ( Difference-in-differences 
(DiD) model with fixed-effects)
4. Creates event study plot based on above results
5. Constructs Balance Table for Regression Discontinuity Design (RDD) set-up
6. Performs RDD (product recalls above and below unionisation threshold)
7. Performs robustness checks of hypothesised channels. 

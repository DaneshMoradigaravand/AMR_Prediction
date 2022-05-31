# Prediction of Bacterial Growth at Sub-inhibitory Antimicrobial Resistance Concentrations from Resistome with Machine Learning

R Shiny application for machine learning prediction of bacterial growth under sub-inhibitory antimicrobial concentrations.  

## Table of contents
1. [Citation](#citation)
2. [Introduction](#content)
3. [Installation](#installation)
4. [Manual](#manual)
5. [Contact](#contact)

----

### Citation <a name="citation"></a>

This R shiny web application has been developed by the Moradigaravand lab as part of the following publication:

**Machine learning prediction of resistance to subinhibitory antimicrobial concentrations from ***Escherichia coli*** genomes**

***Sam Benkwitz-Bedford, Martin Palm, Talip Yasir Demirtas, Ville Mustonen, Anne Farewell, Jonas Warringer, Leopold Parts, Danesh Moradigaravand***
***2021/8/24***
***Msystems***

----

### Abtsract <a name="content"></a>
***Escherichia coli*** is an important cause of bacterial infections worldwide, with multidrug-resistant strains incurring substantial costs on human lives. Besides therapeutic concentrations of antimicrobials in health care settings, the presence of subinhibitory antimicrobial residues in the environment and in clinics selects for antimicrobial resistance (AMR), but the underlying genetic repertoire is less well understood. Here, we used machine learning to predict the population doubling time and cell growth yield of 1,407 genetically diverse E. coli strains expanding under exposure to three subinhibitory concentrations of six classes of antimicrobials from single-nucleotide genetic variants, accessory gene variation, and the presence of known AMR genes. We predicted cell growth yields in the held-out test data with an average correlation (Spearman’s ρ) of 0.63 (0.36 to 0.81 across concentrations) and cell doubling times with an average correlation of 0.59 (0.32 to 0.92 across concentrations), with moderate increases in sample size unlikely to improve predictions further. This finding points to the remaining missing heritability of growth under antimicrobial exposure being explained by effects that are too rare or weak to be captured unless sample size is dramatically increased, or by effects other than those conferred by the presence of individual single-nucleotide polymorphisms (SNPs) and genes. Predictions based on whole-genome information were generally superior to those based only on known AMR genes and were accurate for AMR resistance at therapeutic concentrations. We pinpointed genes and SNPs determining the predicted growth and thereby recapitulated many known AMR determinants. Finally, we estimated the effect sizes of resistance genes across the entire collection of strains, disclosing the growth effects for known resistance genes in each individual strain. Our results underscore the potential of predictive modeling of growth patterns from genomic data under subinhibitory concentrations of antimicrobials, although the remaining missing heritability poses a challenge for achieving the accuracy and precision required for clinical use.Escherichia coli is an important cause of bacterial infections worldwide, with multidrug-resistant strains incurring substantial costs on human lives. Besides therapeutic concentrations of antimicrobials in health care settings, the presence of subinhibitory antimicrobial residues in the environment and in clinics selects for antimicrobial resistance (AMR), but the underlying genetic repertoire is less well understood. Here, we used machine learning to predict the population doubling time and cell growth yield of 1,407 genetically diverse E. coli strains expanding under exposure to three subinhibitory concentrations of six classes of antimicrobials from single-nucleotide genetic variants, accessory gene variation, and the presence of known AMR genes. We predicted cell growth yields in the held-out test data with an average correlation (Spearman’s ρ) of 0.63 (0.36 to 0.81 across concentrations) and cell doubling times with an average correlation of 0.59 (0.32 to 0.92 across concentrations), with moderate increases in sample size unlikely to improve predictions further. This finding points to the remaining missing heritability of growth under antimicrobial exposure being explained by effects that are too rare or weak to be captured unless sample size is dramatically increased, or by effects other than those conferred by the presence of individual single-nucleotide polymorphisms (SNPs) and genes. Predictions based on whole-genome information were generally superior to those based only on known AMR genes and were accurate for AMR resistance at therapeutic concentrations. We pinpointed genes and SNPs determining the predicted growth and thereby recapitulated many known AMR determinants. Finally, we estimated the effect sizes of resistance genes across the entire collection of strains, disclosing the growth effects for known resistance genes in each individual strain. Our results underscore the potential of predictive modeling of growth patterns from genomic data under subinhibitory concentrations of antimicrobials, although the remaining missing heritability poses a challenge for achieving the accuracy and precision required for clinical use.

This R Shiny application invokes the gradient boosted regressor with predictor features from resistome as reported by Ariba software.


----
### Installation <a name="installation"></a>

There are two ways to run the tool:

- The package may be directly executed as a web application on shinyapps cloud, using the following link:

```
https://daneshmoradigaravand.shinyapps.io/AMR_Prediction/?_ga=2.14531380.243691206.1654006523-409120471.1653027237
```

- The tool is available on DockerHub and may be fetched and run using the following commmands:

```
docker pull daneshmoradigaravand/amrpred:latest
docker run --rm -p 3838:3838 amrpred:latest
```

The application is then accessible on the following link on the browser:

```
http://0.0.0.0:3838
```

----
### Manual <a name="manual"></a>

The web application has the following feaatures:

<p align="center">
<img src="https://user-images.githubusercontent.com/35295619/171196026-f1065d15-5f1d-44c3-be3b-9b64863dddb3.jpg" width="500" />
</p>

#### Input:

1- User uploads a labeled tsv file, if they want to train a model. The input fille includes predictors from resistome data. 

2- User uploads an unlabeled file, if they want to use an already trained model for prediction.

3- The training/test split for the model.

4- The hyperparameters for the gradientboosted model.

5- The predicted feature output of the assay, including **yield** or **generation time**, the antibiotics, i.e. **KAN**, **AMP**, **TET**, **CIP** and **TRIM**, at three sub-inhibitory concentratioins, i.e. **Low**, **Medium** and **High**.  

6- Saving/exporting the output

#### Outputs:

7- Correlation plots between the predicted and actual data.

8- Summary statistics of the performance of the model.

9- Importance: Feature importance analysis.

10- Table of predicted output value. 

11- Summary of the accuracy of the model.

----
### Contact <a name="contact"></a>
For queries, please contact [Danesh Moradigaravand](mailto:d.moradigaravand@bham.ac.uk?subject=[GitHub]), Data-Driven Microbiology lab, Center for Computational Biology, University of Birmingham. 
 
-----


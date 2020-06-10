# Loss Aversion Estimation in Brand Choice Using Markov Chain Monte Carlo

## Abstract

I estimate a multinomial logit reference-dependent model with a mixture of normals specification to study the prevalence of reference price effects based on panel data of individual-level purchases in cracker and yogurt market. Prospect Theory and the idea of Loss aversion have recently inspired choice modelers to think about the reference price effect. I use two scanner panel data sets to test for the presence of reference price effects and I conducted a random walk Metropolis MCMC sampler to capture the unob- served heterogeneity and the possible correlation between parameters using a mixture of normals. The result shows that:(1)the degree of loss aversion effect is small and it even disappears after considering the heterogeneity and possible correlations. (2)The promotion activities in the store could possibly make people less sensitive about the price change.

## Paper

- [Loss Aversion Estimation in Brand Choice Using Markov Chain Monte Carlo](https://github.com/hihowme/persp-research-econ_Spr20/blob/master/Finalpaper/finalpaper/Finalpaper_haihao.pdf)

## Data


- [Cracker Market Choice Data](https://github.com/hihowme/persp-research-econ_Spr20/blob/master/Finalpaper/data/crack_market.csv) data from the `mlogit` package in R, and can be found in the data section.
- [Yogurt Market Choice Data](https://github.com/hihowme/persp-research-econ_Spr20/blob/master/Finalpaper/data/yogurt100.csv) data from the `bayesm` package in R, and can be found in the data section.

## Code

- [MCMC Implementation Code in R](https://github.com/hihowme/persp-research-econ_Spr20/blob/master/Finalpaper/R_code/MCMC_R.md)

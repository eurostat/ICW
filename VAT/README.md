# How to impute Value-Added Tax in the Household Budget Survey?
![statistics-explained](http://ec.europa.eu/eurostat/statistics-explained/skins/statexpflat/media/StatExplainedTitle-02.png)<br />
![experimental](http://ec.europa.eu/eurostat/statistics-explained/images/9/95/Experimental.png)<br />

Analysing the effect of VAT across groups of households requires not only to measure consumption and its composition at the household level, but also to have detailed pieces of information regarding the VAT rate applied according to the type of purchased good.

On the one hand, the classification commonly used to describe households' consumption for the European Household Budget Survey is the Classification of Individual Consumption According to Purpose ([COICOP](https://unstats.un.org/unsd/cr/registry/regcst.asp?Cl=5)). 
On the other hand, the [European Commission's Directorate-General for Taxation and Customs Union](https://ec.europa.eu/taxation_customs/home_en) disseminates some harmonised data on VAT tax rates in the European Union. The principle of the exercise is therefore to perform a mapping between the different classifications so as to come up with an estimation of the tax rate applied to the products that households declare having purchased in the Household Budget Survey. The scripts made available in this repository perform  such a mapping.
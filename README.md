blogR
=====

Scripts and data to recreate analyses published on http://benjaminlmoore.wordpress.com and/or http://blm.io

Some of these scripts make use of [`blmR`](https://github.com/blmoore/blmR) which can be installed with: `devtools::install_github("blmoore/blmR")`. Calls to `theme_blm()` can just be replaced with `theme_bw()` for similar looking plots.

## Contents 

1. Analysis of UK general election data  [[Post](http://benjaminlmoore.wordpress.com/2014/03/18/guardian-data-blog-uk-elections/) | [Code](R/guardian_UKelection.R)] 

2. Author inflation assessed via the PLOS API  [[Post](http://benjaminlmoore.wordpress.com/2014/04/06/author-inflation-in-academic-literature/) | [Code](R/plos_authInflation.R)]

3. Most "overrated" films via Rotten Tomatoes API [[Post](http://benjaminlmoore.wordpress.com/2014/05/05/what-are-the-most-overrated-films/) | [Code](R/overrated_films.R) | [Interactive](http://rcharts.io/viewer/?6c9ed5eed37fe3c03fa5)]

4. Celebrity twitter demographics by gender [[Post](http://benjaminlmoore.wordpress.com/2014/05/25/celebrity-twitter-followers-by-gender/) | [Code](R/twitter_followersGender.R) | [Interactive](http://blm.io/twitter)]

5. Careers of Hollywood action heroes charted [[Post + Interactive](http://blm.io/action) | [Code 1](blogpy/imdb_getActors.py), [Code 2](R/imdb_actionHeroes.R) ]

6. Are more expensive motorcycle helmets more protective? [[Post](http://blm.io/blog/motorcycle-helmet-safety-price/) | [Code](R/sharp_bikehelms.R)]

7. Scottish independence: what do the polls say? [[Post](http://blm.io/blog/scottish-independence-polls/) | [Code](R/indyref.R)]

8. Recreating vaccination heatmaps in R [[Post](https://benjaminlmoore.wordpress.com/2015/04/09/recreating-the-vaccination-heatmaps-in-r/) | [Code](R/measles_incidence_heatmap.R)]

9. UK general election polls in rCharts and shiny [[Post](http://blm.io/blog/uk-general-election-rcharts-shiny/) | [Code](R/ge2015_polls.R) | [Shiny](shiny/ge2015/)] 


# Replication package for the manuscript *Measuring affective states from technical debt*

* All artifacts used for the experiment can be found in `artifacts/`. In `artifacts/README.md` you find more information about the content.
* For replication please have a look in `docs/`, in particular `docs/index.Rmd` should be of interest. It's written in `RMarkdown` so fire up `RStudio` and have a look. Note, however, that one needs to install quite a lot of packages (in particular [rstan](https://mc-stan.org/users/interfaces/rstan) and [brms](https://github.com/paul-buerkner/brms) are needed), so we recommed you to use the `Docker` image instead (see below).
* The data collected from the experiment can be found in `data/`.
* `analysis.R` is the original script.

The replication package is also published at <https://torkar.github.io/affective_states/>.

## Docker
If you want to replicate the results from the replication package in an easy way, we recommend to install `Docker`, give it plenty of RAM and CPU, and then run the following in the terminal,

```{bash}
docker run -d -p 8787:8787 -e PASSWORD=foo -e ROOT=TRUE torkar/docker_bda:affective_states
```

Then point your browser to <http://localhost:8787>, enter `rstudio` as username and `foo` as password (change password above to whatever you want). In the browser you now have `RStudio`, and a first step would be to go (lower-right corner) to the directory `development/affectice_states/` and click on the project file `Jesper & Erik.Rproj`, and start looking around. 

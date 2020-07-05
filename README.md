# Replication package for the manuscript *Measuring affective states from technical debt*

* All artifacts used for the experiment can be found in `artifacts/`. In `artifacts/README.md` you find more information about the content.
* For replication please have a look in `docs/`, in particular `docs/index.Rmd` should be of interest. It's written in RMarkdown so fire up `RStudio` and have a look. Note, however, that one needs to install quite a lot of packages (in particular [rstan](https://mc-stan.org/users/interfaces/rstan) and [brms](https://github.com/paul-buerkner/brms) are needed)
* The data collected from the experiment can be found in `data/`
* `analysis.R` is the original script

The replication package is also published at <https://torkar.github.io/affective_states/>.

If you want to replicate the results from the replication package in an easy way we recommend to install `Docker`, give it plenty of RAM and CPU, and then run the following in the terminal,

```{bash}
docker run -d -p 8787:8787 -e PASSWORD=foo torkar/affective_states
```

***
`docker_bda` is a submodule, which is used by <hub.docker.com> to automatically build a `Docker`image.

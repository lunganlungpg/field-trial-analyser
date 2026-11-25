FROM rocker/r-ver:4.5.1

LABEL org.opencontainers.image.source="https://github.com/lunganlungpg/field-trial-analyser"
LABEL org.opencontainers.image.description="Serverless Field Trial Analyser (Shinylive / webR + zero-dependency fieldtrial R package)"

WORKDIR /opt/field-trial-analyser
COPY . .

RUN R CMD INSTALL . && Rscript tests/run_tests.R

CMD ["Rscript", "tests/test-package.R"]

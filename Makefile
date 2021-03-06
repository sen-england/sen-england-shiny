# Defs

# Utils

run:  ## run shiny app
	Rscript -e 'shiny::runApp(port = 12345)'

data-prep: data-prep.R ## prepraring data
	Rscript $<

preprocess: preprocess.R  ## preprocess data
	Rscript $<

test:  ## execute unit-testing routines
	@files=tests/test_*.R; \
	echo Files to be tested:; \
	for file in $${files}; do \
	  echo "    - $${file}"; \
	done; \
	for file in $${files}; do \
		echo "File: $${file}"; \
		Rscript $${file}; \
	done

help:
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

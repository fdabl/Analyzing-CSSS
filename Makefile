all: data/derived/processed_data.csv

clean:
		rm -rf data/derived

load_acsss:
		Rscript -e 'devtools::install("ACSSS")'

data/derived/processed_data.csv: data/raw/cleaned_csss-all.csv
		mkdir -p data/derived/
		Rscript scripts/process_data.R $< $@

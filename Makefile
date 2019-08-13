# There is probably a quicker and easier way to do this, but since I am still
# getting use to Makefiles, its probably best to make things as explicit as possible
NODE_ITERS = \
	2005.Beijing_nodes.csv \
	2005.SantaFe_nodes.csv \
	2006.Beijing_nodes.csv \
	2006.SantaFe_nodes.csv \
	2007.Beijing_nodes.csv \
	2007.SantaFe_nodes.csv \
	2008.SantaFe_nodes.csv \
	2009.SantaFe_nodes.csv \
	2010.SantaFe_nodes.csv \
	2011.SantaFe_nodes.csv \
	2012.SantaFe_nodes.csv \
	2013.SantaFe_nodes.csv \
	2014.SantaFe_nodes.csv \
	2015.SantaFe_nodes.csv \
	2016.SantaFe_nodes.csv \
	2017.SantaFe_nodes.csv \
	2018.SantaFe_nodes.csv

all: $(addprefix data/derived/nodefiles/, $(NODE_ITERS))
#data/derived/processed_data.csv

clean:
		rm -rf data/derived

load_acsss:
		Rscript -e 'devtools::install("ACSSS")'

data/derived/processed_data.csv: data/raw/cleaned_csss-all.csv
		mkdir -p data/derived/
		Rscript scripts/process_data.R $< $@

data/derived/nodefiles/%_nodes.csv: data/derived/processed_data.csv
	mkdir -p data/derived/nodefiles/
	Rscript scripts/build_nodefile.R $< $@

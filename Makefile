# These are all the iterations that we will be using, including instances of the
# summer school both in Santa Fe and in Beijing
ITERS = \
	2005.Beijing \
	2005.SantaFe \
	2006.Beijing \
	2006.SantaFe \
	2007.Beijing \
	2007.SantaFe \
	2008.SantaFe \
	2009.SantaFe \
	2010.SantaFe \
	2011.SantaFe \
	2012.SantaFe \
	2013.SantaFe \
	2014.SantaFe \
	2015.SantaFe \
	2016.SantaFe \
	2017.SantaFe \
	2018.SantaFe

# Populate the lists of Node and Edge filenames
NODE_ITERS = $(addsuffix _nodes.csv, $(ITERS))
EDGE_ITERS = $(addsuffix _edges.csv, $(ITERS))


all: $(addprefix data/derived/nodefiles/, $(NODE_ITERS)) \
		 $(addprefix data/derived/edgefiles/, $(EDGE_ITERS)) \
		 figures/prop_womem_over_time.png

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

data/derived/edgefiles/%_edges.csv: data/derived/processed_data.csv
	mkdir -p data/derived/edgefiles/
	Rscript scripts/build_edgefile.R $< $@

figures/prop_womem_over_time.png: data/derived/processed_data.csv
	mkdir -p figures/
	Rscript scripts/plot_prop_women_over_time.R $< $@

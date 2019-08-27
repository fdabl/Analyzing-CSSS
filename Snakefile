from os.path import join as osjoin
import pandas as pd


# store the local path to the dropbox folder in the 'PROJ_HOME_DIR' file.
PROJ_HOME = open('PROJ_HOME_DIR').read().strip()
DATA_DIR = osjoin(PROJ_HOME, 'data')
RAW_DIR = osjoin(DATA_DIR, 'raw')
DERIVED_DIR = osjoin(DATA_DIR, 'derived')
NODEFILE_DIR = osjoin(DERIVED_DIR, 'nodefiles')
EDGEFILE_DIR = osjoin(DERIVED_DIR, 'edgefiles')
NULLDATA_DIR = osjoin(DERIVED_DIR, 'nulldata')

FIG_DIR = osjoin(PROJ_HOME, 'figures')



# Raw data file
CLEANED_RAW_DATA = osjoin(RAW_DIR, 'cleaned_csss-all.csv')
PROCESSED_DATA = osjoin(DERIVED_DIR, 'processed_data.csv')


# The iterations of the summer school
ITERS = [
    '2005.Beijing',
    '2005.SantaFe',
    '2006.Beijing',
    '2006.SantaFe',
    '2007.Beijing',
    '2007.SantaFe',
    '2008.SantaFe',
    '2009.SantaFe',
    '2010.SantaFe',
    '2011.SantaFe',
    '2012.SantaFe',
    '2013.SantaFe',
    '2014.SantaFe',
    '2015.SantaFe',
    '2016.SantaFe',
    '2017.SantaFe',
    '2018.SantaFe'
]

# The node attributes
ATTR = ['gender', 'discp', 'prstg']

# The homophily functions
HOM_FUNCS = ['ei']

###############
# Graph files
###############
NODEFILES = osjoin(NODEFILE_DIR, '{iter}_nodes.csv')
EDGEFILES = osjoin(EDGEFILE_DIR, '{iter}_edges.csv')


###################
# Begin Rules
###################
rule all:
    input:
        expand(NODEFILES, iter=ITERS),
        expand(EDGEFILES, iter=ITERS)

rule process_data:
    input: CLEANED_RAW_DATA
    output: PROCESSED_DATA
    shell: 'Rscript scripts/process_data.R {input} {output}'

rule generate_nodefiles:
    input: PROCESSED_DATA
    output: NODEFILES
    shell: 'Rscript scripts/build_nodefile.R {input} {output}'

rule generate_edgefiles:
    input: PROCESSED_DATA
    output: EDGEFILES
    shell: 'Rscript scripts/build_edgefile.R {input} {output}'

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

###############
# Graph files
###############
NODEFILES = osjoin(NODEFILE_DIR, '{iter}_nodes.csv')
EDGEFILES = osjoin(EDGEFILE_DIR, '{iter}_edges.csv')

###############
# Nulldata
###############
NULLDATA_FILES = osjoin(DERIVED_DIR, 'nulldata', '{iter}', '{func}', '{iter}_{attr}_{func}_nulldata.csv')
NULLDATA_AGGREGATED = osjoin(DERIVED_DIR, 'nulldata', 'aggregated_nulldata.csv')
NULLDATA_PERCENTILE = osjoin(DERIVED_DIR, 'nulldata', 'percentile_nulldata.csv')

NULLDATA_BOOTSTRAP = osjoin(DERIVED_DIR, "bootstrap", "{iter}_nullboot.csv")
NULLDATA_BOOTSTRAP_AGG = osjoin(DERIVED_DIR, "bootstrap", "aggregated_nullboot.csv")


###############
# Figures
###############
PROP_WOMEN_OVER_TIME_PLOT = osjoin(FIG_DIR, 'prop_women_over_time.png')
NULLDATA_HIST_PLOTS = osjoin(FIG_DIR, 'nullhist', '{iter}', '{func}', '{iter}_{attr}_{func}_nullhist.png')
NULL_PERCENTILE_PLOTS = osjoin(FIG_DIR, 'nullpercentile', '{attr}_nullpercentile.png')
BOOTSTRAP_NULL = osjoin(FIG_DIR, 'bootstrapped_nullmodel.png')


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
    '2018.SantaFe',
    '2019.SantaFe'
]

# The node attributes
ATTRIBUTES = ['gender', 'discp', 'prstg', 'pos.var', 'cntry']

# The homophily functions
HOM_FUNCS = ['ei', 'hhi', 'perc_sim']


###################
# Begin Rules
###################
rule all:
    input:
        expand(NODEFILES, iter=ITERS),
        expand(EDGEFILES, iter=ITERS),
        expand(NULLDATA_FILES, iter=ITERS, func=HOM_FUNCS, attr=ATTRIBUTES),
        NULLDATA_AGGREGATED,
        NULLDATA_PERCENTILE,
        PROP_WOMEN_OVER_TIME_PLOT,
        expand(NULLDATA_HIST_PLOTS, iter=ITERS, func=HOM_FUNCS, attr=ATTRIBUTES),
        expand(NULL_PERCENTILE_PLOTS, attr=ATTRIBUTES),
        expand(NULLDATA_BOOTSTRAP, iter = ITERS),
        NULLDATA_BOOTSTRAP_AGG,
        BOOTSTRAP_NULL

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

rule generate_nulldata:
    input: rules.generate_nodefiles.output, rules.generate_edgefiles.output
    output: NULLDATA_FILES
    shell: 'Rscript scripts/generate_nulldata.R {input} {wildcards.attr} {wildcards.func} {output}'

rule aggregate_nulldata:
    #input: expand(rules.generate_nulldata.output, iter =
    input: expand(NULLDATA_FILES, iter=ITERS, func=HOM_FUNCS, attr=ATTRIBUTES)
    output: NULLDATA_AGGREGATED,
    shell: "Rscript scripts/aggregate_nulldata.R {input} {output}"

rule calculate_null_percentiles:
    input: rules.aggregate_nulldata.output
    output: NULLDATA_PERCENTILE
    shell: "Rscript scripts/get_nulldata_percentile.R {input} {output}"

rule homophily_bootstrap:
    input: rules.generate_nodefiles.output,
           rules.generate_edgefiles.output
    output: NULLDATA_BOOTSTRAP
    shell: "Rscript scripts/nulltest_bootstrap.R {input} {output}"

rule aggregate_null_bootstrap:
    #input: expand(rules.generate_nulldata.output, iter =
    input: expand(NULLDATA_BOOTSTRAP, iter=ITERS)
    output: NULLDATA_BOOTSTRAP_AGG,
    shell: "Rscript scripts/aggregate_null_bootstrap.R {input} {output}"

rule plot_prop_women_over_time:
    input: PROCESSED_DATA
    output: PROP_WOMEN_OVER_TIME_PLOT,
    shell: 'Rscript scripts/plot_prop_women_over_time.R {input} {output}'

rule plot_nullhist:
    input: rules.generate_nulldata.output
    output: NULLDATA_HIST_PLOTS
    shell: 'Rscript scripts/plot_nulldata_hist.R {input} {output}'

rule plot_percentile:
    input: rules.calculate_null_percentiles.output
    output: NULL_PERCENTILE_PLOTS
    shell: 'Rscript scripts/plot_percentile_actual_null.R {input} {wildcards.attr} {output}'

rule plot_bootstrapped_nullmodel:
    input: rules.aggregate_null_bootstrap.output,
    output: BOOTSTRAP_NULL,
    shell: 'Rscript scripts/plot_bootstrapped_nullmodel.R {input} {output}'

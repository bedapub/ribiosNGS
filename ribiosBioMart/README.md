About
=====

ribiosBioMart is the 'sans Ensambl' middle ware to access the Ensembl BioMart data inside an plain [MySQL/MariaDB](https://mariadb.org/) database.

It aims to provide the same functions as the [ws-based](https://en.wikipedia.org/wiki/Web_service) [biomaRt](https://bioconductor.org/packages/release/bioc/html/biomaRt.html) R package

Installation
============

You need to install [Bioconductor](https://www.bioconductor.org/install/) and the following R packages from the GitHub repository:

``` r
library(devtools)
install_github("Accio/ribios/ribiosBase")
install_github("Accio/ribios/ribiosBioMart")
```

Additionally, you need an operational MariaDB server instance populated with a copy of the latest [Ensembl BioMart data](ftp://ftp.ensembl.org/pub/release-92/mysql/).

Examples
========

Connect to database server & list available datasets
----------------------------------------------------

``` r
library(ribiosBioMart)
library(RMySQL)

# Your acces data to your database server
# listLocalDatasets, useLocalMart, and getLocalBM will use this to create a 
# single-use connection for each function call
conn <- EnsemblDBCredentials(host = "TODO",
                             port = 12345,
                             user = "TODO",
                             passwd = "TODO",
                             ensembl_version = 93)

# Alternatively: Use a direct connection for the entire session.
# You will need to manage / close it yourself
#conn <- dbConnect (MySQL (), 
#                   user="TODO", 
#                   password="TODO",
#                   dbname="ensembl_mart_93", 
#                   host="TODO", 
#                   port=12345)
ds <-listLocalDatasets(conn)
ds[1:5,]
```

Initialize mart object and retrieve available attributes & filters
------------------------------------------------------------------

``` r
localMart = useLocalMart(conn, dataset="hsapiens_gene_ensembl")

attrs = listLocalAttributes(localMart)
attrs[1:5,]

filters = listLocalFilters(localMart)
filters[1:5,]
```

Perform queries
---------------

Here: [Annotate a set of Affymetrix identifiers with HUGO symbol and chromosomal locations of corresponding genes](https://www.bioconductor.org/packages/devel/bioc/vignettes/biomaRt/inst/doc/biomaRt.html):

``` r
affyids=c("202763_at","209310_s_at","207500_at")
getLocalBM(attributes = c('affy_hg_u133_plus_2', 'hgnc_symbol', 'chromosome_name',
                         'start_position', 'end_position', 'band'),
           filters = 'affy_hg_u133_plus_2', 
           values = affyids, 
           mart = localMart)
```

The functionality of *getLocalBM* should be more or less the same as those of *getBM* in the biomaRt package

Limitations
===========

The *getLocalBM* does not support filters / attributes that require access to a database schema other than *ensembl\_mart\_92*. This package will therefore abort its execution with an error if the user tries to utilize one of the following attributes / filters:

Filters:
--------

-   "name\_2" ("qtl\_feature.name\_2")
-   "qtl\_region" ("qtl\_feature.qtl\_region")
-   "go\_parent\_term" ("go\_clos")
-   "go\_parent\_name" ("go\_name")
-   "so\_consequence\_name" ("so\_mini\_parent\_name")

Attributes:
-----------

-   structure\_gene\_stable\_id
-   structure\_transcript\_stable\_id
-   structure\_translation\_stable\_id
-   structure\_canonical\_transcript\_id
-   structure\_chrom\_name
-   structure\_gene\_chrom\_start
-   structure\_gene\_chrom\_end
-   structure\_transcript\_chrom\_start
-   structure\_transcript\_chrom\_end
-   structure\_transcription\_start\_site
-   structure\_transcript\_length
-   structure\_transcript\_chrom\_strand
-   structure\_external\_gene\_name
-   structure\_external\_source\_name
-   structure\_5\_utr\_start
-   structure\_5\_utr\_end
-   structure\_3\_utr\_start
-   structure\_3\_utr\_end
-   structure\_cds\_length
-   structure\_cdna\_length
-   structure\_peptide\_length
-   struct\_transcript\_count
-   structure\_translation\_count
-   structure\_description
-   structure\_biotype
-   structure\_ensembl\_exon\_id
-   structure\_cds\_start
-   structure\_cds\_end
-   homologs\_ensembl\_gene\_id
-   homologs\_ensembl\_transcript\_id
-   homologs\_ensembl\_peptide\_id
-   homologs\_canonical\_transcript\_id
-   homologs\_chromosome\_name
-   homologs\_start\_position
-   homologs\_end\_position
-   homologs\_strand
-   homologs\_band
-   homologs\_external\_gene\_name
-   homologs\_external\_gene\_source
-   homologs\_ensembl\_CDS\_length
-   homologs\_ensembl\_cDNA\_length
-   homologs\_ensembl\_peptide\_length
-   homologs\_transcript\_count
-   homologs\_percentage\_gc\_content
-   homologs\_description
-   snp\_ensembl\_gene\_id
-   snp\_ensembl\_transcript\_id
-   snp\_ensembl\_peptide\_id
-   sequence\_canonical\_transcript\_id
-   snp\_chromosome\_name
-   snp\_start\_position
-   snp\_end\_position
-   snp\_strand
-   snp\_band
-   snp\_external\_gene\_name
-   snp\_external\_gene\_source
-   snp\_ensembl\_CDS\_length
-   snp\_ensembl\_cDNA\_length
-   snp\_ensembl\_peptide\_length
-   snp\_transcript\_count
-   snp\_percentage\_gc\_content
-   snp\_description
-   transcript\_exon\_intron
-   gene\_exon\_intron
-   transcript\_flank
-   gene\_flank
-   coding\_transcript\_flank
-   coding\_gene\_flank
-   5utr
-   3utr
-   gene\_exon
-   cdna
-   coding
-   peptide
-   upstream\_flank
-   downstream\_flank
-   sequence\_gene\_stable\_id
-   sequence\_description
-   sequence\_external\_gene\_name
-   sequence\_external\_source\_name
-   sequence\_str\_chrom\_name
-   sequence\_gene\_chrom\_start
-   sequence\_gene\_chrom\_end
-   sequence\_gene\_biotype
-   sequence\_family
-   sequence\_upi
-   sequence\_uniprot\_swissprot\_accession
-   sequence\_uniprot\_sptrembl
-   sequence\_uniprot\_sptrembl\_predicted
-   sequence\_transcript\_stable\_id
-   sequence\_translation\_stable\_id
-   sequence\_canonical\_transcript\_id
-   sequence\_biotype
-   sequence\_transcript\_biotype
-   sequence\_transcript\_chrom\_strand
-   sequence\_transcript\_chrom\_start
-   sequence\_transcript\_chrom\_end
-   sequence\_transcription\_start\_site
-   sequence\_peptide\_length
-   sequence\_transcript\_length
-   sequence\_cdna\_length
-   sequence\_exon\_stable\_id
-   sequence\_type
-   sequence\_exon\_chrom\_start
-   sequence\_exon\_chrom\_end
-   sequence\_exon\_chrom\_strand
-   sequence\_rank
-   sequence\_phase
-   sequence\_end\_phase
-   sequence\_cdna\_coding\_start
-   sequence\_cdna\_coding\_end
-   sequence\_genomic\_coding\_start
-   sequence\_genomic\_coding\_end
-   sequence\_constitutive

Final Note
==========

If you used a direct DBIConnection for the database access, don't forget to close your database connections

``` r
# dbDisconnect (conn)
```

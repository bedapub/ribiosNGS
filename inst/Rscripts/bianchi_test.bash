#!/bin/bash

export RS_DIR=/SOFT/bi/apps/ribios/ribiosBic/inst/Rscripts/
export DATA_DIR=/SOFT/bi/apps/ribios/ribiosBic/inst/extdata/
export OUT_DIR=/DATA/tmp/

## testing file -> RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample_exprs.txt -outfile ${OUT_DIR}/bianchi_test_tab.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample_exprs.txt -ffile ${DATA_DIR}/sample_fdata.txt -outfile ${OUT_DIR}/bianchi_test_tab_fdata.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample_exprs.txt -pfile ${DATA_DIR}/sample_pdata.txt -outfile ${OUT_DIR}/bianchi_test_tab_pdata.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample_exprs.txt -ffile ${DATA_DIR}/sample_fdata.txt -pfile ${DATA_DIR}/sample_pdata.txt -outfile ${OUT_DIR}/bianchi_test_tab_fpdata.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample_exprs.txt -chiptype HG-U133_PLUS_2 -ffile ${DATA_DIR}/sample_fdata.txt -pfile ${DATA_DIR}/sample_pdata.txt -outfile ${OUT_DIR}/bianchi_test_tab_fpdata_anno.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample_exprs.txt -chiptype HG-U133_PLUS_2 -summfeat false -ffile ${DATA_DIR}/sample_fdata.txt -pfile ${DATA_DIR}/sample_pdata.txt -outfile ${OUT_DIR}/bianchi_test_tab_fpdata_anno_nosumm.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample_exprs.txt -chiptype HG-U133_PLUS_2  -summfeat false -ffile ${DATA_DIR}/sample_fdata.txt -topvar 100 -pfile ${DATA_DIR}/sample_pdata.txt -outfile ${OUT_DIR}/bianchi_test_tab_fpdata_anno_nosumm_top100.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample_exprs.txt -chiptype HG-U133_PLUS_2 -summfeat true -ffile ${DATA_DIR}/sample_fdata.txt -pfile ${DATA_DIR}/sample_pdata.txt -outfile ${OUT_DIR}/bianchi_test_tab_fpdata_anno_summ.RData

${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample.gct -outfile ${OUT_DIR}/bianchi_test_gct.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample.gct -clsfile ${DATA_DIR}/sample.cls -outfile ${OUT_DIR}/bianchi_test_gct_cls.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample.gct -clsfile ${DATA_DIR}/sample.cls -chiptype HG_U95AV2 -outfile ${OUT_DIR}/bianchi_test_ann_gct_cls.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample.gct -clsfile ${DATA_DIR}/sample.cls -chiptype HG_U95AV2 -summfeat false -outfile ${OUT_DIR}/bianchi_test_ann_gct_cls_nosumm.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample.gct -clsfile ${DATA_DIR}/sample.cls -chiptype HG_U95AV2 -summfeat false -topvar 100 -outfile ${OUT_DIR}/bianchi_test_ann_gct_cls_nosumm_top100.RData
${RS_DIR}/bianchi_preprocess.Rscript -infile ${DATA_DIR}/sample.gct -clsfile ${DATA_DIR}/sample.cls -chiptype HG_U95AV2 -summfeat true -outfile ${OUT_DIR}/bianchi_test_ann_gct_cls_summ.RData

## testing RData->ISA
./bianchi_ISA.Rscript -infile ${OUT_DIR}/bianchi_test_tab.RData -binary -outfile ${OUT_DIR}/bianchi_test_tab.isa.bic

## testing RData -> rqubic
./bianchi_QUBIC.Rscript -infile ${OUT_DIR}/bianchi_test_tab.RData -binary -outfile ${OUT_DIR}/bianchi_test_tab.qubic.bic
./bianchi_QUBIC.Rscript -infile ${OUT_DIR}/bianchi_test_tab.RData -binary -rank 1 -prop 0.1 -outfile ${OUT_DIR}/bianchi_test_tab.qubic.nocore.bic
./bianchi_QUBIC.Rscript -infile ${OUT_DIR}/bianchi_test_tab.RData -binary -rank 1,2,3 -prop 0.1,0.2,0.4 -coreNo 3 -outfile ${OUT_DIR}/bianchi_test_tab.qubic_core.bic
./bianchi_QUBIC.Rscript -infile ${DATA_DIR}/sample_exprs.txt -rank 1,3 -prop 0.1,0.2 -coreNo 3 -outfile ${OUT_DIR}/bianchi_test_tab.qubic_txt.bic
./bianchi_QUBIC.Rscript -infile ${DATA_DIR}/sample.gct -rank 1,3 -prop 0.1,0.2 -coreNo 3 -outfile ${OUT_DIR}/bianchi_test_tab.qubic_gct.bic -parafile ${OUT_DIR}/bianchi_test_tab.qubic_gct.para
./bianchi_QUBIC.Rscript -infile ${OUT_DIR}/bianchi_test_tab.RData -binary -rank 1,2 -prop 0.1,0.4 -coreNo 3 -outfile ${OUT_DIR}/bianchi_test_tab_all.qubic.bic -tolerance 0.85 -filter 0.1 -report 10 -parafile ${OUT_DIR}/bianchi_test_tab_all.qubic.para


## testing ISA+rqubic->bic

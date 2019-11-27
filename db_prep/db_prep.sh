#!/bin/bash

###Static variables
BUILD="hg38"
ANNO="/home/pss41/resources/annovar/"
GATK="/data/Resources/Software/Javas/GenomeAnalysisTK.jar"
REF="/data/Resources/References/hg38.bwa/hg38.bwa.fa"
REF_VCF="/home/pss41/G1K_vcf/ALL.WGS_GRCh38.genotypes.CHR.20170504.bcf"
ADMIXTURE="/home/pss41/admixture/admixture"
CHRS="/home/pss41/db_prep/CHRMS.bed.gz"
CORES=16

###Command-line variables and script start
INPUT_LIST="NULL"

###Help block - provides details on arguments
for arg in "$@"; do
	if [[ "$arg" == "--help" ]]; then
		echo -e "
Help Documentation
		"			               
		exit
	fi
done

###Default output - No arguments
if [[ $# -eq 0 ]]; then
	echo -e `date +[%D-%R]` "no args"
	exit
fi

###Command-line argument passing
while [[ $# > 1 ]]
	do
	key="$1"
	case $key in
		--in|-i)
		INPUT_LIST=${2}
		shift
		;;
	esac
	shift
done

START_DIR=$(pwd)

cp ${INPUT_LIST} ${HOME}/INPUT.list
cp ${CHRS} ${HOME}/CHRMS.bed.gz
cd ${HOME}
mkdir ${HOME}/.temp_data
mv INPUT.list .temp_data/
mv CHRMS.bed.gz .temp_data/
cd .temp_data/

zcat CHRMS.bed.gz | cut -f1 > chr_list

echo -e "COHORT\tDATA_ID\tSAMPLE_ID\tAGE\tPHENOTYPE\tMORT_STATUS" > sample_data.info
echo -e "SEX\tETHNO" > pheno_sex.info

for DATA in `cat INPUT.list`; do

	VCF_NAME=$(echo ${DATA} | sed 's#.*/##')
	COHORT_NAME=$(echo ${DATA} | sed 's#.*/##g' | cut -d . -f1)
	FOLDER=$(echo ${DATA} | sed 's#\(.*/\)\(.*\)#\1#')

	echo ${VCF_NAME}
	echo ${COHORT_NAME}
	echo ${DATA}
	echo ${FOLDER}

	cp ${DATA} ${VCF_NAME}
	
	bcftools view -Ou --min-ac 1 --max-af 0.05 -i 'QUAL > 30' ${VCF_NAME} | \
	bcftools view -i 'AVG(FMT/DP) > 10 & AVG(FMT/GQ) > 30' -Ou | \
	bcftools filter -e 'F_MISSING > 0.1' -Ou | \
	bcftools norm -m -any -f ${REF} -Oz -o ${VCF_NAME}.gz
	bcftools index ${VCF_NAME}.gz
	bcftools view -R CHRMS.bed.gz -Oz -o ${VCF_NAME}_sub.vcf.gz ${VCF_NAME}.gz

	mv ${VCF_NAME}_sub.vcf.gz ${VCF_NAME}.gz	
	bcftools index ${VCF_NAME}.gz
	
	cat chr_list | xargs -n1 -P${CORES} -I {} mkdir temp_{}
	cat chr_list | xargs -n1 -P${CORES} -I {} bcftools view -r {} -Oz -o temp_{}/${VCF_NAME}.splt.{}.vcf.gz ${VCF_NAME}.gz
	
	cat chr_list | xargs -n1 -P${CORES} -I {} ${ANNO}table_annovar.pl temp_{}/${VCF_NAME}.splt.{}.vcf.gz ${ANNO}humandb/ -buildver ${BUILD} \
		-out temp_{}/${COHORT_NAME}.{} \
		-remove -protocol refGene,1000g2015aug_all,exac03,avsnp150,dbnsfp33a,clinvar_20180603,cosmic70,dbscsnv11 \
		-operation g,f,f,f,f,f,f,f \
		-nastring . \
		-vcfinput
	
	cat chr_list | xargs -n1 -P${CORES} -I {} mv temp_{}/${COHORT_NAME}.{}.${BUILD}_multianno.vcf ${COHORT_NAME}.{}.${BUILD}_multianno.vcf
	rm -r temp_*	
	
	ls *chr*_multianno.vcf | sort -V > chr_files
	
	bcftools concat -f chr_files -Ov -o ${COHORT_NAME}.${BUILD}_multianno.vcf
	rm *chr*_multianno.vcf
	rm chr_files

	bcftools view -h ${COHORT_NAME}.${BUILD}_multianno.vcf > ${COHORT_NAME}.header.txt
	sed -i 's/\(##INFO=<ID=ExAC.*\)Number=\.,Type=String/\1Number=1,Type=Float/g' ${COHORT_NAME}.header.txt
	bcftools reheader -h ${COHORT_NAME}.header.txt ${COHORT_NAME}.${BUILD}_multianno.vcf | bcftools view -Ob -i 'INFO/ExAC_ALL < 0.05 & INFO/1000g2015aug_all < 0.05 | INFO/ExAC_ALL == "." & INFO/1000g2015aug_all == "." | INFO/ExAC_ALL == "." & INFO/1000g2015aug_all < 0.05 | INFO/ExAC_ALL < 0.05 & INFO/1000g2015aug_all == "."' | \
	bcftools view -Ov \
		-e 'INFO/Func.refGene="downstream" | INFO/Func.refGene="intergenic" | INFO/Func.refGene~"ncRNA" | INFO/Func.refGene~"upstream" | INFO/Func.refGene~"UTR"' \
		-o ${COHORT_NAME}.FILT.vcf

	WD=$(pwd)	
	##Run R script to filter variants
	Rscript ${START_DIR}/db_filtering.R ${WD} ${COHORT_NAME}

	## Population analysis
	cat ${VCF_NAME} | grep -m 1 "#C" | tr '\t' '\n' | sed -e '1,9d' > sample_list_pop
	bcftools view -h ${REF_VCF} | grep -m 1 "#C" | tr '\t' '\n' | sed -e '1,9d' >> sample_list_pop
	
	bcftools view -Ob -o ${COHORT_NAME}.vcf.gz ${VCF_NAME}
	bcftools index ${COHORT_NAME}.vcf.gz

	bcftools merge -Ob ${COHORT_NAME}.vcf.gz ${REF_VCF} | \
	bcftools view --max-alleles 2 --min-alleles 2 -Ob |
	bcftools view --min-alleles 2 \
			--max-alleles 2 \
			-Ob \
			-o ${COHORT_NAME}_REF.bcf

	plink1.90 --bcf ${COHORT_NAME}_REF.bcf \
			--out ${COHORT_NAME}_REF.maf0.05 \
			--make-bed --maf 0.05 \
			--hwe 0.00005 \
			--const-fid --biallelic-only \
			--geno 0.005 \
			--bp-space 2000 \
			--allow-extra-chr

	${ADMIXTURE} ${COHORT_NAME}_REF.maf0.05.bed 5
 
	Rscript ${START_DIR}/admixture_plotting.R ${COHORT_NAME}
 
	Rscript ${START_DIR}/PCA_data.R ${COHORT_NAME}
	
	sed -i '1d' admixture.${COHORT_NAME}.tsv	
	
	## Sex calculation
	ls ${FOLDER}*.bam > bam_${COHORT_NAME}_sexcalc.txt

	echo -e "Sample\tX/Y_ratio\tPredicted_sex" > sex_calc_${COHORT_NAME}.tsv
	for i in `cat bam_${COHORT_NAME}_sexcalc.txt`; do
   		## ratio calculator
        	X=$(samtools view -c ${i} chrX 2> /dev/null)
	      	Y=$(samtools view -c ${i} chrY 2> /dev/null)
	       	ratio=$(( X / Y ))

	       	if [[ "$ratio" -le 10 ]]; then
                	echo -e "${i}\t${ratio}\tM" >> sex_calc_${COHORT_NAME}.tsv
        	elif [[ "$ratio" -ge 19 ]]; then
                	echo -e "${i}\t${ratio}\tF" >> sex_calc_${COHORT_NAME}.tsv
        	else
                	echo -e "${i}\t${ratio}\t?" >> sex_calc_${COHORT_NAME}.tsv
     	  	fi
	done

	vim -c "%s%/\S\+/%%g|wq" sex_calc_${COHORT_NAME}.tsv
	vim -c "%s%_hg38\S\+%%g|wq" sex_calc_${COHORT_NAME}.tsv
	sed -i '1d' sex_calc_${COHORT_NAME}.tsv

	paste <(cut -f3 sex_calc_${COHORT_NAME}.tsv) <(sort -k1,1 admixture.${COHORT_NAME}.tsv | cut -f7) >> pheno_sex.info
	
done

paste pheno_sex.info sample_data.info > sample_data.final.info
	
Rscript ${START_DIR}/db_cohort_merge.R ${WD}

#mv db_*.RData ${HOME}/
#mv sample_data.final.info ${HOME}/

#cd ..
#rm -r .temp_data/ 

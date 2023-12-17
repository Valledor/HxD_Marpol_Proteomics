#!bin/bash

GENES=/mnt/d/DSMarchantia_mofa2/MetaNetworks/input/Proteins.txt
INPUTDIR=/mnt/d/DSMarchantia_mofa2/MetaNetworks/input/medianCentered_sex/
OUTPUTDIR=/mnt/d/DSMarchantia_mofa2/MetaNetworks/output_Treatment/
dirs=$(ls $OUTPUTDIR)

echo $dirs
for d in $dirs
 do
 INPUT=$INPUTDIR/$d/*.tsv
 dos2unix $INPUT
 echo "Processing $d dir ... | $(date)"
 cd $OUTPUTDIR/$d/
 # GENIE3
 /home/rocesv/seidr/genie3 -i $INPUT -g $GENES -o genie3_scores.tsv --scale
 /home/rocesv/seidr/seidr import -r -z -n GENIE3 -o genie3_scores.sf -F m -i genie3_scores.tsv -g $GENES
 # llr-ensemble
 /home/rocesv/seidr/llr-ensemble -i $INPUT -g $GENES -s -x 7 -X 14 -P 2860 -p 715
 /home/rocesv/seidr/seidr import -r -z -n LLR -o llr_scores.sf -F m -i llr_scores.tsv -g $GENES
 # svm-ensemble
 /home/rocesv/seidr/svm-ensemble -i $INPUT -g $GENES -s -x 7 -X 14 -P 2860 -p 715 -k POLY
 /home/rocesv/seidr/seidr import -r -z -n SVM -o svm_scores.sf -F m -i svm_scores.tsv -g $GENES
 # CLR;mi
 /home/rocesv/seidr/mi -m CLR -i $INPUT -g $GENES -M $OUTPUTDIR/$d/mi_scores.tsv -o $OUTPUTDIR/$d/clr_scores.tsv
 /home/rocesv/seidr/seidr import -r -u -z -n CLR -o clr_scores.sf -F lm -i clr_scores.tsv -g $GENES
 # ARACNE;mi
 /home/rocesv/seidr/mi -m ARACNE -i $INPUT -g $GENES -M $OUTPUTDIR/$d/mi_scores.tsv -o $OUTPUTDIR/$d/aracne_scores.tsv
 /home/rocesv/seidr/seidr import -r -u -z -n ARACNE -o aracne_scores.sf -F lm -i aracne_scores.tsv -g $GENES
 /home/rocesv/seidr/seidr import -r -u -n MI -o mi_scores.sf -F lm -i mi_scores.tsv -g $GENES
 # Partial Correlation
 /home/rocesv/seidr/pcor -i $INPUT -g $GENES -s
 /home/rocesv/seidr/seidr import -A -r -u -n PCOR -o pcor_scores.sf -F lm -i pcor_scores.tsv -g $GENES
 # Spearman correlation
 /home/rocesv/seidr/correlation -m spearman -i $INPUT -g $GENES -s
 /home/rocesv/seidr/seidr import -A -r -u -n SPEARMAN -o spearman_scores.sf -F lm -i spearman_scores.tsv -g $GENES
 # NARROMI
 /home/rocesv/seidr/narromi -i $INPUT -g $GENES
 /home/rocesv/seidr/seidr import -r -z -n NARROMI -o narromi_scores.sf -F m -i narromi_scores.tsv -g $GENES
 # PLSNET
 /home/rocesv/seidr/plsnet -i $INPUT -g $GENES -o $OUTPUTDIR/$d/plsnet_scores.tsv --scale
 /home/rocesv/seidr/seidr import -r -z -n PLSNET -o plsnet_scores.sf -F m -i plsnet_scores.tsv -g $GENES
 # TIGRESS
 /home/rocesv/seidr/tigress -i $INPUT -g $GENES --scale -o $OUTPUTDIR/$d/tigress_scores.tsv
 /home/rocesv/seidr/seidr import -r -z -n TIGRESS -o tigress_scores.sf -F m -i tigress_scores.tsv -g $GENES
 # TOMSIMILARITY
 /home/rocesv/seidr/tomsimilarity -i $INPUT -g $GENES -s -m bicor -T signed
 /home/rocesv/seidr/seidr import -A -r -u -n TOM -o bicor_tom_scores.sf -F lm -i bicor_tom_scores.tsv -g $GENES
 # Elastic-Net
 #/home/rocesv/seidr/el-ensemble -i $INPUT -g $GENES -s -x 7 -X 14 -P 2860 -p 715 -o elnet_scores.tsv
 #/home/rocesv/seidr/seidr import -r -z -n ELNET -o elnet_scores.sf -F m -i elnet_scores.tsv -g $GENES
 # AGGREGATING
 /home/rocesv/seidr/seidr aggregate -m irp genie3_scores.sf llr_scores.sf svm_scores.sf clr_scores.sf aracne_scores.sf mi_scores.sf pcor_scores.sf spearman_scores.sf narromi_scores.sf plsnet_scores.sf tigress_scores.sf bicor_tom_scores.sf #elnet_scores.sf
 # POST-PROCESSING
 /home/rocesv/seidr/seidr backbone -F 1 -o aggregated.bb.1.sf aggregated.sf
 /home/rocesv/seidr/seidr reheader aggregated.bb.1.sf
 /home/rocesv/seidr/seidr graphstats aggregated.bb.1.sf > aggregated.bb.1.sf_graphstats.txt
 /home/rocesv/seidr/seidr backbone -F 1.28 -o aggregated.bb.1.28.sf aggregated.sf
 /home/rocesv/seidr/seidr reheader aggregated.bb.1.28.sf
 /home/rocesv/seidr/seidr graphstats aggregated.bb.1.28.sf > aggregated.bb.1.28.sf_graphstats.txt
 done

# Examples commands for network comparison
# /home/rocesv/seidr/seidr compare -t /mnt/d/DSMarchantia_mofa2/MetaNetworks/input/dict.txt /mnt/d/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment/pending/T2_Female/aggregated.bb.1.sf /mnt/d/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment/pending/T2_Male/aggregated.bb.1.sf > /mnt/d/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment_diff/T2_diff/Metanetwork_Compare_T2_FemaleVSMale_Edges_1.txt
# /home/rocesv/seidr/seidr compare -n -t /mnt/d/DSMarchantia_mofa2/MetaNetworks/input/dict.txt /mnt/d/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment/pending/T2_Female/aggregated.bb.1.sf /mnt/d/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment/pending/T2_Male/aggregated.bb.1.sf > /mnt/d/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment_diff/T2_diff/Metanetwork_Compare_T2_FemaleVSMale_Nodes_1.txt
# Example command for infomap network community discovery
# infomap MetaNetwork_FemaleGeneral_InfomapInputPajek.txt . --ftree --clu -s 404 -N 1 -v
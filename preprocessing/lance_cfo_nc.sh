#
# lance l'extraction et la mise en forme des netcdf swim
# puis lance filtre_SWI
#
# 11/2020
#----------------------------------------

dat=$1

#repertoire ou se trouvent les exe fortran
#directory where fortran exe are located

#repertoire de travail
#work directory

export chemin_fabrique= "/path/to/preprocessing/"

#sh ext_cfosat.sh $dat
##if [ -a read_cfo_nc ]; then
##  rm read_cfo_nc 
##fi
#cp $repexe/read_cfo_nc ./
list_fic=`ls cnes_obs-wave_glo_phy-spc_nrt_cfo-l3*.nc`
#boucle sur les netcdf
for fic in $list_fic
do
${chemin_fabrique}/read_cfo_nc $fic ${fic}_10.tmp 3
done

cat *10.tmp > SWI_WV1
#cp $repexe/filtre_sar.for_ori ./
datdeb=`awk ' NR == 1 {print int(substr($1,3,10)/100)*100} ' SWI_WV1`
datfin=`awk ' NF == 5 {fin=substr($1,3,10)}
END {print fin} ' SWI_WV1`
# calcul de l'intervalle temporel
# time interval calculation
echo "(`date -d 20${datfin:0:6} +%s` - `date -d 20${datdeb:0:6} +%s`)/ (3*3600) + 1" | bc -l > nb.tmp
nb=`awk ' {print int($1)}' nb.tmp`
sed "s/yymmddhhmm/$datdeb/g" filtre_sar.for_ori > job.tmp
sed "s/zz/$nb/g" job.tmp > filtre_sar.for
ifort filtre_sar.for
./a.out

#rm SWIM.nc??? SWIM.nc*tmp nb.tmp job.tmp SWIM.txt

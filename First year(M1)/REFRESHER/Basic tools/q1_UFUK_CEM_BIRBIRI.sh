#UFUK CEM BIRBIRI


#!/bin/sh


ls_code=ls
ls_l_code="ls -l"
rm_code="rm"
mv_code="mv"

ls_code=ls
echo ${#ls_code}

if [[ ${ls_code} == $3 ]]
then ls ~/$1
elif [[ ${ls_l_code} == $3 ]]
then ls -l ~/$1

elif [[ ${rm_code} == $3 ]]
then echo rm -rf *

elif [[ ${mv_code} == $3 ]]
then

for file in *
do
echo ${file}
old_name=${file}
extension="$2"
new_name=${old_name}${extension} #concatenate old fiel name and the extension string
mv $file $new_name #rename files

done
fi

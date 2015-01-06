echo "compile demon, executes make on changes in source files in this dir tree... invoke with makefile parameter"
chsum1=""
while [[ true ]]
do
    chsum2=`find . \( -name "*.cpp" -or -name "*.h" -or -name "*.rs" \)  -type f -exec md5sum {} \;`
    #echo $chsum1 $chsum2
    if [[ $chsum1 != $chsum2 ]] ; then
    	clear
    	echo "remake..."
        make -j4 $1
        chsum1=$chsum2
    	echo "remake done..."
    fi
    sleep 1
done


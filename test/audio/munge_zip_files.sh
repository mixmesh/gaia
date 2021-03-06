#/bin/bash
unzip -jo *.zip
for f in *.wav; do
    f2=`echo $f | sed -e 's/[ ()]//g'`
    mv "${f}" "${f2}" 2>/dev/null
    f3=${f2%.wav}.s16
    echo "Converts ${f2} to ${f3}"
    sox "${f2}" -c 1 -r 16000 -b 16 -e signed-integer --endian little -t raw "${f3}"
done

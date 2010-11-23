#!/bin/bash
ok=0
failed=0
for i in $( ls *Request.xml ); do
	item=${i:0:6}
	./run_test.sh $item > /dev/null
	res=$?
	if [ "$res" = "0" ]; then
		status="OK"
		ok=$((ok+1))
	else
		failed=$((failed+1))
		if [ "$res" = "1" ]; then
			status="Result from XACML engine differs from expected!!!!"
		elif [ "$res" = "2" ]; then
			status="Result from DKAL using COMM RULES differs from expected!!!!"
		elif [ "$res" = "3" ]; then
			status="Result from DKAL using INFONS differs from expected!!!!"
		elif [ "$res" = "4" ]; then
			status="Abnormal termination!"
		else
			status="Unknown"
		fi
	fi
	
	echo $item: $status
	echo "-------------------------------------------------------------------------"
done
echo "Done: $ok tests passed, $failed tests failed"


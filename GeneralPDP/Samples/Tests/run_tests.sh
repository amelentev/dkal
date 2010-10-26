#!/bin/bash
for i in $( ls *Request.xml ); do
	item=${i:0:6}
	./run_test.sh $item > /dev/null
	res=$?
	if [ "$res" = "0" ]; then
		status="OK"
	elif [ "$res" = "1" ]; then
		status="Result from XACML engine differs from expected!!!!"
	elif [ "$res" = "2" ]; then
		status="Result from DKAL engine differs from expected!!!!"
	elif [ "$res" = "3" ]; then
		status="Abnormal termination!"
	else
		status="Unknown"
	fi
	echo $item: $status
	echo "------------------------------------------------------------------------------------------------"
done


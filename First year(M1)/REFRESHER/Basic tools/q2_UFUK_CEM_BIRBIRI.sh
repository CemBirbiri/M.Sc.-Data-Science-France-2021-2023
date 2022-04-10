
#UFUK CEM BIRBIRI


#!/bin/sh

r=$(( $RANDOM % 100 + 0 ))
echo "Random number is:"
echo $r

flag=1
while [ $flag -eq 1 ]
do
echo "Give me a number between 0-100 please"
 read user_number
	if ((r ==user_number));
then
echo "Well done"
flag=0

elif ((r < user_number));
then
echo "Your number is greater than the random number."

elif ((r > user_number));
then
echo "Your number is less than the random number"
 
fi 

done


#! /bin/zsh

for delta10 in {20..50}
do
for ratio20 in {20..44}
do
ruby general_argument.rb $delta10 $ratio20 | ~/src/the-omega-project/omega_calc/obj/oc | grep -v "#" | ./maru_batu.rb $delta10 $ratio20
echo ""
done
done


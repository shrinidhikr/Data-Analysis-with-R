#based on income range

f = file.choose()
f1 = read.csv(f)
f1

ins_yes_based_inc=vector()
ins_yes_based_inc
ins_no_based_inc=vector()
ins_no_based_inc

for(i in 1:nrow(f1))
{
   ins_yes_based_inc[i]=0
   ins_no_based_inc[i]=0
}

for(i in 1:nrow(f1))
{
  if(f1[i,1]>=10000&&f1[i,1]<20000&&f1[i,4]=="Y")
    ins_yes_based_inc[1]=ins_yes_based_inc[1]+1
  if(f1[i,1]>=10000&&f1[i,1]<20000&&f1[i,4]=="N")
    ins_no_based_inc[1]=ins_no_based_inc[1]+1
  if(f1[i,1]>=20000&&f1[i,1]<30000&&f1[i,4]=="Y")
    ins_yes_based_inc[2]=ins_yes_based_inc[2]+1
  if(f1[i,1]>=20000&&f1[i,1]<30000&&f1[i,4]=="N")
    ins_no_based_inc[2]=ins_no_based_inc[2]+1
  if(f1[i,1]>=30000&&f1[i,1]<40000&&f1[i,4]=="Y")
    ins_yes_based_inc[3]=ins_yes_based_inc[3]+1
  if(f1[i,1]>=30000&&f1[i,1]<40000&&f1[i,4]=="N")
    ins_no_based_inc[3]=ins_no_based_inc[3]+1
  if(f1[i,1]>=40000&&f1[i,1]<50000&&f1[i,4]=="Y")
    ins_yes_based_inc[4]=ins_yes_based_inc[4]+1
  if(f1[i,1]>=40000&&f1[i,1]<50000&&f1[i,4]=="N")
    ins_no_based_inc[4]=ins_no_based_inc[4]+1
  if(f1[i,1]>=50000&&f1[i,1]<60000&&f1[i,4]=="Y")
    ins_yes_based_inc[5]=ins_yes_based_inc[5]+1
  if(f1[i,1]>=50000&&f1[i,1]<60000&&f1[i,4]=="N")
    ins_no_based_inc[5]=ins_no_based_inc[5]+1
  if(f1[i,1]>=60000&&f1[i,1]<70000&&f1[i,4]=="Y")
    ins_yes_based_inc[6]=ins_yes_based_inc[6]+1
  if(f1[i,1]>=60000&&f1[i,1]<70000&&f1[i,4]=="N")
    ins_no_based_inc[6]=ins_no_based_inc[6]+1
  if(f1[i,1]>=70000&&f1[i,1]<80000&&f1[i,4]=="Y")
    ins_yes_based_inc[7]=ins_yes_based_inc[7]+1
  if(f1[i,1]>=70000&&f1[i,1]<80000&&f1[i,4]=="N")
    ins_no_based_inc[7]=ins_no_based_inc[7]+1
  if(f1[i,1]>=80000&&f1[i,1]<90000&&f1[i,4]=="Y")
    ins_yes_based_inc[8]=ins_yes_based_inc[8]+1
  if(f1[i,1]>=80000&&f1[i,1]<90000&&f1[i,4]=="N")
    ins_no_based_inc[8]=ins_no_based_inc[8]+1
  if(f1[i,1]>=90000&&f1[i,1]<100000&&f1[i,4]=="Y")
    ins_yes_based_inc[9]=ins_yes_based_inc[9]+1
  if(f1[i,1]>=90000&&f1[i,1]<100000&&f1[i,4]=="N")
    ins_no_based_inc[9]=ins_no_based_inc[9]+1
}  

ins_no_based_inc
ins_yes_based_inc

ins_more_each_range=vector()
tree_cost=0
actual_cost_matching=0
for(i in 1:9)
{
  if(ins_yes_based_inc[i]==0&&ins_no_based_inc[i]==0)
    ins_more_each_range[i]=0
  else if(ins_yes_based_inc[i]>=ins_no_based_inc[i])
  {
    ins_more_each_range[i]="Y"
    tree_cost=tree_cost+1
    actual_cost_matching=actual_cost_matching+ins_yes_based_inc[i]
  }
  else
  {
    ins_more_each_range[i]="N"
    tree_cost=tree_cost+1
    actual_cost_matching=actual_cost_matching+ins_no_based_inc[i]
  }
}

ins_more_each_range
actual_cost_matching
tree_cost
t_acc=actual_cost_matching/nrow(f1)
t_acc
goodness_score=t_acc/tree_cost
goodness_score


#based on credit card limit

cc_ss=0
cc_sn=0
cc_ns=0
cc_nn=0

for(i in 1:nrow(f1))
{
  if(f1[i,5]=="Y"&&f1[i,4]=="Y")
    cc_ss=cc_ss+1
  if(f1[i,5]=="Y"&&f1[i,4]=="N")
    cc_sn=cc_sn+1
  if(f1[i,5]=="N"&&f1[i,4]=="N")
    cc_nn=cc_nn+1
  if(f1[i,5]=="N"&&f1[i,4]=="Y")
    cc_ns=cc_ns+1
}

cc_ss
cc_sn
cc_nn
cc_ns

tre_cs=2

if(cc_ss>=cc_sn)
    cc_lic="Y"
if(cc_ss<cc_sn)
    cc_lic="N"
if(cc_nn>=cc_ns)
   ncc_lic="N"
if(cc_nn<cc_ns)
   ncc_lic="Y"

act=0
tre_acc=0
goodsc=0

if(cc_lic=="Y")
  act=act+cc_ss
if(ncc_lic=="N")
  act=act+cc_nn
if(ncc_lic=="Y")
  act=act+cc_ns
if(cc_lic=="N")
  act=act+cc_sn

act
tre_acc=act/nrow(f1)
tre_acc
goodsc=tre_acc/tre_cs
goodsc

#based on age (greater than or less than 41)
agl_s=0
agl_n=0
agg_s=0
agg_n=0

for(i in 1:nrow(f1))
{
  if(f1[i,7]<=41&&f1[i,4]=="Y")
    agl_s=agl_s+1
  if(f1[i,7]<=41&&f1[i,4]=="N")
    agl_n=agl_n+1
  if(f1[i,7]>41&&f1[i,4]=="N")
    agg_n=agg_n+1
  if(f1[i,7]>41&&f1[i,4]=="Y")
    agg_s=agg_s+1
}

agl_s
agl_n
agg_s
agg_n

act1=0
tre_cs1=2

if(agl_s>=agl_n)
  act1=act1+agl_s
if(agl_s<agl_n)
  act1=act1+agl_n
if(agg_n>=agg_s)
  act1=act1+agg_n
if(agg_n<agg_s)
  act1=act1+agg_s

tre_acc1=0
goodsc1=0

act1
tre_acc1=act1/nrow(f1)
tre_acc1
goodsc1=tre_acc1/tre_cs1
goodsc1

#based on gender
m_s=0
f_n=0
f_s=0
m_n=0

for(i in 1:nrow(f1))
{
  if(f1[i,6]=="M"&&f1[i,4]=="Y")
    m_s=m_s+1
  if(f1[i,6]=="M"&&f1[i,4]=="N")
    m_n=m_n+1
  if(f1[i,6]=="F"&&f1[i,4]=="N")
    f_n=f_n+1
  if(f1[i,6]=="F"&&f1[i,4]=="Y")
    f_s=f_s+1
}

m_s
f_n
f_s
m_n

act11=0
tre_cs11=2

if(m_s>=m_n)
  act11=act11+m_s
if(m_s<m_n)
  act11=act11+m_n
if(f_n>=f_s)
  act11=act11+f_n
if(f_n<f_s)
  act11=act11+f_s

tre_acc11=0
goodsc11=0

act11
tre_acc11=act11/nrow(f1)
tre_acc11
goodsc11=tre_acc11/tre_cs11
goodsc11


# Question 1

n1=16
n2=2
s="add"


new.Function=function(n1,n2,s)
{
  if(s=="add")
  {
    op=n1+n2
    return(op)
  }
  else if (s=="subtract")
  {
    op=n1-n2
    return(op)
  }
  
  else if (s=="divide")
  {
  op=n1/n2
  return(op)
  }
  
  else if(s=="multiply")
  {
    op=n1*n2
    return(op)
  }
  
  else if (s=="log")
  {
    op=log(n1,base=n2)
    return(op)
    
  }
  
  else if (s=="power")
  {
    op=n1^n2
    return(op)
  }
  
  else
  {
    op=-1
    return(op)
  }
  
  
}

out<-new.Function(10,10,"divide")

print(out)


# Question 2

new.oddnum=function(n)
{
for (i in 1:n) {
  if (i % 2 ==1) print(i)
}
}
new.oddnum(10)

# Question 4

new.classdetails=function(s)
{
  return(class(s))
}
  
op=new.classdetails(TRUE)
print(op)



# Question 5

new.primeno=function(num)
{
if(num > 1) {
  # check for factors
  flag = 1
  for(i in 2:(num-1)) {
    if ((num %% i) == 0) {
      flag = 0
      break
    }
  }
} 

if(num == 2)    flag = 1
if(flag == 1) {
  return("prime")
} else {
  return("not Prime")
}

}

op=new.primeno(7)
print(op)


# Question 3
i=1
v=letters
for(i in v )
{
  if(i=="a"||i=="e"||i=="i"||i=="o"||i=="u")
  {
    print (i)
}
}

install.packages("plyr")


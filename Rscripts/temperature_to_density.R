temperature_to_density <- function(temp, salt){
  
  #from calculate_density_UNESCO in GLM code
  
  term <- rep(NA, 15)
  
  c1=999.842594
  c2=6.793952E-2
  c3=9.095290E-3
  c4=1.001685E-4
  c5=1.120083E-6
  c6=6.536332E-9
  d1=8.24493E-1 
  d2=4.0899E-3
  d3=7.6438E-5
  d4=8.2467E-7   
  d5=5.3875E-9   
  d6=5.72466E-3
  d7=1.0227E-4
  d8=1.6546E-6   
  d9=4.8314E-4
  
  t1 = temp
  s1 = salt
  tm = round(t1*10000)
  t1 = tm/10000
  
  t2 = sqrt(t1)
  t3 = t2*t1
  t4 = t3*t1
  t5 = t4*t1
  s2 = s1*s1
  s32 =s1 ^ 1.5
  
  term[1]  =  c1
  term[2]  =  c2 * t1
  term[3]  = -c3 * t2
  term[4]  =  c4 * t3
  term[5]  = -c5 * t4
  term[6]  =  c6 * t5
  term[7]  =  d1
  term[8]  = -d2 * t1
  term[9]  =  d3 * t2
  term[10]  = -d4 * t3
  term[11] =  d5 * t4
  term[12] = -d6
  term[13] =  d7 * t1
  term[14] = -d8 * t2
  term[15] =  d9
  
  dpure  =  term[6]  + term[5]  + term[4]  + term[2] + term[3] + term[1]
  csal1  = (term[11] + term[10]  + term[9]  + term[8] + term[7]) * s1
  csal32 = (term[14] + term[13] + term[12]) * s32
  csal2  =  term[15] * s2
  
  return(dpure + csal1 + csal32 + csal2)
}
#!/usr/bin/env python  
#David Tack
from collections import Counter

def read_in_data(afyle='GenotypicData_test.csv'):
  '''Loads Genotypic Data, creates index off all possible alleles per locus'''
  SSR,unique_codes,derpingtons = {},{},['','0','NA','Na','na','.',' ']
  for line in open(afyle,'r'):
    if line.startswith('SSR'):
      zkeys = [x for x in line.strip().split(',')[1:] if '_' not in x]
      #print zkeys
    else:
      bits = line.strip().split(',')
      name = bits[0]
      allele_s =[bits[1:][i:i+2] for i in range(0,len(bits[1:]),2)]
      for x, b in zip(zkeys, allele_s):
	locus, copies = x, [int(q) for q in b if q not in derpingtons]
	#Okay, if you are actually reading comments, here's where it gets fun. We are going to use a nested dictionary
	#Where the values of sub-dicts will themselves be counters. Maddness though this may seem the information as it is, is well organized
	SSR.setdefault(name, {})[locus] = Counter(copies)
	for modifier in copies:
	  unique_codes['*'.join([locus,str(modifier)])] = ''
  return SSR,sorted(unique_codes.keys())

def file_dumper(SSR_dict,SSR_index,outfyle='out.csv'):
  '''Id don't even know'''
  with open(outfyle,'w') as f:
    #Okay, let's make a header first
    col_names = ['SSR']+SSR_index
    f.write(','.join(col_names)+'\n')
    #Okay, now to walk through this dictionary...
    for k, v in SSR_dict.items():
      #One list comprehension to rule them all!
      #So we have a dictionary, which the first key unlocks the plant line
      #The second key specifies the locus
      #The third key specifies the number of repeats, and Counters are cool because everything is zero unless there is an entry
      #By iterating through the spiffy index of all alleles, we can query presence and number of each, getting mostly zeros
      #Nested lookups really don't take much power. Honestly if something is going break here, its if you have too many alleles
      values = [str(SSR_dict[k][locus][int(count)]) for locus, count in [zed.split('*') for zed in SSR_index]]
      f.write(','.join([k]+values)+'\n')

#Workflow
if __name__ == '__main__':
  print ''
  data, index_list = read_in_data()
  print ''
  file_dumper(data, index_list)
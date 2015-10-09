#!/usr/bin/env python

#This script will accept genotype data for microsatellites in the Jeff-count format and output the same data
# in the (useable) biallelic format of e.g. STRUCTURE.

# version 2! The 'moving parts' for this script were completely re-written be David Tack.

Usage = "\nCall with the pattern: 'biallele.py your_input_filename.csv your_output_filename.csv"

import sys
from collections import Counter

# grab in & out filenames from command line
if len(sys.argv) != 3:
	exit( Usage )
else:
	InFile = sys.argv[1]
	OutFileName = sys.argv[2]


def read_in_data( GeneFile= InFile ):
  '''Loads Genotypic Data, creates index off all possible alleles per locus'''
  SSR,unique_codes,datastream = {},{},['','0','NA','Na','na','.',' ']
  for line in open( GeneFile, 'r' ):
    if line.startswith( 'SSR' ):
      zkeys = [x for x in line.strip().split(',')[1:] if '_' not in x]
      #print zkeys
    else:
      bits = line.strip().split(',')
      name = bits[0]
      allele_s =[bits[1:][i:i+2] for i in range(0,len(bits[1:]),2)]
      for x, b in zip(zkeys, allele_s):
	locus, copies = x, [int(q) for q in b if q not in datastream]
	# Okay, if you are actually reading comments, here's where it gets fun. We are going to use a nested dictionary
	# Where the values of sub-dicts will themselves be counters. Maddness though this may seem the information as it is, is well organized
	SSR.setdefault(name, {})[locus] = Counter(copies)
	for modifier in copies:
	  unique_codes['*'.join([locus,str(modifier)])] = ''
  return SSR,sorted(unique_codes.keys())


def file_dumper( SSR_dict, SSR_index, outfyle=OutFileName ):
  '''Id don't even know'''
  with open( outfyle, 'w' ) as f:
    #Okay, let's make a header first
    col_names = ['SSR']+SSR_index
    f.write(','.join(col_names)+'\n')
    #Okay, now to walk through this dictionary...
    for k, v in SSR_dict.items():
      # One list comprehension to rule them all!
      # So we have a dictionary, which the first key unlocks the plant line
      # The second key specifies the locus
      # The third key specifies the number of repeats, and Counters are cool because everything is zero unless there is an entry
      # By iterating through the spiffy index of all alleles, we can query presence and number of each, getting mostly zeros
      # Nested lookups really don't take much power. Honestly if something is going break here, its if you have too many alleles
      values = [str(SSR_dict[k][locus][int(count)]) for locus, count in [zed.split('*') for zed in SSR_index]]
      f.write(','.join([k]+values)+'\n')


# biallele_out
if __name__ == '__main__':
  print ''
  data, index_list = read_in_data()
  print ''
  file_dumper(data, index_list)
  sys.stderr.write( "Mischief Managed!\n" )

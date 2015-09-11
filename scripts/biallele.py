#!/usr/bin/env python

#This script will accept genotype data for microsatellites in the Jeff-count format and output the same data
# in the (useable) biallelic format of e.g. STRUCTURE.

Usage = "\nCall with the pattern: 'biallele.py your_input_filename.csv your_output_filename.csv"

try:
    import sys, pandas as pa, numpy as np, collections
except ImportError:
    sys.exit( "You appear to be missing a required python module. Please run `pip install pandas numpy` at the shell" )


# grab in & out filenames from command line
if len(sys.argv) != 3:
	exit( Usage )
else:
	InFile = sys.argv[1]
	OutFileName = sys.argv[2]

data=pa.read_csv( InFile )

# we need the data columns, and for now the row names can be put to one side...
rownames=list( data[ data.columns[0] ] )
loci_dup=list( data.columns )[1:]

# ...and the number of columns is 2n
dubnloci=len( loci_dup )

# loci is columns 1&2, 3&4, etc..
loci=[ loci_dup[i:(i+2)] for i in range( 0, dubnloci, 2 ) ]
nloci=len( loci )-1

biallele_out = pa.DataFrame()

for i in range( 0, nloci ):
    thesedata=data[ loci[i] ]
    posalleles=Counter( thesedata[ thesedata.columns[0]] ) + Counter( thesedata[ thesedata.columns[1]] )
    colnums=list(posalleles.keys())
    # outframe=pa.DataFrame( data=np.zeros( ( len( rownames ), len( levels ) ) ) )
    outframe=pa.DataFrame( columns=colnums, index=rownames )
    # set empty columns as appropriate
    for index, row in thesedata.iterrows():
        alleles=Counter( row[ thesedata.columns[0:2] ] )
        outframe.iloc[ index ] = alleles
#         print outframe.iloc[ index ]
    outframe.rename(columns=lambda x: loci[i][0] + '_' + str(x), inplace=True)
    levels=[ thesedata.columns[0] + '_' + str(k) for k in list(posalleles.keys()) ]
#     print levels
    outframe = outframe.fillna( 0 )
#     print outframe
    biallele_out = pa.concat( [biallele_out, outframe], axis=1 )
    sys.stderr.write( "%s loci processed\n" %i )

# biallele_out

Location=sys.path[0]
biallele_out.to_csv( Location + OutFileName, float_format=int() )

sys.stderr.write( "Mischief Managed!\n" )

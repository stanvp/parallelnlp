# Script for benchmarking classifiers.
# command line args: 
#	[result file name] 
#   [algorithm] 
#   [traning mode]
#   [wikipedia train corpus file] 
#   [wikipedia test corpus file] 
#   [wikipedia features file]
#   [evaluation]
# algorithm can be: maxent or naivebayes
# traning mode can be: parallel or sequential


import os
from time import time
import sys

out = 'bench/%s.bench' % sys.argv[1]
algorithm = sys.argv[2]
trainingmode = sys.argv[3]
corpusPath = sys.argv[4]
testCorpusPath = sys.argv[5]
featuresCorpusPath = sys.argv[6]
evaluation = sys.argv[7]

javaPath = "java"

print "Running %s - %s" % (algorithm, trainingmode)
print 'corpusPath = %s' % corpusPath

javaargs="-Xmx4G -Xmx8G"

for numCores in [2,3,5,7,9]:
    print "Running with %d cores..." % (numCores - 1)
    opts = javaargs + " -Dactors.corePoolSize=%d -Dactors.maxPoolSize=%d" % (numCores, numCores)
    
    os.putenv("JAVA_OPTS", opts)
    
    iterations = 5
    
    cmd = "time "+ javaPath + " " + opts + " -cp parallelnlp-assembly-1.0-SNAPSHOT.jar menthor.apps.WikipediaClassifier %s %s %s %s %s %s %s %s" % (algorithm, trainingmode, corpusPath, testCorpusPath, featuresCorpusPath, evaluation, out, iterations)
    os.system(cmd)    
    
	print 'running command: %s' % cmd


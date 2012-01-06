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

out = file('bench/%s.bench' % sys.argv[1], 'w')
algorithm = sys.argv[2]
trainingmode = sys.argv[3]
corpusPath = sys.argv[4]
testCorpusPath = sys.argv[5]
featuresCorpusPath = sys.argv[6]
evaluation = sys.argv[7]

javaPath = "java"

print "Running %s - %s" % (algorithm, trainingmode)
print 'corpusPath = %s' % corpusPath

#javaargs="-Xms1G -Xmx8G"
javaargs="-Xmx4G -Xmx8G"
out.write("JAVA OPTS: "+javaargs+"\n")

#for numCores in [2] + range(3, 14, 2): # range(x,y,s) "from x to y-1 in steps of s"
for numCores in [2,3,4,5,6,7,8]:
    print "Running with %d cores..." % (numCores - 1)
    opts = javaargs + " -Dactors.corePoolSize=%d -Dactors.maxPoolSize=%d" % (numCores, numCores)
    os.putenv("JAVA_OPTS", opts)
    out.write("#cores "+str(numCores - 1)+"\t")

    for run in range(5):
        start = time()
        cmd = "time "+ javaPath + " " + opts + " -cp parallelnlp-assembly-1.0-SNAPSHOT.jar menthor.apps.WikipediaClassifier %s %s %s %s %s %s" % (algorithm, trainingmode, corpusPath, testCorpusPath, featuresCorpusPath, evaluation)
        print 'running command: %s' % cmd

        os.system(cmd)
        elapsed = time() - start
        print elapsed
        out.write('\t%f' % elapsed)

    out.write('\n')


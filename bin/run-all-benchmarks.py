# Script for benchmarking all classifiers
# command line args: [wikipedia corpus path]

import os
from time import time
import sys

corpusPath = sys.argv[1]
os.system("python run-benchmark.py maxent_sequential.out maxent sequential %s" % corpusPath)
os.system("python run-benchmark.py maxent_parallel.out maxent parallel %s" % corpusPath)
os.system("python run-benchmark.py naivebayes_sequential.out naivebayes sequential %s" % corpusPath)
os.system("python run-benchmark.py naivebayes_parallel.out naivebayes parallel %s" % corpusPath)

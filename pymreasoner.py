import logging

import mreasoner
import clozure

# Setup logger
logging.basicConfig(level=logging.DEBUG)

# Initialize LISP
cl = clozure.ClozureCL()

# Initialize MReasoner instance
mr = mreasoner.MReasoner(
    ccl_path=cl.exec_path(),
    mreasoner_dir='mReasoner'
)

print('Query result: {}'.format(mr.query('IA4')))

mr.terminate()

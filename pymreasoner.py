""" pymreasoner entry point. Serves as a test environment for development.

"""

import logging

import mreasoner
import clozure

def main():
    """ Entry point function.

    """

    # Setup logger
    logging.basicConfig(level=logging.DEBUG)

    # Initialize LISP
    cloz = clozure.ClozureCL()

    # Initialize MReasoner instance
    mreas = mreasoner.MReasoner(
        ccl_path=cloz.exec_path(),
        mreasoner_dir='mReasoner'
    )

    print('Query result: {}'.format(mreas.query('IA4')))

    mreas.terminate()

if __name__ == '__main__':
    main()

""" pymreasoner entry point. Serves as a test environment for development.

"""

import logging
import argparse

import numpy as np

import mreasoner
import clozure

def parse_arguments():
    parser = argparse.ArgumentParser(description='mReasoner CLI')
    parser.add_argument(
        '-i', '--interactive',
        help='Launch interactive shell',
        action='store_true')
    parser.add_argument(
        '-ll', '--loglevel',
        help='Set logger level (debug, info)',
        metavar='X'
    )

    args = vars(parser.parse_args())

    # Postprocess argument values
    if args['loglevel'] == 'info':
        args['loglevel'] = logging.INFO
    elif args['loglevel'] == 'debug':
        args['loglevel'] = logging.DEBUG
    else:
        args['loglevel'] = logging.CRITICAL

    return args

def main():
    """ Entry point function.

    """

    # Parse arguments
    args = parse_arguments()

    # Setup logger
    logging.basicConfig(level=args['loglevel'])

    # Initialize LISP
    cloz = clozure.ClozureCL()

    # Initialize MReasoner instance
    mreas = mreasoner.MReasoner(
        ccl_path=cloz.exec_path(),
        mreasoner_dir='mReasoner'
    )

    if args['interactive']:
        while True:
            inp = input('> ')
            if inp in ['quit', 'q', 'exit']:
                break

            if inp.startswith('query'):
                task = inp.split()
                if len(task) != 2:
                    print('ERROR: Invalid query')
                else:
                    resp = mreas.query(task[1])
                    print(resp)
            elif inp.startswith('param'):
                setcmd = inp.split()
                if len(setcmd) == 1:
                    print(mreas.params)
                elif len(setcmd) != 3:
                    print('ERROR: Invalid set command')
                else:
                    param = setcmd[1]
                    value = float(setcmd[2])
                    mreas.set_param(param, value)
    else:
        # Load the test fitting data
        tasks = np.load('data/tasks.npy')
        resps = np.load('data/resps.npy')

        # Test fitting mReasoner
        train_x = tasks[0]
        train_y = resps[0]

        res = mreas.fit(train_x, train_y)
        print(res)

    mreas.terminate()

if __name__ == '__main__':
    main()

""" Command line interface for mReasoner. Launches an interactive shell for querying mReasoner.

"""

import logging
import argparse

import mreasoner

def parse_arguments():
    """ Parse the command line arguments.

    Returns
    -------
    dict(str, object)
        Command line arguments.

    """

    parser = argparse.ArgumentParser(description='mReasoner CLI')
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
        args['loglevel'] = logging.WARNING

    return args

def main():
    """ Entry point function.

    """

    # Parse arguments
    args = parse_arguments()

    # Setup logger
    logging.basicConfig(level=args['loglevel'])

    # Initialize LISP
    cloz = mreasoner.ClozureCL()

    # Initialize MReasoner instance
    mreas = mreasoner.MReasoner(
        ccl_path=cloz.exec_path(),
        mreasoner_dir='mReasoner'
    )

    print('Interactive mReasoner shell. Commands:')
    print('quit, q, exit        - terminate the shell')
    print('param <name>         - print parameter value')
    print('param <name> <value> - set parameter value')
    print('query <task>         - query syllogistic task (e.g., AA1)')
    print('---------------------------------------------------------')

    try:
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
    except KeyboardInterrupt:
        # Gracefully handle keyboard interrupt
        pass

    # Finalize by terminating the mreasoner connection
    mreas.terminate()

if __name__ == '__main__':
    main()

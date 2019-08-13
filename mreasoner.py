""" mReasoner interface.

"""

import os
import logging
import subprocess
import threading
import queue

import scipy.optimize as so

class MReasoner():
    """ LISP mReasoner wrapper. Executes a Clozure Common LISP subprocess to run an unmodified
    version of mReasoner. Provides basic interfacing mechanisms for inference generation and
    parameter fitting.

    """

    def __init__(self, ccl_path, mreasoner_dir):
        """ Constructs the mReasoner instance by launching the LISP subprocess.

        Parameters
        ----------
        ccl_path : str
            Path to the Clozure Common LISP executable.

        mreasoner_dir : str
            Path to the mReasoner source code directory.

        """

        # Initialize logger instance
        self.logger = logging.getLogger(__name__)

        self.proc = subprocess.Popen(
            [ccl_path],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT
        )

        # Instantiate the result queue
        self.resp_queue = queue.Queue()

        # Register the readers
        def stdout_reader(proc):
            # Setup thread logger
            logger = logging.getLogger(__name__ + '-reader')
            logger.debug('Starting reader...')

            while True:
                # Read text from mReasoner output
                text = proc.stdout.readline().decode('ascii').strip()
                logger.debug('mReasoner:%s', text)

                # Ignore comments and query results
                if text.startswith(';') or text.startswith('?'):
                    continue

                # Catch the termination signal
                if 'TERMINATE' in text:
                    logger.debug('termination handling initiated...')
                    break
                if ('While executing:' in text) or ('Error:' in text):
                    self.resp_queue.put('HALT')


                # Catch mReasoner computation output
                if text == '"RESULT"':
                    # Actual result is in line 2 after
                    logger.debug('RESULT observed!')
                    text = proc.stdout.readline().decode('ascii').strip().replace('"', '')
                    logger.debug('Ignoring:%s', text)
                    text = proc.stdout.readline().decode('ascii').strip().replace('"', '')
                    logger.debug('Queue-Result:%s', text)
                    self.resp_queue.put(text)

            logger.debug('terminating...')

        self.readerstdout = threading.Thread(target=stdout_reader, args=(self.proc,), daemon=True)
        self.readerstdout.start()

        # Load mReasoner and setup result variable
        mreasoner_file = mreasoner_dir + os.sep + "+mReasoner.lisp"
        self._send('(load "{}")'.format(mreasoner_file))
        self._send('(defvar resp 0)')

        # Initialize parameter values
        self.params = {
            'epsilon': 0.0,
            'lambda': 4.0,
            'omega': 1.0,
            'sigma': 0.0
        }

        self.set_param('epsilon', self.params['epsilon'])
        self.set_param('lambda', self.params['lambda'])
        self.set_param('omega', self.params['omega'])
        self.set_param('sigma', self.params['sigma'])

    def _send(self, cmd):
        """ Send a command to the Clozure Common LISP subprocess.

        Parameters
        ----------
        cmd : str
            Command to send.

        """

        # Normalize the command
        cmd.strip()

        self.logger.debug('Send:%s', cmd)
        self.proc.stdin.write('{}\n'.format(cmd).encode('ascii'))
        self.proc.stdin.flush()

    @staticmethod
    def construct_premises(syllog):
        """ Constructs mReasoner representation of the premises for a given syllogism identifier.

        Parameters
        ----------
        syllog : str
            Syllogistic problem identifier (e.g., 'AA1', 'OE3').

        Returns
        -------
        (p1, p2) : str
            Tuple of the mReasoner representations of both premises.

        """

        template_quant = {
            'A': 'All {} are {}',
            'I': 'Some {} are {}',
            'E': 'No {} are {}',
            'O': 'Some {} are not {}'
        }

        template_fig = {
            '1': [['A', 'B'], ['B', 'C']],
            '2': [['B', 'A'], ['C', 'B']],
            '3': [['A', 'B'], ['C', 'B']],
            '4': [['B', 'A'], ['B', 'C']]
        }

        prem1 = template_quant[syllog[0]].format(*template_fig[syllog[-1]][0])
        prem2 = template_quant[syllog[1]].format(*template_fig[syllog[-1]][1])
        return prem1, prem2

    def query(self, syllog):
        """ Queries mReasoner for a prediction for a given syllogistic problem.

        Parameters
        ----------
        syllog : str
            Syllogistic problem identifier (e.g., 'AA1', 'EO3') to generate prediction for.

        Returns
        -------
        str
            Generated syllogistic response identifier (e.g., 'Aac', 'Ica')

        """

        prem1, prem2 = self.construct_premises(syllog)

        # Send the conclusion generation query
        cmd = "(what-follows? (list (parse '({})) (parse '({}))))".format(prem1, prem2)
        cmd = '(setf resp {})'.format(cmd)
        self.logger.debug('Query:%s', cmd)
        self._send(cmd)

        # Send the result interpretation query
        cmd = '(abbreviate (first resp))'
        cmd = '(prin1 "RESULT")(prin1 {})'.format(cmd)
        self.logger.debug('Query:%s', cmd)
        self._send(cmd)

        # Retrieve queue output
        queue_out = self.resp_queue.get()
        if queue_out == 'HALT':
            exit()

        return queue_out

    def terminate(self):
        """ Terminate mReasoner and its parent instance of Clozure Common LISP.

        """

        # Shutdown the threads
        self._send('(prin1 "TERMINATE")')
        self.logger.info('Waiting for stdout...')
        self.readerstdout.join()

        # Terminate Clozure
        self._send('(quit)')

    def set_param(self, param, value):
        """ Set mReasoner parameter to a specified value.

        Parameter
        ---------
        param : str
            Parameter identifier. Can be one of ['epsilon', 'lambda', 'omega', 'sigma'].

        value : float
            Parameter value.

        Raises
        ------
        ValueError
            If invalid param is specified.

        """

        if param not in self.params:
            raise ValueError('Attempted to set invalid parameter: {}'.format(param))
        self.params[param] = value

        # Send parameter change to mReasoner
        cmd = '(setf +{}+ {:f})'.format(param, value)
        self.logger.info('Param-Set: %s->%f:%s', param, value, cmd)
        self._send(cmd)

    def _fit_fun(self, x, *args):
        """ Fitting helper function. Receives parameter values and computes accuracy on given
        training and test data.

        Parameters
        ----------
        x : list(float)
            List of parameters.

        Results
        -------
        float
            Predictive accuracy.

        """

        train_x, train_y = args

        # Set the parameters
        param_names = ['epsilon', 'lambda', 'omega', 'sigma']
        for idx in range(len(x)):
            self.set_param(param_names[idx], x[idx])

        hits = 0
        for idx in range(len(train_x)):
            task = train_x[idx]
            resp = train_y[idx]

            pred = self.query(task)

            hits += (resp == pred)

        acc = hits / len(train_x)
        self.logger.info('Fitting (p=%s): %f', x, acc)
        return acc

    def fit(self, train_x, train_y, num_fits=10):
        """ Fits mReasoner parameters to the specified data.

        Parameters
        ----------
        train_x : list(str)
            List of syllogistic task encodings (e.g., 'AA1').

        train_y : list(str)
            List of syllogistic response encodings (e.g., 'Aac').

        Returns
        -------
        float
            Fit result accuracy.

        """

        self.logger.info('Fitting!')

        results = []
        for _ in range(num_fits):
            start_params = [x[1] for x in sorted(self.params.items())]
            param_bounds = [[0.0, 1.0], [0.1, 8.0], [0.0, 1.0], [0.0, 1.0]]

            res = so.minimize(
                self._fit_fun,
                start_params,
                method='L-BFGS-B',
                bounds=param_bounds,
                args=(train_x, train_y))

            if res.success:
                results.append((res.fun, res.x))

        print(results)

        return 0

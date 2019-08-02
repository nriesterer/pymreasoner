import logging
import subprocess
import threading
import queue

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
            stdout = subprocess.PIPE,
            stderr = subprocess.STDOUT
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
                logger.debug('mReasoner:{}'.format(text))

                # Ignore comments and query results
                if text.startswith(';') or text.startswith('?'):
                    continue

                # Catch the termination signal
                if 'TERMINATE' in text:
                    logger.debug('termination handling initiated...')
                    break

                # Catch mReasoner computation output
                if text == '"RESULT"':
                    # Actual result is in line 2 after
                    logger.debug('RESULT observed!')
                    proc.stdout.readline()
                    text = proc.stdout.readline().decode('ascii').strip().replace('"', '')
                    logger.info('Queue-Result:{}'.format(text))
                    self.resp_queue.put(text)

            logger.debug('terminating...')

        self.readerstdout = threading.Thread(target=stdout_reader, args=(self.proc,), daemon=True)
        self.readerstdout.start()

        # Load mReasoner and setup result variable
        self._send('(load "mReasoner/+mReasoner.lisp")')
        self._send('(defvar resp 0)')

        # Initialize parameter values
        self.p_epsilon = 0.0
        self.p_lambda = 4.0
        self.p_omega = 1.0
        self.p_sigma = 0.0

        self.set_param('epsilon', self.p_epsilon)
        self.set_param('lambda', self.p_lambda)
        self.set_param('omega', self.p_omega)
        self.set_param('sigma', self.p_sigma)

    def _send(self, cmd):
        """ Send a command to the Clozure Common LISP subprocess.

        Parameters
        ----------
        cmd : str
            Command to send.

        """

        # Normalize the command
        cmd.strip()

        self.logger.debug('Send:{}'.format(cmd))
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

        p1 = template_quant[syllog[0]].format(*template_fig[syllog[-1]][0])
        p2 = template_quant[syllog[1]].format(*template_fig[syllog[-1]][1])
        return p1, p2

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
        self.logger.info('Query:{}'.format(cmd))
        self._send(cmd)

        # Send the result interpretation query
        cmd = '(abbreviate (first resp))'
        cmd = '(prin1 "RESULT")(prin1 {})'.format(cmd)
        self.logger.info('Query:{}'.format(cmd))
        self._send(cmd)

        return self.resp_queue.get()

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

        """

        if param not in ['epsilon', 'lambda', 'omega', 'sigma']:
            raise ValueError('Attempted to set invalid parameter: {}'.format(param))

        if param == 'epsilon':
            self.p_epsilon = value
        elif param == 'lambda':
            self.p_lambda = value
        elif param == 'omega':
            self.p_omega = value
        elif param == 'sigma':
            self.p_omega = value

        # Send parameter change to mReasoner
        cmd = '(setf +{}+ {:f})'.format(param, value)
        self.logger.info('Param-Set: {}->{}:{}'.format(param, value, cmd))
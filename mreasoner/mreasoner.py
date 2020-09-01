""" Interface class for the LISP-based mReasoner implementation.

"""

import copy
import logging
import os
import platform
import queue
import subprocess
import threading
import time
import select

import urllib.request
import zipfile

import numpy as np
import scipy.optimize as so

from . import lispcompiler


FASL_ENDINGS = {
    'Darwin': 'dx64fsl',
    'Windows': 'wx64fsl',
    'Linux': 'lx64fsl'
}

def source_path(mreas_path='.mreasoner'):
    """ Determines the source path of mReasoner if existent. Downloads a copy if necessary.

    Parameters
    ----------
    mreas_path : str
        Target path for the mReasoner source copy.

    Returns
    -------
    str
        Path to the directory containing the mReasoner sources.

    """

    if not os.path.exists(mreas_path):
        # Create the mreasoner directory
        os.mkdir(mreas_path)

        # Download the mreasoner source
        link = 'https://github.com/nriesterer/pymreasoner/raw/master/mReasoner-r6684.zip'
        dl_target = mreas_path + os.sep + 'mReasoner.zip'
        urllib.request.urlretrieve(link, dl_target)

        # Unzip content
        with zipfile.ZipFile(dl_target, 'r') as zip_ref:
            zip_ref.extractall(mreas_path)

    # Look for mReasoner directory
    for name in os.listdir(mreas_path):
        path = mreas_path + os.sep + name
        if not os.path.isdir(path) or name.startswith('_'):
            continue
        mreas_path = path + os.sep + 'src'

    return mreas_path

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
        self.param_bounds = [[0.0, 1.0], [0.1, 8.0], [0.0, 1.0], [0.0, 1.0]]

        # Load mReasoner in CCL environment
        mreasoner_file = mreasoner_dir + os.sep + "+mReasoner.lisp"

        # Start mReasoner process
        self.proc = subprocess.Popen(
            [ccl_path, '--load', mreasoner_file],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT
        )

        out = self.wait_for_output('Licence, Version 2.0.', timeout=10)
        assert out == 'Licence, Version 2.0.'

        # Initialize parameter values
        self.DEFAULT_PARAMS = {
            'epsilon': 0.0,
            'lambda': 4.0,
            'omega': 1.0,
            'sigma': 0.0
        }

    def wait_for_output(self, text, timeout=10):
        def worker(proc, result):
            while True:
                line = proc.stdout.readline().decode('ascii').strip()
                if text in line:
                    result.append(line)
                    return

        result = []
        th = threading.Thread(target=worker, args=(self.proc,result), daemon=True)
        th.start()
        th.join(timeout=timeout)

        return result[0]

    def query(self, premises, param_dict=None):
        if param_dict == None:
            param_dict = self.DEFAULT_PARAMS

        # Prepare the command
        cmd = [
            "(progn",
            "    (setf *stochastic* t)",
            "    (setf +epsilon+ {})".format(param_dict['epsilon']),
            "    (setf +lambda+ {})".format(param_dict['lambda']),
            "    (setf +omega+ {})".format(param_dict['omega']),
            "    (setf +sigma+ {})".format(param_dict['sigma']),
            '    (format nil "QUERY_RESULT: ~{~A~^, ~}"',
            "        (map 'list",
            "            (lambda (x) (abbreviate x))",
            "            (what-follows?",
            "                (list",
            "                    (parse '({}))".format(premises[0]),
            "                    (parse '({})))))))".format(premises[1]),
        ]
        cmd = '\n'.join(cmd)

        # Send the query command and wait for the output line
        self._send(cmd)
        out = self.wait_for_output('QUERY_RESULT', timeout=10)
        self.logger.debug('Output line received: %s', out)

        # Clean up and return output
        predictions = out[17:-1].split(', ')
        return predictions

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

    def terminate(self):
        """ Terminate mReasoner and its parent instance of Clozure Common LISP.

        """

        # Terminate Clozure
        self._send('(quit)')
        self.proc.kill()

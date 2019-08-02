import urllib.request
import os
import zipfile
import subprocess
import threading
import queue
import logging

# Setup logger
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Download common lisp if not existent already
# dl_urls = {
#     'mac': 'https://github.com/Clozure/ccl/releases/download/v1.11.5/ccl-1.11.5-darwinx86.tar.gz',
#     'win': 'https://github.com/Clozure/ccl/releases/download/v1.11.5/ccl-1.11.5-windowsx86.zip'
# }
# destination = 'ccl'

# if not os.path.isdir(destination):
#     os.mkdir(destination)

# extract_destination = destination + os.sep + 'ccl'
# if not os.path.isdir(extract_destination):
#     # Download ccl archive
#     dl_path = destination + os.sep + url.split('/')[-1]
#     urllib.request.urlretrieve(url, dl_path)

#     # Extract the archive
#     with zipfile.ZipFile(dl_path, 'r') as zip_ref:
#         zip_ref.extractall(destination)

class MReasoner():
    def __init__(self, ccl_path, mreasoner_dir):
        self.proc = subprocess.Popen(
            [cmd],
            stdin=subprocess.PIPE,
            stdout = subprocess.PIPE,
            stderr = subprocess.STDOUT
        )

        # Instantiate the result queue
        self.resp_queue = queue.Queue()

        # Register the readers
        def stdout_reader(proc):
            # Setup thread logger
            logger = logging.getLogger('reader')
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

    def _send(self, cmd):
        self.proc.stdin.write('{}\n'.format(cmd).encode('ascii'))
        self.proc.stdin.flush()

    def construct_premises(self, syllog):
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
        prem1, prem2 = self.construct_premises(syllog)

        # Send the conclusion generation query
        cmd = "(what-follows? (list (parse '({})) (parse '({}))))".format(prem1, prem2)
        cmd = '(setf resp {})'.format(cmd)
        logger.info('Query:{}'.format(cmd))
        self._send(cmd)

        # Send the result interpretation query
        cmd = '(abbreviate (first resp))'
        cmd = '(prin1 "RESULT")(prin1 {})'.format(cmd)
        logger.info('Query:{}'.format(cmd))
        self._send(cmd)

        return self.resp_queue.get()

    def terminate(self):
        # Shutdown the threads
        self._send('(prin1 "TERMINATE")')
        logger.info('Waiting for stdout...')
        self.readerstdout.join()

        # Terminate Clozure
        self._send('(quit)')

# Run CCL as subprocess
# cmd = 'ccl/ccl/ccl/wx86cl64.exe'
cmd = 'ccl/darwinx86/dx86cl64'
mr = MReasoner(
    ccl_path=cmd,
    mreasoner_dir='mReasoner'
)

for i in range(10):
    logger.info('Query result: {}'.format(mr.query('IA4')))
    print('a')

mr.terminate()

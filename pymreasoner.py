import urllib.request
import os
import zipfile
import subprocess
import threading
import queue

# Download common lisp if not existent already
url = 'https://github.com/Clozure/ccl/releases/download/v1.11.5/ccl-1.11.5-windowsx86.zip'
destination = 'ccl'

if not os.path.isdir(destination):
    os.mkdir(destination)

extract_destination = destination + os.sep + 'ccl'
if not os.path.isdir(extract_destination):
    # Download ccl archive
    dl_path = destination + os.sep + url.split('/')[-1]
    urllib.request.urlretrieve(url, dl_path)

    # Extract the archive
    with zipfile.ZipFile(dl_path, 'r') as zip_ref:
        zip_ref.extractall(destination)

class MReasoner():
    def __init__(self, ccl_path, mreasoner_dir):
        self.proc = subprocess.Popen(
            [cmd],
            stdin=subprocess.PIPE,
            stdout = subprocess.PIPE,
            stderr = subprocess.PIPE
        )

        # Instantiate the result queue
        self.resp_queue = queue.Queue()

        # Register the readers
        def reader(buffer, name):
            print('Starting reader...')
            while True:
                text = buffer.readline().decode('ascii').strip()
                print('{}:'.format(name), text)

                if 'TERMINATE' in text:
                    break

                if name == 'STDOUT':
                    self.resp_queue.put(text)

            print('{}:'.format(name), 'terminating...')

        self.readerstdout = threading.Thread(target=reader, args=(self.proc.stdout, 'STDOUT'), daemon=True)
        self.readerstdout.start()
        self.readerstderr = threading.Thread(target=reader, args=(self.proc.stderr, 'STDERR'), daemon=True)
        self.readerstderr.start()

        # Load mReasoner

    def _send(self, cmd):
        self.proc.stdin.write('{}\n'.format(cmd).encode('ascii'))
        self.proc.stdin.flush()

    def query(self):
        return self.resp_queue.get()

    def terminate(self):
        # Shutdown the threads
        #self._send('(prin1 "TERMINATE")')
        self._send('(write-string "TERMINATE" *error-output*)')

        #print('Waiting for stdout...')
        #self.readerstdout.join()
        print('Waiting for stderr...')
        self.readerstderr.join()

        self._send('(quit)')

# Run CCL as subprocess
cmd = 'ccl/ccl/ccl/wx86cl64.exe'
mr = MReasoner(
    ccl_path=cmd,
    mreasoner_dir='mReasoner'
)

mr._send('(+ 1 1)')
mr.query()

mr.terminate()

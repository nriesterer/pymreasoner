import urllib.request
import os
import zipfile
import platform
import tarfile

import logging


DL_URLS = {
    'Darwin': 'https://github.com/Clozure/ccl/releases/download/v1.12-dev.5/darwinx86.tar.gz',
    'Windows': 'https://github.com/Clozure/ccl/releases/download/v1.12-dev.5/windows86.zip',
    'Linux': 'https://github.com/Clozure/ccl/releases/download/v1.12-dev.5/linuxx86.tar.gz'
}

class ClozureCL():
    def __init__(self, ccl_dir='.ccl'):
        # Initialize the logger
        logger = logging.getLogger(__name__)

        # Initialize member variables
        self.ccl_dir = ccl_dir
        self.system = platform.system()

        # Download if Clozure CL is not available
        if not os.path.isdir(self.ccl_dir):
            logger.info('Initializing local Clozure copy...')

            # Create the directory
            logger.debug('Make ccl_dir="{}"'.format(self.ccl_dir))
            os.mkdir(self.ccl_dir)

            # Download the binary archive
            dl_url = DL_URLS[self.system]
            dl_target = ccl_dir + os.sep + os.path.basename(dl_url)
            logger.debug('Downloading CCL ({})->"{}"...'.format(dl_url, dl_target))
            urllib.request.urlretrieve(dl_url, dl_target)

            # Unarchive the binaries
            if self.system == 'Darwin' or self.system == 'Linux':
                logger.debug('Untaring "{}"->"{}"...'.format(dl_target, self.ccl_dir))
                self.untar(dl_target, self.ccl_dir)
            elif self.system == 'Windows':
                logger.debug('Unzipping "{}"->"{}"...'.format(dl_target, self.ccl_dir))
                self.unzip(dl_target, self.ccl_dir)
            else:
                raise ValueError(
                    'Unsupported platform ({}). Please contact the package maintainer.'.format(
                        self.system))

        # Extract the 64 bit executable
        self.ccl_path = None
        for fname in os.listdir(self.ccl_dir):
            fpath = self.ccl_dir + os.sep + fname
            if os.path.isdir(fpath) or fname.endswith('.image'):
                continue
            if ('cl64' in fname) and os.access(fpath, os.X_OK):
                self.ccl_path = fpath
        logger.debug('Executable detected: "{}"'.format(self.ccl_path))

    def exec_path(self):
        return self.ccl_path

    def unzip(self, source, target):
        if not source.endswith('zip'):
            raise ValueError('Not a zip file: "{}"'.format(source))

        with zipfile.ZipFile(source, 'r') as zip_ref:
            zip_ref.extractall(target)

    @staticmethod
    def untar(source, target):
        if not source.endswith('tar.gz'):
            raise ValueError('Not a tar.gz file: "{}"'.format(source))


        with tarfile.open(source) as tar:
            tar.extractall(path=target)

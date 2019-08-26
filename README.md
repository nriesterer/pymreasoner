pymreasoner - mReasoner for Python
==================================

Python Interface for the cognitive inference model mReasoner by Sangeet Khemlani and Phil Johnson-Laird.

> Khemlani, S., & Johnson-Laird, P. N. (2013). The processes of inference. *Argument & Computation*, 4, 1-20.

### Description

This project aims at providing a Python-based interface to the LISP-based cognitive inference model mReasoner. It follows the approach to create a live-session using a Clozure Common LISP interpreter to directly communicate with the original and unmodified implementation of mReasoner.

File overview:

- `ccobratest/`: CCOBRA model implementation
- `mreasoner/`: Python package sources
- `cli.py`: mReasoner command line interface
- `mReasoner-r6684.zip`: mReasoner source code
- `setup.py`: Python package installer

### Requirements

- Python >=3.7

### Installation

Download the repository and launch a terminal/cmd:

```
$> cd /path/to/repository
$> python setup.py install
```

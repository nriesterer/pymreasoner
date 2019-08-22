pymreasoner - mReasoner for Python
==================================

Python Interface for the cognitive inference model mReasoner by Sangeet Khemlani and Phil Johnson-Laird.

> Khemlani, S., & Johnson-Laird, P. N. (2013). The processes of inference. *Argument & Computation*, 4, 1-20.

### Description

This project aims at providing a Python-based interface to the LISP-based cognitive inference model mReasoner. It follows the approach to create a live-session using a Clozure Common LISP interpreter to directly communicate with the original and unmodified implementation of mReasoner.

File overview:

- `mReasoner/`: mReasoner LISP source code (version r6684)
- `clozure.py`: Helper class for maintaining Clozure Common LISP binaries
- `mreasoner.py`: mReasoner interface class. Creates a Clozure CL session loading mReasoner's LISP code and provides methods for interaction
- `pymreasoner.py`: Main entry point for developmental purposes

### Requirements

- Python >=3.7

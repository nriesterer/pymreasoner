pymreasoner - mReasoner for Python
==================================

Python interface for the mental models based syllogistic reasoning model mReasoner by Sangeet Khemlani and Phil Johnson-Laird.

> Khemlani, S., & Johnson-Laird, P. N. (2013). The processes of inference. *Argument & Computation*, 4, 1-20.

Find the development version of mReasoner here: [https://github.com/skhemlani/mReasoner](https://github.com/skhemlani/mReasoner)

### Description

This project aims at providing a Python-based interface to the LISP-based cognitive inference model mReasoner. It follows the approach to create a live-session using a Clozure Common LISP interpreter to directly communicate with the original and unmodified implementation of mReasoner.

File overview:

- `benchmark/`: Example CCOBRA benchmarks.
- `caches/`: Precomputed cache files.
- `mreasoner/`: Python package sources.
- `ccobra_mreasoner_cache.py`: Cached CCOBRA model.
- `ccobra_mreasoner.py`: Uncached CCOBRA model.
- `create_cache.py`: Helper module to create mReasoner prediction caches.
- `mReasoner-2587fda.zip`: mReasoner source code taken from [https://github.com/skhemlani/mReasoner](https://github.com/skhemlani/mReasoner) (commit `2587fda`)

### Requirements

- Python >=3.7
- scipy
- numpy

### Quickstart

To run the included benchmark analyses, download the repository and launch a terminal/cmd:

```
$> cd /path/to/repository/benchmark
$> ccobra coverage.json
$> ccobra adaption.json
```

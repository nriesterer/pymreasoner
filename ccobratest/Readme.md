ccobratest
==========

Demo implementation for a mReasoner-based model for the CCOBRA model evaluation framework. Doubles as a test bed for the Python-based mReasoner interface.

### Requirements

- pymreasoner
- [CCOBRA](https://github.com/CognitiveComputationLab/ccobra)

### Contents

- `data`: Folder containing example evaluation data (`Ragni2016` from the CCOBRA framework)
- `bench.json`: Benchmark definition
- `ccobra_mreasoner.py`: mReasoner model for CCOBRA

### How To Use

1. Install the [CCOBRA](https://github.com/CognitiveComputationLab/ccobra) model evaluation framework
2. Run CCOBRA on `bench.json`:
   ```
   $> /path/to/ccobratest
   $> ccobra bench.json
   ```

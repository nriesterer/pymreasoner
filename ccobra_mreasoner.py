""" CCOBRA model wrapper for mReasoner.

"""

import sys
import collections
import threading
import copy

import ccobra
import mreasoner
import numpy as np

import time

import logging

logging.basicConfig(level=logging.INFO)

class CCobraMReasoner(ccobra.CCobraModel):
    """ mReasoner CCOBRA model implementation.

    """

    def __init__(self, name='mReasoner', n_samples=2, fit_its=5):
        """ Initializes the CCOBRA model by launching the interactive LISP subprocess.

        Parameters
        ----------
        name : str
            Name for the CCOBRA model.

        n_samples : int
            Number of samples to draw from mReasoner in order to mitigate the effects
            of its randomized inference processes.

        method : str
            Parameter optimization technique ('grid' or 'random').

        fit_its : int
            Number of iterations for the parameter optimization (grid or random). If set to 0,
            fitting is deactivated.

        num_threads : int
            Number of parallel threads to perform the parameter optimization with.

        """

        super(CCobraMReasoner, self).__init__(name, ['syllogistic'], ['single-choice'])

        # Launch mreasoner interface
        self.cloz = mreasoner.ClozureCL()
        self.mreas_path = mreasoner.source_path()
        self.mreasoner = mreasoner.MReasoner(self.cloz.exec_path(), self.mreas_path)

        self.params = copy.deepcopy(self.mreasoner.DEFAULT_PARAMS)

        # Store instance variables
        self.n_samples = n_samples
        self.fit_its = fit_its

        # Initialize auxiliary variables
        self.n_pre_train_dudes = 0
        self.pre_train_data = np.zeros((64, 9))
        self.person_train_data = np.zeros((64, 9))
        self.history = np.zeros((64, 9))

        self.start_time = None

    def __deepcopy__(self, memo):
        """ Custom deepcopy required because thread locks cannot be pickled. Deepcopy realized by
        creating a fresh instance of the mReasoner model and syncing parameters.

        Parameters
        ----------
        memo : dict
            Memo dictionary of objects already copied. Should be passed to nested deepcopy calls.

        Returns
        -------
        CCobraMReasoner
            Copied object instance.

        """

        # Create the new instance
        new = CCobraMReasoner(self.name, self.n_samples, self.fit_its)

        # Copy member variables
        new.n_pre_train_dudes = self.n_pre_train_dudes
        new.pre_train_data = self.pre_train_data
        new.person_train_data = self.person_train_data
        new.history = self.history
        new.params = self.params

        return new

    def end_participant(self, subj_id, model_log, **kwargs):
        """ When the prediction phase is finished, terminate the LISP subprocess.

        """

        # Parameterization output
        print('End Participant ({:.2f}s, {} its) id={} params={}'.format(
            time.time() - self.start_time,
            self.fit_its,
            subj_id,
            str(list(self.params.items())).replace(' ', ''),
        ))

        sys.stdout.flush()

        # Terminate the mReasoner instance
        self.mreasoner.terminate()

    def start_participant(self, **kwargs):
        """ Model setup method. Stores the time for use in end_participant().

        """

        self.start_time = time.time()

    def pre_train(self, dataset):
        """ Pre-trains the model by fitting mReasoner.

        Parameters
        ----------
        dataset : list(list(dict(str, object)))
            Training data.

        """

        print('pre-training')

        # Check if fitting is deactivated
        if self.fit_its == 0 or self.evaluation_type == 'coverage':
            return

        # Extract the training data to fit mReasoner with
        self.n_pre_train_dudes = len(dataset)
        self.pre_train_data = np.zeros((64, 9))
        for subj_data in dataset:
            for task_data in subj_data:
                item = task_data['item']
                enc_task = ccobra.syllogistic.encode_task(item.task)
                enc_resp = ccobra.syllogistic.encode_response(task_data['response'], item.task)

                task_idx = ccobra.syllogistic.SYLLOGISMS.index(enc_task)
                resp_idx = ccobra.syllogistic.RESPONSES.index(enc_resp)
                self.pre_train_data[task_idx, resp_idx] += 1

        div_mask = (self.pre_train_data.sum(axis=1) != 0)
        self.pre_train_data[div_mask] /= self.pre_train_data[div_mask].sum(axis=1, keepdims=True)

        # Fit the model
        self.fit()

    def person_train(self, dataset, **kwargs):
        """ Perform the person training of mReasoner.

        """

        # Check if fitting is deactivated
        if self.fit_its == 0:
            return

        # Extract the training data to fit mReasoner with
        self.person_train_data = np.zeros((64, 9))
        for task_data in dataset:
            item = task_data['item']
            enc_task = ccobra.syllogistic.encode_task(item.task)
            enc_resp = ccobra.syllogistic.encode_response(task_data['response'], item.task)

            task_idx = ccobra.syllogistic.SYLLOGISMS.index(enc_task)
            resp_idx = ccobra.syllogistic.RESPONSES.index(enc_resp)
            self.person_train_data[task_idx, resp_idx] += 1

        div_mask = (self.person_train_data.sum(axis=1) != 0)
        self.person_train_data[div_mask] /= self.person_train_data[div_mask].sum(axis=1, keepdims=True)

        # Fit the model
        self.fit()

    def fit(self):
        # Merge the training datasets
        history_copy = self.history.copy()
        div_mask = (history_copy.sum(axis=1) != 0)
        history_copy[div_mask] /= history_copy[div_mask].sum(axis=1, keepdims=True)

        train_data = self.pre_train_data + self.person_train_data + history_copy

        best_score = 0
        best_param_dicts = []

        for p_epsilon in np.linspace(*self.mreasoner.param_bounds[0], self.fit_its):
            print('epsilon:', p_epsilon)
            for p_lambda  in np.linspace(*self.mreasoner.param_bounds[1], self.fit_its):
                print('   lambda:', p_lambda)
                for p_omega in np.linspace(*self.mreasoner.param_bounds[2], self.fit_its):
                    print('      omega:', p_omega)
                    for p_sigma in np.linspace(*self.mreasoner.param_bounds[3], self.fit_its):
                        param_dict = {
                            'epsilon': p_epsilon,
                            'lambda': p_lambda,
                            'omega': p_omega,
                            'sigma': p_sigma
                        }

                        # Generate mReasoner prediction matrix
                        pred_mat = np.zeros((64, 9))
                        for syl_idx, syllog in enumerate(ccobra.syllogistic.SYLLOGISMS):
                            premises = self.syllog_to_premises(syllog)

                            for _ in range(self.n_samples):
                                # Obtain mReasoner prediction
                                predictions = self.mreasoner.query(premises, param_dict=param_dict)
                                for pred in predictions:
                                    if pred in ccobra.syllogistic.RESPONSES:
                                        pred_mat[syl_idx, ccobra.syllogistic.RESPONSES.index(pred)] += 1 / len(predictions)

                        # Compare predictions with data
                        pred_mask = (pred_mat == pred_mat.max(axis=1, keepdims=True))
                        score = np.sum(np.mean(train_data * pred_mask, axis=1))

                        if score > best_score:
                            best_score = score
                            best_param_dicts = [param_dict]
                        elif score == best_score:
                            best_param_dicts.append(param_dict)

        # Randomly select ont of the best param dicts
        self.params = best_param_dicts[int(np.random.randint(0, len(best_param_dicts)))]
        self.best_param_dicts = best_param_dicts

    def syllog_to_premises(self, syllog):
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
        return [prem1, prem2]

    def predict(self, item, **kwargs):
        """ Queries mReasoner for a prediction.

        Parameters
        ----------
        item : ccobra.Item
            Task item.

        Returns
        -------
        list(str)
            Syllogistic response prediction.

        """

        # Extract premises
        syllog = ccobra.syllogistic.Syllogism(item)
        premises = self.syllog_to_premises(syllog.encoded_task)

        # Sample predictions from mReasoner
        pred_scores = np.zeros((9,))
        for _ in range(self.n_samples):
            predictions = self.mreasoner.query(premises, self.params)
            for pred in predictions:
                if pred not in ccobra.syllogistic.RESPONSES:
                    print('Invalid Response:', pred)
                else:
                    resp_idx = ccobra.syllogistic.RESPONSES.index(pred)
                    pred_scores[resp_idx] += 1 / len(predictions)

        # Determine best prediction
        cand_idxs = np.arange(len(pred_scores))[pred_scores == pred_scores.max()]
        pred_idx = np.random.choice(cand_idxs)
        return syllog.decode_response(ccobra.syllogistic.RESPONSES[pred_idx])

    def adapt(self, item, truth, **kwargs):
        """ Adapts mReasoner to the participant responses.

        """

        # Encode syllogistic information
        enc_task = ccobra.syllogistic.encode_task(item.task)
        enc_resp = ccobra.syllogistic.encode_response(truth, item.task)

        # Update history
        task_idx = ccobra.syllogistic.SYLLOGISMS.index(enc_task)
        resp_idx = ccobra.syllogistic.RESPONSES.index(enc_resp)
        self.history[task_idx, resp_idx] += 1

        # Perform training
        self.fit()

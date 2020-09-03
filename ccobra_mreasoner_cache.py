""" CCOBRA model wrapper for mReasoner.

"""

import sys
import collections
import threading
import copy
import logging
import time

import ccobra
import numpy as np

import mreasoner
import create_cache

logging.basicConfig(level=logging.INFO)

class CCobraMReasoner(ccobra.CCobraModel):
    """ mReasoner CCOBRA model implementation.

    """

    def __init__(self, name='mReasoner', n_samples=2, fit_its=5, cache_file=None):
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

        # Store instance variables
        self.n_samples = n_samples
        self.fit_its = fit_its

        # Prepare mReasoner parameters
        self.params = copy.deepcopy(mreasoner.DEFAULT_PARAMS)

        # Initialize auxiliary variables
        self.n_pre_train_dudes = 0
        self.pre_train_data = np.zeros((64, 9))
        self.history = np.zeros((64, 9))

        # Prepare prediction cache
        self.prediction_cache = None
        if cache_file:
            self.prediction_cache = np.load(cache_file)
            if self.prediction_cache.shape[0] != fit_its:
                print('WARNING: fit_its mismatch between model and cache.')
            fit_its = self.prediction_cache.shape[0]
        else:
            self.prediction_cache = create_cache.generate_cache(self.fit_its, self.n_samples)

        self.start_time = None

    def end_participant(self, subj_id, model_log, **kwargs):
        """ When the prediction phase is finished, terminate the LISP subprocess.

        """

        # Parameterization output
        print('End Participant ({:.2f}s, {} its) id={} params={}'.format(
            time.time() - self.start_time,
            self.fit_its,
            subj_id,
            str([(x, y[1]) for x, y in self.params.items()]).replace(' ', ''),
        ))
        sys.stdout.flush()

        # Fill model log
        model_log.update({x: y[1] for x, y in self.params.items()})

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
        for task_data in dataset:
            item = task_data['item']
            enc_task = ccobra.syllogistic.encode_task(item.task)
            enc_resp = ccobra.syllogistic.encode_response(task_data['response'], item.task)

            task_idx = ccobra.syllogistic.SYLLOGISMS.index(enc_task)
            resp_idx = ccobra.syllogistic.RESPONSES.index(enc_resp)
            self.history[task_idx, resp_idx] += 1

        # Fit the model
        self.fit()

    def fit(self):
        # Merge the training datasets
        history_copy = self.history.copy()
        div_mask = (history_copy.sum(axis=1) != 0)
        history_copy[div_mask] /= history_copy[div_mask].sum(axis=1, keepdims=True)

        train_data = self.pre_train_data
        train_data[div_mask] = history_copy[div_mask]

        best_score = 0
        best_param_dicts = []

        for idx_epsilon, p_epsilon in enumerate(np.linspace(*mreasoner.PARAM_BOUNDS[0], self.fit_its)):
            print('epsilon:', p_epsilon)
            for idx_lambda, p_lambda  in enumerate(np.linspace(*mreasoner.PARAM_BOUNDS[1], self.fit_its)):
                print('   lambda:', p_lambda)
                for idx_omega, p_omega in enumerate(np.linspace(*mreasoner.PARAM_BOUNDS[2], self.fit_its)):
                    for idx_sigma, p_sigma in enumerate(np.linspace(*mreasoner.PARAM_BOUNDS[3], self.fit_its)):
                        param_dict = {
                            'epsilon': (idx_epsilon, p_epsilon),
                            'lambda': (idx_lambda, p_lambda),
                            'omega': (idx_omega, p_omega),
                            'sigma': (idx_sigma, p_sigma)
                        }

                        # Generate mReasoner prediction matrix
                        pred_mat = self.prediction_cache[
                            idx_epsilon, idx_lambda, idx_omega, idx_sigma]

                        # Compare predictions with data
                        pred_mask = (pred_mat == pred_mat.max(axis=1, keepdims=True))
                        score = np.sum(np.mean(train_data * pred_mask, axis=1))

                        if score > best_score:
                            print('New best ({}): {}'.format(score, str(param_dict)))
                            best_score = score
                            best_param_dicts = [param_dict]
                        elif score == best_score:
                            best_param_dicts.append(param_dict)

        # Randomly select ont of the best param dicts
        self.params = best_param_dicts[int(np.random.randint(0, len(best_param_dicts)))]
        print(best_param_dicts)
        exit()
        self.best_param_dicts = best_param_dicts

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

        # Sample predictions from mReasoner
        pred_mat = self.prediction_cache[
            self.params['epsilon'][0], self.params['lambda'][0], self.params['omega'][0], self.params['sigma'][0]]
        syl_idx = ccobra.syllogistic.SYLLOGISMS.index(syllog.encoded_task)
        preds = pred_mat[syl_idx]

        # Determine best prediction
        cand_choices = np.array(ccobra.syllogistic.RESPONSES)[preds == preds.max()]
        return syllog.decode_response(np.random.choice(cand_choices))

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

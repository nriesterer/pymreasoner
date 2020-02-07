""" CCOBRA model wrapper for mReasoner.

"""

import sys
import collections
import threading

import ccobra
import mreasoner
import numpy as np

import time

import logging

logging.basicConfig(level=logging.INFO)

class CCobraMReasoner(ccobra.CCobraModel):
    """ mReasoner CCOBRA model implementation.

    """

    def __init__(self, name='mReasoner', n_samples=2, method='grid', fit_its=5, num_threads=4):
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

        # Store instance variables
        self.n_samples = n_samples
        if method not in ['random', 'grid']:
            raise ValueError(
                "Invalid method for parameter search. {} not in ['random', 'grid']".format(method))
        self.method = method
        self.fit_its = fit_its
        self.num_threads = num_threads

        # Initialize auxiliary variables
        self.has_pre_train = None
        self.has_person_train = None

        self.pre_train_data = None
        self.n_pre_train_dudes = 0
        self.best_params = None
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
        new = CCobraMReasoner(
            self.name, self.n_samples, self.method, self.fit_its, self.num_threads)

        # Copy member variables
        new.has_pre_train = self.has_pre_train
        new.has_person_train = self.has_person_train

        new.pre_train_data = self.pre_train_data
        new.n_pre_train_dudes = self.n_pre_train_dudes
        new.best_params = self.best_params
        new.history = self.history

        # Deep copy properties of mreasoner instance
        for param, value in self.mreasoner.params.items():
            new.mreasoner.set_param(param, value)

        return new

    def setup_environment(self, evaluation_type, pre_train_domains, person_train_domains, prediction_domains):
        """ Setup mReasoner environment by determining which trainings need to be handled.

        """

        self.has_pre_train = (pre_train_domains is not None)
        self.has_person_train = (person_train_domains is not None)

    def end_participant(self, subj_id, **kwargs):
        """ When the prediction phase is finished, terminate the LISP subprocess.

        """

        # Parameterization output
        print('End Participant ({:.2f}s, {} its) id={} params={}'.format(
            time.time() - self.start_time,
            self.fit_its,
            subj_id,
            str(list(self.mreasoner.params.items())).replace(' ', ''),
        ))

        for params in self.best_params:
            param_labels = ['epsilon', 'lambda', 'omega', 'sigma']
            param_dict = dict(zip(param_labels, params))
            print('  params={}'.format(str(list(param_dict.items())).replace(' ', '')))

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

        # Check if fitting is deactivated
        if self.fit_its == 0:
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

        self.pre_train_data = self.pre_train_data / np.sum(self.pre_train_data, axis=1).reshape(64, 1)

        # Only fit directly, if not person trained later
        if not self.has_person_train:
            self.fit(self.pre_train_data)

    def fit(self, fit_data):
        """ Fits mReasoner.

        Parameters
        ----------
        fit_data : list(str, str)
            Training data list.

        """

        if self.method == 'grid':
            self.fit_mreasoner_grid(fit_data)
        elif self.method == 'random':
            self.fit_mreasoner_random(fit_data)
        else:
            assert False, 'Invalid Fitting method.'

    def fit_mreasoner_random(self, fit_data):
        """ Fit mReasoner parameters using random search.

        Parameters
        ----------
        fit_data : np.array
            Data matrix of shape 64x9

        """

        # Initialize the result queue
        start_time = time.time()
        thread_results = []

        def work_fun(fit_data, mreas_path, cloz_path, fit_its, n_samples):
            """ Worker for the parallel random search fitting of individuals.

            """

            # Create local mReasoner copy
            thread_mreasoner = mreasoner.MReasoner(cloz_path, mreas_path)

            # Initialize result storage variables
            best_score = 0
            best_params = []

            # Perform the fit
            for _ in range(fit_its):
                # Generate random parameter values
                p_epsilon = np.random.uniform(*thread_mreasoner.param_bounds[0])
                p_lambda  = np.random.uniform(*thread_mreasoner.param_bounds[1])
                p_omega = np.random.uniform(*thread_mreasoner.param_bounds[2])
                p_sigma = np.random.uniform(*thread_mreasoner.param_bounds[3])
                params = [p_epsilon, p_lambda, p_omega, p_sigma]
                thread_mreasoner.set_param_vec(params)

                # Retrieve prediction samples
                score = 0
                for syl_idx, syl in enumerate(ccobra.syllogistic.SYLLOGISMS):
                    for _ in range(n_samples):
                        preds = thread_mreasoner.query(syl)
                        if not isinstance(preds, list):
                            preds = [preds]

                        for pred in preds:
                            pred_idx = ccobra.syllogistic.RESPONSES.index(pred)
                            score += (fit_data[syl_idx, pred_idx] / len(preds))

                # Store result if better than before
                if score > best_score:
                    best_score = score
                    best_params = [params]
                elif score == best_score:
                    best_params.append(params)

            # Terminate the worker's mReasoner instance and return best parameterization
            thread_mreasoner.terminate()
            thread_results.append((best_score, best_params))

        # Start the threads
        threads = []
        n_threads = min(self.num_threads, self.fit_its)
        for thread_idx in range(n_threads):
            # Compute the number of parameterizations to test in this thread
            cur_fit_its = 0
            if thread_idx < (n_threads - 1):
                cur_fit_its = self.fit_its // n_threads
            else:
                cur_fit_its = self.fit_its - (self.fit_its // n_threads) * (n_threads - 1)

            # Start the thread
            th = threading.Thread(target=work_fun, args=(
                fit_data, self.mreas_path, self.cloz.exec_path(), cur_fit_its, self.n_samples))
            th.start()
            threads.append(th)

        # Wait for the threads to finish
        for th in threads:
            th.join()

        # Evaluate the results
        best_score_value = np.max([score for score, _ in thread_results])
        best_params = []
        for res_score, res_params in thread_results:
            if res_score == best_score_value:
                best_params.extend(res_params)
        self.best_params = best_params

        # In case of multiple equivalent parameterizations, choose one and apply it to mReasoner
        used_params = best_params[np.random.randint(0, len(best_params))]
        self.mreasoner.set_param_vec(used_params)

        # Debug output
        print('   Fit ({:.2f}s, {} its) score={:.4f} params={}'.format(
            time.time() - start_time,
            self.fit_its,
            best_score_value,
            str(list(self.mreasoner.params.items())).replace(' ', ''),
        ))

        for params in self.best_params:
            param_labels = ['epsilon', 'lambda', 'omega', 'sigma']
            param_dict = dict(zip(param_labels, params))
            print('      params={}'.format(str(list(param_dict.items())).replace(' ', '')))

        sys.stdout.flush()

    def fit_mreasoner_grid(self, fit_data):
        """ Fit mReasoner parameters using grid search

        Parameters
        ----------
        train_x : list(str)
            List of syllogistic task encodings (e.g., 'AA1').

        train_y : list(str)
            List of syllogistic response encodings (e.g., 'Aca').

        """

        # Initialize the result queue
        start_time = time.time()
        thread_results = []

        def work_fun(fit_data, mreas_path, cloz_path, epsilon_values, fit_its, n_samples):
            """ Worker for the parallel grid search fitting of individuals.

            """

            # Create local mReasoner copy
            thread_mreasoner = mreasoner.MReasoner(cloz_path, mreas_path)

            # Initialize result storage variables
            best_score = 0
            best_params = []

            # Perform the fit
            for p_epsilon in epsilon_values:
                for p_lambda  in np.linspace(*thread_mreasoner.param_bounds[1], fit_its):
                    for p_omega in np.linspace(*thread_mreasoner.param_bounds[2], fit_its):
                        for p_sigma in np.linspace(*thread_mreasoner.param_bounds[3], fit_its):
                            params = [p_epsilon, p_lambda, p_omega, p_sigma]
                            thread_mreasoner.set_param_vec(params)


                            # Retrieve prediction samples
                            score = 0
                            for syl_idx, syl in enumerate(ccobra.syllogistic.SYLLOGISMS):
                                for _ in range(n_samples):
                                    preds = thread_mreasoner.query(syl)
                                    if not isinstance(preds, list):
                                        preds = [preds]

                                    for pred in preds:
                                        pred_idx = ccobra.syllogistic.RESPONSES.index(pred)
                                        score += (fit_data[syl_idx, pred_idx] / len(preds))

                            # Store result if better than before
                            if score > best_score:
                                best_score = score
                                best_params = [params]
                            elif score == best_score:
                                best_params.append(params)

            # Terminate the worker's mReasoner instance and return best parameterization
            thread_results.append((best_score, best_params))
            thread_mreasoner.terminate()

        # Compute assignment of epsilon values to threads
        epsilon_values = np.linspace(0, 1, self.fit_its)
        epsilon_ass = []
        n_threads = min(self.num_threads, self.fit_its)
        for thread_idx in range(n_threads):
            epsilon_ass.append(epsilon_values[thread_idx::n_threads])

        # Start the threads
        threads = []
        for epsilon_conf in epsilon_ass:
            # Start the thread
            th = threading.Thread(target=work_fun, args=(
                fit_data, self.mreas_path, self.cloz.exec_path(), epsilon_conf, self.fit_its, self.n_samples))
            th.start()
            threads.append(th)

        # Wait for the threads to finish
        for th in threads:
            th.join()

        # Evaluate the results
        best_score_value = np.max([score for score, _ in thread_results])
        best_params = []
        for res_score, res_params in thread_results:
            if res_score == best_score_value:
                best_params.extend(res_params)
        self.best_params = best_params

        # In case of multiple equivalent parameterizations, choose one and apply it to mReasoner
        used_params = best_params[np.random.randint(0, len(best_params))]
        self.mreasoner.set_param_vec(used_params)

        # Debug output
        print('   Fit ({:.2f}s, {} its) score={:.4f} params={}'.format(
            time.time() - start_time,
            self.fit_its,
            best_score_value,
            str(list(self.mreasoner.params.items())).replace(' ', ''),
        ))

        for params in self.best_params:
            param_labels = ['epsilon', 'lambda', 'omega', 'sigma']
            param_dict = dict(zip(param_labels, params))
            print('      params={}'.format(str(list(param_dict.items())).replace(' ', '')))

        sys.stdout.flush()

    def person_train(self, dataset, **kwargs):
        """ Perform the person training of mReasoner.

        """

        # Check if fitting is deactivated
        if self.fit_its == 0:
            return

        # Extract the training data to fit mReasoner with
        self.history = np.zeros((64, 9))
        for task_data in dataset:
            item = task_data['item']
            enc_task = ccobra.syllogistic.encode_task(item.task)
            enc_resp = ccobra.syllogistic.encode_response(task_data['response'], item.task)

            task_idx = ccobra.syllogistic.SYLLOGISMS.index(enc_task)
            resp_idx = ccobra.syllogistic.RESPONSES.index(enc_resp)
            self.history[task_idx, resp_idx] += 1

        self.history = self.history / np.sum(self.history, axis=1).reshape(64, 1)

        train_dat = self.merge_training_data()
        self.fit(train_dat)

    def merge_training_data(self):
        """ Merges pre training and adaption/person training data.

        """

        if not self.has_pre_train:
            return self.history

        history_mask = (self.history.sum(axis=1) * -1 + 1).reshape(64, 1).astype('int')
        result_data = self.pre_train_data * history_mask + self.history
        return result_data

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

        enc_task = ccobra.syllogistic.encode_task(item.task)

        # Sample predictions from mReasoner
        enc_resp_cands = []
        for _ in range(self.n_samples):
            pred = self.mreasoner.query(enc_task)
            if isinstance(pred, list):
                enc_resp_cands.extend(pred)
            else:
                enc_resp_cands.append(pred)

        # Determine best prediction
        pred_counts = dict(collections.Counter(enc_resp_cands))
        max_count = np.max(list(pred_counts.values()))
        max_preds = [pred for pred, pred_count in pred_counts.items() if pred_count == max_count]

        enc_resp = np.random.choice(max_preds)
        return ccobra.syllogistic.decode_response(enc_resp, item.task)

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
        train_dat = self.merge_training_data()
        self.fit(train_dat)

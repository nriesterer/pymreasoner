""" CCOBRA model wrapper for mReasoner.

"""

import ccobra
import mreasoner


class CCobraMReasoner(ccobra.CCobraModel):
    """ mReasoner CCOBRA model implementation.

    """

    def __init__(self, name='mReasoner', fit=False):
        """ Initializes the CCOBRA model by launching the interactive LISP subprocess.

        """

        super(CCobraMReasoner, self).__init__(name, ['syllogistic'], ['single-choice'])

        # Launch mreasoner interface
        cloz = mreasoner.ClozureCL()
        mreas_path = mreasoner.source_path()

        self.mreasoner = mreasoner.MReasoner(cloz.exec_path(), mreas_path)

        # Member variables
        self.fit = fit

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

        new = CCobraMReasoner()

        # Deep copy properties of mreasoner instance
        for param, value in self.mreasoner.params.items():
            new.mreasoner.set_param(param, value)

        return new

    def end_participant(self, _, **kwargs):
        """ When the prediction phase is finished, terminate the LISP subprocess.

        """

        self.mreasoner.terminate()

    def pre_train(self, dataset):
        """ Pre-trains the model by fitting mReasoner.

        Parameters
        ----------
        dataset : list(list(dict(str, object)))
            Training data.

        """

        if not self.fit:
            return

        train_x = []
        train_y = []
        for subj_data in dataset:
            for task_data in subj_data:
                item = task_data['item']
                enc_task = ccobra.syllogistic.encode_task(item.task)
                enc_resp = ccobra.syllogistic.encode_response(task_data['response'], item.task)
                train_x.append(enc_task)
                train_y.append(enc_resp)

        self.mreasoner.fit(train_x, train_y, 5)

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
        enc_resp = self.mreasoner.query(enc_task)
        return ccobra.syllogistic.decode_response(enc_resp, item.task)

class Protocol:
    """
    Protocol Specification Interface
    """

    def init(self, state):
        """
        Model:
        Define the miner's initial state, before protocol execution begins.

        Technically:
        This function modifies state by setting its attributes.
        The function returns None.
        """
        raise NotImplementedError

    def mining(self, state):
        """
        Model:
        Define how the miner extends the blockchain

        Technically:
        The miner's state is read-only.
        The function returns a list of parent blocks.
        The model then attaches a new block with these parents to the DAG.
        """
        raise NotImplementedError

    def update(self, state, block):
        """
        Model:
        Define how the miner reacts when it learns about a new block.

        Technically:
        The miner's state is read-write.
        The function modifies the state and returns None.
        """
        raise NotImplementedError

    def history(self, state):
        """
        Conceptually:
        Define how the miner's linear block history.

        Technically:
        The miner's state is read-only.
        The function returns a list of blocks.
        """
        raise NotImplementedError

    def progress(self, block):
        """
        Conceptually:
        Define the difficulty adjustment target; the DAA shall try to maintain
        a constant progress rate per time.

        Technically:
        The function returns a number defining the progress contribution of
        a single block in a linear history.
        """
        raise NotImplementedError

    def coinbase(self, block):
        """
        Conceptually:
        Define the reward scheme.

        Technically:
        The function returns a list of (miner, amount) tuples associated with
        a single block in a linear history.
        """
        raise NotImplementedError

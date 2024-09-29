class Protocol:
    """
    Protocol Specification Interface
    """

    def init(self):
        """
        Model:
        Define the miner's initial state, before protocol execution begins.

        Technically:
        This function may modify self.state by setting its attributes.
        The function returns None.
        """
        raise NotImplementedError

    def mining(self):
        """
        Model:
        Define how the miner extends the blockchain

        Technically:
        The miner's state (self.state) is read-only.
        The function returns a list of parent blocks.
        The model then attaches a new block with these parents to the DAG.
        """
        raise NotImplementedError

    def update(self, block):
        """
        Model:
        Define how the miner reacts when it learns about a new block.

        Technically:
        The miner's state (self.state) is read-write.
        The function modifies the state and returns None.
        """
        raise NotImplementedError

    def history(self):
        """
        Conceptually:
        Define how the miner's linear block history.

        Technically:
        The miner's state (self.state) is read-only.
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

    def relabel_state(self, new_ids):
        """
        Some algorithms need to relabel the block ids to merge isomorphic
        states. This is mostly transparent to the protocol spec except when
        the protocol references block ids in its state. Example: the preferred
        block in Bitcoin.

        This functions modifies the protocol state in-place, updating all block
        references.
        """
        raise NotImplementedError

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
        The function returns a set of parent blocks.
        The model then attaches a new block with these parents to the DAG.

        Note:
        It's a set of parents because outgoing edges in the DAG are not
        ordered. Protocols relying on the ordering of parents, i.e. you would
        return a list here, are currently not supported.
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

    def color_block(self, block):
        """
        DAG normalization works better if the block coloring captures also the
        miner's state. Finding the right colors requires input from the
        protocol designer. E.g. in bitcoin the preferred block has a different
        color than all other blocks.

        This function returns the color of the given block.

        We currently support at most 2 colors. The function must return 0 or 1.
        """
        raise NotImplementedError

    def collect_garbage(self):
        """
        Some algorithms benefit from discarding stale blocks. Deciding which
        blocks are stale depends on the protocol. E.g., in Bitcoin, all blocks
        off the longest chain are stale; in Ghostdag, no block is stale as all
        blocks are merged into the chain.

        This function returns the tips that are still relevant. Relevant tips
        and their past are considered _not_ stale. E.g., for Bitcoin this
        function returns the preferred tip of the chain; for Ghostdag it would
        return all tips.
        """
        raise NotImplementedError

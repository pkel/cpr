from .model import DAG, Protocol, Miner
from typing import Callable


class SingleMinerSim:
    def __init__(self, protocol: type[Protocol], *args, **kwargs):
        self.dag = DAG()
        self.miner = Miner(self.dag, protocol, *args, **kwargs)

    def step(self):
        parents = self.miner.mining()
        block = self.dag.append(parents, 0)
        self.miner.deliver(block)

    def reward_and_progress(self):
        history = self.miner.history()
        rew, prg = 0, 0
        for b in history:
            for _, amount in self.miner.coinbase(b):
                rew += amount
            prg += self.miner.progress(b)
        return (rew, prg)

    def sim(self, max_progress):
        i = 0
        prg = 0
        while prg < max_progress:
            self.step()
            rew, prg = self.reward_and_progress()

            i += 1

        return rew, prg


from queue import PriorityQueue


class DiscreteEventSim:
    def __init__(self):
        self.clock = 0
        self.event_queue = PriorityQueue()

    def delay(self, seconds, fun, *args, **kwargs):
        self.event_queue.put((self.clock + seconds, fun, args, kwargs))

    def step(self):
        self.clock, fun, args, kwargs = self.event_queue.get()
        fun(*args, **kwargs)

    def loop(self, stop_condition: (lambda x: false)):
        while not (self.event_queue.empty() or stop_condition(self)):
            self.step()


class NetworkSim(DiscreteEventSim):
    def __init__(
        self,
        protocol: type[Protocol],
        *args,
        n_miners: int,
        mining_delay: Callable[[], float],
        select_miner: Callable[[], int],
        message_delay: Callable[[], float],
        **kwargs
    ):
        # init discrete event simulator
        super().__init__()

        self.dag = DAG()
        self.miners = [
            Miner(self.dag, protocol, *args, **kwargs) for i in range(n_miners)
        ]
        self.judge = Miner(self.dag, protocol, *args, **kwargs)

        self.mining_delay = mining_delay
        self.select_miner = select_miner
        self.message_delay = message_delay

        # schedule first mining event
        self.delay(self.mining_delay(), self.mining)

    def mining(self):
        miner_id = self.select_miner()
        miner = self.miners[miner_id]
        parents = miner.mining()
        block = self.dag.append(parents, miner_id)
        miner.deliver(block)
        self.judge.deliver(block)

        # communication
        for i, m in enumerate(self.miners):
            if i != miner_id:
                self.delay(self.message_delay(), self.deliver, m, block)

        # next mining event
        self.delay(self.mining_delay(), self.mining)

    def deliver(self, miner, block):
        # deliver once
        if block in miner.visible:
            return

        # deliver in-order
        for p in miner.dag.parents(block):
            self.deliver(miner, p)

        miner.deliver(block)

    def reward_and_progress(self):
        history = self.judge.history()
        rew, prg = 0, 0
        for b in history:
            for _, amount in self.judge.coinbase(b):
                rew += amount
            prg += self.judge.progress(b)
        return (rew, prg)

    def sim(self, max_progress):
        def stop_condition(self):
            rew, prg = self.reward_and_progress()
            return prg >= max_progress

        self.loop(stop_condition)

        rew, prg = self.reward_and_progress()

        return dict(
            time=self.clock,
            blocks=self.dag.size(),
            rew=rew,
            prg=prg,
        )

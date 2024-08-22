from .. import protocols
from ..draft import SingleMinerSim

print(SingleMinerSim(protocols.Bitcoin).sim(100))
print(SingleMinerSim(protocols.Ghostdag, k=7).sim(100))

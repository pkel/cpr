from .. import protocols
from ..draft import SingleMinerNetwork

print(SingleMinerNetwork(protocols.Bitcoin).sim(100))
print(SingleMinerNetwork(protocols.Ghostdag, k=7).sim(100))

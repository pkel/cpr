k = 3

## omitted, boring helper functions


def tips(subgraph):
    return {b for b in subgraph if len(children(b) & subgraph) == 0}


def past_or_future(relation, subgraph, block):
    acc = set()
    stack = set(relation(block)) & subgraph
    while len(stack) > 0:
        b = stack.pop()
        if b not in acc:
            acc.add(b)
            for p in set(relation(b)) & subgraph:
                stack.add(p)
    return acc


def past(subgraph, block):
    return past_or_future(parents, subgraph, block)


def future(subgraph, block):
    return past_or_future(children, subgraph, block)


def anticone(subgraph, block):
    return subgraph - {block} - past(subgraph, block) - future(subgraph, block)


## omitted, boring protocol spec functions


def init():
    pass


def update(block):
    pass


def progress(block):
    return 1


def coinbase(block):
    return [(miner_of(block), 1)]

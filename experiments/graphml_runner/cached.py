import joblib
import networkx
import os
import subprocess

loc = os.path.dirname(__file__)

memory = joblib.Memory(loc + "/_cache", verbose=0)


@memory.cache
def sim(G, **kwargs):
    G = G.copy()
    for k, v in kwargs.items():
        G.graph[k] = v

    # force determinism
    if "seed" not in G.graph:
        G.graph["seed"] = 42

    type_of_node_key = type(list(G.nodes.keys())[0])

    with subprocess.Popen(
        ["_build/default/simulator/bin/graphml_runner.exe"],
        cwd=loc + "/../..",
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
    ) as process:
        for line in networkx.generate_graphml(G):
            try:
                process.stdin.write(line)
            except BrokenPipeError:
                pass
        out, err = process.communicate()
        G = networkx.parse_graphml(out)
        node_labels = {x: type_of_node_key(x) for x in G.nodes.keys()}
        return networkx.relabel_nodes(G, node_labels)


# rebuild simulator once per import

subprocess.run(
    ["dune", "build", "simulator/bin/graphml_runner.exe"],
    cwd=loc + "/../..",
    check=True,
)

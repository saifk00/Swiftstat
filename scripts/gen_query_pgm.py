import sys

def generate_query(n, j):
    # a query that involves j nodes
    assert(j >= 2)
    j -= 1 # one of them is the last C node

    s = "E["
    # (k+1)*Bk + (k+2)B{k+1} ...
    for k in range(j):
        s += f"{10*k+1}*B{k} + "

    s += f"C{n} | A0] #Q1<0.05, 0.95>"

    return s

def generate_dynamic_pgm(n, j):
    connections = ["A0 -> B0"]
    query = generate_query(n, j)
    nodes = [
"""
A0 : [0, 1] {
    0.25, 0.75;
}
""",
"""
B0<A0> : [0, 1] {
    0.75, 0.25;
    0.25, 0.75;
}
""",
"""
C0: [300, 500] {
    0.5, 0.5;
}
"""]

    for i in range(1, n+1):
        connections.extend([
        f"A{i} -> B{i}",
        f"B{i-1} -> B{i}",
        f"B{i-1} -> A{i}",
        f"C{i-1} -> C{i}",
        f"C{i-1} -> A{i}"])

        nodes.extend([
f"""
A{i}<B{i-1}, C{i-1}> : [0, 1] {{
    0.125, 0.875;
    0.875, 0.125;
    0.5, 0.5;
    0.25, 0.75;
}}
""",
f"""
B{i}<B{i-1}, A{i}> : [0, 1] {{
    0.25, 0.75;
    0.5, 0.5;
    0.125, 0.875;
    0.875, 0.125;
}}
""",
f"""
C{i}<C{i-1}> : [300, 500] {{
    0.125, 0.875;
    0.5, 0.5
}}
"""
])

    conn_text = "\n".join(connections)
    node_text = "\n".join(nodes)
    pgm_text = f"""
/**
* dynamic bayesian network with T={n}
* |V|={len(nodes)} |E|={len(connections)}
*/
dynamic_{n} bayesian

connections:
{conn_text}

nodes:
{node_text}

queries:
{query}
"""
    return pgm_text


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(f"usage: {sys.argv[0]} <n: size> <j: query complexity>")
    n = int(sys.argv[1])
    j = int(sys.argv[2])

    with open(f"models/dynamic_{n}_query_{j}.pgm", 'w') as f:
        f.write(generate_dynamic_pgm(n, j))
        print(f"wrote to models/dynamic_{n}_query_{j}.pgm")

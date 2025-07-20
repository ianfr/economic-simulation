# Available Agent-Based Simulation Models

## SimpleExchange

Based on reference (2) in [README.md](README.md).

A simple random exchange model where agents are paired randomly and exchange money. The model supports configurable debt limits:

- `debt_limit = 0.0`: Debt-free exchange (original behavior)
- `debt_limit > 0.0`: Agents can go into debt up to the specified limit

The exchange condition is: agent can give money if `cash >= -debt_limit + delta`, where `delta` is the exchange amount (default 1.0).

Configuration parameters:
- `n_agents`: Number of agents in the simulation
- `n_steps`: Number of simulation steps
- `init_cash`: Initial cash amount for each agent
- `debt_limit`: Maximum debt allowed (0.0 = debt-free)
- `seed`: Random seed for reproducibility
- `write_every`: Frequency of CSV output (every N steps)

## KineticIsing

Same as `SimpleExchange`, except also:
- Agents trade money only when they form buy-sell pairs (opposite **spins**)
- Agents also have a small probability of randomly flipping their spin

Configuration paramaters (on top of those for `SimpleExchange`):
- `flip_prob`: The probability of an agent randomly flipping its spin during an evolution step
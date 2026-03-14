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
- `exchange_delta`: Amount to exchange between agents (default 1.0)
- `seed`: Random seed for reproducibility
- `write_every`: Frequency of CSV output (every N steps)

## CoSimpleExchange

Same as above, but acts as a demonstration of how to use coarrays with Boltzmannomics. 
Note that since the exchange model is so simple, the simulation is communication-bound
so using coarrays is slower than serial execution, but it's a useful reference for future
development with more complex agents.

## KineticIsing

Same as `SimpleExchange`, except also:
- Agents trade money only when they form buy-sell pairs (opposite **spins**)
- Agents also have a small probability of randomly flipping their spin
- Each agent has a binary spin state: `.true.` = buy (+1), `.false.` = sell (-1)
- Money exchange only occurs between agents with opposite spins (buyer-seller pairs)
- Additional metrics computed: magnetization and spin correlation

Configuration parameters (on top of those for `SimpleExchange`):
- `flip_prob`: The probability of an agent randomly flipping its spin during an evolution step

Additional metrics tracked:
- `magnetization`: Overall tendency of agents to buy vs sell
- `spin_correlation`: Measure of spin alignment in the population

## CCMExchange

Based on the Chakraborti and Chakrabarti model with individual saving propensities. This model implements a more sophisticated exchange mechanism where each agent has a personal saving tendency that affects wealth exchanges.

Key features:
- Each agent has an individual saving propensity randomly assigned at initialization
- Wealth exchanges are governed by a 2x2 transition matrix based on saving propensities
- The exchange matrix uses parameter `eps` (randomly chosen for each exchange) and individual saving propensities `lambda_i` and `lambda_j`

Exchange transition matrix:
```
| lambda_i + eps*(1-lambda_i)      eps*(1-lambda_j)             |
| (1-eps)*(1-lambda_i)          lambda_j + (1-eps)*(1-lambda_j) |
```

Where:
- `lambda_i`, `lambda_j` are the saving propensities of the two agents
- `eps` is randomly chosen on [0,1] for every exchange

Configuration parameters:
- `n_agents`: Number of agents in the simulation
- `n_steps`: Number of simulation steps
- `init_cash`: Initial cash amount for each agent
- `debt_limit`: Maximum debt allowed (0.0 = debt-free)
- `exchange_delta`: Amount to exchange between agents (default 1.0)
- `min_saving_propensity`: Minimum individual saving propensity (default 0.1)
- `max_saving_propensity`: Maximum individual saving propensity (default 0.9)
- `seed`: Random seed for reproducibility
- `write_every`: Frequency of CSV output (every N steps)

## ConservativeExchangeMarket

Based on references (3) III.B and (4) in [README.md](README.md). This model implements an extremal dynamics approach where the poorest agent is identified and participates in wealth exchange.

Key features:
- Agents are arranged on a 1D lattice with nearest-neighbor connections
- Each agent has `2k` neighbors (`k` on each side) with periodic boundary conditions
- Lattice connections can be rewired at initialization with probability `rewiring_probability`
- Uses extremal dynamics: the poorest agent is always selected for exchange
- Conservative exchange: total wealth is preserved during transactions
- Wealth can become negative (debt is allowed)
- Static lattice topology (Case I scenario)

Exchange mechanism:
1. Find the poorest agent in the population
2. Randomly select one of their `2k` neighbors
3. Redistribute total wealth between the poorest agent and selected neighbor
4. Wealth redistribution fraction is randomly chosen

Configuration parameters:
- `n_agents`: Number of agents in the simulation
- `n_steps`: Number of simulation steps
- `k`: Number of nearest neighbors on each side (default 1)
- `rewiring_probability`: Probability of rewiring connections during initialization (default 0.1)
- `seed`: Random seed for reproducibility
- `write_every`: Frequency of CSV output (every N steps)

Note: Initial wealth is randomly distributed between 0 and 1 for each agent.

## CoDiscreteBouchaudMezard

Based on Di Matteo, Aste, and Hyde (2003), "Exchanges in complex networks: income and wealth distributions". This model implements a discrete-time Bouchaud-Mezard wealth exchange process on a network, accelerated by coarray Fortran.

Unlike CoSimpleExchange (which is communication-bound due to trivial per-agent computation), this model is **compute-bound**: each agent must iterate over all its neighbors to sum weighted wealth contributions every step, making it appropriate for SPMD parallelism via coarrays.

Key features:
- Agents are connected through a configurable network (1D ring lattice with optional small-world rewiring)
- Each agent distributes a fraction `q0` of its wealth equally to its neighbors each step
- Additive Gaussian noise models market/social fluctuations
- Wealth distribution is shaped by the degree distribution of the underlying network
- On scale-free networks, the model produces power-law tails in the wealth distribution
- On homogeneous networks, the wealth distribution decays exponentially
- The expectation value of each agent's wealth is proportional to its connectivity (Eq. 10 in the paper)

Wealth update equation (Eq. 3 from the paper):
```
w_l(t+1) = A_l(t) + (1 - q0) * w_l(t) + sum_{j in I_l} (q0 / z_j) * w_j(t)
```

Where:
- `A_l(t)` is additive Gaussian noise with mean 0 and standard deviation `sigma0`
- `q0` is the fraction of wealth each agent distributes to neighbors
- `z_j` is the degree (number of neighbors) of agent j
- `I_l` is the set of neighbors of agent l

Coarray parallelism strategy:
- Each coimage owns a contiguous slice of agents with their wealth stored in a coarray
- Each step, every image computes new wealth for its local agents by reading neighbor wealth values from local and remote images via one-sided coarray reads
- The heavy per-agent neighborhood summation (O(2k) FLOPs per agent) keeps each image compute-busy between sync points, achieving a favorable compute-to-communication ratio
- Only two `sync all` barriers per step (vs. three in CoSimpleExchange)

Configuration parameters:
- `n_agents`: Number of agents in the simulation (default 1000)
- `n_steps`: Number of simulation steps (default 10000)
- `init_wealth`: Initial wealth for each agent (default 100.0)
- `q0`: Fraction of wealth distributed to neighbors each step (default 0.1)
- `sigma0`: Standard deviation of additive Gaussian noise (default 5.0)
- `k`: Number of nearest neighbors on each side; total degree = 2k (default 3)
- `rewiring_probability`: Probability of rewiring each connection during initialization (default 0.0 = regular lattice)
- `seed`: Random seed for reproducibility (default 20250314)
- `write_every`: Frequency of CSV output (every N steps, default 100)

Metrics tracked:
- `gini_coefficient`: Gini coefficient of the wealth distribution

## StochasticPreferences

Based on Silver, Slud, and Takamoto (2002) - reference (5) in [README.md](README.md). This model implements an exchange market where agents have stochastic preferences for two goods and make optimal allocation decisions based on Cobb-Douglas utility functions.

Key features:
- Agents hold two goods (A and B) with quantities `a_it` and `b_it`
- Each agent has a stochastic preference `f_it` randomly drawn from [0,1] each time period
- Wealth is calculated as `w_it = a_it + θ_t * b_it` where `θ_t` is the market price
- Market-clearing price is determined by `θ_t = Σ(1-f_it)*a_(i,t-1) / Σf_it*b_(i,t-1)`
- Agents allocate wealth optimally based on Cobb-Douglas utility maximization
- Total quantities of goods A and B are conserved (`αN` and `βN` respectively)

Exchange mechanism:
1. Update stochastic preferences for all agents (new random values each period)
2. Compute market-clearing price based on aggregate preferences and holdings
3. Update agent allocations using optimal allocation equations
4. Update wealth based on new holdings and current price

Configuration parameters:
- `n_agents`: Number of agents in the simulation (default 1000)
- `n_steps`: Number of simulation steps (default 10000)
- `alpha`: Conservation parameter for good A (αN total, default 0.5)
- `beta`: Conservation parameter for good B (βN total, default 0.5)
- `init_good_a`: Initial holdings of good A per agent (default 50.0)
- `init_good_b`: Initial holdings of good B per agent (default 50.0)
- `seed`: Random seed for reproducibility (default 20250715)
- `write_every`: Frequency of CSV output (every N steps, default 1000)

Metrics tracked:
- `gini_good_a`: Gini coefficient for good A distribution
- `gini_good_b`: Gini coefficient for good B distribution
- `gini_wealth`: Gini coefficient for wealth distribution
- `price`: Current market price θ_t


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

## BMClassSplit

Based on Bouchaud-Mézard model with class heterogeneity - references (1) Fig. 7 and (3) in [README.md](README.md). This model implements a **non-conservative** wealth exchange system on a heterogeneous network designed to produce split Boltzmann-Pareto distributions observed in real income data.

Key features:
- **Two agent classes**: "Hub" agents (highly connected) and "regular" agents (sparsely connected)
- **Heterogeneous network structure**: Hubs have ~20 connections, regular agents have ~3 connections
- **Non-conservative dynamics**: Total wealth is NOT conserved - can grow or shrink over time
- **Multiplicative exchanges**: Wealth flows proportional to agent wealth (richer agents exchange larger amounts)
- **Stochastic returns**: Gaussian noise creates/destroys wealth randomly (η_i term)
- **Wealth-dependent transaction rates**: Flow rates biased toward richer agents
- **Preferential attachment**: Agents preferentially connect to hubs, creating realistic network topology
- **Mixed distribution outcome**: Lower wealth follows log-normal (Boltzmann), upper wealth follows power-law (Pareto)

The model reproduces the empirically observed split distribution where:
- The bulk of the population (regular agents) exhibits exponential/Boltzmann wealth distribution
- A small elite (hub agents) exhibits power-law/Pareto wealth distribution
- The transition creates the characteristic "kink" seen in real income data

Exchange mechanism (based on Eq. 22 from Ref. 3):
```
dw_i/dt = η_i(t)w_i(t) + Σ(J_ij w_j(t)) - Σ(J_ji w_i(t))
```
Where:
1. Each agent i randomly selects one network neighbor j per timestep
2. **Stochastic return**: η_i is a Gaussian random variable that multiplies agent i's wealth (creates/destroys wealth)
3. **Income flow**: Agent i receives J_ij * w_j from neighbor j (proportional to j's wealth)
4. **Expense flow**: Agent i sends J_ji * w_i to neighbor j (proportional to i's wealth)
5. **Transaction rates J_ij**: Base rate modulated by wealth bias - richer agents have higher inflow rates
6. Network topology constrains exchange partners, with hubs participating in more exchanges
7. **No conservation**: The sum of (η_i w_i) across all agents is non-zero, allowing total wealth to fluctuate

Configuration parameters:
- `n_agents`: Number of agents in the simulation (default 10000)
- `n_steps`: Number of simulation steps (default 100000)
- `init_cash`: Initial cash/wealth for each agent (default 100.0)
- `hub_fraction`: Fraction of agents designated as hubs (default 0.05 = 5%)
- `hub_connectivity`: Average number of connections per hub agent (default 20)
- `regular_connectivity`: Average number of connections per regular agent (default 3)
- `wealth_bias_strength`: Strength of wealth-dependent exchange bias, β parameter (default 0.5)
  - 0.0 = no bias (symmetric exchange like SimpleExchange)
  - 0.5 = moderate bias (realistic regime)
  - 1.0 = strong bias (extreme inequality)
- `seed`: Random seed for reproducibility (default 0)
- `write_every`: Frequency of CSV output (every N steps, default 1000)

Metrics tracked:
- `gini_coefficient`: Gini coefficient measuring overall wealth inequality
- `hub_wealth_fraction`: Fraction of total wealth held by hub agents

Notes:
- Network is initialized once at start (static topology)
- Preferential connection to hubs creates scale-free-like structure
- Hub agents naturally accumulate more wealth due to higher connectivity and wealth bias
- Resulting distribution should show characteristic split with crossover around 70-80th percentile
- For best results reproducing Fig. 7 from Ref. (1), use: `hub_fraction=0.05, wealth_bias_strength=0.5, n_steps>100000`


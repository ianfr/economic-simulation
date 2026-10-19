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
- `min_saving_propensity`: Minimum individual saving propensity, required, must lie on [0, 1]
- `max_saving_propensity`: Maximum individual saving propensity, required, must lie on [0, 1]
- `seed`: Random seed for reproducibility
- `write_every`: Frequency of CSV output (every N steps)

## CCMExchangeTLU

Same money-conserving pair-selection and 2x2 exchange rule as `CCMExchange`, but agents choose their saving
propensities through a threshold logic unit (TLU) over four bounded, dimensionless inputs
(`1`, own-wealth deviation, partner-wealth deviation, reward memory). Supports fixed, random, heuristic,
fixed-TLU, and online-learning stochastic-TLU policies in one model so fixed-vs-adaptive comparisons share
infrastructure and metrics. Rewards are myopic individual cash gains in a conservative exchange, not a
social-welfare target.

Policy modes (`policy_mode`):
- `ccm_fixed`: one continuous saving propensity per agent sampled on
  `[min_saving_propensity, max_saving_propensity]` at initialization, never changed (CCM reference).
- `binary_fixed`: `lambda_low` or `lambda_high` assigned once per agent with probability 0.5.
- `binary_random`: low/high saving chosen with probability 0.5 at each matched exchange.
- `heuristic`: high saving when own pre-trade wealth is below the population initial mean, else low saving.
- `tlu_fixed`: deterministic threshold decisions with fixed, randomly initialized weights.
- `tlu_learning`: stochastic-threshold (`p = sigmoid(z / policy_temperature)`) decisions with a per-trade
  Bernoulli score-function (REINFORCE) weight update against the prior `reward_ema` baseline.

Note: `ccm_fixed` uses a continuous saving distribution while `binary_fixed` is the direct control for the
two-action agents.

Configuration parameters (on top of the `CCMExchange` ones, which keep their meaning):
- `policy_mode`: one of the six modes above (default `'tlu_learning'`)
- `lambda_low`, `lambda_high`: two-level saving propensities on [0, 1] (defaults 0.2, 0.8)
- `learning_rate`: REINFORCE step size, >= 0 (default 0.01; 0 keeps stochastic actions but freezes weights)
- `policy_temperature`: sigmoid temperature, > 0 (default 1.0)
- `weight_init_scale`: weights initialized uniform on `[-scale, +scale]` (default 0.1)
- `weight_clip`: per-weight clip bound, > 0 (default 5.0)
- `reward_clip`: per-trade reward clip bound, > 0 (default 2.0)
- `memory_decay`: EMA decay on [0, 1) (default 0.9)
- `use_partner_wealth`, `use_memory`: feature ablations; disabled inputs are set to zero (both default `.true.`)

Reproducibility: matchmaking and exchange epsilons use the same RNG streams as `CCMExchange`; policy
initialization/exploration uses a separate stream derived deterministically from the nonzero `seed`, so
policy draws never consume the pairing/exchange sequences. Identical seed, settings, compiler, and execution
mode reproduce CPU results (cross-thread-count or cross-compiler bitwise identity is not guaranteed).

Population CSV columns (`agent_id,cash,saving_propensity,last_action,last_policy_probability,last_reward,reward_ema,w0,w1,w2,w3`):
at `t = 0`, `last_reward` and `reward_ema` are 0; non-TLU modes keep weights at 0; `ccm_fixed` uses
`last_action = -1` and `last_policy_probability = -1` as inapplicable sentinels (its `high_action_fraction`
metric is likewise 0 by construction); `mean_weight_norm` is 0 for non-TLU modes.

Metrics tracked (in order):
- `gini_coefficient`, `top_10pct_wealth_share`, `top_1pct_wealth_share`
- `mean_saving_propensity`, `high_action_fraction` (binary policies; 0 for `ccm_fixed`)
- `mean_abs_reward` (most recent step), `rejected_trade_fraction`, `total_wealth`, `mean_weight_norm`

To run it, copy `templates/CCMExchangeTLU/in.nml` to `in.nml` and `templates/CCMExchangeTLU/sim_type.nml` to
`sim_type.nml`, then build and run as usual.

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


# Economic Simulation Analysis Tool

The `analyze.py` script provides unified post-processing for economic simulation data with advanced distribution fitting capabilities.

## Features

- **Unified Analysis**: Combines histogram generation, statistics plotting, and metrics visualization in a single script
- **JSON Configuration**: Flexible configuration system using JSON files instead of CLI arguments
- **Advanced Distribution Fitting**: Supports fitting up to 2 distributions per variable using scipy.stats
- **Data Splitting**: Ability to split data at a threshold and fit different distributions to each part
- **Comprehensive Distribution Support**: Includes exponential distribution that mimics the old Boltzmann fitting
- **Parallel Processing**: Optional parallel processing for faster histogram generation

## Quick Start

1. **Create a default configuration file:**
   ```bash
   python analyze.py --create-config
   ```

2. **Edit the configuration file** (`analyze_config.json`) to match your analysis needs

3. **Run the analysis:**
   ```bash
   python analyze.py
   ```

## Configuration File Structure

The JSON configuration file has the following main sections:

### Analysis Section
Controls which analyses to perform:
```json
"analysis": {
    "create_histograms": true,
    "create_stats_plots": true, 
    "create_metrics_plots": true,
    "parallel_jobs": 1,
    "histogram_bins": 30,
    "output_dirs": {
        "histograms": "histograms",
        "stats": "stats"
    }
}
```

### Distribution Fitting Section
Configures distribution fitting for specific variables:
```json
"distribution_fitting": {
    "enabled": true,
    "fit_last_step_only": false,
    "variables": {
        "cash": {
            "distributions": ["expon"]
        },
        "wealth": {
            "split_value": 100.0,
            "distributions_low": ["expon"],
            "distributions_high": ["lognorm"]
        }
    },
    "fit_quality_threshold": 0.05
}
```

**Key Options:**
- `enabled`: Enable/disable distribution fitting
- `fit_last_step_only`: If true, only fit distributions to the highest timestep data (useful for performance)
- `fit_quality_threshold`: Minimum p-value for Kolmogorov-Smirnov test to accept a fit

### Plotting Section
Controls plot appearance and features:
```json
"plotting": {
    "figure_size": [10, 6],
    "dpi": 300,
    "show_fit_statistics": true,
    "include_qq_plots": false
}
```

## Distribution Fitting

### Single Distribution Fitting
For variables without data splitting, specify distributions to fit:
```json
"cash": {
    "distributions": ["expon", "lognorm", "gamma"]
}
```

### Split Data Fitting
For variables where you want to fit different distributions to different ranges:
```json
"wealth": {
    "split_value": 100.0,
    "distributions_low": ["expon"],
    "distributions_high": ["lognorm"]
}
```

### Available Distributions

The script supports all continuous distributions from scipy.stats, including:
- `expon` - Exponential distribution (mimics old Boltzmann fitting when using scale parameter)
- `lognorm` - Log-normal distribution
- `gamma` - Gamma distribution  
- `norm` - Normal distribution
- `pareto` - Pareto distribution
- `weibull_min` - Weibull distribution
- And many more...

The exponential distribution (`expon`) with the scale parameter provides the same functionality as the old Boltzmann distribution fitting.

## Command Line Options

```bash
python analyze.py [options]

Options:
  --config, -c FILE     JSON configuration file (default: analyze_config.json)
  --create-config       Create default configuration file and exit
  --histograms-only     Create histograms only
  --stats-only          Create statistics plots only  
  --metrics-only        Create metrics plots only
```

## Features Overview

The `analyze.py` script provides comprehensive post-processing capabilities with:

1. **JSON Configuration**: Flexible configuration system using JSON files
2. **Advanced Distribution Fitting**: Supports multiple distributions with quality assessment
3. **Data Splitting**: Can fit different distributions to different data ranges  
4. **Unified Interface**: Single script for histograms, statistics, and metrics

### Distribution Fitting Capabilities

**Exponential Distribution** (equivalent to previous Boltzmann fitting):
```json
{
    "distributions": ["expon"]
}
```

**Multiple Distributions**:
```json
{
    "distributions": ["expon", "lognorm", "gamma"]
}
```

The exponential distribution in scipy.stats provides the same mathematical form with better parameter estimation and goodness-of-fit testing.

## Output Files

- **Histograms**: `histograms/` directory with distribution plots and fits
- **Statistics**: `stats/` directory with time-series plots of population statistics  
- **Metrics**: `stats/` directory with simulation metrics plots

Each histogram shows:
- Data histogram
- Up to 2 fitted distributions (best fits by p-value)
- Goodness-of-fit statistics (p-values)
- Basic data statistics (mean, std, etc.)
- Split visualization (if data splitting is used)

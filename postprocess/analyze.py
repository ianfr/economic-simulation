#!/usr/bin/env python3
"""
Unified analysis script for economic simulation post-processing.

This script combines histogram generation and statistics plotting functionality
with advanced distribution fitting capabilities. It uses JSON configuration
for flexible analysis specification.
"""

import os
import json
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib
import seaborn as sns
import numpy as np
from pathlib import Path
import re
import argparse
from joblib import Parallel, delayed
import scipy.stats as stats
from scipy.optimize import curve_fit
import warnings
from typing import Dict, List, Optional, Tuple, Any

matplotlib.use('Agg')  # Use non-interactive backend for parallel processing

# Configure seaborn style and color palette
sns.set_style("whitegrid")
sns.set_palette("husl")

# Available continuous distributions from scipy.stats
AVAILABLE_DISTRIBUTIONS = [
    'exponpow', 'alpha', 'anglit', 'arcsine', 'argus', 'beta', 'betaprime',
    'bradford', 'burr', 'burr12', 'cauchy', 'chi', 'chi2', 'cosine',
    'crystalball', 'dgamma', 'dweibull', 'erlang', 'expon', 'exponnorm',
    'exponweib', 'f', 'fatiguelife', 'fisk', 'foldcauchy', 'foldnorm',
    'genlogistic', 'gennorm', 'genpareto', 'genexpon', 'genextreme',
    'gausshyper', 'gamma', 'gengamma', 'genhalflogistic', 'geninvgauss',
    'gilbrat', 'gompertz', 'gumbel_r', 'gumbel_l', 'halfcauchy',
    'halflogistic', 'halfnorm', 'halfgennorm', 'hypsecant', 'invgamma',
    'invgauss', 'invweibull', 'johnsonsb', 'johnsonsu', 'laplace',
    'levy', 'levy_l', 'logistic', 'loggamma', 'loglaplace', 'lognorm',
    'lomax', 'maxwell', 'mielke', 'moyal', 'nakagami', 'ncx2', 'ncf',
    'nct', 'norm', 'norminvgauss', 'pareto', 'pearson3', 'powerlaw',
    'powerlognorm', 'powernorm', 'rdist', 'rayleigh', 'rice', 'recipinvgauss',
    'semicircular', 'skewnorm', 't', 'trapz', 'triang', 'truncexpon',
    'truncnorm', 'tukeylambda', 'uniform', 'vonmises', 'vonmises_line',
    'wald', 'weibull_min', 'weibull_max', 'wrapcauchy'
]

def load_config(config_path: str) -> Dict[str, Any]:
    """Load configuration from JSON file."""
    try:
        with open(config_path, 'r') as f:
            config = json.load(f)
        return config
    except FileNotFoundError:
        print(f"Configuration file {config_path} not found.")
        return {}
    except json.JSONDecodeError as e:
        print(f"Error parsing JSON configuration: {e}")
        return {}

def create_default_config(config_path: str):
    """Create a default configuration file."""
    default_config = {
        "analysis": {
            "create_histograms": True,
            "create_stats_plots": True,
            "create_metrics_plots": True,
            "create_advanced_plots": False,
            "parallel_jobs": 1,
            "histogram_bins": 30,
            "output_dirs": {
                "histograms": "histograms",
                "stats": "stats"
            }
        },
        "distribution_fitting": {
            "enabled": False,
            "fit_last_step_only": True,
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
        },
        "plotting": {
            "figure_size": [10, 6],
            "dpi": 300,
            "show_fit_statistics": True,
            "include_qq_plots": False,
            "seaborn_style": "whitegrid",
            "seaborn_palette": "husl",
            "seaborn_context": "notebook"
        }
    }
    
    with open(config_path, 'w') as f:
        json.dump(default_config, f, indent=4)
    
    print(f"Created default configuration file: {config_path}")

def extract_time_from_filename(filename: str) -> Optional[int]:
    """Extract the time step number from filename like '5.step.csv'."""
    match = re.search(r'^(\d+)\.step\.csv$', filename)
    if match:
        return int(match.group(1))
    return None

def fit_distribution(data: np.ndarray, dist_name: str) -> Tuple[Any, float, Dict[str, float]]:
    """
    Fit a distribution to data and return the fitted distribution, p-value, and parameters.
    
    Returns:
        (fitted_distribution, p_value, parameters_dict)
    """
    try:
        # Get the distribution from scipy.stats
        dist = getattr(stats, dist_name)
        
        # Filter out non-finite values
        clean_data = data[np.isfinite(data)]
        
        if len(clean_data) < 10:  # Need enough data points
            return None, 0.0, {}
        
        # Fit the distribution
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            params = dist.fit(clean_data)
        
        # Perform Kolmogorov-Smirnov test
        ks_stat, p_value = stats.kstest(clean_data, lambda x: dist.cdf(x, *params))
        
        # Create parameter dictionary
        param_names = dist.shapes.split(',') if dist.shapes else []
        param_names = [name.strip() for name in param_names] + ['loc', 'scale']
        param_dict = dict(zip(param_names, params))
        
        return dist(*params), p_value, param_dict
        
    except Exception as e:
        print(f"Error fitting {dist_name}: {e}")
        return None, 0.0, {}

def fit_distributions_to_data(data: np.ndarray, distributions: List[str], 
                            fit_threshold: float = 0.05) -> List[Tuple[str, Any, float, Dict[str, float]]]:
    """
    Fit multiple distributions to data and return results sorted by p-value.
    
    Returns:
        List of (dist_name, fitted_distribution, p_value, parameters)
    """
    results = []
    
    for dist_name in distributions:
        if dist_name not in AVAILABLE_DISTRIBUTIONS:
            print(f"Warning: Distribution '{dist_name}' not available. Skipping.")
            continue
            
        fitted_dist, p_value, params = fit_distribution(data, dist_name)
        print(f"  {dist_name}: p-value = {p_value:.6f} (threshold = {fit_threshold})")
        
        if fitted_dist is not None and p_value >= fit_threshold:
            results.append((dist_name, fitted_dist, p_value, params))
    
    # Sort by p-value (descending - better fits first)
    results.sort(key=lambda x: x[2], reverse=True)
    
    return results

def split_data_and_fit(data: np.ndarray, split_value: float, 
                      distributions_low: List[str], distributions_high: List[str],
                      fit_threshold: float = 0.05) -> Tuple[List, List]:
    """
    Split data at a threshold and fit different distributions to each part.
    
    Returns:
        (low_fits, high_fits) - each is a list of fit results
    """
    low_data = data[data <= split_value]
    high_data = data[data > split_value]
    
    low_fits = []
    high_fits = []
    
    if len(low_data) >= 10:  # Need enough data points
        low_fits = fit_distributions_to_data(low_data, distributions_low, fit_threshold)
    
    if len(high_data) >= 10:
        high_fits = fit_distributions_to_data(high_data, distributions_high, fit_threshold)
    
    return low_fits, high_fits

def create_histogram_with_fits(data: pd.Series, column_name: str, file_name: str,
                             config: Dict[str, Any], output_dir: Path) -> Dict[str, Any]:
    """Create histogram with distribution fits for a single column using seaborn."""
    
    try:
        fig_config = config.get('plotting', {})
        fit_config = config.get('distribution_fitting', {})
        
        # Set up the plot
        fig_size = fig_config.get('figure_size', [10, 6])
        plt.figure(figsize=fig_size)
        
        # Remove non-finite values
        clean_data = data.dropna()
        clean_data = clean_data[np.isfinite(clean_data)]
        
        if len(clean_data) == 0:
            return {'success': False, 'error': 'No valid data'}
        
        # Create histogram using seaborn
        bins = config.get('analysis', {}).get('histogram_bins', 30)
        
        # Check if distribution fitting is enabled to decide on KDE
        fit_enabled = (fit_config.get('enabled', False) and 
                      column_name in fit_config.get('variables', {}))
        
        # Use seaborn for the histogram with better styling
        # Don't show KDE when fitting distributions to avoid visual clutter
        sns.histplot(clean_data, bins=bins, stat='density', alpha=0.7, 
                    color='skyblue', edgecolor='navy', label='Data', 
                    kde=not fit_enabled)
        
        # Fit distributions if enabled
        fit_results = []
        if fit_config.get('enabled', False) and column_name in fit_config.get('variables', {}):
            var_config = fit_config.get('variables', {}).get(column_name, {})
            split_value = var_config.get('split_value')
            fit_threshold = fit_config.get('fit_quality_threshold', 0.05)
            
            if split_value is not None:
                # Split data and fit separately - require explicit distributions for each part
                distributions_low = var_config.get('distributions_low', [])
                distributions_high = var_config.get('distributions_high', [])
                
                if distributions_low or distributions_high:
                    low_fits, high_fits = split_data_and_fit(
                        clean_data.values, split_value, 
                        distributions_low, distributions_high, fit_threshold
                    )
                    
                    print(f"Split fitting for {column_name}: {len(low_fits)} low fits, {len(high_fits)} high fits")
                    if low_fits:
                        print(f"Low data range: {clean_data[clean_data <= split_value].min():.3f} to {split_value:.3f}")
                    if high_fits:
                        print(f"High data range: {split_value:.3f} to {clean_data[clean_data > split_value].max():.3f}")
                    
                    # Calculate data proportions for renormalization
                    low_data = clean_data.values[clean_data.values <= split_value]
                    high_data = clean_data.values[clean_data.values > split_value]
                    total_data = len(clean_data.values)
                    low_proportion = len(low_data) / total_data
                    high_proportion = len(high_data) / total_data
                    
                    # Plot fits for low data with seaborn color palette
                    x_low = np.linspace(clean_data.min(), split_value, 100)
                    colors_low = ['#FF4444', '#CC0000']  # Bright red colors for low fits
                    for i, (dist_name, fitted_dist, p_value, params) in enumerate(low_fits[:2]):
                        y_low = fitted_dist.pdf(x_low)
                        # Scale by the proportion of data in this range
                        # The histogram with density=True normalizes the entire dataset to area=1
                        # So each split should be scaled by its proportion
                        y_low_scaled = y_low * low_proportion
                        plt.plot(x_low, y_low_scaled, color=colors_low[i % len(colors_low)], 
                               linewidth=3, linestyle='--',
                               label=f'{dist_name} (low, p={p_value:.3f})')
                    
                    # Plot fits for high data with more contrasting colors
                    x_high = np.linspace(split_value, clean_data.max(), 100)
                    colors_high = ['#FF8800', '#CC4400']  # Bright orange colors for high fits
                    for i, (dist_name, fitted_dist, p_value, params) in enumerate(high_fits[:2]):
                        y_high = fitted_dist.pdf(x_high)
                        # Scale by the proportion of data in this range
                        y_high_scaled = y_high * high_proportion
                        plt.plot(x_high, y_high_scaled, color=colors_high[i % len(colors_high)], 
                               linewidth=3, linestyle='-.',
                               label=f'{dist_name} (high, p={p_value:.3f})')
                    
                    # Add vertical line at split
                    plt.axvline(split_value, color='black', linestyle=':', 
                              label=f'Split at {split_value}')
                    
                    fit_results = {'low_fits': low_fits, 'high_fits': high_fits, 'split_value': split_value}
                
            else:
                # Fit distributions to entire dataset - use the general distributions field
                distributions = var_config.get('distributions', [])
                
                if distributions:
                    fits = fit_distributions_to_data(clean_data.values, distributions, fit_threshold)
                    
                    # Plot up to 2 best fits using highly contrasting colors
                    x_range = np.linspace(clean_data.min(), clean_data.max(), 200)
                    colors = ['#FF4444', '#00AA00', '#FF8800', '#8800FF']  # High contrast colors
                    
                    for i, (dist_name, fitted_dist, p_value, params) in enumerate(fits[:2]):
                        y_fit = fitted_dist.pdf(x_range)
                        plt.plot(x_range, y_fit, color=colors[i % len(colors)], 
                               linewidth=3, label=f'{dist_name} (p={p_value:.3f})')
                    
                    fit_results = {'fits': fits}
        
        # Add labels and formatting with seaborn styling
        plt.xlabel(column_name.replace('_', ' ').title(), fontsize=12)
        plt.ylabel('Probability Density', fontsize=12)
        plt.title(f'Distribution of {column_name.replace("_", " ").title()}\n(from {file_name})', 
                 fontsize=14, fontweight='bold')
        
        # Seaborn automatically provides better grid styling
        
        # Add statistics text if enabled
        if fig_config.get('show_fit_statistics', True):
            stats_text = f'Count: {len(clean_data)}\n'
            stats_text += f'Mean: {clean_data.mean():.3f}\n'
            stats_text += f'Std: {clean_data.std():.3f}\n'
            stats_text += f'Min: {clean_data.min():.3f}\n'
            stats_text += f'Max: {clean_data.max():.3f}'
            
            plt.text(0.98, 0.98, stats_text, transform=plt.gca().transAxes,
                    verticalalignment='top', horizontalalignment='right',
                    bbox=dict(boxstyle='round', facecolor='white', alpha=0.9, edgecolor='gray'))
        
        # Add legend if there are fits
        if fit_results:
            plt.legend(loc='best', frameon=True, fancybox=True, shadow=True)
        
        # Save the plot
        dpi = fig_config.get('dpi', 300)
        output_file = output_dir / f'{file_name}_{column_name}.png'
        plt.savefig(output_file, dpi=dpi, bbox_inches='tight')
        plt.close()
        
        return {
            'success': True, 
            'output_file': output_file, 
            'fit_results': fit_results,
            'stats': {
                'count': len(clean_data),
                'mean': clean_data.mean(),
                'std': clean_data.std(),
                'min': clean_data.min(),
                'max': clean_data.max()
            }
        }
        
    except Exception as e:
        plt.close()  # Make sure to close the figure on error
        return {'success': False, 'error': str(e)}

def process_histogram_file(csv_file: Path, config: Dict[str, Any], 
                          output_dir: Path) -> List[Dict[str, Any]]:
    """Process a single CSV file for histogram generation."""
    
    results = []
    
    try:
        # Read the CSV file
        df = pd.read_csv(csv_file)
        
        # Skip the first column (assumed to be ID)
        data_columns = df.columns[1:]
        
        if len(data_columns) == 0:
            return [{'success': False, 'error': f'No data columns in {csv_file.name}'}]
        
        # Process each column
        file_base_name = csv_file.stem
        
        for col in data_columns:
            result = create_histogram_with_fits(
                df[col], col, file_base_name, config, output_dir
            )
            result['file'] = csv_file.name
            result['column'] = col
            results.append(result)
    
    except Exception as e:
        results.append({
            'success': False, 
            'error': str(e), 
            'file': csv_file.name,
            'column': 'unknown'
        })
    
    return results

def create_histograms(config: Dict[str, Any], out_dir: Path):
    """Create histograms for all CSV files in the output directory."""
    
    analysis_config = config.get('analysis', {})
    fit_config = config.get('distribution_fitting', {})
    
    # Set up output directory
    hist_dir_name = analysis_config.get('output_dirs', {}).get('histograms', 'histograms')
    hist_dir = Path(hist_dir_name)
    hist_dir.mkdir(exist_ok=True)
    
    # Find CSV files (excluding metrics)
    csv_files = [f for f in out_dir.glob('*.csv') if f.name != 'simulation_metrics.csv']
    
    if not csv_files:
        print("No CSV files found for histogram generation.")
        return
    
    # Determine which files should have distribution fitting
    fit_last_step_only = fit_config.get('fit_last_step_only', False)
    files_to_fit = set()
    
    if fit_last_step_only and fit_config.get('enabled', False):
        # Find the file with the highest timestep
        time_files = []
        for csv_file in csv_files:
            time_step = extract_time_from_filename(csv_file.name)
            if time_step is not None:
                time_files.append((time_step, csv_file))
        
        if time_files:
            # Sort by timestep and get the highest one
            time_files.sort(key=lambda x: x[0])
            highest_timestep_file = time_files[-1][1]
            files_to_fit.add(highest_timestep_file)
            print(f"Distribution fitting will only be applied to: {highest_timestep_file.name}")
        else:
            print("No timestep files found for distribution fitting.")
    elif fit_config.get('enabled', False):
        # Fit all files
        files_to_fit = set(csv_files)
    
    print(f"Creating histograms for {len(csv_files)} files...")
    
    # Process files in parallel
    n_jobs = analysis_config.get('parallel_jobs', 1)
    
    if n_jobs == 1:
        # Sequential processing
        all_results = []
        for csv_file in csv_files:
            # Create modified config for this file
            file_config = config.copy()
            if csv_file not in files_to_fit:
                # Disable fitting for this file
                file_config = dict(config)
                file_config['distribution_fitting'] = dict(fit_config)
                file_config['distribution_fitting']['enabled'] = False
            
            results = process_histogram_file(csv_file, file_config, hist_dir)
            all_results.extend(results)
    else:
        # Parallel processing
        results_lists = Parallel(n_jobs=n_jobs, verbose=1)(
            delayed(process_histogram_file)(
                csv_file, 
                {**config, 'distribution_fitting': {**fit_config, 'enabled': csv_file in files_to_fit}} if csv_file not in files_to_fit else config,
                hist_dir
            )
            for csv_file in csv_files
        )
        all_results = [result for results in results_lists for result in results]
    
    # Report results
    successful = sum(1 for r in all_results if r['success'])
    failed = len(all_results) - successful
    
    print(f"Histogram generation complete: {successful} successful, {failed} failed")
    
    for result in all_results:
        if result['success']:
            print(f"✓ Created: {result['output_file']}")
            if 'fit_results' in result and result['fit_results']:
                fits = result['fit_results']
                if 'fits' in fits:
                    for dist_name, _, p_value, _ in fits['fits'][:2]:
                        print(f"  Fitted {dist_name} (p={p_value:.3f})")
                elif 'low_fits' in fits and 'high_fits' in fits:
                    print(f"  Split at {fits['split_value']}")
                    for dist_name, _, p_value, _ in fits['low_fits'][:1]:
                        print(f"  Low: {dist_name} (p={p_value:.3f})")
                    for dist_name, _, p_value, _ in fits['high_fits'][:1]:
                        print(f"  High: {dist_name} (p={p_value:.3f})")
        else:
            print(f"✗ Failed: {result['file']}, column {result['column']}: {result['error']}")

def create_stats_plots(config: Dict[str, Any], out_dir: Path):
    """Create statistics plots over time."""
    
    analysis_config = config.get('analysis', {})
    
    # Set up output directory
    stats_dir_name = analysis_config.get('output_dirs', {}).get('stats', 'stats')
    stats_dir = Path(stats_dir_name)
    stats_dir.mkdir(exist_ok=True)
    
    # Find time-series CSV files
    csv_files = [f for f in out_dir.glob('*.csv') if f.name != 'simulation_metrics.csv']
    
    if not csv_files:
        print("No CSV files found for statistics plotting.")
        return
    
    # Group files by time step
    time_files = []
    for csv_file in csv_files:
        time_step = extract_time_from_filename(csv_file.name)
        if time_step is not None:
            time_files.append((time_step, csv_file))
    
    time_files.sort(key=lambda x: x[0])
    
    if not time_files:
        print("No files with valid time step format found.")
        return
    
    print(f"Creating statistics plots for {len(time_files)} time steps...")
    
    # Collect statistics
    all_stats = {}
    time_points = []
    
    for time_step, csv_file in time_files:
        try:
            df = pd.read_csv(csv_file)
            data_columns = df.columns[1:]  # Skip ID column
            
            time_points.append(time_step)
            
            for col in data_columns:
                if col not in all_stats:
                    all_stats[col] = {
                        'mean': [], 'median': [], 'std': [], 'min': [], 'max': []
                    }
                
                clean_data = df[col].dropna()
                all_stats[col]['mean'].append(clean_data.mean())
                all_stats[col]['median'].append(clean_data.median())
                all_stats[col]['std'].append(clean_data.std())
                all_stats[col]['min'].append(clean_data.min())
                all_stats[col]['max'].append(clean_data.max())
                
        except Exception as e:
            print(f"Error processing {csv_file.name}: {e}")
    
    # Create plots with seaborn styling
    fig_config = config.get('plotting', {})
    fig_size = fig_config.get('figure_size', [10, 6])
    dpi = fig_config.get('dpi', 300)
    
    for col_name, stats_data in all_stats.items():
        print(f"Creating statistics plots for {col_name}...")
        
        # Combined plot using seaborn styling
        fig, axes = plt.subplots(2, 2, figsize=(14, 10))
        fig.suptitle(f'{col_name.replace("_", " ").title()} Statistics Over Time', 
                    fontsize=16, fontweight='bold')
        
        # Set seaborn style for all subplots
        with sns.axes_style("whitegrid"):
            # Mean
            sns.lineplot(x=time_points, y=stats_data['mean'], marker='o', 
                        linewidth=2.5, markersize=6, ax=axes[0, 0])
            axes[0, 0].set_title('Mean', fontsize=12, fontweight='bold')
            axes[0, 0].set_xlabel('Time Step', fontsize=11)
            axes[0, 0].set_ylabel(col_name, fontsize=11)
            
            # Median
            sns.lineplot(x=time_points, y=stats_data['median'], marker='o', 
                        linewidth=2.5, markersize=6, ax=axes[0, 1], color='green')
            axes[0, 1].set_title('Median', fontsize=12, fontweight='bold')
            axes[0, 1].set_xlabel('Time Step', fontsize=11)
            axes[0, 1].set_ylabel(col_name, fontsize=11)
            
            # Standard deviation
            sns.lineplot(x=time_points, y=stats_data['std'], marker='o', 
                        linewidth=2.5, markersize=6, ax=axes[1, 0], color='red')
            axes[1, 0].set_title('Standard Deviation', fontsize=12, fontweight='bold')
            axes[1, 0].set_xlabel('Time Step', fontsize=11)
            axes[1, 0].set_ylabel(col_name, fontsize=11)
            
            # Min/Max
            sns.lineplot(x=time_points, y=stats_data['min'], marker='o', 
                        linewidth=2.5, markersize=6, ax=axes[1, 1], 
                        color='orange', label='Min')
            sns.lineplot(x=time_points, y=stats_data['max'], marker='o', 
                        linewidth=2.5, markersize=6, ax=axes[1, 1], 
                        color='purple', label='Max')
            axes[1, 1].set_title('Min/Max', fontsize=12, fontweight='bold')
            axes[1, 1].set_xlabel('Time Step', fontsize=11)
            axes[1, 1].set_ylabel(col_name, fontsize=11)
            axes[1, 1].legend(frameon=True, fancybox=True, shadow=True)
        
        plt.tight_layout()
        
        # Save combined plot
        output_file = stats_dir / f'{col_name}_combined.png'
        plt.savefig(output_file, dpi=dpi, bbox_inches='tight')
        plt.close()
        
        print(f"✓ Created: {output_file}")

def create_metrics_plots(config: Dict[str, Any], out_dir: Path):
    """Create plots for simulation metrics."""
    
    analysis_config = config.get('analysis', {})
    
    # Set up output directory
    stats_dir_name = analysis_config.get('output_dirs', {}).get('stats', 'stats')
    stats_dir = Path(stats_dir_name)
    stats_dir.mkdir(exist_ok=True)
    
    metrics_file = out_dir / 'simulation_metrics.csv'
    
    if not metrics_file.exists():
        print("No simulation_metrics.csv file found.")
        return
    
    print("Creating metrics plots...")
    
    try:
        df = pd.read_csv(metrics_file)
        
        if 'timestep' not in df.columns:
            print("Error: 'timestep' column not found in metrics file.")
            return
        
        timesteps = df['timestep']
        metric_columns = [col for col in df.columns if col != 'timestep']
        
        if not metric_columns:
            print("No metric columns found.")
            return
        
        # Plot configuration
        fig_config = config.get('plotting', {})
        fig_size = fig_config.get('figure_size', [10, 6])
        dpi = fig_config.get('dpi', 300)
        
        # Individual plots for each metric using seaborn
        for metric_name in metric_columns:
            plt.figure(figsize=fig_size)
            
            # Use seaborn for better styling
            sns.lineplot(x=timesteps, y=df[metric_name], marker='o', 
                        linewidth=2.5, markersize=4)
            plt.xlabel('Time Step', fontsize=12)
            plt.ylabel(metric_name.replace('_', ' ').title(), fontsize=12)
            plt.title(f'{metric_name.replace("_", " ").title()} Over Time', 
                     fontsize=14, fontweight='bold')
            
            output_file = stats_dir / f'metrics_{metric_name}.png'
            plt.savefig(output_file, dpi=dpi, bbox_inches='tight')
            plt.close()
            
            print(f"✓ Created: {output_file}")
        
        # Combined plot if multiple metrics using seaborn style
        if len(metric_columns) > 1:
            n_metrics = len(metric_columns)
            n_cols = min(2, n_metrics)
            n_rows = (n_metrics + n_cols - 1) // n_cols
            
            fig, axes = plt.subplots(n_rows, n_cols, figsize=(8 * n_cols, 5 * n_rows))
            fig.suptitle('Simulation Metrics Over Time', fontsize=16, fontweight='bold')
            
            if n_metrics == 1:
                axes = [axes]
            elif n_rows == 1:
                axes = [axes]
            else:
                axes = axes.flatten()
            
            # Get a diverse color palette for different metrics
            colors = sns.color_palette("husl", n_colors=n_metrics)
            
            for i, metric_name in enumerate(metric_columns):
                ax = axes[i] if n_metrics > 1 else axes[0]
                
                # Use seaborn lineplot for consistent styling
                sns.lineplot(x=timesteps, y=df[metric_name], marker='o', 
                           linewidth=2.5, markersize=4, ax=ax, color=colors[i])
                ax.set_xlabel('Time Step', fontsize=11)
                ax.set_ylabel(metric_name.replace('_', ' ').title(), fontsize=11)
                ax.set_title(f'{metric_name.replace("_", " ").title()}', 
                           fontsize=12, fontweight='bold')
            
            # Hide unused subplots
            for i in range(len(metric_columns), len(axes)):
                axes[i].set_visible(False)
            
            plt.tight_layout()
            
            output_file = stats_dir / 'metrics_combined.png'
            plt.savefig(output_file, dpi=dpi, bbox_inches='tight')
            plt.close()
            
            print(f"✓ Created: {output_file}")
            
    except Exception as e:
        print(f"Error processing metrics file: {e}")

def create_distribution_evolution_plots(config: Dict[str, Any], out_dir: Path):
    """Create plots showing how distributions evolve over time using seaborn."""
    
    analysis_config = config.get('analysis', {})
    
    # Set up output directory
    stats_dir_name = analysis_config.get('output_dirs', {}).get('stats', 'stats')
    stats_dir = Path(stats_dir_name)
    stats_dir.mkdir(exist_ok=True)
    
    # Find time-series CSV files
    csv_files = [f for f in out_dir.glob('*.csv') if f.name != 'simulation_metrics.csv']
    
    if not csv_files:
        print("No CSV files found for distribution evolution plots.")
        return
    
    # Group files by time step
    time_files = []
    for csv_file in csv_files:
        time_step = extract_time_from_filename(csv_file.name)
        if time_step is not None:
            time_files.append((time_step, csv_file))
    
    time_files.sort(key=lambda x: x[0])
    
    if len(time_files) < 2:
        print("Need at least 2 timesteps for distribution evolution plots.")
        return
    
    print("Creating distribution evolution plots...")
    
    # Plot configuration
    fig_config = config.get('plotting', {})
    dpi = fig_config.get('dpi', 300)
    
    # Sample a few timesteps for comparison (to avoid overcrowding)
    n_timesteps = len(time_files)
    if n_timesteps > 6:
        indices = np.linspace(0, n_timesteps-1, 6, dtype=int)
        selected_files = [time_files[i] for i in indices]
    else:
        selected_files = time_files
    
    # Get column names from the first file
    try:
        first_df = pd.read_csv(selected_files[0][1])
        data_columns = first_df.columns[1:]  # Skip ID column
        
        for col_name in data_columns:
            print(f"Creating distribution evolution plot for {col_name}...")
            
            plt.figure(figsize=(12, 8))
            
            # Collect data from different timesteps
            data_for_plot = []
            for time_step, csv_file in selected_files:
                try:
                    df = pd.read_csv(csv_file)
                    if col_name in df.columns:
                        clean_data = df[col_name].dropna()
                        clean_data = clean_data[np.isfinite(clean_data)]
                        
                        # Add timestep information for grouping
                        temp_df = pd.DataFrame({
                            col_name: clean_data,
                            'timestep': f'T={time_step}'
                        })
                        data_for_plot.append(temp_df)
                        
                except Exception as e:
                    print(f"Error processing {csv_file.name}: {e}")
            
            if data_for_plot:
                # Combine all data
                combined_df = pd.concat(data_for_plot, ignore_index=True)
                
                # Create overlapping density plots using seaborn
                for i, (time_step, _) in enumerate(selected_files):
                    timestep_data = combined_df[combined_df['timestep'] == f'T={time_step}'][col_name]
                    if len(timestep_data) > 0:
                        sns.histplot(timestep_data, kde=True, stat='density', alpha=0.6, 
                                   label=f'T={time_step}', bins=30)
                
                plt.xlabel(col_name.replace('_', ' ').title(), fontsize=12)
                plt.ylabel('Density', fontsize=12)
                plt.title(f'Distribution Evolution: {col_name.replace("_", " ").title()}', 
                         fontsize=14, fontweight='bold')
                plt.legend(title='Timestep', frameon=True, fancybox=True, shadow=True)
                
                output_file = stats_dir / f'distribution_evolution_{col_name}.png'
                plt.savefig(output_file, dpi=dpi, bbox_inches='tight')
                plt.close()
                print(f"✓ Created: {output_file}")
            else:
                plt.close()
                
    except Exception as e:
        print(f"Error creating distribution evolution plots: {e}")

def create_advanced_seaborn_plots(config: Dict[str, Any], out_dir: Path):
    """Create advanced seaborn plots including correlation matrices and pairplots."""
    
    analysis_config = config.get('analysis', {})
    
    # Set up output directory
    stats_dir_name = analysis_config.get('output_dirs', {}).get('stats', 'stats')
    stats_dir = Path(stats_dir_name)
    stats_dir.mkdir(exist_ok=True)
    
    # Find CSV files
    csv_files = [f for f in out_dir.glob('*.csv') if f.name != 'simulation_metrics.csv']
    
    if not csv_files:
        print("No CSV files found for advanced seaborn plots.")
        return
    
    print("Creating advanced seaborn plots...")
    
    # Plot configuration
    fig_config = config.get('plotting', {})
    dpi = fig_config.get('dpi', 300)
    
    # Get the latest timestep file for correlation analysis
    time_files = []
    for csv_file in csv_files:
        time_step = extract_time_from_filename(csv_file.name)
        if time_step is not None:
            time_files.append((time_step, csv_file))
    
    if time_files:
        time_files.sort(key=lambda x: x[0])
        latest_file = time_files[-1][1]
        
        try:
            df = pd.read_csv(latest_file)
            data_columns = df.columns[1:]  # Skip ID column
            
            if len(data_columns) > 1:
                # Create correlation matrix heatmap
                plt.figure(figsize=(10, 8))
                correlation_matrix = df[data_columns].corr()
                
                # Create a more attractive heatmap
                mask = np.triu(np.ones_like(correlation_matrix, dtype=bool))
                sns.heatmap(correlation_matrix, mask=mask, annot=True, cmap='RdBu_r', 
                           center=0, square=True, linewidths=0.5, cbar_kws={"shrink": .8})
                plt.title(f'Variable Correlations (Timestep {time_files[-1][0]})', 
                         fontsize=14, fontweight='bold')
                plt.tight_layout()
                
                output_file = stats_dir / f'correlation_heatmap_{time_files[-1][0]}.png'
                plt.savefig(output_file, dpi=dpi, bbox_inches='tight')
                plt.close()
                print(f"✓ Created: {output_file}")
                
                # Create pairplot if we have 2-4 variables (to avoid overcrowding)
                if 2 <= len(data_columns) <= 4:
                    # Sample data if it's too large for pairplot
                    sample_size = min(1000, len(df))
                    df_sample = df[data_columns].sample(n=sample_size) if len(df) > sample_size else df[data_columns]
                    
                    # Create pairplot
                    pairplot = sns.pairplot(df_sample, diag_kind='hist', plot_kws={'alpha': 0.7})
                    pairplot.fig.suptitle(f'Variable Relationships (Timestep {time_files[-1][0]})', 
                                         fontsize=14, fontweight='bold', y=1.02)
                    
                    output_file = stats_dir / f'pairplot_{time_files[-1][0]}.png'
                    pairplot.savefig(output_file, dpi=dpi, bbox_inches='tight')
                    plt.close()
                    print(f"✓ Created: {output_file}")
            
        except Exception as e:
            print(f"Error creating advanced plots for {latest_file.name}: {e}")

def configure_seaborn_style(config: Dict[str, Any]):
    """Configure seaborn style based on configuration."""
    plot_config = config.get('plotting', {})
    
    # Set style
    style = plot_config.get('seaborn_style', 'whitegrid')
    if style in ['white', 'dark', 'whitegrid', 'darkgrid', 'ticks']:
        sns.set_style(style)
    
    # Set palette
    palette = plot_config.get('seaborn_palette', 'husl')
    try:
        sns.set_palette(palette)
    except ValueError:
        print(f"Warning: Invalid seaborn palette '{palette}', using default 'husl'")
        sns.set_palette('husl')
    
    # Set context
    context = plot_config.get('seaborn_context', 'notebook')
    if context in ['paper', 'notebook', 'talk', 'poster']:
        sns.set_context(context)

def main():
    """Main function."""
    parser = argparse.ArgumentParser(description='Unified analysis script for economic simulations')
    parser.add_argument('--config', '-c', type=str, default='analyze_config.json',
                       help='JSON configuration file (default: analyze_config.json)')
    parser.add_argument('--create-config', action='store_true',
                       help='Create a default configuration file and exit')
    parser.add_argument('--histograms-only', action='store_true',
                       help='Create histograms only')
    parser.add_argument('--stats-only', action='store_true',
                       help='Create statistics plots only')
    parser.add_argument('--metrics-only', action='store_true',
                       help='Create metrics plots only')
    parser.add_argument('--advanced-only', action='store_true',
                       help='Create advanced seaborn plots only')
    
    args = parser.parse_args()
    
    # Create default config if requested
    if args.create_config:
        create_default_config(args.config)
        return
    
    # Load configuration
    config = load_config(args.config)
    if not config:
        print(f"No valid configuration found. Create one with: python analyze.py --create-config")
        return
    
    # Configure seaborn styling
    configure_seaborn_style(config)
    
    # Set up paths
    out_dir = Path('../out')
    if not out_dir.exists():
        print(f"Output directory {out_dir} does not exist.")
        return
    
    # Determine what to run
    analysis_config = config.get('analysis', {})
    
    run_histograms = args.histograms_only or (
        not any([args.stats_only, args.metrics_only, args.advanced_only]) and 
        analysis_config.get('create_histograms', True)
    )
    
    run_stats = args.stats_only or (
        not any([args.histograms_only, args.metrics_only, args.advanced_only]) and 
        analysis_config.get('create_stats_plots', True)
    )
    
    run_metrics = args.metrics_only or (
        not any([args.histograms_only, args.stats_only, args.advanced_only]) and 
        analysis_config.get('create_metrics_plots', True)
    )
    
    run_advanced = args.advanced_only or (
        not any([args.histograms_only, args.stats_only, args.metrics_only]) and 
        analysis_config.get('create_advanced_plots', False)
    )
    
    # Run analysis
    if run_histograms:
        print("=== Creating Histograms ===")
        create_histograms(config, out_dir)
        print()
    
    if run_stats:
        print("=== Creating Statistics Plots ===")
        create_stats_plots(config, out_dir)
        print()
    
    if run_metrics:
        print("=== Creating Metrics Plots ===")
        create_metrics_plots(config, out_dir)
        print()
    
    if run_advanced:
        print("=== Creating Advanced Seaborn Plots ===")
        create_advanced_seaborn_plots(config, out_dir)
        print()
        
        print("=== Creating Distribution Evolution Plots ===")
        create_distribution_evolution_plots(config, out_dir)
        print()
    
    print("Analysis complete!")

if __name__ == "__main__":
    main()

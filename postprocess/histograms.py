# For each CSV file in ../out, create a histogram
# for each column in the CSV file except the first column
# The first column is assumed to be the agent ID or similar identifier
# The histograms will be in postprocess/histograms/
# The histograms will be named after the CSV file, with .png extension

import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path
from scipy import stats
from scipy.optimize import curve_fit
import argparse
from joblib import Parallel, delayed
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend for parallel processing

def fit_boltzmann_distribution(data, bins=30):
    """
    Fit a Boltzmann distribution to the data using scipy.optimize.curve_fit.
    The form is: f(x) = constant * exp(-x/mean(x))
    Returns the fitted constant and the data mean.
    """
    # Remove zero and negative values for exponential fitting
    positive_data = data[data > 0]
    
    if len(positive_data) < 2:
        return None, None, None
    
    # Calculate the mean of the data
    data_mean = np.mean(positive_data)
    
    # Define the Boltzmann distribution function
    def boltzmann_func(x, constant):
        return constant * np.exp(-x / data_mean)
    
    # Create histogram to get bin centers and counts for fitting
    counts, bin_edges = np.histogram(positive_data, bins=bins, density=True)
    bin_centers = (bin_edges[:-1] + bin_edges[1:]) / 2
    
    # Filter out zero counts to avoid fitting issues
    valid_indices = counts > 0
    bin_centers_valid = bin_centers[valid_indices]
    counts_valid = counts[valid_indices]
    
    if len(bin_centers_valid) < 2:
        return None, None, None
    
    try:
        # Fit the function using curve_fit
        # Initial guess for the constant
        initial_guess = [np.max(counts_valid)]
        
        popt, pcov = curve_fit(boltzmann_func, bin_centers_valid, counts_valid, 
                              p0=initial_guess, maxfev=5000)
        
        constant = popt[0]
        
        # Calculate R-squared for goodness of fit
        y_pred = boltzmann_func(bin_centers_valid, constant)
        ss_res = np.sum((counts_valid - y_pred) ** 2)
        ss_tot = np.sum((counts_valid - np.mean(counts_valid)) ** 2)
        r_squared = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0
        
        return constant, data_mean, positive_data, r_squared
        
    except Exception as e:
        print(f"Error fitting Boltzmann distribution: {e}")
        return None, None, None, None

def process_histogram(csv_file, col, hist_dir, fit_boltzmann=False, bins=30):
    """
    Process a single histogram for a given CSV file and column.
    This function is designed to be called in parallel.
    """
    try:
        # Read the CSV file
        df = pd.read_csv(csv_file)
        
        # Create histogram for the specified column
        plt.figure(figsize=(10, 6))
        
        # Create histogram with density=True for probability density
        n, bins, patches = plt.hist(df[col], bins=bins, 
                                  edgecolor='blue', density=True, label='Data', histtype='step')
        
        # Add labels and title
        plt.xlabel(col)
        plt.ylabel('Probability Density')
        plt.title(f'Distribution of {col}\n(from {csv_file.name})')
        plt.grid(True, alpha=0.3)
        
        # Add statistics text
        stats_text = f'Count: {len(df[col])}\nMean: {df[col].mean():.2f}\nStd: {df[col].std():.2f}'
        
        # Fit Boltzmann-Gibbs distribution if requested
        if fit_boltzmann:
            fit_result = fit_boltzmann_distribution(df[col], bins)
            
            if fit_result[0] is not None:  # Check if fitting was successful
                constant, data_mean, positive_data, r_squared = fit_result
                
                # Create x values for the fitted curve
                x_max = df[col].max()
                x_fit = np.linspace(0, x_max, 100)
                
                # Calculate the Boltzmann PDF
                y_fit = constant * np.exp(-x_fit / data_mean)
                
                # Plot the fitted distribution
                plt.plot(x_fit, y_fit, 'r-', linewidth=2, 
                        label=f'Boltzmann fit: {constant:.3f} × exp(-x/{data_mean:.2f})\nR² = {r_squared:.3f}')
                
                # Add vertical line for the mean
                plt.axvline(data_mean, color='red', linestyle='--', alpha=0.7, 
                           label=f'Mean = {data_mean:.2f}')
                
                # Update statistics text
                stats_text += f'\nBoltzmann const: {constant:.3f}\nMean: {data_mean:.2f}\nR²: {r_squared:.3f}'
                
                plt.legend(loc='best')
                fit_info = f"constant={constant:.3f}, mean={data_mean:.2f}, R²={r_squared:.3f}"
            else:
                fit_info = "Could not fit Boltzmann distribution"
        else:
            fit_info = None
        
        # Add statistics text box
        plt.text(0.98, 0.98, stats_text, transform=plt.gca().transAxes, 
                verticalalignment='top', horizontalalignment='right', 
                bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        
        # Save histogram
        base_name = csv_file.stem  # filename without extension
        suffix = '_boltzmann' if fit_boltzmann else ''
        hist_filename = hist_dir / f'{base_name}_{col}{suffix}.png'
        plt.savefig(hist_filename, dpi=300, bbox_inches='tight')
        plt.close()
        
        return {
            'file': csv_file.name,
            'column': col,
            'histogram': hist_filename,
            'fit_info': fit_info,
            'success': True
        }
        
    except Exception as e:
        return {
            'file': csv_file.name,
            'column': col,
            'error': str(e),
            'success': False
        }

def create_histograms(fit_boltzmann=False, n_jobs=1, bins=30):
    """Create histograms for all CSV files in ../out directory."""
    
    # Set up paths
    out_dir = Path('../out')
    hist_dir = Path('histograms')
    
    # Create histogram output directory if it doesn't exist
    hist_dir.mkdir(exist_ok=True)
    
    # Check if out directory exists
    if not out_dir.exists():
        print(f"Output directory {out_dir} does not exist. Creating it.")
        out_dir.mkdir(parents=True, exist_ok=True)
        print("No CSV files found to process.")
        return
    
    # Find all CSV files in the out directory
    csv_files = list(out_dir.glob('*.csv'))
    
    if not csv_files:
        print(f"No CSV files found in {out_dir}")
        return
    
    print(f"Found {len(csv_files)} CSV file(s) to process:")
    for csv_file in csv_files:
        print(f"  - {csv_file.name}")
    
    if fit_boltzmann:
        print("\nBoltzmann distribution fitting enabled (constant × exp(-x/mean)).")
    
    print(f"Using {n_jobs} thread(s) for parallel processing.")
    
    # Collect all (csv_file, column) pairs to process
    tasks = []
    for csv_file in csv_files:
        try:
            # Read just the header to get column names
            df_header = pd.read_csv(csv_file, nrows=0)
            data_columns = df_header.columns[1:]  # Skip first column (ID)
            
            if len(data_columns) == 0:
                print(f"  No data columns found in {csv_file.name}")
                continue
            
            # Add tasks for each column in this file
            for col in data_columns:
                tasks.append((csv_file, col))
                
        except Exception as e:
            print(f"Error reading {csv_file.name}: {str(e)}")
    
    if not tasks:
        print("No valid tasks to process.")
        return
    
    print(f"\nProcessing {len(tasks)} histogram(s) across {len(csv_files)} file(s)...")
    
    # Process histograms in parallel
    results = Parallel(n_jobs=n_jobs, verbose=1)(
        delayed(process_histogram)(csv_file, col, hist_dir, fit_boltzmann, bins)
        for csv_file, col in tasks
    )
    
    # Process results
    successful = 0
    failed = 0
    
    for result in results:
        if result['success']:
            successful += 1
            print(f"✓ Created histogram: {result['histogram']}")
            if result['fit_info']:
                print(f"  Fitted Boltzmann distribution: {result['fit_info']}")
        else:
            failed += 1
            print(f"✗ Error processing {result['file']}, column {result['column']}: {result['error']}")
    
    print(f"\nHistogram creation complete!")
    print(f"  Successful: {successful}")
    print(f"  Failed: {failed}")
    print(f"  Check the '{hist_dir}' directory for output files.")

def main():
    parser = argparse.ArgumentParser(description='Create histograms from CSV files')
    parser.add_argument('--boltzmann', action='store_true', 
                       help='Fit and overlay Boltzmann distribution (constant × exp(-x/mean))')
    parser.add_argument('--threads', '-j', type=int, default=1,
                       help='Number of threads to use for parallel processing (default: 1)')
    parser.add_argument('--bins', '-b', type=int, default=30,
                       help='Number of bins to use for histograms (default: 30)')
    
    args = parser.parse_args()
    
    # Validate number of threads
    if args.threads < 1:
        print("Error: Number of threads must be at least 1")
        return
    
    # Validate number of bins
    if args.bins < 1:
        print("Error: Number of bins must be at least 1")
        return
    
    create_histograms(fit_boltzmann=args.boltzmann, n_jobs=args.threads, bins=args.bins)

if __name__ == "__main__":
    main()
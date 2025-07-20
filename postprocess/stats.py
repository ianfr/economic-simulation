# Read all the CSV files in the ../out directory
# Create line plots of statistics for each column over time
# The "time" index is the number in the start of each filename
# The first column is assumed to be the agent ID or similar identifier
# The line plots will be named after the CSV file, with .png extension
# The line plots will be saved in postprocess/stats/
# Create a separate plot for each statistical property (mean, median, stddev) for each column

import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path
import re
import argparse

def extract_time_from_filename(filename):
    """Extract the time step number from filename like '5.step.csv'."""
    match = re.search(r'^(\d+)\.step\.csv$', filename)
    if match:
        return int(match.group(1))
    return None

def create_metrics_plots():
    """Create line plots for scalar metrics from simulation_metrics.csv file."""
    
    # Set up paths
    out_dir = Path('../out')
    stats_dir = Path('stats')
    metrics_file = out_dir / 'simulation_metrics.csv'
    
    # Create stats output directory if it doesn't exist
    stats_dir.mkdir(exist_ok=True)
    
    # Check if metrics file exists
    if not metrics_file.exists():
        print(f"Metrics file {metrics_file} does not exist.")
        return
    
    print(f"Processing metrics file: {metrics_file}")
    
    try:
        # Read the metrics CSV file
        df = pd.read_csv(metrics_file)
        
        if df.empty:
            print("Metrics file is empty.")
            return
        
        # Get timestep column and metric columns
        if 'timestep' not in df.columns:
            print("Error: 'timestep' column not found in metrics file.")
            return
        
        timesteps = df['timestep']
        metric_columns = [col for col in df.columns if col != 'timestep']
        
        if not metric_columns:
            print("No metric columns found in the file.")
            return
        
        print(f"Found {len(metric_columns)} metric(s): {', '.join(metric_columns)}")
        
        # Create plots for each metric
        for metric_name in metric_columns:
            plt.figure(figsize=(10, 6))
            
            plt.plot(timesteps, df[metric_name], 'o-', linewidth=2, markersize=3)
            plt.xlabel('Time Step')
            plt.ylabel(metric_name.replace('_', ' ').title())
            plt.title(f'{metric_name.replace("_", " ").title()} Over Time')
            plt.grid(True, alpha=0.3)
            
            # Add some formatting
            if len(timesteps) > 1:
                plt.xlim(timesteps.min(), timesteps.max())
            
            # Save the plot
            filename = stats_dir / f'metrics_{metric_name}.png'
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            plt.close()
            
            print(f"  Created metrics plot: {filename}")
        
        # Create a combined plot if there are multiple metrics
        if len(metric_columns) > 1:
            fig, axes = plt.subplots(len(metric_columns), 1, figsize=(12, 4 * len(metric_columns)))
            if len(metric_columns) == 1:
                axes = [axes]
            
            for i, metric_name in enumerate(metric_columns):
                axes[i].plot(timesteps, df[metric_name], 'o-', linewidth=2, markersize=3)
                axes[i].set_xlabel('Time Step')
                axes[i].set_ylabel(metric_name.replace('_', ' ').title())
                axes[i].set_title(f'{metric_name.replace("_", " ").title()} Over Time')
                axes[i].grid(True, alpha=0.3)
                
                if len(timesteps) > 1:
                    axes[i].set_xlim(timesteps.min(), timesteps.max())
            
            plt.tight_layout()
            
            # Save the combined plot
            combined_filename = stats_dir / 'metrics_combined.png'
            plt.savefig(combined_filename, dpi=300, bbox_inches='tight')
            plt.close()
            
            print(f"  Created combined metrics plot: {combined_filename}")
        
    except Exception as e:
        print(f"Error processing metrics file: {str(e)}")
        return
    
    print(f"Metrics plot creation complete! Processed {len(timesteps)} time steps for {len(metric_columns)} metrics.")

def create_stats_plots():
    """Create line plots of statistics over time for all CSV files in ../out directory."""
    
    # Set up paths
    out_dir = Path('../out')
    stats_dir = Path('stats')
    
    # Create stats output directory if it doesn't exist
    stats_dir.mkdir(exist_ok=True)
    
    # Check if out directory exists
    if not out_dir.exists():
        print(f"Output directory {out_dir} does not exist.")
        return
    
    # Find all CSV files in the out directory (excluding simulation_metrics.csv)
    csv_files = [f for f in out_dir.glob('*.csv') if f.name != 'simulation_metrics.csv']
    
    if not csv_files:
        print(f"No CSV files found in {out_dir}")
        return
    
    print(f"Found {len(csv_files)} CSV file(s) to process")
    
    # Group files by time step and sort them
    time_files = []
    for csv_file in csv_files:
        time_step = extract_time_from_filename(csv_file.name)
        if time_step is not None:
            time_files.append((time_step, csv_file))
    
    # Sort by time step
    time_files.sort(key=lambda x: x[0])
    
    if not time_files:
        print("No files with valid time step format found (expected format: 'N.step.csv')")
        return
    
    print(f"Processing {len(time_files)} time step files...")
    
    # Collect statistics for each time step
    all_stats = {}
    time_points = []
    
    for time_step, csv_file in time_files:
        try:
            # Read the CSV file
            df = pd.read_csv(csv_file)
            
            # Skip the first column (assumed to be ID)
            data_columns = df.columns[1:]
            
            if len(data_columns) == 0:
                print(f"  No data columns found in {csv_file.name}")
                continue
            
            time_points.append(time_step)
            
            # Calculate statistics for each column
            for col in data_columns:
                if col not in all_stats:
                    all_stats[col] = {'mean': [], 'median': [], 'std': [], 'min': [], 'max': []}
                
                all_stats[col]['mean'].append(df[col].mean())
                all_stats[col]['median'].append(df[col].median())
                all_stats[col]['std'].append(df[col].std())
                all_stats[col]['min'].append(df[col].min())
                all_stats[col]['max'].append(df[col].max())
                
        except Exception as e:
            print(f"Error processing {csv_file.name}: {str(e)}")
            continue
    
    if not time_points:
        print("No valid data found to plot")
        return
    
    # Create plots for each column and each statistic
    for col_name, stats in all_stats.items():
        print(f"Creating plots for column: {col_name}")
        
        # Create individual plots for each statistic
        stat_names = ['mean', 'median', 'std', 'min', 'max']
        stat_labels = ['Mean', 'Median', 'Standard Deviation', 'Minimum', 'Maximum']
        
        for stat_name, stat_label in zip(stat_names, stat_labels):
            plt.figure(figsize=(10, 6))
            
            plt.plot(time_points, stats[stat_name], 'o-', linewidth=2, markersize=4)
            plt.xlabel('Time Step')
            plt.ylabel(f'{stat_label} of {col_name}')
            plt.title(f'{stat_label} of {col_name} Over Time')
            plt.grid(True, alpha=0.3)
            
            # Add some formatting
            if len(time_points) > 1:
                plt.xlim(min(time_points), max(time_points))
            
            # Save the plot
            filename = stats_dir / f'{col_name}_{stat_name}.png'
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            plt.close()
            
            print(f"  Created plot: {filename}")
        
        # Create a combined plot with all statistics
        plt.figure(figsize=(12, 8))
        
        # Plot each statistic
        plt.subplot(2, 2, 1)
        plt.plot(time_points, stats['mean'], 'o-', color='blue', linewidth=2, markersize=4)
        plt.title('Mean')
        plt.xlabel('Time Step')
        plt.ylabel(col_name)
        plt.grid(True, alpha=0.3)
        
        plt.subplot(2, 2, 2)
        plt.plot(time_points, stats['median'], 'o-', color='green', linewidth=2, markersize=4)
        plt.title('Median')
        plt.xlabel('Time Step')
        plt.ylabel(col_name)
        plt.grid(True, alpha=0.3)
        
        plt.subplot(2, 2, 3)
        plt.plot(time_points, stats['std'], 'o-', color='red', linewidth=2, markersize=4)
        plt.title('Standard Deviation')
        plt.xlabel('Time Step')
        plt.ylabel(col_name)
        plt.grid(True, alpha=0.3)
        
        plt.subplot(2, 2, 4)
        plt.plot(time_points, stats['min'], 'o-', color='orange', linewidth=2, markersize=4, label='Min')
        plt.plot(time_points, stats['max'], 'o-', color='purple', linewidth=2, markersize=4, label='Max')
        plt.title('Min/Max')
        plt.xlabel('Time Step')
        plt.ylabel(col_name)
        plt.legend()
        plt.grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        # Save the combined plot
        combined_filename = stats_dir / f'{col_name}_combined.png'
        plt.savefig(combined_filename, dpi=300, bbox_inches='tight')
        plt.close()
        
        print(f"  Created combined plot: {combined_filename}")
    
    print(f"\nStatistics plot creation complete! Check the '{stats_dir}' directory for output files.")
    print(f"Processed {len(time_points)} time steps for {len(all_stats)} columns.")

def main():
    """Main function to handle command line arguments and execute plotting functions."""
    parser = argparse.ArgumentParser(description='Create plots for simulation data and metrics')
    parser.add_argument('--population', action='store_true', 
                       help='Create population statistics plots only')
    parser.add_argument('--metrics', action='store_true', 
                       help='Create metrics plots only')
    
    args = parser.parse_args()
    
    # If no specific option is given, do both
    if not args.population and not args.metrics:
        args.population = True
        args.metrics = True
    
    if args.population:
        print("Creating population statistics plots...")
        create_stats_plots()
    
    if args.metrics:
        print("\nCreating metrics plots...")
        create_metrics_plots()
    
    print("\nAll requested plots created successfully!")

if __name__ == "__main__":
    main()
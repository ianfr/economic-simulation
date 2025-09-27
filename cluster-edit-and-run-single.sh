#!/bin/bash

# For usage with the future parallel 'study' runner tools on an SSH/MPI cluster
# Assumes the executable has already been built
# Using the GNU Parallel tool is probably the best bet for calling this script

# This script takes in pairs of of .nml keywords and the values to set them to,
# copies the base simulation to a new unique folder on the fileshare, 
# edits the .nml files to set the keywords to the desired values,
# and runs the simulation

# This script is the lower-level wrapper executable that will be called by parallel runner tool(s)
# that will handle launching multiple instances of this script in parallel on the cluster

# Path to the folder on the shared file system for all the nodes/workers
export FILESHARE_BASE_PATH=/home/linuxuser/fshare
export FILESHARE_ORIGINAL_PATH=$FILESHARE_BASE_PATH/sim_original

# Function to display usage
usage() {
    echo "Usage: $0 [keyword1 value1] [keyword2 value2] ..."
    echo "Example: $0 n_agents 1000 alpha 0.3 seed 42"
    echo "Keywords should correspond to variables in the .nml files"
    exit 1
}

# Function to generate unique run ID
generate_run_id() {
    echo "run_$(date +%Y%m%d_%H%M%S)___$$_$RANDOM"
}

# Function to edit namelist files
edit_nml_parameter() {
    local file=$1
    local keyword=$2
    local value=$3
    
    # Use sed to replace the parameter value in the namelist file
    # This handles both integer and string values
    if [[ $value =~ ^[0-9]+(\.[0-9]+)?$ ]]; then
        # Numeric value
        sed -i "s/^\s*${keyword}\s*=.*/${keyword} = ${value}/" "$file"
    else
        # String value (add quotes if not already present)
        if [[ $value != \'*\' ]] && [[ $value != \"*\" ]]; then
            value="'${value}'"
        fi
        sed -i "s/^\s*${keyword}\s*=.*/${keyword} = ${value}/" "$file"
    fi
}

# Check if we have an even number of arguments (keyword-value pairs)
if [ $# -eq 0 ] || [ $(($# % 2)) -ne 0 ]; then
    echo "Error: Arguments must be provided in keyword-value pairs"
    usage
fi

# Check if the original simulation directory exists
if [ ! -d "$FILESHARE_ORIGINAL_PATH" ]; then
    echo "Error: Original simulation directory not found at $FILESHARE_ORIGINAL_PATH"
    echo "Please run install-to-fileshare.sh first to set up the base simulation"
    exit 1
fi

# Generate unique run directory
RUN_ID=$(generate_run_id)
RUN_DIR="$FILESHARE_BASE_PATH/$RUN_ID"

echo "Starting MPI simulation run: $RUN_ID"
echo "Working directory: $RUN_DIR"

# Create run directory and copy simulation files
echo "Copying simulation files..."
mkdir -p "$RUN_DIR"
cp -r "$FILESHARE_ORIGINAL_PATH"/* "$RUN_DIR/"

# Change to the run directory
cd "$RUN_DIR/bin"

# Parse and apply keyword-value pairs
echo "Applying parameter changes:"
for ((i=1; i<=$#; i+=2)); do
    keyword=${!i}
    j=$((i+1))
    value=${!j}
    
    echo "  Setting $keyword = $value"
    
    # Try to edit the parameter in both nml files
    # First check in.nml
    if grep -q "^\s*${keyword}\s*=" in.nml 2>/dev/null; then
        edit_nml_parameter "in.nml" "$keyword" "$value"
        echo "    Updated in in.nml"
    # Then check sim_type.nml
    elif grep -q "^\s*${keyword}\s*=" sim_type.nml 2>/dev/null; then
        edit_nml_parameter "sim_type.nml" "$keyword" "$value"
        echo "    Updated in sim_type.nml"
    else
        echo "    Warning: Parameter '$keyword' not found in any .nml file"
    fi
done

# Display final configuration
echo ""
echo "Final configuration:"
echo "--- in.nml ---"
cat in.nml
echo ""
echo "--- sim_type.nml ---"
cat sim_type.nml
echo ""

# Run the simulation
echo "Starting simulation..."
START_TIME=$(date +%s)

# Execute the simulation
# ./economic-simulation
./Boltzmannomics

SIMULATION_EXIT_CODE=$?
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

echo ""
echo "Simulation completed in ${DURATION} seconds"

if [ $SIMULATION_EXIT_CODE -eq 0 ]; then
    echo "Simulation completed successfully"
    echo "Output files are in: $RUN_DIR/bin/out/"
    
    # Create a summary file with run parameters
    SUMMARY_FILE="$RUN_DIR/bin/run_summary.txt"
    echo "Run ID: $RUN_ID" > "$SUMMARY_FILE"
    echo "Start Time: $(date -d @$START_TIME)" >> "$SUMMARY_FILE"
    echo "End Time: $(date -d @$END_TIME)" >> "$SUMMARY_FILE"
    echo "Duration: ${DURATION} seconds" >> "$SUMMARY_FILE"
    echo "Parameters:" >> "$SUMMARY_FILE"
    
    for ((i=1; i<=$#; i+=2)); do
        keyword=${!i}
        j=$((i+1))
        value=${!j}
        echo "  $keyword = $value" >> "$SUMMARY_FILE"
    done
    
    echo "Run summary saved to: $SUMMARY_FILE"
else
    echo "Simulation failed with exit code: $SIMULATION_EXIT_CODE"
    exit $SIMULATION_EXIT_CODE
fi

echo "MPI run completed: $RUN_ID"
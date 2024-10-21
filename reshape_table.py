import pandas as pd
import argparse

def reshape_table(input_file, output_file):
    # Load the data
    df = pd.read_csv(input_file, sep='\t', index_col=0)

    # Ensure the "unmapped" row is removed
    df = df[~df.index.str.contains("unmapped", case=False)]

    # Initialize an empty list to store the reshaped data
    reshaped_data = []

    # Iterate over each sample (column) and organism (row) in the dataframe
    for sample in df.columns:
        for organism, value in df[sample].items():
            # Only add the organism if the value is greater than zero
            if value > 0:
                reshaped_data.append([sample, organism])

    # Create a new dataframe from the reshaped data
    reshaped_df = pd.DataFrame(reshaped_data, columns=['Sample', 'Organism'])

    # Save the reshaped dataframe to a new TSV file without header or index
    reshaped_df.to_csv(output_file, sep='\t', index=False, header=False)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Reshape table into two columns based on non-zero values.')
    parser.add_argument('input_file', type=str, help='Path to the input file.')
    parser.add_argument('output_file', type=str, help='Path to the output TSV file.')
    args = parser.parse_args()

    # Corrected function call
    reshape_table(args.input_file, args.output_file)
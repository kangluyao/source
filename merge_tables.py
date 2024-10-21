import pandas as pd
import glob
import os
import argparse

def merge_files(input_dir, output_file):
    # Get a list of all txt files in the input directory
    txt_files = glob.glob(os.path.join(input_dir, '*.txt'))

    # Initialize an empty dataframe
    merged_df = pd.DataFrame()

    # Loop through each file and merge it
    for i, file in enumerate(txt_files):
        # Load the current file into a dataframe
        df = pd.read_csv(file, sep='\t')
        
        # Rename the second column to match the filename without the extension
        base_name = os.path.splitext(os.path.basename(file))[0]
        df.columns = ['Genome', base_name]
        
        # If this is the first file, use it to initialize the merged dataframe
        if i == 0:
            merged_df = df
        else:
            # Merge with the accumulated dataframe using a suffix for duplicate columns
            suffix = '_C{}'.format(i+1)
            merged_df = merged_df.merge(df, on='Genome', how='outer', suffixes=('', suffix))

    # Save the merged dataframe to the specified output file
    merged_df.to_csv(output_file, sep='\t', index=False)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Merge multiple txt files based on a common column.')
    parser.add_argument('input_dir', type=str, help='The directory containing the txt files to merge.')
    parser.add_argument('output_file', type=str, help='The path to the output file.')
    
    args = parser.parse_args()
    merge_files(args.input_dir, args.output_file)

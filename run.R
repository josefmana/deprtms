#
# This script run the targets pipeline and saves results
#

# Load packages needed:
library(targets)
library(tarchetypes)

# Run the pipeline:
tar_make()
tar_source()

# Save the outcomes:
save_tables(struct = tar_read(tables_structure))

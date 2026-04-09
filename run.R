library(targets)
library(tarchetypes)

tar_source()
tar_make()

save_tables(struct = tar_read(tables_structure))

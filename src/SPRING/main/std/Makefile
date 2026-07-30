.SUFFIXES:

DIR_MAKE_GRID_DATA         = make_grid_data
DIR_REMAP                  = remap
DIR_MERGE_REMAPPING_TABLES = merge_remapping_tables
DIR_RASTERIZE              = rasterize

all:
	$(MAKE) -C $(DIR_MAKE_GRID_DATA)
	$(MAKE) -C $(DIR_REMAP)
	$(MAKE) -C $(DIR_MERGE_REMAPPING_TABLES)
	$(MAKE) -C $(DIR_RASTERIZE)

install:
	$(MAKE) -C $(DIR_MAKE_GRID_DATA) install
	$(MAKE) -C $(DIR_REMAP) install
	$(MAKE) -C $(DIR_MERGE_REMAPPING_TABLES) install
	$(MAKE) -C $(DIR_RASTERIZE) install

clean:
	$(MAKE) -C $(DIR_MAKE_GRID_DATA) clean
	$(MAKE) -C $(DIR_REMAP) clean
	$(MAKE) -C $(DIR_MERGE_REMAPPING_TABLES) clean
	$(MAKE) -C $(DIR_RASTERIZE) clean

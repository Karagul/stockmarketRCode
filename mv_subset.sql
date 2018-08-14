create materialized view if not exists mv_qs_symbol_subset as SELECT * FROM public."symbolKeySubset";
ALTER TABLE mv_qs_symbol_subset OWNER to postgres;
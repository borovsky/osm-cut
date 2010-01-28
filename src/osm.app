{application, osm, 
 [{description, "OSM cut application"},
  {vsn, "0.1"},
  {modules, [
             osm_application,
             osm_supervisor,
             osm_writer,
             osm_processor,
             test_osm_writer
             ]},
  {applications, [kernel, stdlib]},
  {mod, {osm_application, []}},
  {start_phases, []}
 ]}.

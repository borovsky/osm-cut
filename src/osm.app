{application, osm, 
 [{description, "OSM cut application"},
  {vsn, "0.1"},
  {modules, [
             osm_application,
             osm_cut,
             osm_parser,
             osm_process_complete,
             osm_process_non_complete,
             osm_processor,
             osm_supervisor,
             osm_utils,
             osm_writer,
             polygon_compiler,
             simple_xml_formatter
             ]},
  {applications, [kernel, stdlib]},
  {mod, {osm_application, []}},
  {start_phases, []}
 ]}.

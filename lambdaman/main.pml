let map, lambdaman, ghosts, fruit = world in

setup_depth_max(map);
load_map(map);
init_map_visit(map);
(0, step)

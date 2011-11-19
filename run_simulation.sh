# NOTE: you must compile all of the scala files into the classes dir before running this

# tutorial
#python tools/playgame.py "scala -cp classes MyBot" "python tools/sample_bots/python/HunterBot.py" --map_file tools/maps/example/tutorial1.map --log_dir game_logs --turns 60 --scenario --food none --player_seed 7 --verbose -e

python tools/playgame.py "scala -cp classes MyBot" "python tools/sample_bots/python/HunterBot.py" "python tools/sample_bots/python/LeftyBot.py" "python tools/sample_bots/python/GreedyBot.py" --player_seed 42 --end_wait=0.25 --verbose -e --log_dir game_logs --turns 1000 --map_file tools/maps/maze/maze_04p_01.map

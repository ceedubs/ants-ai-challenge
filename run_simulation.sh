# NOTE: you must compile all of the scala files into the classes dir before running this
python tools/playgame.py "scala -cp classes MyBot" "python tools/sample_bots/python/HunterBot.py" --map_file tools/maps/example/tutorial1.map --log_dir game_logs --turns 60 --scenario --food none --player_seed 7 --verbose -e

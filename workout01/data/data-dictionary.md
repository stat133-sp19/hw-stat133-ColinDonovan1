---
title: "data-dictionary.md"
author: "Colin Donovan"
date: "March 15, 2019"
output: md_document
---
** Workout 01 Data Dictionary **

**Individual player Files **
This folder contains individual files for the following 2018 Golden State Warriors players:
- Andre Iguodala
- Draymond Green
- Kevin Durant
- Klay Thompson
- Stephen Curry

Here is a short description of the data variables found in each file. All of the variable contain the following information about specific shots made by each player:
- team_name: the name of the team the player was signed to, as a character.
- game_date: the date in MM/DD/YY in which the game was played, as a character.
- season: the year of the season during which the shot was made, as an integer.
- period: the number of the period during which the shot was made, as an integer, this can be any number between 1 and 4.
- minutes_remaining: amount of minutes remaining in the period when shot was made, as an integer.
- seconds_remaining: amount of seconds remaining in period when shot was made, as an integer.
- shot_made_flag: whether the shot was made or not, as a character. This can take the values 'y' or 'n'.
- action_type: basketball moves used by player, such as pass by defenders to get access to the baskets or clean pass to teammate to score 3 pointer, as a character.
- shot_type: whether the shot is a 2-point or 3-point field goal, as a character.
- shot_distance: distance from the player to the basket in feet, as an integer.
- opponent: name of the opposing team the shot was made against, as a character.
- x: court x coordinate in inches, as an integer.
-y: court y coordinate in inches, as an integer.
## nba2018-players.csv
The nba2018-players.csv file contains
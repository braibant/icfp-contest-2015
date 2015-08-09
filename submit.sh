API_TOKEN="c0x8nEeXAxmBWDULUUsj0yXRzhbwaWvGaSjlISKxmok="
TEAM_ID=36

curl --user :$API_TOKEN -X POST -H "Content-Type: application/json" \
        -d @"$1" \
        https://davar.icfpcontest.org/teams/$TEAM_ID/solutions

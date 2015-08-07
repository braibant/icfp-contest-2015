API_TOKEN="c0x8nEeXAxmBWDULUUsj0yXRzhbwaWvGaSjlISKxmok="
TEAM_ID=36
OUTPUT=$1

curl --user :$API_TOKEN -X POST -H "Content-Type: application/json" \
        -d '$OUTPUT' \
        https://davar.icfpcontest.org/teams/$TEAM_ID/solutions

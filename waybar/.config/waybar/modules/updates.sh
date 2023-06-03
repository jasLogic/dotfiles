updates=$(checkupdates --nocolor)
n=$(echo -n "$updates" | wc -l)

if [ "$n" -eq 0 ]; then
    n=''
    alt='updated'
else
    alt='pending'
fi

jq --unbuffered --null-input --compact-output \
   --arg text "$n" \
   --arg tooltip "$updates" \
   --arg alt "$alt" \
   '{"text": $text, "tooltip": $tooltip, "alt": $alt, "class": $alt}'

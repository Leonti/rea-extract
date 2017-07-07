#!/usr/bin/env bash

cd /root/reaResults

for i in *; do
    unzip "$i"
    rm "$i"
done

cd /root/reaSoldResults

for i in *; do
    unzip "$i"
    rm "$i"
done

cd /root
./rea-extract &> output.txt

output=$(cat output.txt)

echo $output

content=$'New properties have been extracted:\n\n'$output
subject="REA Extract results "$(date +"%Y.%m.%d")

echo "$content"
echo "$subject"

curl -s --user 'api:'$MAILGUN_API_KEY \
    https://api.mailgun.net/v3/leonti.me/messages \
    -F from='REA Extract <prishelec@gmail.com>' \
    -F to=prishelec@gmail.com \
    -F subject="$subject" \
    -F text="$content"

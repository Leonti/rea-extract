#!/usr/bin/env bash

cd /root/reaResults

for i in *; do
    unzip "$i"
    rm "$i"
done

aws s3 cp s3://leonti-rea-crawler/properties.db /root/properties.db

cd /root
./rea-extract |& tee output.txt

aws s3 cp /root/properties.db s3://leonti-rea-crawler/properties.db

output=$(cat output.txt)

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

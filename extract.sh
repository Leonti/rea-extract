#!/usr/bin/env bash

cd /root/reaResults

for i in */; do
    unzip -r "$i"
    rm "$i"
done

aws s3 cp s3://leonti-rea-crawler/properties.db /root/reaResults/properties.db

./rea-extract > output.txt

aws s3 cp /root/reaResults/properties.db s3://leonti-rea-crawler/properties.db

content=(cat output.txt)
subject="REA Extract results "$(date +"%Y.%m.%d")

echo "$content"
echo "$subject"

curl -s --user 'api:'$MAILGUN_API_KEY \
    https://api.mailgun.net/v3/leonti.me/messages \
    -F from='REA Extract <prishelec@gmail.com>' \
    -F to=prishelec@gmail.com \
    -F subject="$subject" \
    -F text="$content"

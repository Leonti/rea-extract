#!/usr/bin/env bash


if [ -z "$DATE" ]; then
  echo "DATE is not provided, using current date"
  extract_date=$(date +"%Y-%-m-%-d")
else
  echo "Extracting values for date '$DATE'"
  extract_date=$DATE
fi



mkdir /root/reaResults
cd /root/reaResults

aws s3 cp "s3://leonti-rea-crawler/"$extract_date".zip" .

for i in *; do
    unzip "$i"
    rm "$i"
done

mkdir /root/reaSoldResults
cd /root/reaSoldResults

aws s3 cp "s3://leonti-rea-crawler/"$extract_date"-sold.zip" .

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

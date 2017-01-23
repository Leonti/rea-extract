FROM mesosphere/aws-cli

RUN apk add --no-cache --update curl zip bash

WORKDIR /root

COPY .stack-work/install/x86_64-linux/lts-7.2/8.0.1/bin/rea-extract rea-extract
COPY extract.sh extract.sh

ENTRYPOINT ["/bin/bash", "-c"]

CMD ["/root/extract.sh"]

#sudo docker run -v /root/reaResults:/root/reaResults \
#--env MAP_QUEST_KEY=<map_quest_key> \
#--env AWS_ACCESS_KEY_ID=<access_key_id> \
#--env AWS_SECRET_ACCESS_KEY=<secret_access_key> \
#--env AWS_DEFAULT_REGION=us-east-1 leonti/rea-extract

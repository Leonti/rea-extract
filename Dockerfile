FROM mesosphere/aws-cli

RUN apk add --no-cache --update coreutils curl zip bash

WORKDIR /root

COPY .stack-work/install/x86_64-linux/lts-11.0/8.2.2/bin/rea-extract rea-extract
COPY extract.sh extract.sh

ENTRYPOINT ["/bin/bash", "-c"]

CMD ["/root/extract.sh"]

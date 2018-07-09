FROM ubuntu:18.04
RUN apt-get update && \
    apt-get install -y ca-certificates && \
    apt-get install -y libgnutls30 && \
    apt-get install -y netbase && \
    apt-get install -y unzip curl && \
    apt-get -y autoremove && \
    apt-get -y clean && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/* && \
    rm -rf /var/tmp/*

RUN apt-get update && \
    apt-get install -y \
        python \
        python-dev \
        python-pip \
        python-setuptools \
        groff \
        less && \
    pip install --upgrade awscli && \
    apt-get -y autoremove && \
    apt-get -y clean && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/* && \
    rm -rf /var/tmp/*

WORKDIR /root

COPY .stack-work/install/x86_64-linux/lts-11.0/8.2.2/bin/rea-extract rea-extract
COPY extract.sh extract.sh

ENTRYPOINT ["/bin/bash", "-c"]

CMD ["/root/extract.sh"]

FROM hybridpdr

RUN curl -sL https://deb.nodesource.com/setup_10.x | bash - \
    && apt-get update \
    && apt-get -y --no-install-recommends install \
        build-essential \
        mosquitto \
        mosquitto-clients \
        libnss-mdns \
        mdns-scan \
        sshpass \
        nodejs \
        python3-pip \
    && rm -rf /var/lib/apt/lists/*

COPY mathematica_inst.sh /tmp/mathematica_inst.sh
RUN chmod +x /tmp/mathematica_inst.sh
CMD ["/tmp/mathematica_inst.sh"]

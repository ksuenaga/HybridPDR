FROM ocaml/opam2:debian-stable

# Install dependencies for HybridPDR
USER root
RUN curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash - \
    && apt-get install --no-install-recommends -y \
        gfortran \
        libgmp-dev \
        m4 \
        python \
        python3-pip \
        nodejs \
    && rm -rf /var/lib/apt/lists/*

# Install requirements for HybridPDR
USER opam
RUN sed -i -e 's/jobs: 127/jobs: 4/g' ~/.opam/config \
    && git pull \
    && opam update \
    && opam install \
        bau \
        bisect_ppx \
        core \
        dune \
        mparser \
        ocaml-migrate-parsetree \
        odepack \
        ppx_deriving \
        ppx_driver \
        sexplib \
        xml-light \
        z3

WORKDIR $HOME

ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
ENV PATH=$HOME/.local/bin:$PATH
ENV FLASK_APP=flask/app.py

ENV LD_LIBRARY_PATH="$HOME/.opam/4.07/lib/z3"

# Copy and build HybridPDR
COPY --chown=opam:opam src src
RUN . $HOME/.opam/opam-init/variables.sh \
    && dune build src/hpdrMain.exe --profile=release

# Copy list of deps
COPY --chown=opam:opam flask/requirements.txt flask/package.json flask/

# Install deps of flask app
RUN pip3 install -r flask/requirements.txt \
    && npm install --prefix flask

# Copy Flask app and build javascript
COPY --chown=opam:opam flask flask
RUN npm run dev --prefix flask

#ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 5000
CMD ["flask", "run", "--host=0.0.0.0"]

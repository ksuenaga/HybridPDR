FROM ocaml/opam2:debian-stable

# Install dependencies for HybridPDR
USER root
RUN apt-get update && apt-get install --no-install-recommends -y \
        gfortran \
        libgmp-dev \
        m4 \
        python \
    && rm -rf /var/lib/apt/lists/*

# Install requirements for HybridPDR
USER opam
RUN sed -i -e 's/jobs: 127/jobs: 4/g' ~/.opam/config \
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

ENV LD_LIBRARY_PATH="/home/opam/.opam/4.07/lib/z3"

# Copy HybridPDR
COPY --chown=opam:opam src .

# Install dependencies for HybridPDR
USER root
RUN apt-get update && apt-get install --no-install-recommends -y \
        libgdbm-dev \
        libpcre3-dev \
        libssl-dev \
        pkg-config \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Eliom
USER opam
RUN git pull \
    && opam update \
    && opam clean -r \
    && sed -i -e 's http://forge.ocamlcore.org/frs/download.php/379/ocamlify-0.0.1.tar.gz https://launchpad.net/ubuntu/+archive/primary/+sourcefiles/ocamlify/0.0.1-3build1/ocamlify_0.0.1.orig.tar.gz g' ~/.opam/repo/default/packages/ocamlify/ocamlify.0.0.1/opam \
    && opam install eliom

COPY --chown=opam:opam app app
#ENTRYPOINT ["docker-entrypoint.sh"]

EXPOSE 8080
#CMD ["/bin/bash"]

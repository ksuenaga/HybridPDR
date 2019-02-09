FROM hybridcore

USER root
VOLUME ["/home/opam/data/"]
# Copy list of deps
COPY flask/requirements.txt flask/package.json flask/

# Install deps of flask app
RUN pip3 install -r flask/requirements.txt \
    && npm install --prefix flask

# Copy Flask app and build javascript
COPY flask flask
RUN npm run dev --prefix flask

EXPOSE 5000
CMD ["flask", "run", "--host=0.0.0.0"]

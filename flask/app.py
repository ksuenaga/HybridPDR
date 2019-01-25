#!python3

from flask import Flask
from flask import flash, redirect, render_template, request, url_for
from flask import json, jsonify

app = Flask(__name__)
app.secret_key = b'5J\xa8\xf6\xc8%Im3[\xe6\xc3R\x88\x08\xc7'

def verify(xml_raw, tactics, initial, safety):
  res_obj = {
      'xml_raw': xml_raw
    , 'tactics': tactics
    , 'initial': initial
    , 'safety': safety
    }
  return json.dumps(res_obj)

@app.route('/run', methods=['POST'])
def run():
  obj = request.json
  x = obj['xml_raw']
  t = obj['tactics']
  i = obj['initial']
  s = obj['safety']
  res = verify(x, t, i, s)
  return jsonify({
      'xml_raw': x
    , 'tactics': t
    , 'initial': i
    , 'safety': s
    , 'result': res
    })

@app.route('/', methods=['GET'])
def index():
  return render_template('index.html')

@app.route('/form', methods=['GET', 'POST'])
def form():
  if request.method == 'POST':
    form = request.form
    x = form['xml_raw']
    t = form['tactics']
    i = form['initial']
    s = form['safety']
    res = verify(x, t, i, s)
  else:
    x = ""
    t = ""
    i = ""
    s = ""
    res = ""
  return render_template('form.html'
                         , xml_raw = x
                         , tactics = t
                         , initial = i
                         , safety = s
                         , result = res)

app.config['JSON_AS_ASCII'] = False
if app.debug:
  app.config['SEND_FILE_MAX_AGE_DEFAULT'] = 0

if __name__ == '__main__':
  app.run(threaded = True)

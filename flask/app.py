from flask import Flask
from flask import flash, redirect, render_template, request, url_for
from flask import json

app = Flask(__name__)
app.secret_key = b'5J\xa8\xf6\xc8%Im3[\xe6\xc3R\x88\x08\xc7'

@app.route('/', methods=['GET', 'POST'])
def root():
  if request.method == 'POST':
    x = request.form['xml_raw']
    t = request.form['tactics']
    i = request.form['initial']
    s = request.form['safety']
    res = verify(x, t, i, s)
  else:
    x = ""
    t = ""
    i = ""
    s = ""
    res = ""
  return render_template('index.html'
                         , xml_raw = x
                         , tactics = t
                         , initial = i
                         , safety = s
                         , result = res)

def verify(xml_raw, tactics, initial, safety):
  res_obj = {
    'xml_raw': xml_raw,
    'tactics': tactics,
    'initial': initial,
    'safety': safety,
    }
  return json.dumps(res_obj)

if __name__ == '__main__':
  app.run()

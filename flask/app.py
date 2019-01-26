#!python3

from flask import Flask
from flask import flash, redirect, render_template, request, url_for
from flask import json, jsonify
import os, sys
from subprocess import PIPE, Popen, STDOUT, TimeoutExpired
import tempfile

app = Flask(__name__)
app.secret_key = b'5J\xa8\xf6\xc8%Im3[\xe6\xc3R\x88\x08\xc7'

def verify(xml_model, str_tactics, str_initial, str_safety):
  try:
    with tempfile.NamedTemporaryFile(delete = False, mode = 'w', suffix = '.xml') as tmp_model:
      filename_model = tmp_model.name
      tmp_model.write(xml_model)
      tmp_model.close()
    filename_exe = os.path.join(
      os.path.dirname(__file__),
      '../_build/default/src/hpdrMain.exe'
      )
    try:
      with Popen([
          filename_exe,
          '-model', filename_model,
          '-init', str_initial,
          '-safe', str_safety,
          '-initid', '1',
          ], stdin = PIPE, stdout = PIPE, stderr = STDOUT
                 , universal_newlines = True, shell = False) as p_exe:
        try:
          str_result, _ = p_exe.communicate(input=str_tactics, timeout=60)
        except TimeoutExpired:
          p_exe.kill()
          str_result, _ = p_exe.communicate()
        retcode = p_exe.returncode
      err = False if retcode == 0 else True
    except:
      if app.debug:
        import traceback; traceback.print_exc()
      err = True
      str_result = 'Error'
    os.remove(filename_model)
  except:
    if app.debug:
      import traceback; traceback.print_exc()
    err = True
    str_result = 'Error'
  return err, str_result

@app.route('/run', methods=['POST'])
def run():
  obj = request.json
  m = obj['xml_model']
  t = obj['tactics']
  i = obj['initial']
  s = obj['safety']
  err, res = verify(m, t, i, s)
  http_code = 500 if err else 200
  return jsonify({
      'xml_model': m
    , 'tactics': t
    , 'initial': i
    , 'safety': s
    , 'status': 'error' if err else 'ok'
    , 'result': res
    }), http_code

@app.route('/', methods=['GET'])
def index():
  return render_template('index.html')

@app.route('/form', methods=['GET', 'POST'])
def form():
  if request.method == 'POST':
    form = request.form
    m = form['xml_model']
    t = form['tactics']
    i = form['initial']
    s = form['safety']
    err, res = verify(m, t, i, s)
  else:
    m = ""
    t = ""
    i = ""
    s = ""
    res = ""
    err = False
  http_code = 500 if err else 200
  return render_template('form.html'
                         , xml_model = m
                         , tactics = t
                         , initial = i
                         , safety = s
                         , result = res), http_code

app.config['JSON_AS_ASCII'] = False
if app.debug:
  app.config['SEND_FILE_MAX_AGE_DEFAULT'] = 0

if __name__ == '__main__':
  app.run(threaded = True)

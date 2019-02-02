#!python3

from flask import Flask
from flask import flash, redirect, render_template, request, url_for
from flask import json, jsonify
import os, sys
from subprocess import PIPE, Popen, STDOUT, TimeoutExpired
import tempfile

app = Flask(__name__)
app.secret_key = b'5J\xa8\xf6\xc8%Im3[\xe6\xc3R\x88\x08\xc7'

def verify(xml_model, str_tactics, str_initial, str_safety, current_dir):
  filename_model = os.path.join(app.config['DATA_DIR_PATH'], current_dir)
  try:
    with open(filename_model, mode='w') as f:
      f.write(xml_model)
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
  c = obj['current_dir']
  err, res = verify(m, t, i, s, c)
  http_code = 500 if err else 200
  return jsonify({
      'xml_model': m
    , 'tactics': t
    , 'initial': i
    , 'safety': s
    , 'current_dir': c
    , 'status': 'error' if err else 'ok'
    , 'result': res
    }), http_code

@app.route('/', methods=['GET'])
def index():
  return render_template('index.html')

@app.route('/getTree', methods=['GET'])
def get_tree():
  files = sorted(os.listdir(app.config['DATA_DIR_PATH']))
  regular_files = []
  directories = []
  for file in files:
    if os.path.isdir(os.path.join(app.config['DATA_DIR_PATH'], file)):
      directories.append(file)
    else:
      regular_files.append(file)
  directories = [{"title": dir + '/', "key": dir, "folder": True} for dir in directories]
  regular_files = [{"title": file, "key": file} for file in regular_files]
  files = directories + regular_files
  return jsonify(files)

@app.route('/load', methods=['POST'])
def load():
  obj = request.json
  xml = obj['xml_path']
  xml_dir = os.path.join(app.config['DATA_DIR_PATH'], xml)
  with open(xml_dir) as f:
    str = f.read()

  return jsonify({
    'xml_path': xml
  , 'result': str
  })

@app.route('/save', methods=['POST'])
def save():
  obj = request.json
  str = obj['save_str']
  path = obj['save_path']
  save_path = os.path.join(app.config['DATA_DIR_PATH'], path)
  with open(save_path, mode='w') as f:
    f.write(str)
  return jsonify({
    'save_path': path
  })

app.config['JSON_AS_ASCII'] = False
app.config['DATA_DIR_PATH'] = '/home/opam/data'
if app.debug:
  app.config['SEND_FILE_MAX_AGE_DEFAULT'] = 0

if __name__ == '__main__':
  app.run(threaded = True)

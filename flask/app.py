#!python3

from flask import Flask
from flask import flash, redirect, render_template, request, url_for
from flask import json, jsonify
import os, sys, shutil
from subprocess import PIPE, Popen, STDOUT, TimeoutExpired
import tempfile
from time import sleep
import signal

app = Flask(__name__)
app.secret_key = b'5J\xa8\xf6\xc8%Im3[\xe6\xc3R\x88\x08\xc7'

def verify(xml_model, str_tactics, str_initial, str_safety, current_dir, debug):
  filename_model = os.path.join(app.config['DATA_DIR_PATH'], current_dir)
  try:
    with open(filename_model, mode='w') as f:
      f.write(xml_model)
    filename_exe = os.path.join(
      os.path.dirname(__file__),
      '../_build/default/src/hpdrMain.exe'
      )
    # command = [
    #       filename_exe,
    #       '-model', filename_model,
    #       '-init', str_initial,
    #       '-safe', str_safety,
    #       '-initid', '1',
    #       ]
    command = filename_exe + ' -model ' + filename_model + ' -init \"' + str_initial + '\" -safe \"' + str_safety + '\" -initid 1'
    if debug:
      # command.append('-debug')
      command = command + ' -debug'
    try:
      with Popen(command, stdin=PIPE, stdout=PIPE, stderr=STDOUT
                 , universal_newlines=True, shell=True, preexec_fn=os.setsid) as p_exe:
        try:
          app.config['VERIFY_ID'] = os.getpgid(p_exe.pid)
          sleep(5)
          str_result, err_result = p_exe.communicate(input=str_tactics, timeout=60)
        except TimeoutExpired:
          p_exe.kill()
          str_result, err_result = p_exe.communicate()
        retcode = p_exe.returncode
      err = False if retcode == 0 else True
    except:
      if app.debug:
        import traceback; traceback.print_exc()
      err = True
      str_result = 'Error'
      err_result = ''
      retcode = -1
  except:
    if app.debug:
      import traceback; traceback.print_exc()
    err = True
    str_result = 'Error'
    err_result = ''
    retcode = -1
  return err, str_result, err_result, retcode

@app.route('/', methods=['GET'])
def index():
  return render_template('index.html')

@app.route('/project/<path:file_path>')
def project(file_path):
  return render_template('app.html', selectedFilePath=file_path)

@app.route('/ls/<path:subpath>', methods=['GET'])
def ls(subpath):
  base_path = os.path.join(app.config['DATA_DIR_PATH'], subpath)
  files = sorted(os.listdir(base_path))
  regular_files = []
  if subpath == '':
    directories = []
  else:
    directories = ['..']
  for file in files:
    if os.path.isdir(os.path.join(base_path, file)):
      directories.append(file)
    else:
      regular_files.append(file)
  directories = [{'type': 'directory', 'text': dir + '/'} for dir in directories]
  regular_files = [{'type': 'regular_file', 'text': file} for file in regular_files]
  files = directories + regular_files
  files = [{'id': i, 'type': item['type'], 'text': item['text']} for i, item in enumerate(files)]
  return jsonify(files)

@app.route('/ls', methods=['GET'])
def ls_root():
  return ls('')

@app.route('/create', methods=['POST'])
def create():
  obj = request.json
  createname = obj['createfile']
  f_or_d = obj['f_or_d']
  createpath = os.path.join(app.config['DATA_DIR_PATH'], createname)
  if f_or_d == 1:
    os.mkdir(createpath)
  elif f_or_d == 0:
    f = open(createpath, 'w')
    f.close()
  return ''

@app.route('/rename', methods=['POST'])
def rename():
  obj = request.json
  newname = obj['newname']
  oldname = obj['oldname']
  newname_dir = os.path.split(newname)[0]
  oldname_dir = os.path.split(oldname)[0]
  newpath = os.path.join(app.config['DATA_DIR_PATH'], newname)
  oldpath = os.path.join(app.config['DATA_DIR_PATH'], oldname)
  if newname_dir == oldname_dir:
    os.rename(oldpath, newpath)
  else:
    shutil.move(oldpath, newpath)
  return ''

@app.route('/delete', methods=['POST'])
def delete():
  obj = request.json
  deletefile = obj['deletefile']
  deletepath = os.path.join(app.config['DATA_DIR_PATH'], deletefile)
  if os.path.isdir(deletepath):
    shutil.rmtree(deletepath)
  else:
    os.remove(deletepath)
  return ''

@app.route('/loadapp', methods=['POST'])
def load_app():
  obj = request.json
  xml_path = obj['path']
  selected_xml_dir = os.path.join(app.config['DATA_DIR_PATH'], xml_path)
  with open(selected_xml_dir) as f:
    str = f.read()
  return jsonify({
    'result': str
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

@app.route('/run', methods=['POST'])
def run():
  obj = request.json
  m = obj['xml_model']
  t = obj['tactics']
  i = obj['initial']
  s = obj['safety']
  c = obj['current_dir']
  d = obj['debug']
  err, res, eres, code = verify(m, t, i, s, c, d)
  http_code = 500 if err else 200
  return jsonify({
      'status': 'error' if err else 'ok'
    , 'retcode': code
    , 'result': res
    , 'err_res': eres
    }), http_code

@app.route('/stop', methods=['GET'])
def stop():
  os.killpg(app.config['VERIFY_ID'], signal.SIGTERM)
  return ''

app.config['JSON_AS_ASCII'] = False
app.config['DATA_DIR_PATH'] = '/home/opam/data'
app.config['VERIFY_ID'] = 0
if app.debug:
  app.config['SEND_FILE_MAX_AGE_DEFAULT'] = 0

if __name__ == '__main__':
  app.run(threaded = True)

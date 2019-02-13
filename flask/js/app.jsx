import React from 'react';
import { render } from 'react-dom';
import brace from 'brace';
import AceEditor from 'react-ace';
import request from 'superagent';
import mousetrap from 'mousetrap';
import $ from 'jquery';
import { Button, Modal, InputGroup, FormControl } from 'react-bootstrap';

import 'brace/mode/xml';
import 'brace/mode/scheme';
import 'brace/mode/plain_text';
import '../theme/github';

import styles from '../css/app.css';

const defaultTactics =
`;; Pre:(and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;; ------
;; CEX:(or (and (= y (- (/ 5433.0 3125.0))) (= x (/ 990679.0 1000000.0))) false)
(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
     (and (= y 0.0) (>= 0.0 x) (<= -0.70710678118 x)))

;; Pre:(and (<= x 1.0)
;;      (or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;;          (and (= y 0.0) (>= 0.0 x) (<= (- (/ 707107.0 1000000.0)) x))))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Continuous:true
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 5433.0 3125.0))) (= x (/ 990679.0 1000000.0))) false)
(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
    (and (<= y 0.0) (>= y (- (/ 707107.0 1000000.0))) (>= x (- (/ 707107.0 1000000.0))) (<= x (/ 707107.0 1000000.0))))

;; Pre:(let ((a!1 (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))))
;;   (and (<= x 1.0)
;;        (or a!1 (and (= y 0.0) (>= 0.0 x) (<= (- (/ 707107.0 1000000.0)) x)))
;;        (or a!1
;;            (and (<= y 0.0)
;;                 (>= y (- (/ 707107.0 1000000.0)))
;;                 (>= x (- (/ 707107.0 1000000.0)))
;;                 (<= x (/ 707107.0 1000000.0))))))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Guard:(>= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 1796817.0 1000000.0))) (= x (/ 499109.0 500000.0))) false)
(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
    (and (>= x 0.0) (<= x 1.0) (= y 0.0)))

;; Pre:(let ((a!1 (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))))
;; (let ((a!2 (and (or a!1 (<= x 1.0))
;;                 (or a!1 (and (>= x 0.0) (<= x 1.0) (= y 0.0))))))
;;   (or (and (<= x (/ 1.0 2.0)) (<= y (/ 1.0 2.0)) (>= x 0.0) (>= y 0.0)) a!2)))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(and (<= x (/ 1.0 2.0)) (<= y (/ 1.0 2.0)) (>= x 0.0) (>= y 0.0))
;; ------
;; CEX:(or (and (= y (- (/ 1796817.0 1000000.0))) (= x (/ 499109.0 500000.0))) false)
(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
    (and (= y 0.0) (>= 0.0 x) (<= -1.0 x)))

;; Pre:(and (<= x 1.0)
;;      (or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;;          (and (= y 0.0) (>= 0.0 x) (<= (- 1.0) x))))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Continuous:true
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 1796817.0 1000000.0))) (= x (/ 499109.0 500000.0))) false)
(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
    (and (<= y 0.0) (>= y (- 1.0)) (>= x (- 1.0)) (<= x 1.0)))

;; Pre:(let ((a!1 (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))))
;;   (and (<= x 1.0)
;;        (or a!1 (and (= y 0.0) (>= 0.0 x) (<= (- (/ 707107.0 1000000.0)) x)))
;;        (or a!1
;;            (and (<= y 0.0)
;;                 (>= y (- (/ 707107.0 1000000.0)))
;;                 (>= x (- (/ 707107.0 1000000.0)))
;;                 (<= x (/ 707107.0 1000000.0))))
;;        (or a!1 (and (= y 0.0) (>= 0.0 x) (<= (- 1.0) x)))
;;        (or a!1 (and (<= y 0.0) (>= y (- 1.0)) (>= x (- 1.0)) (<= x 1.0)))))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Guard:(>= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 1802123.0 1000000.0))) (= x (/ 499821.0 500000.0))) false)
(or (and (= y 0.0) (<= 0.0 x) (>= (/ 707107.0 1000000.0) x))
    (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0))))

;; Pre:(let ((a!1 (and (<= x (/ 1.0 2.0)) (<= y (/ 1.0 2.0)) (>= x 0.0) (>= y 0.0))))
;; (let ((a!2 (and (or a!1 (<= x 1.0))
;;                 (or (and (= y 0.0) (<= 0.0 x) (>= (/ 707107.0 1000000.0) x))
;;                     (and (>= x 0.0)
;;                          (<= x (/ 1.0 2.0))
;;                          (>= y 0.0)
;;                          (<= y (/ 1.0 2.0)))))))
;;   (or a!1 a!2)))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(and (<= x (/ 1.0 2.0)) (<= y (/ 1.0 2.0)) (>= x 0.0) (>= y 0.0))
;; ------
;; CEX:(or (and (= y (- (/ 1802123.0 1000000.0))) (= x (/ 499821.0 500000.0))) false)
(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
    (and (= y 0.0) (>= 0.0 x) (<= -0.70710678118 x)))

;; Pre:(and (<= x 1.0)
;;      (or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;;          (and (= y 0.0) (>= 0.0 x) (<= (- (/ 707107.0 1000000.0)) x))))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Continuous:true
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 1802123.0 1000000.0))) (= x (/ 499821.0 500000.0))) false)
(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
    (and (<= y 0.0) (>= y (- (/ 707107.0 1000000.0))) (>= x (- (/ 707107.0 1000000.0))) (<= x (/ 707107.0 1000000.0))))
`

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
        xml_path: selectedFilePath
      , xml_model: ""
      , tactics: defaultTactics
      , initial: "x >= 0.0 & x <= 0.5 & y >= 0.0 & y <= 0.5"
      , safety: "x <= 1.0"
      , result: ""
      , debug: false
      , status: ""
      , retcode: 0
      , resStyle: { color: '#00000' }
      , showSaveModal: false
      , cpath: "/"
      , resFname: ""
    };
    this.handleLoad = this.handleLoad.bind(this);
    this.handleCheckDebug = this.handleCheckDebug.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
    this.handleClickSaveBtn = this.handleClickSaveBtn.bind(this);
    this.handleSaveResult = this.handleSaveResult.bind(this);
    this.handleShowSaveModal = this.handleShowSaveModal.bind(this);
    this.handleCloseSaveModal = this.handleCloseSaveModal.bind(this);
  }

  handleSubmit(event) {
    event.preventDefault();
    request
      .post('/run')
      .send({
          xml_model: this.state.xml_model
        , tactics: this.state.tactics
        , initial: this.state.initial
        , safety: this.state.safety
        , current_dir: this.state.xml_path
        , debug: this.state.debug
      })
      .end((err, res) => {
        if (err) {
          this.setState({
              status: res.body.status
            , retcode: res.body.retcode
            , result: "ERROR\nreturn code:" + res.body.retcode + "\n\n" + res.body.err_res
            , resStyle: { color: '#ff0000' }
          });
        } else {
          this.setState({
              status: res.body.status
            , retcode: res.body.retcode
            , result: res.body.result
            , resStyle: { color: '#000000' }
          });
        }
      });
  }

  handleCheckDebug() {
    this.setState({
      debug: document.getElementById('debug').checked
    });
  }

  handleClickSaveBtn(event) {
    event.preventDefault();
    var save_str = this.defEditor.getValue();
    request
      .post('/save')
      .send({
          save_path: this.state.xml_path
        , save_str: save_str
      })
      .end((err, res) => {
        if (err) {
          alert("save error!");
        }
      });
  }

  handleSaveResult(e) {
    e.preventDefault();
    request
      .post('/saveResult')
      .send({
          save_path: this.state.cpath + this.state.resFname
        , save_str: this.state.result
      })
      .end((err, res) => {
        if (err) {
          alert("save error!");
        }
      });
    $('#closeSaveModalBtn').click();
  }

  handleShowSaveModal() {
    var path = "";
    this.state.xml_path.split("/").map((item, i, array) => {
      if (i !== array.length-1) {
        path += item + "/";
      }
    });
    this.setState({
        showSaveModal: true
      , cpath: path
    });
  }

  handleCloseSaveModal() {
    this.setState({ showSaveModal: false });
  }

  handleLoad() {
    request
      .post('/loadapp')
      .send({
        path: this.state.xml_path
      })
      .end((err, res) => {
        if (err) {
          console.log('load error');
        } else {
          this.setState({
            xml_model: res.body.result
          });
          this.defEditor.setValue(this.state.xml_model, -1);
          $('#fileNameWindow').append(this.state.xml_path);
        }
      });
  }

  componentDidMount() {
    this.handleLoad();
    mousetrap.bind(['command+return', 'ctrl+enter', 'f5'], () => {
      console.log('shortcut');
      $('#valbtn').click();
    });
  }

  render() {
    let fontSize = 12;
    let showPrintMargin = false;
    let highlightActiveLine = false;

    return (
      <div>
        <header>
        </header>
        <form>
          <div className={styles.DefContainer}>
            <dl>
              <dt>
                <h5>Definition - file : <span id="fileNameWindow"></span></h5>
              </dt>
              <dd>
                <AceEditor name="xml_model" mode="xml" theme="github"
                  width="598px" height="350px" value={this.state.xml_model}
                  fontSize={fontSize}
                  showPrintMargin={showPrintMargin}
                  onChange={(val) => this.setState({ xml_model: val })}
                  onLoad={(editor) => this.defEditor = editor} wrapEnabled={true}
                  commands={[{
                    name: 'validate',
                    bindKey: { win: 'Ctrl-Enter|f5', mac: 'Command-Return|Ctrl-Return|f5' },
                    exec: () => { $('#valbtn').click() }
                  }]} />
                <Button variant="success" onClick={this.handleClickSaveBtn}
                  className={styles.buttonStyle}>
                  Save
                </Button>
              </dd>
            </dl>
            <dl className={styles.shortEditors}>
              <dt><h5>Initial Condition</h5></dt>
              <dd className={styles.setMarginBtm}>
                <AceEditor name="initial" mode="plain_text" theme="github"
                  width="428px" height="48px" value={this.state.initial}
                  fontSize={fontSize}
                  showPrintMargin={showPrintMargin}
                  highlightActiveLine={highlightActiveLine}
                  onChange={(val) => this.setState({ initial: val })}
                  wrapEnabled={true}
                  commands={[{
                    name: 'validate',
                    bindKey: { win: 'Ctrl-Enter|f5', mac: 'Command-Return|Ctrl-Return|f5' },
                    exec: () => { $('#valbtn').click() }
                  }]} />
              </dd>
              <dt><h5>Safety Condition</h5></dt>
              <dd>
                <AceEditor name="safety" mode="plain_text" theme="github"
                  width="428px" height="48px" value={this.state.safety}
                  fontSize={fontSize}
                  showPrintMargin={showPrintMargin}
                  highlightActiveLine={highlightActiveLine}
                  onChange={(val) => this.setState({ safety: val })}
                  wrapEnabled={true}
                  commands={[{
                    name: 'validate',
                    bindKey: { win: 'Ctrl-Enter|f5', mac: 'Command-Return|Ctrl-Return|f5' },
                    exec: () => { $('#valbtn').click() }
                  }]} />
              </dd>
            </dl>
          </div>
          <div className={styles.ResultContainer}>
            <dl><dt><h5>Tactics</h5></dt>
              <dd><AceEditor name="tactics" mode="scheme" theme="github"
                    width="598px" height="350px" value={this.state.tactics}
                    fontSize={fontSize}
                    showPrintMargin={showPrintMargin}
                    onChange={(val) => this.setState({ tactics: val })}
                    wrapEnabled={true}
                    commands={[{
                      name: 'validate',
                      bindKey: { win: 'Ctrl-Enter|f5', mac: 'Command-Return|Ctrl-Return|f5' },
                      exec: () => { $('#valbtn').click() }
                    }]} />
              </dd>
              <div className={styles.buttonContainerStyle}>
                <div className={styles.debugStyle}>

                  <label className={styles.labelStyle}>
                    <input type="checkbox" id="debug" onClick={this.handleCheckDebug}
                      className={styles.checkboxStyle}/>
                    debug mode
                  </label>
                </div>
                <Button id="valbtn" variant="success" className={styles.buttonStyle} onClick={this.handleSubmit}>
                  Validate
                </Button>
              </div>
            </dl>
            <dl className={styles.resultDl}><dt><h5>Result</h5></dt>
              <dd>
                <textarea name="result" value={this.state.result} readOnly
                  className={styles.textareaStyle} style={this.state.resStyle} />
              </dd>
              <Button variant="success" onClick={this.handleShowSaveModal}
                className={styles.buttonStyle}>
                Save
              </Button>

              <Modal show={this.state.showSaveModal} onHide={this.handleCloseSaveModal}>
                <Modal.Header closeButton>
                  <Modal.Title>Save Result to File</Modal.Title>
                </Modal.Header>
                <Modal.Body>
                  <InputGroup className="mb-3">
                    <InputGroup.Prepend>
                      <InputGroup.Text>{this.state.cpath}</InputGroup.Text>
                    </InputGroup.Prepend>
                    <FormControl type="text" placeholder="Result File Name" value={this.state.resFname} onChange={(e) => this.setState({ resFname: e.target.value })}/>
                    <InputGroup.Append>
                      <Button variant="success" onClick={this.handleSaveResult}>
                        Save
                      </Button>
                    </InputGroup.Append>
                  </InputGroup>
                  <input type="button" id="closeSaveModalBtn" style={{ display: 'none' }} onClick={this.handleCloseSaveModal} />
                </Modal.Body>
              </Modal>

            </dl>
          </div>
        </form>
      </div>
    );
  }
}

render(<App />, document.getElementById('app'));

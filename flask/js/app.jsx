import React from 'react';
import { render } from 'react-dom';
import brace from 'brace';
import AceEditor from 'react-ace';
import request from 'superagent';
import mousetrap from 'mousetrap';
import $ from 'jquery';

import 'brace/mode/xml';
import 'brace/mode/scheme';
import 'brace/mode/plain_text';
import 'brace/theme/github';

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
    };
    this.handleLoad = this.handleLoad.bind(this);
    this.handleClick = this.handleClick.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
    this.handleClickSaveBtn = this.handleClickSaveBtn.bind(this);
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
            status: res.body.status,
            retcode: res.body.retcode,
            result: "ERROR\nretrun code:" + res.body.retcode + "\n" + res.body.result,
            resStyle: { color: '#ff0000' }
          });
        } else {
          this.setState({
            status: res.body.status,
            retcode: res.body.retcode,
            result: res.body.result,
            resStyle: { color: '#000000' }
          });
        }
      });
  }

  handleClick(newValue) {
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
        save_path: this.state.xml_path,
        save_str: save_str
      })
      .end(function(err, res) {
        if (err) {
          alert("save error!");
        }
      });
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
          this.defEditor.setValue(this.state.xml_model);
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
    return (
      <div>
        <form onSubmit={this.handleSubmit}>
          <div className={styles.DefContainer}>
            <dl>
              <dt>
                System Definition - file : <span id="fileNameWindow"></span>
              </dt>
              <dd>
                <AceEditor name="xml_model" mode="xml" theme="github"
                  width="650px" height="350px" value={this.state.xml_model}
                  onChange={(val) => this.setState({ xml_model: val })}
                  onLoad={(editor) => this.defEditor = editor}
                  readOnly={this.state.readOnly} wrapEnabled={true} />
                <input id="saveBtn" type="button" value="Save" onClick={this.handleClickSaveBtn}/>
              </dd>
            </dl>
            <dl className={styles.shortEditors}>
              <dt>Initial Condition</dt>
              <dd className={styles.setMarginBtm}>
                <AceEditor name="initial" mode="plain_text" theme="github"
                  width="600px" height="50px" value={this.state.initial}
                  onChange={(val) => this.setState({ initial: val })}
                  wrapEnabled={true} />
              </dd>
              <dt>Safety Condition</dt>
              <dd>
                <AceEditor name="safety" mode="plain_text" theme="github"
                  width="600px" height="50px" value={this.state.safety}
                  onChange={(val) => this.setState({ safety: val })}
                  wrapEnabled={true} />
              </dd>
            </dl>
          </div>
          <div className={styles.ResultContainer}>
            <dl><dt>Tactics</dt>
              <dd><AceEditor name="tactics" mode="scheme" theme="github"
                    width="650px" height="350px" value={this.state.tactics}
                    onChange={(val) => this.setState({ tactics: val })}
                    wrapEnabled={true} />
              </dd>
              <input type="submit" value="Validate" id="valbtn"/>
              <input type="checkbox" id="debug" onClick={this.handleClick} />
              <label>debug mode</label>
            </dl>
            <dl className={styles.resultDl}><dt>Result</dt>
              <dd><textarea cols="73" rows="20" name="result" readOnly
                            value={this.state.result} style={this.state.resStyle} />
              </dd>
            </dl>
          </div>
        </form>
      </div>
    );
  }
}

render(<App />, document.getElementById('app'));

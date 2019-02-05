import React from 'react';
import { render } from 'react-dom';
import brace from 'brace';
import AceEditor from 'react-ace';
import request from 'superagent';
import mousetrap from 'mousetrap';

import 'brace/mode/xml';
import 'brace/mode/scheme';
import 'brace/mode/plain_text';
import 'brace/theme/github';

import styles from '../css/app.css';

const defaultTactics =
`;; Tactic for circle.xml: Initial location: 1, Initial condition: (and (= x 0.0) (= y 0.0)), Safe region: (<= x 1.0)
(and (= x 0.0) (= y 0.0))
(and (= x 0.0) (= y 0.0))
(and (= x 0.0) (= y 0.0))
(and (= x 0.0) (= y 0.0))
(and (= x 0.0) (= y 0.0))
`

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
        xml_path: selectedFilePath
      , xml_model: ""
      , tactics: defaultTactics
      , initial: "x == 0.0 & y == 0.0"
      , safety: "x <= 1.0"
      , result: ""
      , debug: false
    };
    this.handleLoad = this.handleLoad.bind(this);
    this.handleClick = this.handleClick.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
    this.handleClickSaveBtn = this.handleClickSaveBtn.bind(this);
  }

  componentWillMount() {
    mousetrap.bind(['command+return', 'ctrl+enter', 'f5'], () => {
      console.log('shortcut');
      this.handleSubmit();
      return false;
    });
  }

  handleSubmit() {
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
      .end(function(err, res){
        if (err) {
          this.setState({result: ""});
        } else {
          this.setState({result: res.body.result});
        }
      }.bind(this));
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
              <input type="submit" value="Validate" />
              <input type="checkbox" id="debug" onClick={this.handleClick} />
              <label>debug mode</label>
            </dl>
            <dl className={styles.resultDl}><dt>Result</dt>
              <dd><textarea cols="73" rows="20" name="result"
                            value={this.state.result} readOnly />
              </dd>
            </dl>
          </div>
        </form>
      </div>
    );
  }
}

render(<App />, document.getElementById('app'));

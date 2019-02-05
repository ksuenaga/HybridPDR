import React from 'react';
import { render } from 'react-dom';
import brace from 'brace';
import AceEditor from 'react-ace';
import request from 'superagent';

import 'brace/mode/xml';
import 'brace/mode/scheme';
import 'brace/mode/plain_text';
import 'brace/theme/github';

const defaultTactics = `
;; Tactic for circle.xml: Initial location: 1, Initial condition: (and (= x 0.0) (= y 0.0)), Safe region: (<= x 1.0)
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
          <dl>
          <dt>
            <p>System Definition - file : <span id="fileNameWindow"></span></p>
          </dt>
          <dd>
            <AceEditor
              mode="xml"
              theme="github"
              name="xml_model"
              width="650px" height="350px"
              onChange={(val) => this.setState({ xml_model: val })}
              value={this.state.xml_model}
              onLoad={(editor) => this.defEditor = editor}
              readOnly={this.state.readOnly}
            />
            <input id="saveBtn" type="button" value="Save" onClick={this.handleClickSaveBtn}/>
          </dd>
          <dt>Tactics</dt>
          <dd>
            <AceEditor
              mode="scheme"
              theme="github"
              name="tactics"
              width="650px" height="350px"
              onChange={(val) => this.setState({ tactics: val })}
              value={this.state.tactics}
            />
          </dd>
          <dt>Initial Condition</dt>
          <dd>
            <AceEditor
              mode="plain_text"
              theme="github"
              name="initial"
              width="650px" height="50px"
              onChange={(val) => this.setState({ initial: val })}
              value={this.state.initial}
            />
          </dd>
          <dt>Safety Condition</dt>
          <dd>
            <AceEditor
              mode="plain_text"
              theme="github"
              name="safety"
              width="650px" height="50px"
              onChange={(val) => this.setState({ safety: val })}
              value={this.state.safety}
            />
          </dd>
          <dt>Result</dt>
          <dd>
            <textarea cols="80" rows="20"
                      name="result"
                      value={this.state.result}
                      readOnly
              />
          </dd>
          </dl>
          <input type="submit" value="Validate" />
          <input type="checkbox" id="debug" onClick={this.handleClick} />
          <label>debug mode</label>
        </form>
      </div>
    );
  }
}

render(<App />, document.getElementById('app'));

import React from 'react';
import { render } from 'react-dom';
import brace from 'brace';
import AceEditor from 'react-ace';
import request from 'superagent';

import $ from "jquery";

import 'jquery.fancytree/dist/skin-lion/ui.fancytree.less';  // CSS or LESS

import {createTree} from 'jquery.fancytree';

import 'jquery.fancytree/dist/modules/jquery.fancytree.edit';
import 'jquery.fancytree/dist/modules/jquery.fancytree.filter';


import 'brace/mode/xml';
import 'brace/mode/ocaml'
import 'brace/theme/github';


class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
        xml_model: ""
      , tactics: ""
      , initial: ""
      , safety: ""
      , result: ""
      , debug: false
      , readOnly: true
    };
    this.handleClick = this.handleClick.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
    this.handleOnClickSaveBtn = this.handleOnClickSaveBtn.bind(this);
    this.setTree = this.setTree.bind(this);
  }

  handleSubmit(event) {
    event.preventDefault();
    var current_dir = document.getElementById("saveBtn").getAttribute("savePath");
    request
      .post('/run')
      .send({
          xml_model: this.state.xml_model
        , tactics: this.state.tactics
        , initial: this.state.initial
        , safety: this.state.safety
        , current_dir: current_dir
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

  handleOnClickSaveBtn(event) {
    event.preventDefault();
    var save_path = document.getElementById("saveBtn").getAttribute("savePath");
    if (save_path) {
      var save_str = this.defEditor.getValue();
      request
        .post('/save')
        .send({
          save_path: save_path,
          save_str: save_str
        })
        .end(function(err, res) {
          if (err) {
            alert("save error!");
          }
        });
    } else {
      alert("no file selected");
    }
  }

  setTree() {
    const tree = createTree('#tree', {
      extensions: ['edit', 'filter'],
      source: {
        url: "/getTree",
        cache: false
      },
      selectMode: 1,
      click: (event, data) => {
        event.preventDefault();
        request
          .post('/load')
          .send({
            xml_path: data.node.key
          })
          .end((err, res) => {
            if (err) {
              console.log("error!");
            } else {
              if (this.state.readOnly == true) {
                this.setState({
                  readOnly: false
                });
              }
              this.defEditor.setValue(res.body.result);
              $('#saveBtn').attr("savePath", res.body.xml_path);
              $('#fileNameWindow').empty();
              $('#fileNameWindow').prepend(data.node.title);
            }
          });
      }
    })
  }

  componentDidMount() {
    this.setTree();
  }

  render() {
    return (
      <div>
      <div id="tree"></div>

      <form onSubmit={this.handleSubmit}>
        <dl>
        <dt>
          Definition --- selected file  :
          <span id="fileNameWindow"></span>
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
          <input id="saveBtn" type="button" value="Save" onClick={this.handleOnClickSaveBtn}/>
        </dd>
        <dt>Tactics</dt>
        <dd>
          <AceEditor
            mode="ocaml"
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

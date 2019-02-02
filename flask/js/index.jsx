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
    };
    this.handleSubmit = this.handleSubmit.bind(this);
    this.handleChangeXmlmodel = this.handleChangeXmlmodel.bind(this);
    this.handleChangeTactics = this.handleChangeTactics.bind(this);
    this.handleChangeInitial = this.handleChangeInitial.bind(this);
    this.handleChangeSafety = this.handleChangeSafety.bind(this);
    this.handleChangeResult = this.handleChangeResult.bind(this);
    this.handleOnLoadAce = this.handleOnLoadAce.bind(this);
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
      })
      .end(function(err, res){
        if (err) {
          this.setState({result: ""});
        } else {
          this.setState({result: res.body.result});
        }
      }.bind(this));
  }

  handleChangeXmlmodel(newValue) {
    this.setState({
      xml_model: newValue
    });
  }

  handleChangeTactics(newValue) {
    this.setState({
      tactics: newValue
    });
  }

  handleChangeInitial(newValue) {
    this.setState({
      initial: newValue
    });
  }

  handleChangeSafety(newValue) {
    this.setState({
      safety: newValue
    });
  }

  handleChangeResult(event) {
    const target = event.target;
    const value = target.type === 'checkbox' ? target.checked : target.value;
    const name = target.name;
    this.setState({
      [name]: newValue
    });
  }

  handleOnLoadAce(editor) {
    window.defEditor = editor;
  }

  render() {
    return (
      <div>
      <div id="tree"></div>

      <form onSubmit={this.handleSubmit}>
        <dl>
        <dt>Definition</dt>
        <dd>
          <AceEditor
            mode="xml"
            theme="github"
            name="xml_model"
            width="650px" height="350px"
            onChange={this.handleChangeXmlmodel}
            value={this.state.xml_model}
            onLoad={this.handleOnLoadAce}
          />
        </dd>
        <dt>Tactics</dt>
        <dd>
          <AceEditor
            mode="xml"
            theme="github"
            name="tactics"
            width="650px" height="350px"
            onChange={this.handleChangeTactics}
            value={this.state.tactics}
          />
        </dd>
        <dt>Initial Condition</dt>
        <dd>
          <AceEditor
            mode="xml"
            theme="github"
            name="initial"
            width="650px" height="50px"
            onChange={this.handleChangeInitial}
            value={this.state.initial}
          />
        </dd>
        <dt>Safety Condition</dt>
        <dd>
          <AceEditor
            mode="xml"
            theme="github"
            name="safety"
            width="650px" height="50px"
            onChange={this.handleChangeSafety}
            value={this.state.safety}
          />
        </dd>
        <dt>Result</dt>
        <dd>
          <textarea cols="80" rows="20"
                    name="result"
                    value={this.state.result}
                    onChange={this.handleChangeResult}
                    readOnly
            />
        </dd>
        </dl>
        <input type="submit" value="Validate" />
      </form>
      </div>
    );
  }
}

render(<App />, document.getElementById('app'));

const tree = createTree('#tree', {
  extensions: ['edit', 'filter'],
  source: {
    url: "/getTree",
    cache: false
  },
  selectMode: 1,
  click: function(event, data) {
    event.preventDefault();
    request
      .post('/load')
      .send({
        xml_filename: data.node.title
      })
      .end(function(err, res) {
        if (err) {
          console.log("error!");
        } else {
          console.log('result: \n', res.body.result);
          window.defEditor.setValue(res.body.result);
        }
      });

    console.log("data.title", data.node.title);
  }
});

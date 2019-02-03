import React from 'react';
import { render } from 'react-dom';
import request from 'superagent';
import AceEditor from 'react-ace';
import brace from 'brace';
import $ from 'jquery';

import 'jquery.fancytree/dist/skin-lion/ui.fancytree.less';  // CSS or LESS

import { createTree } from 'jquery.fancytree';

import 'brace/mode/xml';
import 'brace/theme/github';
import 'jquery.fancytree/dist/modules/jquery.fancytree.edit';
import 'jquery.fancytree/dist/modules/jquery.fancytree.filter';


class DirTree extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
        selFile: "",
        selFilePath: "",
        selFileTxt: ""
    };
    this.setTree = this.setTree.bind(this);
  }

  handleClickStart() {
    console.log('start btn clicked');
    window.open('/project');
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
          .post('/preview')
          .send({
            xml_path: data.node.key
          })
          .end((err, res) => {
            if (err) {
              console.log("load preview error!");
            } else {
              this.prevEditor.setValue(res.body.result);
              this.setState({
                selFile: data.node.title,
                selFilePath: data.node.key
              });
              $('#previewFileName').empty();
              $('#previewFileName').prepend(data.node.title);
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
        <div>
          <input type="button" name="startbtn" value="start" onClick={this.handleClickStart} />
        </div>
        <div>
          <p>Preview - file : <span id="previewFileName"></span></p>
        </div>
        <AceEditor
          mode="xml"
          theme="github"
          name="xmlPreview"
          width="650px" height="350px"
          value={this.state.selFileTxt}
          onChange={(val) => this.setState({ selFileTxt: val })}
          onLoad={(editor) => this.prevEditor = editor}
          readOnly={true}
        />
      </div>
    );
  }
}

render(<DirTree />, document.getElementById('dirPage'));

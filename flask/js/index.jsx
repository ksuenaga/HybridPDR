import React from 'react';
import { render } from 'react-dom';
import request from 'superagent';
import ListGroup from 'react-bootstrap/ListGroup'

import '../css/index.css'

class Explorer extends React.Component {
  constructor(props) {
    super(props);
    this.state = {items: [], path: "/"};
    this.handleClick = this.handleClick.bind(this);
    this.handleLoad = this.handleLoad.bind(this);
  }

  componentDidMount() {
    window.addEventListener('load', this.handleLoad);
  }

  handleClick(e) {
    e.preventDefault();
    var elem = e.target;
    if (elem.tagName !== "A") {
      return false;
    }
    if (elem.className.includes("directory")) {
      if (elem.textContent === "../") {
        var pathArray = this.state.path.split('/');
        var path = "";
        for (var i = 0; i < pathArray.length - 2; i++) {
          path += pathArray[i];
          path += "/";
        }
        this.setState({path: path}, this.move);
      } else {
        this.setState({path: this.state.path + elem.textContent}, this.move);
      }
    } else {
      if (this.state.path === "/") {
        window.open('/project/' + elem.textContent);
      } else {
        window.open('/project/' + this.state.path.slice(1) + '/' + elem.textContent);
      }
    }
  }

  handleLoad(e) {
    this.move();
  }

  move() {
    request
      .get('/ls' + (this.state.path === "/" ? "" : this.state.path))
      .then(res => {
        this.setState({items: res.body});
      });
  }

  render() {
    return (
      <div>
        <header>
        </header>
        <div className="container">
          <BreadcrumbList path={this.state.path} />
          <div onClick={this.handleClick}>
            <FileList items={this.state.items} />
          </div>
        </div>
      </div>
    );
  }
}


const listGroupStyle = {
  border: '#dfe2e5',
};

const listGroupItemStyle = {
  padding: '6px 10px',
};

const aStyle = {
  color: '#0366d6',
  fontSize: '14px',
  cursor: 'pointer',
};

class FileList extends React.Component {
  render() {
    return (
      <ListGroup style={listGroupStyle}>
        {this.props.items.map(item => (
          <ListGroup.Item key={item.id} style={listGroupItemStyle}>
            <a className={item.type} style={aStyle}>{item.text}</a>
          </ListGroup.Item>
        ))}
      </ListGroup>
    );
  }
}

const breadcrumbListContainerStyle = {
  overflow: 'hidden',
  paddingLeft: '10px',
  marginBottom: '10px',
};

const breadcrumbListStyle = {
  float: 'left',
  fontSize: '16px',
};

class BreadcrumbList extends React.Component {
  constructor(props) {
    super(props);
  }

  handleClick(e) {
    e.stopPropagation();
  }

  render() {
    return (
      <div style={breadcrumbListContainerStyle}>
        {this.props.path.split("/").slice(1).map((item, i) => (
          <div key={i} style={breadcrumbListStyle}>
            <span onClick={this.handleClick}>/{item}</span>
          </div>
        ))}
      </div>
    );
  }
}

render(<Explorer />, document.getElementById('dirPage'));

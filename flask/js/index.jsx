import React from 'react';
import { render } from 'react-dom';
import request from 'superagent';

class Explorer extends React.Component {
  constructor(props) {
    super(props);
    this.state = {items: []};
    this.path = "/";
    this.handleClick = this.handleClick.bind(this);
    this.handleLoad = this.handleLoad.bind(this);
  }

  componentDidMount() {
    window.addEventListener('load', this.handleLoad);
  }

  handleClick(e) {
    e.preventDefault();
    var elem = e.target;
    if (elem.className === "directory") {
      if (elem.textContent === "../") {
        var pathArray = this.path.split('/');
        this.path = "";
        for (var i = 0; i < pathArray.length - 2; i++) {
          this.path += pathArray[i];
          this.path += "/";
        }
      } else {
        this.path = this.path + elem.textContent;
      }
      this.move();
    } else {
      window.open('/project');
    }
  }

  handleLoad(e) {
    this.move();
  }

  move() {
    request
      .get('/ls' + (this.path === "/" ? "" : this.path))
      .then(res => {
        this.setState({items: res.body});
      });
  }

  render() {
    return (
      <div onClick={this.handleClick}>
        <FileList items={this.state.items} />
      </div>
    );
  }
}

class FileList extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <ul>
        {this.props.items.map(item => (
          <li key={item.id}><a className={item.type}>{item.text}</a></li>
        ))}
      </ul>
    );
  }
}

render(<Explorer />, document.getElementById('dirPage'));

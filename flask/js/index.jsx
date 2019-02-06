import React from 'react';
import { render } from 'react-dom';
import request from 'superagent';
import ListGroup from 'react-bootstrap/ListGroup'

import styles from '../css/index.css'


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
        window.open('/project/' + this.state.path.slice(1) + elem.textContent);
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
        <div className={styles.containerStyle}>
          <div className={styles.descContainrStyle}>
            <h3 className={styles.headingStyle}>HybridPDR</h3>
            <em className={styles.descStyle}>Select a definition file of a hybrid system.</em>
          </div>
          <BreadcrumbList path={this.state.path} />
          <div onClick={this.handleClick}>
            <FileList items={this.state.items} />
          </div>
        </div>
      </div>
    );
  }
}


class FileList extends React.Component {
  render() {
    return (
      <ListGroup className={styles.listGroupStyle}>
        {this.props.items.map(item => (
          <ListGroup.Item key={item.id} className={styles.listGroupItemStyle}>
            <a className={item.type+' '+styles.aStyle}>{item.text}</a>
          </ListGroup.Item>
        ))}
      </ListGroup>
    );
  }
}


class BreadcrumbList extends React.Component {
  constructor(props) {
    super(props);
  }

  handleClick(e) {
    e.stopPropagation();
  }

  render() {
    return (
      <div className={styles.breadcrumbListContainerStyle}>
        {this.props.path.split("/").slice(1).map((item, i) => (
          <div key={i} className={styles.breadcrumbListStyle}>
            <span onClick={this.handleClick}>/{item}</span>
          </div>
        ))}
      </div>
    );
  }
}

render(<Explorer />, document.getElementById('dirPage'));

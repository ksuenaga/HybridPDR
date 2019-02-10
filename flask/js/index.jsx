import React from 'react'
import { render } from 'react-dom'
import PropTypes from 'prop-types'
import request from 'superagent'
import ListGroup from 'react-bootstrap/ListGroup'
import Modal from 'react-modal'

import styles from '../css/index.css'


Modal.setAppElement('#dirPage');

class Explorer extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
        items: []
      , path: "/"
      , modalIsOpen: false
      , renameVal: ""
      , crudPath: ""
    };
    this.handleClick = this.handleClick.bind(this);
    this.handleLoad = this.handleLoad.bind(this);
    this.handleRenameFile = this.handleRenameFile.bind(this);
    this.openModal = this.openModal.bind(this);
    this.afterOpenModal = this.afterOpenModal.bind(this);
    this.closeModal = this.closeModal.bind(this);
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

  handleRenameFile(e) {
    e.preventDefault();
    if (this.state.renameVal !== this.state.crudPath) {
      request
        .post('/rename')
        .send({
          newname: this.state.renameVal,
          oldname: this.state.crudPath
        })
        .end((err, res) => {
          if (err) {
            alert('rename error');
          } else {
            console.log('rename succeed');
          }
        });
    }
  }

  openModal(e) {
    var fname = e.target.parentNode.previousSibling.textContent;
    if (this.state.path === "/") {
      this.setState({
          renameVal: fname
        , crudPath: fname
      });
    } else {
      this.setState({
          renameVal: this.state.path.slice(1) + fname
        , crudPath: this.state.path.slice(1) + fname
      });
    }

    this.setState({ modalIsOpen: true });
  }

  afterOpenModal() {
    console.log('open modal');
  }

  closeModal() {
    this.setState({ modalIsOpen: false });
    this.move();
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
          <div>
            <FileList items={this.state.items} clickFile={this.handleClick} rename={this.openModal}/>
            <Modal contentLabel="rename modal"
              className={styles.modalStyle} overlayClassName={this.modalOverlayStyle}
              isOpen={this.state.modalIsOpen}
              onAfterOpen={this.afterOpenModal}
              onRequestClose={this.closeModal} >
              <div>
                <p>rename file</p>
                <button onClick={this.closeModal}>close</button>
              </div>
              <form onSubmit={this.handleRenameFile}>
                <input type="text" value={this.state.renameVal} onChange={(e) => this.setState({ renameVal: e.target.value })} />
                <input type="submit" value="rename" />
              </form>
            </Modal>
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
            <a className={item.type+' '+styles.aStyle} onClick={this.props.clickFile}>{item.text}</a>
            <div className={styles.rdBox}>
              <a className={styles.rename} onClick={this.props.rename}>Rename</a>
              <a className={styles.delete}>Delete</a>
            </div>
          </ListGroup.Item>
        ))}
      </ListGroup>
    );
  }
}
FileList.propTypes = {
  clickFile: PropTypes.func,
  rename: PropTypes.func
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

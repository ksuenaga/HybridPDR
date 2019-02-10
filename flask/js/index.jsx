import React from 'react';
import { render } from 'react-dom';
import PropTypes from 'prop-types';
import request from 'superagent';
import ListGroup from 'react-bootstrap/ListGroup';
import InputGroup from 'react-bootstrap/InputGroup';
import Form from 'react-bootstrap/Form';
import Button from 'react-bootstrap/Button';
import Modal from 'react-bootstrap/Modal';
import FormControl from 'react-bootstrap/FormControl';

import styles from '../css/index.css'


class Explorer extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
        items: []
      , path: "/"
      , showRenameModal: false
      , showDeleteModal: false
      , renameVal: ""
      , crudPath: ""
    };
    this.handleClick = this.handleClick.bind(this);
    this.handleLoad = this.handleLoad.bind(this);
    this.handleRenameFile = this.handleRenameFile.bind(this);
    this.handleDeleteFile = this.handleDeleteFile.bind(this);
    this.handleChangeTxtArea = this.handleChangeTxtArea.bind(this);
    this.handleShowRenameModal = this.handleShowRenameModal.bind(this);
    this.handleCloseRenameModal = this.handleCloseRenameModal.bind(this);
    this.handleShowDeleteModal = this.handleShowDeleteModal.bind(this);
    this.handleCloseDeleteModal = this.handleCloseDeleteModal.bind(this);
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

  handleChangeTxtArea(val) {
    this.setState({ renameVal: val });
  }

  handleShowRenameModal(e) {
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

    this.setState({ showRenameModal: true });
  }

  handleCloseRenameModal() {
    this.setState({ showRenameModal: false });
    this.move();
  }

  handleDeleteFile(e) {
    e.preventDefault();
    request
      .post('/delete')
      .send({
        deletefile: this.state.crudPath
      })
      .end((err, res) => {
        if (err) {
          alert('delete error');
        } else {
          console.log('delete succeed');
        }
      });
  }

  handleShowDeleteModal(e) {
    var fname = e.target.parentNode.previousSibling.textContent;
    if (this.state.path === "/") {
      this.setState({ crudPath: fname });
    } else {
      this.setState({ crudPath: this.state.path.slice(1) + fname });
    }

    this.setState({ showDeleteModal: true });
  }

  handleCloseDeleteModal() {
    this.setState({ showDeleteModal: false });
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
            <FileList items={this.state.items} clickFile={this.handleClick} rename={this.handleShowRenameModal} delete={this.handleShowDeleteModal}/>
            {/*
            <RenameModal show={this.state.showRenameModal} onHide={this.handleCloseRenameModal} renamefile={this.handleRenameFile} changetxt={this.handleChangeTxtArea} />
            */}
            <Modal show={this.state.showRenameModal} onHide={this.handleCloseRenameModal}>
              <Modal.Header closeButton>
                <Modal.Title>Rename File</Modal.Title>
              </Modal.Header>
              <Modal.Body>
                <Form>
                  <InputGroup className="mb-3">
                    <Form.Control type="text" value={this.state.renameVal} onChange={(e) => this.setState({ renameVal: e.target.value })}/>
                    <InputGroup.Append>
                      <Button variant="info" onClick={this.handleRenameFile}>
                        rename
                      </Button>
                    </InputGroup.Append>
                  </InputGroup>
                </Form>
              </Modal.Body>
            </Modal>
            <Modal show={this.state.showDeleteModal} onHide={this.handleCloseDeleteModal}>
              <Modal.Header closeButton>
                <Modal.Title>Delete File</Modal.Title>
              </Modal.Header>
              <Modal.Body>
                <p>Are you sure to delete this file or directory?</p>
                <FormControl type="text" value={this.state.crudPath} readOnly />
              </Modal.Body>
              <Modal.Footer>
                <Button variant="warning" onClick={this.handleDeleteFile}>
                  delete
                </Button>
                <Button variant="light" onClick={this.handleCloseDeleteModal}>
                  cancel
                </Button>
              </Modal.Footer>
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
              <a className={styles.delete} onClick={this.props.delete}>Delete</a>
            </div>
          </ListGroup.Item>
        ))}
      </ListGroup>
    );
  }
}
FileList.propTypes = {
  clickFile: PropTypes.func,
  rename: PropTypes.func,
  delete: PropTypes.func
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

/*
class RenameModal extends React.Component {
  render() {
    return (
      <Modal {...this.props} size="lg" aria-labelledby="contained-modal-title-vcenter" centered renameval={this.props.renameVal}>
        <Modal.Header closeButton>
          <Modal.Title id="contained-modal-title-vcenter">Rename File</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          <Form>
            <InputGroup className="mb-3">
              <Form.Control type="text" value={renameval} onChange={this.props.changetxt}/>
              <InputGroup.Append>
                <Button variant="info" onClick={this.props.renamefile}>
                  rename
                </Button>
              </InputGroup.Append>
            </InputGroup>
          </Form>
        </Modal.Body>
      </Modal>
    );
  }
}
RenameModal.propTypes = {
  renamefile: PropTypes.func,
  changetxt: PropTypes.func
};
*/

render(<Explorer />, document.getElementById('dirPage'));

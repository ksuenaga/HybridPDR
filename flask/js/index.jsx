import React from 'react';
import { render } from 'react-dom';
import PropTypes from 'prop-types';
import request from 'superagent';
import $ from 'jquery';
import { ListGroup, InputGroup, Button, Modal, FormControl, FormCheck, OverlayTrigger, Tooltip, Breadcrumb } from 'react-bootstrap';

import styles from '../css/index.css'


class Explorer extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
        items: []
      , path: "/"
      , showRenameModal: false
      , showDeleteModal: false
      , showCreateModal: false
      , createVal: ""
      , renameVal: ""
      , crudPath: ""
      , radioChecked: 0
    };
    this.handleClick = this.handleClick.bind(this);
    this.handleLoad = this.handleLoad.bind(this);
    this.handleBreadcrumbMove = this.handleBreadcrumbMove.bind(this);
    this.handleRenameFile = this.handleRenameFile.bind(this);
    this.handleDeleteFile = this.handleDeleteFile.bind(this);
    this.handleCreateFile = this.handleCreateFile.bind(this);
    this.handleChangeRadio = this.handleChangeRadio.bind(this);
    this.handleShowRenameModal = this.handleShowRenameModal.bind(this);
    this.handleCloseRenameModal = this.handleCloseRenameModal.bind(this);
    this.handleShowDeleteModal = this.handleShowDeleteModal.bind(this);
    this.handleCloseDeleteModal = this.handleCloseDeleteModal.bind(this);
    this.handleShowCreateModal = this.handleShowCreateModal.bind(this);
    this.handleCloseCreateModal = this.handleCloseCreateModal.bind(this);
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

  handleBreadcrumbMove(dirpath) {
    this.setState({ path: dirpath }, this.move);
  }

  handleRenameFile(e) {
    e.preventDefault();
    if (this.state.renameVal !== this.state.crudPath) {
      request
        .post('/rename')
        .send({
            newname: this.state.renameVal
          , oldname: this.state.crudPath
          , current_dir: this.state.path.slice(1)
        })
        .end((err, res) => {
          if (err) {
            alert('rename error');
          } else {
            console.log('rename succeed');
            $('#closeRenameBtn').click();
          }
        });
    }
  }

  handleShowRenameModal(e) {
    var fname = e.target.nextSibling.textContent;
    this.setState({
        renameVal: fname
      , crudPath: fname
      , showRenameModal: true
    });
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
          $('#cancelBtn').click();
        }
      });
  }

  handleShowDeleteModal(e) {
    var fname = e.target.nextSibling.textContent;
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

  handleCreateFile(e) {
    e.preventDefault();
    request
      .post('/create')
      .send({
        createfile: this.state.crudPath + this.state.createVal,
        f_or_d: this.state.radioChecked
      })
      .end((err, res) => {
        if (err) {
          alert('create error');
        } else {
          console.log('create succeed');
          $('#closeCreateBtn').click();
        }
      });
  }

  handleChangeRadio() {
    if (this.state.radioChecked === 0) {
      this.setState({ radioChecked: 1 });
    } else {
      this.setState({ radioChecked: 0 });
    }
  }

  handleShowCreateModal() {
    this.setState({
        showCreateModal: true
      , crudPath: this.state.path.slice(1)
      , createVal: ""
    });
  }

  handleCloseCreateModal() {
    this.setState({ showCreateModal: false });
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
          <BreadcrumbList path={this.state.path} create={this.handleShowCreateModal} move={this.handleBreadcrumbMove}/>

          <Modal show={this.state.showCreateModal} onHide={this.handleCloseCreateModal}>
            <Modal.Header closeButton>
              <Modal.Title>Create File</Modal.Title>
            </Modal.Header>
            <Modal.Body>
              <div key="radio" className="mb-3">
                <FormCheck inline label="file" type="radio" name="formHorizontalRadios" value="file" checked={this.state.radioChecked === 0 ? true : false} onChange={this.handleChangeRadio}/>
                <FormCheck inline label="directory" type="radio" name="formHorizontalRadios" value="directory" checked={this.state.radioChecked === 1 ? true : false} onChange={this.handleChangeRadio} />
              </div>
              <InputGroup className="mb-3">
                <InputGroup.Prepend>
                  <InputGroup.Text>{this.state.crudPath}</InputGroup.Text>
                </InputGroup.Prepend>
                <FormControl type="text" placeholder="New File or Directory" value={this.state.createVal} onChange={(e) => this.setState({ createVal: e.target.value })}/>
                <InputGroup.Append>
                  <Button variant="success" onClick={this.handleCreateFile}>
                    create
                  </Button>
                </InputGroup.Append>
              </InputGroup>
              <input type="button" id="closeCreateBtn" style={{ display: 'none' }} onClick={this.handleCloseCreateModal} />
            </Modal.Body>
          </Modal>

          <div>
            <FileList items={this.state.items} clickFile={this.handleClick} rename={this.handleShowRenameModal} delete={this.handleShowDeleteModal}/>

            <Modal show={this.state.showRenameModal} onHide={this.handleCloseRenameModal}>
              <Modal.Header closeButton>
                <Modal.Title>Rename File</Modal.Title>
              </Modal.Header>
              <Modal.Body>
                <InputGroup className="mb-3">
                  <InputGroup.Prepend>
                    <InputGroup.Text>{this.state.path.slice(1)}</InputGroup.Text>
                  </InputGroup.Prepend>
                  <FormControl type="text" value={this.state.renameVal} onChange={(e) => this.setState({ renameVal: e.target.value })}/>
                  <InputGroup.Append>
                    <Button variant="info" onClick={this.handleRenameFile}>
                      rename
                    </Button>
                  </InputGroup.Append>
                </InputGroup>
                <input type="button" id="closeRenameBtn" style={{ display: 'none' }} onClick={this.handleCloseRenameModal} />
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
                <Button variant="danger" onClick={this.handleDeleteFile}>
                  delete
                </Button>
                <Button id="cancelBtn" variant="light" onClick={this.handleCloseDeleteModal}>
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


class BreadcrumbList extends React.Component {
  constructor(props) {
    super(props);
    this.handleClickHome = this.handleClickHome.bind(this);
    this.handleClickDir = this.handleClickDir.bind(this);
  }

  handleClickHome(e) {
    e.stopPropagation();
    this.props.move("/");
  }

  handleClickDir(e) {
    e.stopPropagation();
    var itemtxt = e.target.textContent;
    console.log(itemtxt);
    console.log(this.props.path);
    this.props.move(this.props.path.split(itemtxt)[0] + itemtxt + "/");
  }

  render() {
    var breadPath = this.props.path.split("/").slice(1);
    return (
      <Breadcrumb className={styles.breadcrumbListContainerStyle}>
        {breadPath[0] === "" ? <Breadcrumb.Item active>data</Breadcrumb.Item> : <Breadcrumb.Item onClick={this.handleClickHome}>data</Breadcrumb.Item>}
        {breadPath.map((item, i, array) => (
          i === array.length-2 ? <Breadcrumb.Item key={i} active>{item}</Breadcrumb.Item> : <Breadcrumb.Item key={i} onClick={this.handleClickDir}>{item}</Breadcrumb.Item>
        ))}
        <div className={styles.newDiv}>
          <OverlayTrigger placement="top"
            overlay={
              <Tooltip>Add new file or directory</Tooltip>
            }>
            <a className={styles.new} onClick={this.props.create}><i className="fas fa-plus"></i></a>
          </OverlayTrigger>
        </div>
      </Breadcrumb>
    );
  }
}
BreadcrumbList.propTypes = {
    create: PropTypes.func
  , move: PropTypes.func
};


class FileList extends React.Component {
  render() {
    return (
      <ListGroup className={styles.listGroupStyle}>
        {this.props.items.map(item => (
          <ListGroup.Item key={item.id} className={styles.listGroupItemStyle}>
            {item.type==='directory' ? <span><i className="far fa-folder"></i>&ensp;</span> : <span>&thinsp;<i className="far fa-file"></i>&ensp;</span>}
            <a className={item.type+' '+styles.aStyle} onClick={this.props.clickFile}>{item.text}</a>
            <div className={styles.rdBox}>
              <OverlayTrigger placement="top"
                overlay={
                  <Tooltip>Rename file or directory</Tooltip>
                }>
                <a className={styles.rename} onClick={this.props.rename}>
                  <i className="fas fa-pen"></i>
                  <span className={styles.invisibleSpan}>{item.text}</span>
                </a>
              </OverlayTrigger>
              <OverlayTrigger placement="top"
                overlay={
                  <Tooltip>Delete file or directory</Tooltip>
                }>
                <a className={styles.delete} onClick={this.props.delete}>
                  <i className="far fa-trash-alt"></i>
                  <span className={styles.invisibleSpan}>{item.text}</span>
                </a>
              </OverlayTrigger>
            </div>
          </ListGroup.Item>
        ))}
      </ListGroup>
    );
  }
}
FileList.propTypes = {
    clickFile: PropTypes.func
  , rename: PropTypes.func
  , delete: PropTypes.func
};


render(<Explorer />, document.getElementById('dirPage'));

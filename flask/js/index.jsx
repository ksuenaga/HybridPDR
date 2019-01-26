const React = require('react');
const ReactDOM = require('react-dom');
const request = require('superagent');


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
    this.handleChange = this.handleChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
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

  handleChange(event) {
    const target = event.target;
    const value = target.type === 'checkbox' ? target.checked : target.value;
    const name = target.name;
    this.setState({
      [name]: value
    });
  }

  render() {
    return (
      <form onSubmit={this.handleSubmit}>
        <dl>
        <dt>Definition</dt>
        <dd>
          <textarea cols="80" rows="20"
                    name="xml_model"
                    value={this.state.xml_model}
                    onChange={this.handleChange}
            />
        </dd>
        <dt>Tactics</dt>
        <dd>
          <textarea cols="80" rows="10"
                    name="tactics"
                    value={this.state.tactics}
                    onChange={this.handleChange}
            />
        </dd>
        <dt>Initial Condition</dt>
        <dd>
          <input size="80"
                 name="initial"
                 value={this.state.initial}
                 onChange={this.handleChange}
            />
        </dd>
        <dt>Safety Condition</dt>
        <dd>
          <input size="80"
                 name="safety"
                 value={this.state.safety}
                 onChange={this.handleChange}
            />
        </dd>
        <dt>Result</dt>
        <dd>
          <textarea cols="80" rows="20"
                    name="result"
                    value={this.state.result}
                    onChange={this.handleChange}
                    readOnly
            />
        </dd>
        </dl>
        <input type="submit" value="Validate" />
      </form>
    );
  }
}

ReactDOM.render(<App />, document.getElementById('app'));

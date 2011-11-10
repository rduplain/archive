from flask import Flask, jsonify, render_template, request


app = Flask(__name__)


@app.route('/', methods=['GET', 'POST'])
def index():
    if request.method == 'POST' and request.is_xhr:
        app.logger.debug('request.form: ' + repr(request.form))
        # See success callback in index.html.
        # Return the form as-is to provide a round-trip demo.
        return jsonify(**request.form)
    return render_template('index.html')


if __name__ == '__main__':
    app.run(debug=True)

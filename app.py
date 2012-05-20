import os

from flask import Flask, make_response, render_template, request


app = Flask(__name__)


@app.route('/templated.svg')
def templated_svg():
    "Example using a template in the templates directory."
    width = request.args.get('width', '800')
    height = request.args.get('height', '600')
    svg = render_template('letters.svg', width=width, height=height)
    response = make_response(svg)
    response.content_type = 'image/svg+xml'
    return response


@app.route('/database.svg')
def database_svg():
    "Example using a string stored somewhere."
    # Read in ./images/letters.svg.
    svg = open(os.path.join(app.root_path, 'images', 'letters.svg')).read()
    response = make_response(svg)
    response.content_type = 'image/svg+xml'
    return response


if __name__ == '__main__':
    app.run(debug=True)

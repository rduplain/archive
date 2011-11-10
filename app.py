import os

from flask import Flask, render_template, request

from werkzeug.utils import secure_filename


# Set this configuration to absolute or relative path to upload directory.
UPLOAD_FOLDER = 'files'


def init_app(app):
    if not os.path.isabs(app.config['UPLOAD_FOLDER']):
        folder = os.path.join(os.getcwd(), app.config['UPLOAD_FOLDER'])
        app.config['UPLOAD_FOLDER'] = folder
    if not os.path.exists(app.config['UPLOAD_FOLDER']):
        os.makedirs(app.config['UPLOAD_FOLDER'])


app = Flask(__name__)
app.config.from_object(__name__)
init_app(app)


def save_file(filestorage, app=app):
    filename = secure_filename(filestorage.filename)
    filepath = os.path.join(app.config['UPLOAD_FOLDER'], filename)
    filestorage.save(filepath)


def save_files(request=request, app=app):
    for _, filestorage in request.files.iteritems():
        # Workaround: larger uploads cause a dummy file named '<fdopen>'.
        # See the Flask mailing list for more information.
        if filestorage.filename not in (None, 'fdopen', '<fdopen>'):
            save_file(filestorage, app=app)


@app.route('/', methods=['GET', 'POST'])
def index():
    if request.method == 'POST':
        save_files()
        return 'Uploaded'
    if request.method == 'GET':
        return render_template('index.html')


if __name__ == '__main__':
    app.run(debug=True)

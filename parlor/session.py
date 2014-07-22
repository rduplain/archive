import copy
import datetime as dt
import json
import uuid

from jeni import Provider, UnsetError, annotate
from sqlalchemy import Column, DateTime, Integer, Text
from sqlalchemy.ext.declarative import declarative_base


def default_uid_factory():
    return str(uuid.uuid4())


now = dt.datetime.now


class SessionModelMixin(object):
    """A login session keyed by uuid, distinct from web session."""
    id = Column(Integer, primary_key=True, index=True)
    uid = Column(Text, default=default_uid_factory, index=True)
    created = Column(DateTime, default=now)
    modified = Column(DateTime, default=now, onupdate=now)
    data_modified = Column(DateTime, default=now)
    data = Column(Text, nullable=True)


def init(injector_class, sql_ns='session'):
    def sql_note(name):
        return '{}_{}'.format(sql_ns, name)

    Base = declarative_base()

    class Session(SessionModelMixin, Base):
        __tablename__ = 'session'
        user_uid = Column(Text, index=True)

    @annotate
    def load_session(query: sql_note('query'), session_uid: 'session_uid'):
        if session_uid is None:
            return None
        return query(Session).filter_by(uid=session_uid).first()

    @annotate
    def save_session(data, db: sql_note('session'), record=None):
        if record is None:
            record = db.query(Session).filter_by(uid=data['uid']).first()
        if record is None:
            record = Session(uid=data['uid'], user_uid=data.get('user_uid'))
            db.add(record)
        record.data = json.dumps(data)
        record.data_modified = now()
        db.commit()

    @injector_class.provider('session')
    class SessionProvider(Provider):
        @annotate
        def __init__(self, db: sql_note('session')):
            self.db = db
            self.session = None
            self.record = None
            self.session_orig = None

        @annotate
        def get(self, load_session: annotate.partial(load_session), name=None):
            if self.session is None:
                self.record = load_session()
                if self.record is None:
                    self.session = {}
                else:
                    self.session = json.loads(self.record.data)
                self.session_orig = copy.copy(self.session)
            if name is not None:
                if name not in self.session:
                    raise UnsetError()
                return self.session[name]
            return self.session

        def close(self):
            if 'uid' in self.session and self.session != self.session_orig:
                save_session(data=self.session, db=self.db, record=self.record)

    @annotate
    def session_start(
            user_uid,
            db: sql_note('session'),
            session: 'session',
            uid_factory: 'uid_factory',
            clear=False):
        record = Session()
        record.uid = uid_factory()
        record.user_uid = user_uid
        if clear:
            session.clear()
        record.data = json.dumps(session)
        session['uid'] = record.uid
        db.add(record)
        db.commit()
        return session['uid']

    @injector_class.factory('session_start')
    @annotate
    def session_start_factory(start: annotate.partial(session_start)):
        return start

    @annotate
    def session_end(db: sql_note('session'), session: 'session'):
        session_uid = session.get('uid')
        session.clear()
        if session_uid is None:
            raise ValueError('session has no uid.')
        rowcount = db.query(Session).filter_by(uid=session_uid).delete()
        if rowcount == 0:
            raise ValueError('No session records for {}.'.format(session_uid))
        db.commit()
        return session_uid

    @injector_class.factory('session_end')
    @annotate
    def session_end_factory(end: annotate.partial(session_end)):
        return end

    return Base

"Demo ExpressionFile with pandas on full data set of US baby names."

# data: http://www.ssa.gov/oact/babynames/limits.html
# deps: pip install pandas

from __future__ import print_function

import csv
import glob
import os
import re
import sys
import time

import pandas as pd
from sqlalchemy import create_engine
from sqlalchemy import Column, Integer, String, Text
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker

from expression_file import ExpressionFile


Base = declarative_base()


class BirthCount(Base):
    __tablename__ = 'birth_count'

    id = Column(Integer, primary_key=True)
    name = Column(Text, index=True)
    sex = Column(String(1))
    births = Column(Integer)
    year = Column(Integer)


def stitch_names(csv_writer, basedir='.'):
    "Combine all years of baby name data into a single .csv file."
    csv_writer.writerow(('name', 'sex', 'births', 'year'))
    year_re = re.compile(r'yob(\d{4})\.txt')
    count = 0
    for filepath in sorted(glob.glob(os.path.join(basedir, 'yob*.txt'))):
        year_match = year_re.search(filepath)
        if year_match is None:
            continue
        year = year_match.group(1)
        for row in csv.reader(open(filepath)):
            count += 1
            csv_writer.writerow(list(row) + list((year,)))
    return count


def create_database(dict_reader, url='sqlite:///names.db'):
    engine = create_engine(url)
    Base.metadata.drop_all(engine)
    Base.metadata.create_all(engine)
    Session = sessionmaker(bind=engine)
    session = Session()

    count, at_a_time = 0, 1000
    for record in dict_reader:
        session.add(BirthCount(**record))
        count += 1
        if count >= at_a_time:
            session.commit()
            count = 0
    session.commit()


def demo_read_directly(filepath='names.csv'):
    return pd.read_csv(filepath)


def demo_read_all(url='sqlite:///names.db'):
    engine = create_engine(url)
    fd = ExpressionFile(engine, BirthCount.__table__.c.values())
    return pd.read_csv(fd)


def demo_query_mary(url='sqlite:///names.db'):
    engine = create_engine(url)
    columns = BirthCount.__table__.c
    name = columns.name
    births = columns.births
    fd = ExpressionFile(engine, [name, births], where=name=='Mary')
    return pd.read_csv(fd)


def demo_query_mary_sorted(url='sqlite:///names.db'):
    from sqlalchemy import sql

    engine = create_engine(url)
    columns = BirthCount.__table__.c
    name = columns.name
    sex = columns.sex
    births = columns.births
    year = columns.year
    fd = ExpressionFile(
        engine,
        [name, births, year],
        where=sql.and_(
            name=='Mary',
            sex=='F',
            year.between(1901, 2000)),
        q=lambda q: q.order_by(births.desc()))
    return pd.read_csv(fd)


def main():
    names_csv_filepath = 'names.csv'
    if not os.path.exists(names_csv_filepath):
        names_writer = csv.writer(open(names_csv_filepath, 'w'))
        print('Stitching together all tables into a single csv ...')
        names_count = stitch_names(names_writer)
        if names_count == 0:
            print(
                'Download http://www.ssa.gov/oact/babynames/names.zip '
                'and unpack in the current working directory.',
                file=sys.stderr)
            os.remove(names_csv_filepath)
            sys.exit(1)

    names_db_filepath = 'names.db'
    names_db_url = 'sqlite:///{}'.format(names_db_filepath)
    if not os.path.exists(names_db_filepath):
        print('Building sqlite database ...')
        names_reader = csv.DictReader(open(names_csv_filepath))
        create_database(names_reader, url=names_db_url)

    start_time = time.time()
    print('Reading all records from csv file ... ')
    print(demo_read_directly(names_csv_filepath))
    print('... {0:.3f}s.'.format(time.time()-start_time))

    start_time = time.time()
    print('Reading all records from sqlite ...')
    print(demo_read_all(url=names_db_url))
    print('... {0:.3f}s.'.format(time.time()-start_time))

    start_time = time.time()
    print('Reading only Mary ...')
    print(demo_query_mary(url=names_db_url))
    print('... {0:.3f}s.'.format(time.time()-start_time))

    start_time = time.time()
    print('Reading 20th century Mary, sorted ...')
    print(demo_query_mary_sorted(url=names_db_url))
    print('... {0:.3f}s.'.format(time.time()-start_time))


if __name__ == '__main__':
    main()

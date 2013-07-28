from __future__ import print_function

import csv
import io

import filelike
from sqlalchemy.sql import select


class ExpressionFile(filelike.FileLikeBase):
    "Build CSV in file-like container given SQLAlchemy expression constructs."

    DEFAULT_BATCH = 100

    def __init__(self, engine, columns, where=None, q=None, batch=None):
        super(ExpressionFile, self).__init__()

        self._engine = engine
        self._columns = columns
        self._where = where

        if q is None:
            # Default to a pass-through.
            q = lambda q: q

        self._batch = batch or self.DEFAULT_BATCH
        self._q_fn = q

        self._yieldcount = None
        self._rowcount = None
        self._result = None

    @property
    def rowcount(self):
        "If this is 0, your consumer probably will not like the empty CSV."
        if self._rowcount is None:
            self._first_read()
        return self._rowcount

    def _first_read(self):
        if self._where is not None:
            statement = select(self._columns, self._where)
        else:
            statement = select(self._columns)
        statement = self._q_fn(statement)
        count_result = self._engine.execute(statement.alias('countme').count())
        self._rowcount = count_result.fetchone()[0]
        self._result = self._engine.execute(statement)

    def _read(self, sizehint=-1):
        # NOTE: ignores size hint, which FileLikeBase allows.
        if self._result is None:
            self._first_read()
        if self._yieldcount is None:
            # Prepare CSV header.
            self._yieldcount = 0
            return ','.join(c.name for c in self._columns) + '\n'
        remaining = self._rowcount - self._yieldcount
        if remaining <= 0:
            # At EOF; return None as FileLikeBase expects.
            return
        fd = io.BytesIO()
        writer = csv.writer(fd)
        batch = min(self._batch, remaining)
        rows = self._result.fetchmany(batch)
        for row in rows:
            self._yieldcount += 1
            writer.writerow(row)
        return fd.getvalue()

CREATE TABLE sessions (
  id   INTEGER PRIMARY KEY,
  -- the name of the borg file in the same directory as the cmd.db
  path TEXT    UNIQUE NOT NULL
);

CREATE TABLE commands (
  id   INTEGER PRIMARY KEY,
  -- When the command was executed
  executed_at  DATETIME NOT NULL,
  -- When the command was completed
  completed_at  DATETIME NOT NULL,
  -- The borg file this command originated from
  session  INTEGER NOT NULL,
  -- The command that was executed
  text TEXT    NOT NULL,
  -- The directory in which the command was executed
  directory TEXT    NOT NULL,
  prompted INTEGER NOT NULL,
  executed INTEGER NOT NULL,
  completed INTEGER NOT NULL,
  FOREIGN KEY(session) REFERENCES sessions(id)
);

CREATE TABLE selections (
  id   INTEGER PRIMARY KEY,
  command  INTEGER NOT NULL,
  input BOOLEAN NOT NULL,
  from_row   INTEGER NOT NULL,
  from_col   INTEGER NOT NULL,
  to_row   INTEGER NOT NULL,
  to_col   INTEGER NOT NULL,
  FOREIGN KEY(command) REFERENCES commands(id)
);

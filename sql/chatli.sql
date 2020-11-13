CREATE TABLE chatli_user
(
    id uuid PRIMARY KEY,
    username varchar NOT NULL,
    phone_number varchar,
    email varchar,
    avatar varchar
);

CREATE TABLE message
(
    id uuid PRIMARY KEY,
    chat_id uuid NOT NULL,
    payload varchar,
    sender uuid NOT NULL,
    timestamp TIMESTAMP
);

CREATE TABLE chat
(
    id uuid PRIMARY KEY,
    name varchar NOT NULL,
    description varchar
);

CREATE TABLE participant
(
    id SERIAL PRIMARY KEY,
    chat_id uuid,
    user_id uuid
);
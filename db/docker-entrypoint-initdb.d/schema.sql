CREATE TABLE country (
  id SERIAL PRIMARY KEY,
  country_name text NOT NULL
);

INSERT INTO
  country (country_name)
VALUES
  ('Japan'),
  ('China'),
  ('Australia'),
  ('Russia');

CREATE TABLE public.users (
	id serial NOT NULL,
	name varchar NOT NULL,
	age int NOT NULL,
	email varchar NOT NULL,
	registration_date date NOT NULL,
	CONSTRAINT users_pk PRIMARY KEY (id),
	CONSTRAINT users_email_unique UNIQUE (email)
);


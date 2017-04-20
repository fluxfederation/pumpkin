--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.3
-- Dumped by pg_dump version 9.5.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: ar_internal_metadata; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE ar_internal_metadata (
    key character varying NOT NULL,
    value character varying,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


--
-- Name: bugs; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE bugs (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    primary_occurrence_id uuid NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    issue_url character varying
);


--
-- Name: events; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE events (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    bug_id uuid NOT NULL,
    name character varying NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


--
-- Name: occurrences; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE occurrences (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    message character varying,
    occurred_at timestamp without time zone NOT NULL,
    data json NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    environment_id uuid NOT NULL,
    bug_id uuid
);


--
-- Name: bug_with_latest_details; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW bug_with_latest_details AS
 SELECT DISTINCT ON (bugs.id) bugs.id,
    bugs.primary_occurrence_id,
    bugs.created_at,
    bugs.updated_at,
    bugs.issue_url,
    ev_latest.id AS latest_event_id,
    ev_latest.name AS latest_event_name,
    occ_latest.occurred_at AS last_occurred_at
   FROM ((bugs
     JOIN occurrences occ_latest ON ((bugs.id = occ_latest.bug_id)))
     JOIN events ev_latest ON ((bugs.id = ev_latest.bug_id)))
  ORDER BY bugs.id, occ_latest.occurred_at DESC, ev_latest.created_at DESC;


--
-- Name: environments; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE environments (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name text,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE schema_migrations (
    version character varying NOT NULL
);


--
-- Name: ar_internal_metadata_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY ar_internal_metadata
    ADD CONSTRAINT ar_internal_metadata_pkey PRIMARY KEY (key);


--
-- Name: bugs_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY bugs
    ADD CONSTRAINT bugs_pkey PRIMARY KEY (id);


--
-- Name: events_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY events
    ADD CONSTRAINT events_pkey PRIMARY KEY (id);


--
-- Name: occurrences_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY occurrences
    ADD CONSTRAINT occurrences_pkey PRIMARY KEY (id);


--
-- Name: patches_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY environments
    ADD CONSTRAINT patches_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: index_bugs_on_primary_occurrence_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_bugs_on_primary_occurrence_id ON bugs USING btree (primary_occurrence_id);


--
-- Name: index_environments_on_name; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_environments_on_name ON environments USING btree (name);


--
-- Name: index_events_on_bug_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_events_on_bug_id ON events USING btree (bug_id);


--
-- Name: index_occurrences_on_bug_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_occurrences_on_bug_id ON occurrences USING btree (bug_id);


--
-- Name: index_occurrences_on_environment_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_occurrences_on_environment_id ON occurrences USING btree (environment_id);


--
-- PostgreSQL database dump complete
--

SET search_path TO "$user", public;

INSERT INTO schema_migrations (version) VALUES ('20160914224317'), ('20160914233248'), ('20160914233416'), ('20160914235925'), ('20160915001629'), ('20160915022656'), ('20161019211306'), ('20161020213813'), ('20161021015303'), ('20161021020642'), ('20170419231713');



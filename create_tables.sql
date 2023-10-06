--
-- PostgreSQL database dump
--

-- Dumped from database version 12.3
-- Dumped by pg_dump version 14.8 (Homebrew)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: atlas_app; Type: SCHEMA; Schema: -; Owner: atlas_app_user
--

CREATE SCHEMA atlas_app;


ALTER SCHEMA atlas_app OWNER TO atlas_app_user;

--
-- Name: atlas_bap_dashboard; Type: SCHEMA; Schema: -; Owner: atlas_bap_dashboard_user
--

CREATE SCHEMA atlas_bap_dashboard;


ALTER SCHEMA atlas_bap_dashboard OWNER TO atlas_bap_dashboard_user;

--
-- Name: atlas_bpp_dashboard; Type: SCHEMA; Schema: -; Owner: atlas_bpp_dashboard_user
--

CREATE SCHEMA atlas_bpp_dashboard;


ALTER SCHEMA atlas_bpp_dashboard OWNER TO atlas_bpp_dashboard_user;

--
-- Name: atlas_driver_offer_bpp; Type: SCHEMA; Schema: -; Owner: atlas_driver_offer_bpp_user
--

CREATE SCHEMA atlas_driver_offer_bpp;


ALTER SCHEMA atlas_driver_offer_bpp OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: atlas_public_transport; Type: SCHEMA; Schema: -; Owner: atlas_public_transport_user
--

CREATE SCHEMA atlas_public_transport;


ALTER SCHEMA atlas_public_transport OWNER TO atlas_public_transport_user;

--
-- Name: atlas_registry; Type: SCHEMA; Schema: -; Owner: atlas_registry_user
--

CREATE SCHEMA atlas_registry;


ALTER SCHEMA atlas_registry OWNER TO atlas_registry_user;

--
-- Name: atlas_scheduler_example; Type: SCHEMA; Schema: -; Owner: atlas_scheduler_example_user
--

CREATE SCHEMA atlas_scheduler_example;


ALTER SCHEMA atlas_scheduler_example OWNER TO atlas_scheduler_example_user;

--
-- Name: atlas_special_zone; Type: SCHEMA; Schema: -; Owner: atlas_special_zone_user
--

CREATE SCHEMA atlas_special_zone;


ALTER SCHEMA atlas_special_zone OWNER TO atlas_special_zone_user;

--
-- Name: tiger; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA tiger;


ALTER SCHEMA tiger OWNER TO postgres;

--
-- Name: tiger_data; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA tiger_data;


ALTER SCHEMA tiger_data OWNER TO postgres;

--
-- Name: topology; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA topology;


ALTER SCHEMA topology OWNER TO postgres;

--
-- Name: SCHEMA topology; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA topology IS 'PostGIS Topology schema';


--
-- Name: fuzzystrmatch; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS fuzzystrmatch WITH SCHEMA public;


--
-- Name: EXTENSION fuzzystrmatch; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION fuzzystrmatch IS 'determine similarities and distance between strings';


--
-- Name: postgis; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA public;


--
-- Name: EXTENSION postgis; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION postgis IS 'PostGIS geometry, geography, and raster spatial types and functions';


--
-- Name: postgis_tiger_geocoder; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder WITH SCHEMA tiger;


--
-- Name: EXTENSION postgis_tiger_geocoder; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION postgis_tiger_geocoder IS 'PostGIS tiger geocoder and reverse geocoder';


--
-- Name: postgis_topology; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS postgis_topology WITH SCHEMA topology;


--
-- Name: EXTENSION postgis_topology; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION postgis_topology IS 'PostGIS topology spatial types and functions';


--
-- Name: uuid_generate_v4(); Type: FUNCTION; Schema: atlas_app; Owner: atlas_app_user
--

CREATE FUNCTION atlas_app.uuid_generate_v4() RETURNS character
    LANGUAGE plpgsql
    AS $$
    BEGIN
        RETURN (uuid_in((md5((random())::text))::cstring));
    END;
$$;


ALTER FUNCTION atlas_app.uuid_generate_v4() OWNER TO atlas_app_user;

--
-- Name: uuid_generate_v4(); Type: FUNCTION; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

CREATE FUNCTION atlas_bap_dashboard.uuid_generate_v4() RETURNS character
    LANGUAGE plpgsql
    AS $$
    BEGIN
        RETURN (uuid_in((md5((random())::text))::cstring));
    END;
$$;


ALTER FUNCTION atlas_bap_dashboard.uuid_generate_v4() OWNER TO atlas_bap_dashboard_user;

--
-- Name: uuid_generate_v4(); Type: FUNCTION; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

CREATE FUNCTION atlas_bpp_dashboard.uuid_generate_v4() RETURNS character
    LANGUAGE plpgsql
    AS $$
    BEGIN
        RETURN (uuid_in((md5((random())::text))::cstring));
    END;
$$;


ALTER FUNCTION atlas_bpp_dashboard.uuid_generate_v4() OWNER TO atlas_bpp_dashboard_user;

--
-- Name: uuid_generate_v4(); Type: FUNCTION; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE FUNCTION atlas_driver_offer_bpp.uuid_generate_v4() RETURNS character
    LANGUAGE plpgsql
    AS $$
    BEGIN
        RETURN (uuid_in((md5((random())::text))::cstring));
    END;
$$;


ALTER FUNCTION atlas_driver_offer_bpp.uuid_generate_v4() OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: geojson_to_bin(text); Type: FUNCTION; Schema: atlas_special_zone; Owner: atlas_special_zone_user
--

CREATE FUNCTION atlas_special_zone.geojson_to_bin(geojson text) RETURNS text
    LANGUAGE sql
    AS $$
select (ST_SetSRID(ST_GeomFromGeoJSON(geojson), 4326))::text;
$$;


ALTER FUNCTION atlas_special_zone.geojson_to_bin(geojson text) OWNER TO atlas_special_zone_user;

--
-- Name: no_overlaps_in_special_zone(character, public.geometry); Type: FUNCTION; Schema: atlas_special_zone; Owner: atlas_special_zone_user
--

CREATE FUNCTION atlas_special_zone.no_overlaps_in_special_zone(id character, g public.geometry) RETURNS boolean
    LANGUAGE sql
    AS $_$
SELECT NOT EXISTS (
  SELECT 1 FROM atlas_special_zone.special_zone a
  WHERE a.id != $1
    AND a.geom && g
    AND ST_Relate(a.geom, g, '2********'));
$_$;


ALTER FUNCTION atlas_special_zone.no_overlaps_in_special_zone(id character, g public.geometry) OWNER TO atlas_special_zone_user;

--
-- Name: add_gates_to_array(text[]); Type: FUNCTION; Schema: public; Owner: atlas_driver_offer_bpp_user
--

CREATE FUNCTION public.add_gates_to_array(input_array text[]) RETURNS text[]
    LANGUAGE plpgsql
    AS $$
DECLARE
    output_array text[];
    i integer;
BEGIN

    -- Loop through each element in the input_array and concatenate 'A'
    FOR i IN 1..array_length(input_array, 1) LOOP
       IF position('address' IN input_array[i]) = 0 THEN
	      input_array[i] := trim(input_array[i]);
        input_array[i] := substring(input_array[i],1,length(input_array[i]) - 1) || ',address = Nothing}' ;
       End IF;
    END LOOP;

    RETURN input_array;
END;
$$;


ALTER FUNCTION public.add_gates_to_array(input_array text[]) OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: add_gates_to_array_rider(text[]); Type: FUNCTION; Schema: public; Owner: atlas_app_user
--

CREATE FUNCTION public.add_gates_to_array_rider(input_array text[]) RETURNS text[]
    LANGUAGE plpgsql
    AS $$
DECLARE
    output_array text[];
    i integer;
BEGIN

    -- Loop through each element in the input_array and concatenate 'A'
    FOR i IN 1..array_length(input_array, 1) LOOP
       IF position('address' IN input_array[i]) = 0 THEN
	      input_array[i] := trim(input_array[i]);
        input_array[i] := substring(input_array[i],1,length(input_array[i]) - 1) || ',address = Nothing}' ;
       End IF;
    END LOOP;

    RETURN input_array;
END;
$$;


ALTER FUNCTION public.add_gates_to_array_rider(input_array text[]) OWNER TO atlas_app_user;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: aadhaar_otp_req; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.aadhaar_otp_req (
    id character(36) NOT NULL,
    person_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_app.aadhaar_otp_req OWNER TO atlas_app_user;

--
-- Name: aadhaar_otp_verify; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.aadhaar_otp_verify (
    id character(36) NOT NULL,
    person_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_app.aadhaar_otp_verify OWNER TO atlas_app_user;

--
-- Name: aadhaar_verification; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.aadhaar_verification (
    person_id character(36) NOT NULL,
    person_name text,
    person_gender text,
    person_dob text,
    person_image_path text,
    aadhaar_number_hash text,
    is_verified boolean DEFAULT false,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_app.aadhaar_verification OWNER TO atlas_app_user;

--
-- Name: app_installs; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.app_installs (
    id character(36) NOT NULL,
    device_token character varying(255) NOT NULL,
    source character varying(255) NOT NULL,
    merchant_id character(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    platform character(36),
    app_version character(36),
    bundle_version character(36)
);


ALTER TABLE atlas_app.app_installs OWNER TO atlas_app_user;

--
-- Name: beckn_request; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.beckn_request (
    id character varying(36) NOT NULL,
    beckn_request text NOT NULL,
    signature_header text NOT NULL,
    time_stamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.beckn_request OWNER TO atlas_app_user;

--
-- Name: black_list_org; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.black_list_org (
    id character(36) NOT NULL,
    subscriber_id character varying(255) NOT NULL,
    type character varying(255)
);


ALTER TABLE atlas_app.black_list_org OWNER TO atlas_app_user;

--
-- Name: booking; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.booking (
    id character(36) NOT NULL,
    status character varying(255) NOT NULL,
    provider_id character varying(255) NOT NULL,
    provider_mobile_number character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    rider_id character(36) NOT NULL,
    from_location_id character(36),
    to_location_id character(36),
    estimated_fare numeric(30,2) NOT NULL,
    discount numeric(30,2),
    estimated_total_fare numeric(30,2) NOT NULL,
    distance numeric(30,2),
    vehicle_variant character varying(60) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    bpp_ride_booking_id character(36),
    provider_name character varying(255) NOT NULL,
    provider_url character varying(255) NOT NULL,
    reallocations_count integer DEFAULT 0,
    fare_product_type character varying(255) NOT NULL,
    trip_terms_id character(36),
    rental_slab_id character(36),
    merchant_id character(36) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51'::bpchar NOT NULL,
    quote_id character(36),
    primary_exophone character varying(255) NOT NULL,
    otp_code character(4),
    transaction_id character(36) NOT NULL,
    special_location_tag text,
    payment_method_id character(36),
    payment_url text,
    fulfillment_id text,
    driver_id text,
    item_id text DEFAULT ''::text NOT NULL
);


ALTER TABLE atlas_app.booking OWNER TO atlas_app_user;

--
-- Name: booking_cancellation_reason; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.booking_cancellation_reason (
    booking_id character(36) NOT NULL,
    source character varying(255) NOT NULL,
    reason_code character varying(255),
    additional_info character varying(255),
    reason_stage character varying(255),
    ride_id character(36),
    driver_cancellation_location_lat double precision,
    driver_cancellation_location_lon double precision,
    driver_dist_to_pickup bigint,
    merchant_id character(36)
);


ALTER TABLE atlas_app.booking_cancellation_reason OWNER TO atlas_app_user;

--
-- Name: booking_location; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.booking_location (
    id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    city character varying(255),
    state character varying(255),
    country character varying(255),
    street character varying(255),
    door character varying(255),
    building character varying(255),
    area_code character varying(255),
    area character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ward character varying(255),
    place_id text
);


ALTER TABLE atlas_app.booking_location OWNER TO atlas_app_user;

--
-- Name: call_status; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.call_status (
    id character(36) NOT NULL,
    call_id character varying(255) NOT NULL,
    ride_id character(36) NOT NULL,
    status character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    recording_url character varying(255),
    conversation_duration bigint,
    dtmf_number_used character varying(255)
);


ALTER TABLE atlas_app.call_status OWNER TO atlas_app_user;

--
-- Name: callback_request; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.callback_request (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    customer_name character varying(255),
    customer_phone_encrypted character varying(255) NOT NULL,
    customer_phone_hash bytea NOT NULL,
    customer_mobile_country_code character varying(255) NOT NULL,
    status character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.callback_request OWNER TO atlas_app_user;

--
-- Name: cancellation_reason; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.cancellation_reason (
    reason_code character varying(255) NOT NULL,
    description character varying(255) NOT NULL,
    enabled boolean NOT NULL,
    on_search boolean DEFAULT true NOT NULL,
    on_confirm boolean DEFAULT true NOT NULL,
    on_assign boolean DEFAULT true NOT NULL,
    priority smallint DEFAULT 0 NOT NULL
);


ALTER TABLE atlas_app.cancellation_reason OWNER TO atlas_app_user;

--
-- Name: directions_cache; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.directions_cache (
    id character(36) NOT NULL,
    origin_hash text NOT NULL,
    dest_hash text NOT NULL,
    slot integer NOT NULL,
    response text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.directions_cache OWNER TO atlas_app_user;

--
-- Name: disability; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.disability (
    id character varying(36) NOT NULL,
    tag character varying(255) NOT NULL,
    description character varying(255) NOT NULL
);


ALTER TABLE atlas_app.disability OWNER TO atlas_app_user;

--
-- Name: disability_translation; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.disability_translation (
    disability_id character varying(36) NOT NULL,
    disability_tag character varying(255) NOT NULL,
    translation character varying(255) NOT NULL,
    language character varying(255) NOT NULL
);


ALTER TABLE atlas_app.disability_translation OWNER TO atlas_app_user;

--
-- Name: driver_offer; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.driver_offer (
    id character(36) NOT NULL,
    estimate_id character(36) NOT NULL,
    driver_name character varying(255) NOT NULL,
    distance_to_pickup double precision NOT NULL,
    duration_to_pickup integer NOT NULL,
    valid_till timestamp with time zone NOT NULL,
    rating double precision,
    bpp_quote_id character(36) NOT NULL,
    merchant_id character(36),
    status character varying(255) DEFAULT 'ACTIVE'::character varying NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    driver_id text
);


ALTER TABLE atlas_app.driver_offer OWNER TO atlas_app_user;

--
-- Name: estimate; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.estimate (
    id character(36) NOT NULL,
    request_id character(36) NOT NULL,
    estimated_fare numeric(30,10) NOT NULL,
    discount double precision,
    estimated_total_fare numeric(30,2),
    provider_id character varying(255) NOT NULL,
    provider_url character varying(255) NOT NULL,
    provider_name character varying(255) NOT NULL,
    provider_mobile_number character varying(255) NOT NULL,
    provider_completed_rides_count integer NOT NULL,
    vehicle_variant character varying(60) NOT NULL,
    trip_terms_id character(36),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    min_total_fare numeric(30,2) NOT NULL,
    max_total_fare numeric(30,2) NOT NULL,
    night_shift_multiplier numeric(10,2),
    night_shift_start time without time zone,
    night_shift_end time without time zone,
    drivers_location text[],
    waiting_charge_per_min double precision,
    status character varying(255) NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    device text,
    estimated_duration integer,
    estimated_distance integer,
    bpp_estimate_id character(36) NOT NULL,
    night_shift_charge integer,
    special_location_tag text,
    merchant_id character(36),
    item_id text DEFAULT ''::text NOT NULL
);


ALTER TABLE atlas_app.estimate OWNER TO atlas_app_user;

--
-- Name: estimate_breakup; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.estimate_breakup (
    id character(36) NOT NULL,
    estimate_id character(36) NOT NULL,
    title character varying(255) NOT NULL,
    price_currency character varying(255) NOT NULL,
    price_value numeric(30,2) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.estimate_breakup OWNER TO atlas_app_user;

--
-- Name: exophone; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.exophone (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    primary_phone character varying(255) NOT NULL,
    backup_phone character varying(255) NOT NULL,
    is_primary_down boolean NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    call_service character varying(255) DEFAULT 'Exotel'::character varying NOT NULL
);


ALTER TABLE atlas_app.exophone OWNER TO atlas_app_user;

--
-- Name: fare_breakup; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.fare_breakup (
    id character(36) NOT NULL,
    booking_id character(36) NOT NULL,
    description text NOT NULL,
    amount double precision NOT NULL
);


ALTER TABLE atlas_app.fare_breakup OWNER TO atlas_app_user;

--
-- Name: feedback_form; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.feedback_form (
    category_name character varying(255) NOT NULL,
    id character varying(36) NOT NULL,
    rating integer,
    question character varying(255) NOT NULL,
    answer text[] NOT NULL,
    answer_type character varying(255) NOT NULL
);


ALTER TABLE atlas_app.feedback_form OWNER TO atlas_app_user;

--
-- Name: geometry; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.geometry (
    region character varying(255) NOT NULL,
    geom public.geometry(MultiPolygon),
    id character(36) DEFAULT atlas_app.uuid_generate_v4() NOT NULL
);


ALTER TABLE atlas_app.geometry OWNER TO atlas_app_user;

--
-- Name: hot_spot_config; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.hot_spot_config (
    id text,
    hot_spot_geo_hash_precision integer,
    nearby_geohash_precision integer,
    block_radius integer,
    min_frequency_of_hot_spot integer,
    weight_of_manual_pickup integer,
    weight_of_manual_saved integer,
    weight_of_auto_pickup integer,
    weight_of_auto_saved integer,
    weight_of_trip_start integer,
    max_num_hot_spots_to_show integer,
    weight_of_trip_end integer,
    weight_of_special_location integer,
    should_take_hot_spot boolean
);


ALTER TABLE atlas_app.hot_spot_config OWNER TO atlas_app_user;

--
-- Name: issue; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.issue (
    id character(36) NOT NULL,
    customer_id character(36) NOT NULL,
    booking_id character varying(36) DEFAULT NULL::character varying,
    contact_email character varying(100),
    reason character varying(500) NOT NULL,
    description character varying(1000) DEFAULT NULL::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ticket_id character varying(255),
    status character varying(255) DEFAULT 'OPEN'::character varying NOT NULL
);


ALTER TABLE atlas_app.issue OWNER TO atlas_app_user;

--
-- Name: location; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.location (
    id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    street text,
    door text,
    city text,
    state text,
    country text,
    building text,
    area_code text,
    area text,
    ward text,
    place_id text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.location OWNER TO atlas_app_user;

--
-- Name: location_backup; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.location_backup (
    id character(36),
    location_type character varying(255),
    lat double precision,
    long double precision,
    point public.geography(Point,4326),
    ward character varying(255),
    district character varying(255),
    city character varying(255),
    state character varying(255),
    country character varying(255),
    pincode character varying(255),
    address character varying(255),
    bound character varying(255),
    info text,
    created_at timestamp with time zone,
    updated_at timestamp with time zone
);


ALTER TABLE atlas_app.location_backup OWNER TO atlas_app_user;

--
-- Name: location_mapping; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.location_mapping (
    id character(36) NOT NULL,
    location_id character varying(255) NOT NULL,
    tag character varying(255) NOT NULL,
    entity_id character varying(255) NOT NULL,
    "order" integer NOT NULL,
    version character varying(255) NOT NULL
);


ALTER TABLE atlas_app.location_mapping OWNER TO atlas_app_user;

--
-- Name: merchant; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.merchant (
    id character(36) NOT NULL,
    short_id character varying(255) NOT NULL,
    origin_restriction text[],
    destination_restriction text[],
    registry_url character varying(255) NOT NULL,
    gateway_url character varying(255) NOT NULL,
    name character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    driver_offer_base_url text NOT NULL,
    driver_offer_api_key character varying(128) NOT NULL,
    driver_offer_merchant_id character varying(255) NOT NULL,
    subscriber_id character(36) NOT NULL,
    city text DEFAULT 'Kochi'::text NOT NULL,
    geo_hash_precision_value integer DEFAULT 9 NOT NULL,
    signing_public_key text NOT NULL,
    signature_expiry integer NOT NULL,
    cipher_text text DEFAULT 'TXlTZWNyZXRLZXkxMjM0NQo='::text,
    country text DEFAULT 'India'::text NOT NULL,
    bap_unique_key_id text NOT NULL,
    bap_id text NOT NULL,
    dir_cache_slot json,
    time_diff_from_utc integer DEFAULT 19800 NOT NULL,
    distance_weightage integer DEFAULT 60 NOT NULL,
    minimum_driver_rates_count integer,
    is_avoid_toll boolean DEFAULT true NOT NULL,
    aadhaar_verification_try_limit integer NOT NULL,
    aadhaar_key_expiry_time integer
);


ALTER TABLE atlas_app.merchant OWNER TO atlas_app_user;

--
-- Name: merchant_config; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.merchant_config (
    merchant_id character(36) NOT NULL,
    fraud_booking_cancellation_count_threshold integer NOT NULL,
    fraud_booking_total_count_threshold integer NOT NULL,
    fraud_booking_cancellation_count_window json DEFAULT '{"period":24, "periodType":"Hours"}'::json NOT NULL,
    fraud_booking_cancelled_by_driver_count_threshold integer DEFAULT 5 NOT NULL,
    fraud_booking_cancelled_by_driver_count_window json DEFAULT '{"period":24, "periodType":"Hours"}'::json NOT NULL,
    fraud_search_count_threshold integer DEFAULT 5 NOT NULL,
    fraud_search_count_window json DEFAULT '{"period":24, "periodType":"Hours"}'::json NOT NULL,
    id character(36) NOT NULL,
    enabled boolean DEFAULT true NOT NULL,
    fraud_ride_count_threshold integer DEFAULT 0 NOT NULL,
    fraud_ride_count_window json DEFAULT '{"period":24, "periodType":"Hours"}'::json NOT NULL
);


ALTER TABLE atlas_app.merchant_config OWNER TO atlas_app_user;

--
-- Name: merchant_message; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.merchant_message (
    merchant_id character(36) NOT NULL,
    message_key character varying(255) NOT NULL,
    message character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.merchant_message OWNER TO atlas_app_user;

--
-- Name: merchant_payment_method; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.merchant_payment_method (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    payment_type character varying(30) NOT NULL,
    payment_instrument character varying(255) NOT NULL,
    collected_by character varying(30) NOT NULL,
    priority integer NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.merchant_payment_method OWNER TO atlas_app_user;

--
-- Name: merchant_service_config; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.merchant_service_config (
    merchant_id character(36) NOT NULL,
    service_name character varying(30) NOT NULL,
    config_json json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.merchant_service_config OWNER TO atlas_app_user;

--
-- Name: merchant_service_usage_config; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.merchant_service_usage_config (
    merchant_id character(36) NOT NULL,
    get_distances character varying(30) NOT NULL,
    get_routes character varying(30) NOT NULL,
    snap_to_road character varying(30) NOT NULL,
    get_place_name character varying(30) NOT NULL,
    get_place_details character varying(30) NOT NULL,
    auto_complete character varying(30) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    sms_providers_priority_list text[] NOT NULL,
    whatsapp_providers_priority_list text[] NOT NULL,
    initiate_call character varying(30) NOT NULL,
    get_pickup_routes text DEFAULT 'Google'::text,
    get_trip_routes text DEFAULT 'Google'::text,
    use_fraud_detection boolean DEFAULT false NOT NULL,
    notify_person character varying(30) NOT NULL,
    get_distances_for_cancel_ride text NOT NULL,
    enable_dashboard_sms boolean NOT NULL,
    issue_ticket_service character varying(30) DEFAULT 'Kapture'::character varying NOT NULL,
    get_exophone character varying(255) DEFAULT 'Exotel'::character varying NOT NULL,
    aadhaar_verification_service character varying(30) NOT NULL
);


ALTER TABLE atlas_app.merchant_service_usage_config OWNER TO atlas_app_user;

--
-- Name: on_search_event; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.on_search_event (
    id character(36) NOT NULL,
    bpp_id character varying(255) NOT NULL,
    message_id character varying(255) NOT NULL,
    error_code character varying(255),
    error_type character varying(255),
    error_message text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.on_search_event OWNER TO atlas_app_user;

--
-- Name: payment_order; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.payment_order (
    id character(36) NOT NULL,
    short_id character varying(36) NOT NULL,
    person_id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    amount integer NOT NULL,
    currency character varying(30) NOT NULL,
    status character varying(100) NOT NULL,
    web_payment_link text,
    iframe_payment_link text,
    mobile_payment_link text,
    client_auth_token_encrypted character varying(255),
    client_auth_token_hash bytea,
    client_auth_token_expiry timestamp with time zone,
    get_upi_deep_links_option boolean,
    environment character varying(100),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    payment_service_order_id character varying(255) NOT NULL,
    service character varying(255),
    client_id character varying(255),
    description character varying(1024),
    return_url character varying(255),
    action character varying(255),
    request_id character varying(255),
    payment_merchant_id character varying(255),
    create_mandate text,
    mandate_max_amount integer,
    mandate_start_date timestamp with time zone,
    mandate_end_date timestamp with time zone,
    bank_error_message text,
    bank_error_code text
);


ALTER TABLE atlas_app.payment_order OWNER TO atlas_app_user;

--
-- Name: payment_transaction; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.payment_transaction (
    id character(36) NOT NULL,
    txn_uuid character varying(255),
    payment_method_type character varying(100),
    payment_method character varying(100),
    resp_message character varying(255),
    resp_code character varying(255),
    gateway_reference_id character varying(100),
    order_id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    amount numeric(30,2) NOT NULL,
    currency character varying(30) NOT NULL,
    date_created timestamp with time zone,
    status_id integer NOT NULL,
    status character varying(100) NOT NULL,
    juspay_response text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    mandate_id text,
    mandate_max_amount integer,
    mandate_frequency text,
    mandate_status text,
    mandate_start_date timestamp with time zone,
    mandate_end_date timestamp with time zone,
    bank_error_message text,
    bank_error_code text
);


ALTER TABLE atlas_app.payment_transaction OWNER TO atlas_app_user;

--
-- Name: person; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.person (
    id character(36) NOT NULL,
    first_name character varying(255),
    middle_name character varying(255),
    last_name character varying(255),
    role character varying(255) NOT NULL,
    gender character varying(255) NOT NULL,
    identifier_type character varying(255),
    password_hash bytea,
    mobile_number_encrypted character varying(255),
    mobile_number_hash bytea,
    mobile_country_code character varying(255),
    identifier character varying(255),
    rating character varying(255),
    is_new boolean NOT NULL,
    udf1 character varying(255),
    udf2 character varying(255),
    device_token character varying(255),
    description character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    merchant_id character(36) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51'::bpchar NOT NULL,
    email_encrypted character varying(255),
    email_hash bytea,
    enabled boolean DEFAULT true NOT NULL,
    client_version character(36),
    bundle_version character(36),
    whatsapp_notification_enroll_status character varying(255),
    unencrypted_mobile_number character varying(255),
    referral_code character varying(15),
    referred_at timestamp with time zone,
    has_taken_valid_ride boolean NOT NULL,
    language character varying(255),
    blocked boolean NOT NULL,
    blocked_at timestamp without time zone,
    notification_token character varying(255),
    blocked_by_rule_id character(36),
    total_ratings integer DEFAULT 0 NOT NULL,
    total_rating_score integer DEFAULT 0 NOT NULL,
    is_valid_rating boolean DEFAULT false NOT NULL,
    has_disability boolean,
    aadhaar_verified boolean DEFAULT false NOT NULL
);


ALTER TABLE atlas_app.person OWNER TO atlas_app_user;

--
-- Name: person_default_emergency_number; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.person_default_emergency_number (
    person_id character(36) NOT NULL,
    name character varying(255) NOT NULL,
    mobile_country_code character varying(255) NOT NULL,
    mobile_number_encrypted character varying(255) NOT NULL,
    mobile_number_hash bytea NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.person_default_emergency_number OWNER TO atlas_app_user;

--
-- Name: person_disability; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.person_disability (
    person_id character(36) NOT NULL,
    disability_id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    description character varying(255),
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.person_disability OWNER TO atlas_app_user;

--
-- Name: person_flow_status; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.person_flow_status (
    person_id character(36) NOT NULL,
    flow_status json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.person_flow_status OWNER TO atlas_app_user;

--
-- Name: person_stats; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.person_stats (
    person_id character(36) NOT NULL,
    user_cancelled_rides integer NOT NULL,
    driver_cancelled_rides integer NOT NULL,
    completed_rides integer NOT NULL,
    weekend_rides integer NOT NULL,
    weekday_rides integer NOT NULL,
    off_peak_rides integer NOT NULL,
    evening_peak_rides integer NOT NULL,
    morning_peak_rides integer NOT NULL,
    weekend_peak_rides integer NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.person_stats OWNER TO atlas_app_user;

--
-- Name: place_name_cache; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.place_name_cache (
    id character(36) NOT NULL,
    formatted_address text,
    plus_code text,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    place_id text NOT NULL,
    address_components text[] NOT NULL,
    geo_hash text NOT NULL
);


ALTER TABLE atlas_app.place_name_cache OWNER TO atlas_app_user;

--
-- Name: product_instance_backup; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.product_instance_backup (
    id character(36),
    case_id character varying(255),
    product_id character varying(255),
    person_id character varying(255),
    person_updated_at timestamp with time zone,
    short_id character varying(36),
    entity_id character varying(255),
    entity_type character varying(255),
    quantity bigint,
    price numeric(30,10),
    type character varying(255),
    status character varying(255),
    start_time timestamp with time zone,
    end_time timestamp with time zone,
    valid_till timestamp with time zone,
    from_location_id character varying(255),
    to_location_id character varying(255),
    organization_id character varying(255),
    parent_id character varying(255),
    info text,
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    created_at timestamp with time zone,
    updated_at timestamp with time zone,
    actual_distance double precision,
    actual_price double precision
);


ALTER TABLE atlas_app.product_instance_backup OWNER TO atlas_app_user;

--
-- Name: quote; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.quote (
    id character(36) NOT NULL,
    request_id character varying(255) NOT NULL,
    estimated_fare numeric(30,10) NOT NULL,
    provider_id character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    vehicle_variant character varying(60) DEFAULT ''::character varying NOT NULL,
    discount numeric(30,2),
    estimated_total_fare numeric(30,2),
    total_fare numeric(30,2),
    provider_mobile_number character varying(255) NOT NULL,
    distance_to_nearest_driver numeric(30,2),
    provider_name character varying(255) NOT NULL,
    provider_completed_rides_count integer NOT NULL,
    provider_url character varying(255) NOT NULL,
    rental_slab_id character(36),
    trip_terms_id character(36),
    fare_product_type character varying(255) NOT NULL,
    driver_offer_id character(36),
    merchant_id character varying(36) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51'::character varying NOT NULL,
    special_zone_quote_id character(36),
    special_location_tag text,
    item_id text DEFAULT ''::text NOT NULL
);


ALTER TABLE atlas_app.quote OWNER TO atlas_app_user;

--
-- Name: quote_bak_1022; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.quote_bak_1022 (
    id character(36),
    request_id character varying(255),
    estimated_fare numeric(30,10),
    provider_id character varying(255),
    created_at timestamp with time zone,
    vehicle_variant character varying(60),
    discount double precision,
    estimated_total_fare numeric(30,2),
    total_fare numeric(30,2),
    provider_mobile_number character varying(255),
    distance_to_nearest_driver double precision,
    provider_name character varying(255),
    provider_completed_rides_count integer,
    bpp_quote_id character(36),
    provider_url character varying(255)
);


ALTER TABLE atlas_app.quote_bak_1022 OWNER TO atlas_app_user;

--
-- Name: quote_bak_1026; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.quote_bak_1026 (
    id character(36),
    request_id character varying(255),
    estimated_fare numeric(30,10),
    provider_id character varying(255),
    created_at timestamp with time zone,
    vehicle_variant character varying(60),
    discount double precision,
    estimated_total_fare numeric(30,2),
    total_fare numeric(30,2),
    provider_mobile_number character varying(255),
    distance_to_nearest_driver double precision,
    provider_name character varying(255),
    provider_completed_rides_count integer,
    bpp_quote_id character(36),
    provider_url character varying(255)
);


ALTER TABLE atlas_app.quote_bak_1026 OWNER TO atlas_app_user;

--
-- Name: quote_terms_bak_1027; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.quote_terms_bak_1027 (
    id character(36),
    quote_id character(36),
    description character varying(1000)
);


ALTER TABLE atlas_app.quote_terms_bak_1027 OWNER TO atlas_app_user;

--
-- Name: rating; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.rating (
    id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    rating_value integer NOT NULL,
    feedback_details character varying(255),
    rider_id character varying(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.rating OWNER TO atlas_app_user;

--
-- Name: registration_token; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.registration_token (
    id character(36) NOT NULL,
    auth_medium character varying(255) NOT NULL,
    auth_type character varying(255) NOT NULL,
    auth_value_hash character varying(1024) NOT NULL,
    token character varying(1024) NOT NULL,
    verified boolean NOT NULL,
    auth_expiry bigint NOT NULL,
    token_expiry bigint NOT NULL,
    attempts bigint NOT NULL,
    entity_id character(36) NOT NULL,
    entity_type character(36) NOT NULL,
    info text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    merchant_id text DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51'::text NOT NULL
);


ALTER TABLE atlas_app.registration_token OWNER TO atlas_app_user;

--
-- Name: rental_quote_bak_1027; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.rental_quote_bak_1027 (
    quote_id character(36),
    base_distance integer,
    base_duration_hr integer
);


ALTER TABLE atlas_app.rental_quote_bak_1027 OWNER TO atlas_app_user;

--
-- Name: rental_slab; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.rental_slab (
    id character(36) NOT NULL,
    base_distance integer NOT NULL,
    base_duration integer NOT NULL
);


ALTER TABLE atlas_app.rental_slab OWNER TO atlas_app_user;

--
-- Name: ride; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.ride (
    id character(36) NOT NULL,
    booking_id character(36) NOT NULL,
    short_id character varying(36) NOT NULL,
    status character varying(255) NOT NULL,
    driver_name character varying(255) NOT NULL,
    driver_rating numeric(10,2),
    driver_mobile_number character varying(255) NOT NULL,
    driver_registered_at timestamp with time zone NOT NULL,
    vehicle_number character varying(255) NOT NULL,
    vehicle_model character varying(255) NOT NULL,
    vehicle_color character varying(255) NOT NULL,
    otp character(4) NOT NULL,
    tracking_url character varying(255),
    fare numeric(30,2),
    total_fare numeric(30,2),
    chargeable_distance numeric(30,2),
    vehicle_variant character varying(60) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    bpp_ride_id character(36) NOT NULL,
    ride_start_time timestamp with time zone,
    ride_end_time timestamp with time zone,
    ride_rating bigint,
    driver_arrival_time timestamp with time zone,
    merchant_id character(36),
    traveled_distance numeric(30,2),
    driver_mobile_country_code text,
    driver_image text
);


ALTER TABLE atlas_app.ride OWNER TO atlas_app_user;

--
-- Name: ride_booking_bak_1022; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.ride_booking_bak_1022 (
    id character(36),
    request_id character(36),
    quote_id character(36),
    status character varying(255),
    provider_id character varying(255),
    provider_mobile_number character varying(255),
    start_time timestamp with time zone,
    rider_id character(36),
    from_location_id character(36),
    to_location_id character(36),
    estimated_fare double precision,
    discount double precision,
    estimated_total_fare numeric(30,2),
    distance double precision,
    vehicle_variant character varying(60),
    created_at timestamp with time zone,
    updated_at timestamp with time zone,
    bpp_ride_booking_id character(36),
    provider_name character varying(255),
    provider_url character varying(255),
    reallocations_count integer
);


ALTER TABLE atlas_app.ride_booking_bak_1022 OWNER TO atlas_app_user;

--
-- Name: ride_booking_bak_1026; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.ride_booking_bak_1026 (
    id character(36),
    request_id character(36),
    quote_id character(36),
    status character varying(255),
    provider_id character varying(255),
    provider_mobile_number character varying(255),
    start_time timestamp with time zone,
    rider_id character(36),
    from_location_id character(36),
    to_location_id character(36),
    estimated_fare double precision,
    discount double precision,
    estimated_total_fare numeric(30,2),
    distance double precision,
    vehicle_variant character varying(60),
    created_at timestamp with time zone,
    updated_at timestamp with time zone,
    bpp_ride_booking_id character(36),
    provider_name character varying(255),
    provider_url character varying(255),
    reallocations_count integer
);


ALTER TABLE atlas_app.ride_booking_bak_1026 OWNER TO atlas_app_user;

--
-- Name: saved_location; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.saved_location (
    id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    city character varying(500),
    state character varying(500),
    country character varying(500),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    street character varying(500),
    door character varying(500),
    building character varying(500),
    area_code character varying(500),
    area character varying(500),
    tag character varying(255) NOT NULL,
    rider_id character(36) NOT NULL,
    place_id text,
    ward character varying(255),
    is_moved boolean
);


ALTER TABLE atlas_app.saved_location OWNER TO atlas_app_user;

--
-- Name: schema_migrations; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_app.schema_migrations OWNER TO atlas_app_user;

--
-- Name: search_request; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.search_request (
    id character(36) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    valid_till timestamp with time zone NOT NULL,
    rider_id character varying(255) NOT NULL,
    from_location_id character varying(36),
    to_location_id character varying(36),
    distance numeric(30,2),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    merchant_id character(36) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51'::bpchar NOT NULL,
    bundle_version text,
    client_version text,
    language character varying(255),
    max_distance double precision,
    device text,
    estimated_ride_duration integer,
    customer_extra_fee integer,
    auto_assign_enabled boolean,
    auto_assign_enabled_v2 boolean,
    available_payment_methods character(36)[] NOT NULL,
    selected_payment_method_id character(36),
    disability_tag character(255)
);


ALTER TABLE atlas_app.search_request OWNER TO atlas_app_user;

--
-- Name: search_request_bak_1022; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.search_request_bak_1022 (
    id character(36),
    start_time timestamp with time zone,
    valid_till timestamp with time zone,
    rider_id character varying(255),
    from_location_id character varying(36),
    to_location_id character varying(36),
    distance double precision,
    created_at timestamp with time zone
);


ALTER TABLE atlas_app.search_request_bak_1022 OWNER TO atlas_app_user;

--
-- Name: search_request_location; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.search_request_location (
    id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    city character varying(255),
    state character varying(255),
    country character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    street character varying(255),
    door character varying(255),
    building character varying(255),
    area_code character varying(255),
    area character varying(255),
    ward character varying(255),
    place_id text
);


ALTER TABLE atlas_app.search_request_location OWNER TO atlas_app_user;

--
-- Name: search_request_location_1026; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.search_request_location_1026 (
    id character(36),
    lat double precision,
    lon double precision,
    city character varying(255),
    state character varying(255),
    country character varying(255),
    created_at timestamp with time zone,
    updated_at timestamp with time zone,
    street character varying(255),
    door character varying(255),
    building character varying(255),
    area_code character varying(255),
    area character varying(255)
);


ALTER TABLE atlas_app.search_request_location_1026 OWNER TO atlas_app_user;

--
-- Name: sos; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.sos (
    id character(36) NOT NULL,
    flow character varying(255),
    status character varying(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    person_id character(36) NOT NULL,
    ride_id character(36) NOT NULL
);


ALTER TABLE atlas_app.sos OWNER TO atlas_app_user;

--
-- Name: special_location; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.special_location (
    id character(36) NOT NULL,
    location_name character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    gates text[] NOT NULL,
    geom public.geometry(MultiPolygon),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.special_location OWNER TO atlas_app_user;

--
-- Name: special_zone_quote; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.special_zone_quote (
    id character(36) NOT NULL,
    quote_id character(100) NOT NULL
);


ALTER TABLE atlas_app.special_zone_quote OWNER TO atlas_app_user;

--
-- Name: tag; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.tag (
    id character(36) NOT NULL,
    created_by character(36) NOT NULL,
    created_by_entity_type character varying(255) NOT NULL,
    tag_type character varying(255) NOT NULL,
    tag character varying(255) NOT NULL,
    info text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.tag OWNER TO atlas_app_user;

--
-- Name: tag_category_mapping; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.tag_category_mapping (
    id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_app.tag_category_mapping OWNER TO atlas_app_user;

--
-- Name: trip_terms; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.trip_terms (
    id character(36) NOT NULL,
    descriptions text NOT NULL
);


ALTER TABLE atlas_app.trip_terms OWNER TO atlas_app_user;

--
-- Name: webengage; Type: TABLE; Schema: atlas_app; Owner: atlas_app_user
--

CREATE TABLE atlas_app.webengage (
    id character(36) NOT NULL,
    version text,
    content_template_id character(36),
    principal_entity_id character(36),
    info_message_id character(36),
    web_message_id character(36),
    to_number character(36),
    status character(36) DEFAULT NULL::bpchar
);


ALTER TABLE atlas_app.webengage OWNER TO atlas_app_user;

--
-- Name: access_matrix; Type: TABLE; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

CREATE TABLE atlas_bap_dashboard.access_matrix (
    id character(36) NOT NULL,
    role_id character(36) NOT NULL,
    api_entity character varying(255) NOT NULL,
    user_access_type character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    user_action_type character varying(255) NOT NULL
);


ALTER TABLE atlas_bap_dashboard.access_matrix OWNER TO atlas_bap_dashboard_user;

--
-- Name: merchant; Type: TABLE; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

CREATE TABLE atlas_bap_dashboard.merchant (
    id character(36) NOT NULL,
    short_id character varying(255) NOT NULL,
    server_name character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is2fa_mandatory boolean DEFAULT false NOT NULL
);


ALTER TABLE atlas_bap_dashboard.merchant OWNER TO atlas_bap_dashboard_user;

--
-- Name: merchant_access; Type: TABLE; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

CREATE TABLE atlas_bap_dashboard.merchant_access (
    id character(36) NOT NULL,
    person_id character(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    merchant_id character(36) DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd'::bpchar NOT NULL,
    secret_key character varying(255),
    is2fa_enabled boolean DEFAULT false NOT NULL
);


ALTER TABLE atlas_bap_dashboard.merchant_access OWNER TO atlas_bap_dashboard_user;

--
-- Name: person; Type: TABLE; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

CREATE TABLE atlas_bap_dashboard.person (
    id character(36) NOT NULL,
    first_name character varying(255) NOT NULL,
    last_name character varying(255) NOT NULL,
    email_encrypted character varying(255),
    email_hash bytea,
    mobile_number_encrypted character varying(255) NOT NULL,
    mobile_number_hash bytea NOT NULL,
    mobile_country_code character varying(255) NOT NULL,
    password_hash bytea,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    role_id character(36) DEFAULT 'e5a69a26-d165-455a-a711-33a41e0d47c6'::bpchar NOT NULL
);


ALTER TABLE atlas_bap_dashboard.person OWNER TO atlas_bap_dashboard_user;

--
-- Name: registration_token; Type: TABLE; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

CREATE TABLE atlas_bap_dashboard.registration_token (
    id character(36) NOT NULL,
    token character varying(1024) NOT NULL,
    person_id character(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    merchant_id character(36) DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd'::bpchar NOT NULL
);


ALTER TABLE atlas_bap_dashboard.registration_token OWNER TO atlas_bap_dashboard_user;

--
-- Name: role; Type: TABLE; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

CREATE TABLE atlas_bap_dashboard.role (
    id character(36) NOT NULL,
    name character varying(255) NOT NULL,
    dashboard_access_type character varying(255) NOT NULL,
    description character varying(1024) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_bap_dashboard.role OWNER TO atlas_bap_dashboard_user;

--
-- Name: schema_migrations; Type: TABLE; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

CREATE TABLE atlas_bap_dashboard.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_bap_dashboard.schema_migrations OWNER TO atlas_bap_dashboard_user;

--
-- Name: transaction; Type: TABLE; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

CREATE TABLE atlas_bap_dashboard.transaction (
    id character(36) NOT NULL,
    requestor_id character(36),
    merchant_id character(36),
    common_driver_id character(36),
    common_ride_id character(36),
    endpoint character varying(255) NOT NULL,
    request text,
    response text,
    response_error text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    server_name character varying(255)
);


ALTER TABLE atlas_bap_dashboard.transaction OWNER TO atlas_bap_dashboard_user;

--
-- Name: access_matrix; Type: TABLE; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

CREATE TABLE atlas_bpp_dashboard.access_matrix (
    id character(36) NOT NULL,
    role_id character(36) NOT NULL,
    api_entity character varying(255) NOT NULL,
    user_access_type character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    user_action_type character varying(255) NOT NULL
);


ALTER TABLE atlas_bpp_dashboard.access_matrix OWNER TO atlas_bpp_dashboard_user;

--
-- Name: merchant; Type: TABLE; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

CREATE TABLE atlas_bpp_dashboard.merchant (
    id character(36) NOT NULL,
    short_id character varying(255) NOT NULL,
    server_name character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is2fa_mandatory boolean DEFAULT false NOT NULL
);


ALTER TABLE atlas_bpp_dashboard.merchant OWNER TO atlas_bpp_dashboard_user;

--
-- Name: merchant_access; Type: TABLE; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

CREATE TABLE atlas_bpp_dashboard.merchant_access (
    id character(36) NOT NULL,
    person_id character(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    merchant_id character(36) DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd'::bpchar NOT NULL,
    secret_key character varying(255),
    is2fa_enabled boolean DEFAULT false NOT NULL
);


ALTER TABLE atlas_bpp_dashboard.merchant_access OWNER TO atlas_bpp_dashboard_user;

--
-- Name: person; Type: TABLE; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

CREATE TABLE atlas_bpp_dashboard.person (
    id character(36) NOT NULL,
    first_name character varying(255) NOT NULL,
    last_name character varying(255) NOT NULL,
    email_encrypted character varying(255),
    email_hash bytea,
    mobile_number_encrypted character varying(255) NOT NULL,
    mobile_number_hash bytea NOT NULL,
    mobile_country_code character varying(255) NOT NULL,
    password_hash bytea,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    role_id character(36) DEFAULT 'e5a69a26-d165-455a-a711-33a41e0d47c6'::bpchar NOT NULL
);


ALTER TABLE atlas_bpp_dashboard.person OWNER TO atlas_bpp_dashboard_user;

--
-- Name: registration_token; Type: TABLE; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

CREATE TABLE atlas_bpp_dashboard.registration_token (
    id character(36) NOT NULL,
    token character varying(1024) NOT NULL,
    person_id character(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    merchant_id character(36) DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd'::bpchar NOT NULL
);


ALTER TABLE atlas_bpp_dashboard.registration_token OWNER TO atlas_bpp_dashboard_user;

--
-- Name: role; Type: TABLE; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

CREATE TABLE atlas_bpp_dashboard.role (
    id character(36) NOT NULL,
    name character varying(255) NOT NULL,
    dashboard_access_type character varying(255) NOT NULL,
    description character varying(1024) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_bpp_dashboard.role OWNER TO atlas_bpp_dashboard_user;

--
-- Name: schema_migrations; Type: TABLE; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

CREATE TABLE atlas_bpp_dashboard.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_bpp_dashboard.schema_migrations OWNER TO atlas_bpp_dashboard_user;

--
-- Name: transaction; Type: TABLE; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

CREATE TABLE atlas_bpp_dashboard.transaction (
    id character(36) NOT NULL,
    requestor_id character(36),
    merchant_id character(36),
    common_driver_id character(36),
    common_ride_id character(36),
    endpoint character varying(255) NOT NULL,
    request text,
    response text,
    response_error text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    server_name character varying(255)
);


ALTER TABLE atlas_bpp_dashboard.transaction OWNER TO atlas_bpp_dashboard_user;

--
-- Name: aadhaar_otp_req; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_req (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.aadhaar_otp_req OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: aadhaar_otp_verify; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_verify (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.aadhaar_otp_verify OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: aadhaar_verification; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.aadhaar_verification (
    driver_id character(36) NOT NULL,
    driver_name text,
    driver_gender text,
    driver_dob text,
    driver_image text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    aadhaar_number_hash text,
    is_verified boolean DEFAULT true,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    driver_image_path text
);


ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: bap_metadata; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.bap_metadata (
    id text NOT NULL,
    name text NOT NULL,
    logo_url text NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.bap_metadata OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: beckn_request; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.beckn_request (
    id character varying(36) NOT NULL,
    beckn_request text NOT NULL,
    signature_header text NOT NULL,
    time_stamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.beckn_request OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: booking; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.booking (
    id character(36) NOT NULL,
    status character varying(255) NOT NULL,
    provider_id character(36) NOT NULL,
    bap_id character varying(255) NOT NULL,
    bap_uri character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    rider_id character(36),
    from_location_id character(36),
    to_location_id character(36),
    vehicle_variant character varying(255) NOT NULL,
    estimated_distance integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    quote_id character(36) NOT NULL,
    fare_parameters_id character(36) NOT NULL,
    estimated_fare double precision NOT NULL,
    rider_name character varying(255),
    estimated_duration integer NOT NULL,
    primary_exophone character varying(255) NOT NULL,
    booking_type character(36) DEFAULT 'NormalBooking'::bpchar NOT NULL,
    special_zone_otp_code character(4),
    transaction_id character(36) NOT NULL,
    max_estimated_distance double precision,
    area text,
    special_location_tag text,
    payment_method_id character(36),
    bap_city text,
    bap_country text,
    payment_url text,
    disability_tag text
);


ALTER TABLE atlas_driver_offer_bpp.booking OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: booking_cancellation_reason; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.booking_cancellation_reason (
    driver_id character(36),
    booking_id character(36) NOT NULL,
    ride_id character(36),
    source character varying(255) NOT NULL,
    reason_code character varying(255),
    additional_info character varying(255),
    driver_cancellation_location_lat double precision,
    driver_cancellation_location_lon double precision,
    driver_dist_to_pickup bigint,
    merchant_id character(36)
);


ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: booking_location; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.booking_location (
    id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    city character varying(255),
    state character varying(255),
    country character varying(255),
    street character varying(255),
    building character varying(255),
    area_code character varying(255),
    area character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    door character varying(255)
);


ALTER TABLE atlas_driver_offer_bpp.booking_location OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: business_event; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.business_event (
    id character(36) NOT NULL,
    driver_id character(36),
    event_type character varying(255) NOT NULL,
    time_stamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    booking_id character(36),
    when_pool_was_computed character varying(255),
    vehicle_variant character varying(255),
    distance double precision,
    duration double precision,
    ride_id character varying(255)
);


ALTER TABLE atlas_driver_offer_bpp.business_event OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: call_status; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.call_status (
    id character(36) NOT NULL,
    call_id character varying(255) NOT NULL,
    recording_url character varying(255),
    status character varying(255) NOT NULL,
    conversation_duration bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    dtmf_number_used character varying(255),
    entity_id character(36) DEFAULT 'UNKOWN'::bpchar NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.call_status OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: cancellation_reason; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.cancellation_reason (
    reason_code character varying(255) NOT NULL,
    description character varying(255) NOT NULL,
    enabled boolean NOT NULL,
    priority smallint DEFAULT 0 NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.cancellation_reason OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: comment; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.comment (
    id character varying(255) NOT NULL,
    issue_report_id character varying(255) NOT NULL,
    comment character varying(255) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    author_id character(36) NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.comment OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_availability; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_availability (
    id character(36) NOT NULL,
    driver_id character varying(255) NOT NULL,
    merchant_id character varying(255) NOT NULL,
    total_available_time integer NOT NULL,
    last_available_time timestamp without time zone NOT NULL,
    bucket_start_time timestamp with time zone NOT NULL,
    bucket_end_time timestamp with time zone NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.driver_availability OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_block_reason; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_block_reason (
    reason_code text NOT NULL,
    block_reason text,
    block_time_in_hours integer
);


ALTER TABLE atlas_driver_offer_bpp.driver_block_reason OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_fee; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_fee (
    id character(36) NOT NULL,
    driver_id character varying(255) NOT NULL,
    total_earnings integer NOT NULL,
    num_rides integer NOT NULL,
    govt_charges integer NOT NULL,
    platform_fee numeric(30,2) NOT NULL,
    cgst numeric(30,2) NOT NULL,
    sgst numeric(30,2) NOT NULL,
    pay_by timestamp with time zone NOT NULL,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone NOT NULL,
    status character varying(255) NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    collected_by text,
    fee_type text DEFAULT 'RECURRING_INVOICE'::text NOT NULL,
    merchant_id character(36) DEFAULT 'favorit0-0000-0000-0000-00000favorit'::bpchar NOT NULL,
    offer_id character varying(100),
    plan_offer_title text,
    stage_updated_at timestamp with time zone,
    bill_number integer,
    autopay_payment_stage text,
    fee_without_discount integer,
    collected_at timestamp without time zone,
    scheduler_try_count integer DEFAULT 1 NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.driver_fee OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_flow_status; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_flow_status (
    person_id character(36) NOT NULL,
    flow_status json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.driver_flow_status OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_go_home_request; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_go_home_request (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    point public.geography(Point,4326) NOT NULL,
    status character varying(36) NOT NULL,
    num_cancellation integer DEFAULT 0 NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    reached_home boolean
);


ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_home_location; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_home_location (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    home_address text NOT NULL,
    tag text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.driver_home_location OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_information; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_information (
    driver_id character(36) NOT NULL,
    active boolean DEFAULT false NOT NULL,
    on_ride boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    enabled boolean NOT NULL,
    verified boolean DEFAULT false NOT NULL,
    referral_code character varying(255),
    admin_id character(36),
    blocked boolean NOT NULL,
    last_enabled_on timestamp with time zone,
    can_downgrade_to_hatchback boolean DEFAULT false NOT NULL,
    can_downgrade_to_sedan boolean DEFAULT false NOT NULL,
    can_downgrade_to_taxi boolean DEFAULT false NOT NULL,
    mode text,
    merchant_id character(36),
    num_of_locks integer DEFAULT 0 NOT NULL,
    aadhaar_verified boolean DEFAULT false NOT NULL,
    subscribed boolean NOT NULL,
    payment_pending boolean NOT NULL,
    blocked_reason text,
    block_expiry_time timestamp with time zone,
    auto_pay_status text,
    comp_aadhaar_image_path text,
    available_upi_apps text,
    payer_vpa text,
    enabled_at timestamp with time zone
);


ALTER TABLE atlas_driver_offer_bpp.driver_information OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_intelligent_pool_config; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config (
    merchant_id character(36) NOT NULL,
    availability_time_weightage integer NOT NULL,
    acceptance_ratio_weightage integer NOT NULL,
    cancellation_ratio_weightage integer NOT NULL,
    availability_time_window_option json NOT NULL,
    acceptance_ratio_window_option json NOT NULL,
    cancellation_ratio_window_option json NOT NULL,
    min_quotes_to_qualify_for_intelligent_pool integer NOT NULL,
    min_quotes_to_qualify_for_intelligent_pool_window_option json NOT NULL,
    intelligent_pool_percentage integer,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    speed_normalizer double precision DEFAULT 28,
    driver_speed_weightage integer DEFAULT 5,
    location_update_sample_time integer DEFAULT 3,
    min_location_updates integer DEFAULT 3,
    default_driver_speed double precision DEFAULT 27.0,
    actual_pickup_distance_weightage integer DEFAULT 0 NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_license; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_license (
    id character(36) NOT NULL,
    driver_id character varying(36) NOT NULL,
    driver_dob timestamp with time zone,
    license_expiry timestamp with time zone NOT NULL,
    class_of_vehicles text[],
    verification_status character varying(10) NOT NULL,
    consent boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    consent_timestamp timestamp with time zone NOT NULL,
    failed_rules text[],
    driver_name character varying(255),
    document_image_id1 character varying(36) DEFAULT 'no-id'::character varying NOT NULL,
    document_image_id2 character varying(36),
    license_number_hash bytea,
    license_number_encrypted character varying(255)
);


ALTER TABLE atlas_driver_offer_bpp.driver_license OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_location; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_location (
    driver_id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    point public.geography(Point,4326) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    coordinates_calculated_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    merchant_id character(36) DEFAULT '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f'::bpchar NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.driver_location OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_plan; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_plan (
    driver_id character(36) NOT NULL,
    plan_id character(36) NOT NULL,
    plan_type text NOT NULL,
    mandate_id text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    mandate_setup_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


ALTER TABLE atlas_driver_offer_bpp.driver_plan OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_pool_config; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_pool_config (
    merchant_id character(36) NOT NULL,
    min_radius_of_search integer NOT NULL,
    max_radius_of_search integer NOT NULL,
    radius_step_size integer NOT NULL,
    driver_position_info_expiry integer,
    actual_distance_threshold integer,
    max_driver_quotes_required integer,
    driver_quote_limit integer,
    driver_request_count_limit integer,
    driver_batch_size integer NOT NULL,
    max_number_of_batches integer NOT NULL,
    max_parallel_search_requests integer NOT NULL,
    pool_sorting_type character varying(20) NOT NULL,
    single_batch_process_time integer,
    trip_distance integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    radius_shrink_value_for_drivers_on_ride bigint DEFAULT 300,
    driver_to_destination_distance_threshold bigint DEFAULT 300,
    driver_to_destination_duration bigint DEFAULT 10,
    distance_based_batch_split text[] DEFAULT ARRAY['BatchSplitByPickupDistance { batchSplitSize = 1, batchSplitDelay = 0 }'::text, 'BatchSplitByPickupDistance { batchSplitSize = 1, batchSplitDelay = 4 }'::text] NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.driver_pool_config OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_quote; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_quote (
    id character(36) NOT NULL,
    status character varying(255) NOT NULL,
    search_request_id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    distance_to_pickup bigint NOT NULL,
    duration_to_pickup bigint NOT NULL,
    vehicle_variant character varying(255) NOT NULL,
    valid_till timestamp without time zone NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    driver_name text NOT NULL,
    driver_rating double precision,
    distance integer NOT NULL,
    fare_parameters_id character(36) NOT NULL,
    estimated_fare double precision NOT NULL,
    search_request_for_driver_id character(36),
    provider_id character(36) DEFAULT 'favorit0-0000-0000-0000-00000favorit'::bpchar NOT NULL,
    search_try_id character(36) NOT NULL,
    special_location_tag text,
    estimate_id text,
    go_home_request_id character(36)
);


ALTER TABLE atlas_driver_offer_bpp.driver_quote OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_rc_association; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_rc_association (
    id character(36) NOT NULL,
    driver_id character varying(36) NOT NULL,
    rc_id character varying(36) NOT NULL,
    associated_on timestamp with time zone NOT NULL,
    associated_till timestamp with time zone,
    consent boolean DEFAULT true NOT NULL,
    consent_timestamp timestamp with time zone NOT NULL,
    is_rc_active boolean DEFAULT false NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.driver_rc_association OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_referral; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_referral (
    referral_code character varying(15) NOT NULL,
    driver_id character varying(255) NOT NULL,
    linked_at timestamp with time zone
);


ALTER TABLE atlas_driver_offer_bpp.driver_referral OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: driver_stats; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.driver_stats (
    driver_id character(36) NOT NULL,
    idle_since timestamp with time zone,
    total_rides integer DEFAULT 0 NOT NULL,
    total_distance double precision DEFAULT 0 NOT NULL,
    rides_cancelled integer,
    total_rides_assigned integer,
    total_earnings integer DEFAULT 0,
    bonus_earned integer DEFAULT 0,
    late_night_trips integer DEFAULT 0,
    earnings_missed integer DEFAULT 0,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.driver_stats OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: estimate; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.estimate (
    id character(36) NOT NULL,
    vehicle_variant character varying(36) NOT NULL,
    min_fare integer NOT NULL,
    max_fare integer NOT NULL,
    estimate_breakup_list text[] NOT NULL,
    night_shift_multiplier numeric(30,2),
    night_shift_start character varying(255),
    night_shift_end character varying(255),
    waiting_charge_per_min integer,
    waiting_or_pickup_charges integer,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    night_shift_charge integer,
    request_id character(36) NOT NULL,
    special_location_tag text
);


ALTER TABLE atlas_driver_offer_bpp.estimate OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: exophone; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.exophone (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    primary_phone character varying(255) NOT NULL,
    backup_phone character varying(255) NOT NULL,
    is_primary_down boolean NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    exophone_type character varying(255) DEFAULT 'CALL_RIDE'::character varying NOT NULL,
    call_service character varying(255) DEFAULT 'Exotel'::character varying NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.exophone OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_parameters; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.fare_parameters (
    id character(36) NOT NULL,
    driver_selected_fare integer,
    base_fare integer NOT NULL,
    service_charge integer,
    customer_extra_fee integer,
    fare_parameters_type character varying(50) NOT NULL,
    govt_charges integer,
    waiting_charge integer,
    night_shift_charge integer,
    night_shift_rate_if_applies double precision
);


ALTER TABLE atlas_driver_offer_bpp.fare_parameters OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_parameters_progressive_details; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details (
    fare_parameters_id character(36) NOT NULL,
    dead_km_fare integer NOT NULL,
    extra_km_fare integer
);


ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_parameters_slab_details; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.fare_parameters_slab_details (
    fare_parameters_id character(36) NOT NULL,
    platform_fee integer,
    sgst numeric(30,2),
    cgst numeric(30,2)
);


ALTER TABLE atlas_driver_offer_bpp.fare_parameters_slab_details OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_policy; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.fare_policy (
    id character(36) NOT NULL,
    night_shift_start time without time zone,
    night_shift_end time without time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    min_allowed_trip_distance integer,
    max_allowed_trip_distance integer,
    service_charge integer,
    govt_charges double precision,
    fare_policy_type character varying(50) NOT NULL,
    description text
);


ALTER TABLE atlas_driver_offer_bpp.fare_policy OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_policy_27_07_bak; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.fare_policy_27_07_bak (
    id character(36),
    organization_id character(36),
    fare_for_pickup double precision,
    night_shift_start time without time zone,
    night_shift_end time without time zone,
    night_shift_rate double precision,
    created_at timestamp with time zone,
    updated_at timestamp with time zone,
    fare_per_km double precision
);


ALTER TABLE atlas_driver_offer_bpp.fare_policy_27_07_bak OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_policy_driver_extra_fee_bounds; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds (
    id integer NOT NULL,
    fare_policy_id character(36) NOT NULL,
    start_distance integer NOT NULL,
    min_fee integer NOT NULL,
    max_fee integer NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_policy_driver_extra_fee_bounds_id_seq; Type: SEQUENCE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE SEQUENCE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds_id_seq OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_policy_driver_extra_fee_bounds_id_seq; Type: SEQUENCE OWNED BY; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER SEQUENCE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds_id_seq OWNED BY atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds.id;


--
-- Name: fare_policy_progressive_details; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details (
    fare_policy_id character(36) NOT NULL,
    base_distance integer NOT NULL,
    base_fare integer NOT NULL,
    dead_km_fare integer NOT NULL,
    waiting_charge json,
    night_shift_charge json,
    free_wating_time integer NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_policy_progressive_details_per_extra_km_rate_section; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section (
    id integer NOT NULL,
    fare_policy_id character(36) NOT NULL,
    start_distance integer NOT NULL,
    per_extra_km_rate numeric(30,2) NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_policy_progressive_details_per_extra_km_rate_sectio_id_seq; Type: SEQUENCE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE SEQUENCE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_sectio_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_sectio_id_seq OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_policy_progressive_details_per_extra_km_rate_sectio_id_seq; Type: SEQUENCE OWNED BY; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER SEQUENCE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_sectio_id_seq OWNED BY atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section.id;


--
-- Name: fare_policy_slabs_details_slab; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab (
    id integer NOT NULL,
    fare_policy_id character(36) NOT NULL,
    start_distance integer NOT NULL,
    base_fare integer NOT NULL,
    waiting_charge json,
    night_shift_charge json,
    free_wating_time integer NOT NULL,
    platform_fee_charge integer,
    platform_fee_cgst integer,
    platform_fee_sgst integer
);


ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_policy_slabs_details_slab_id_seq; Type: SEQUENCE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE SEQUENCE atlas_driver_offer_bpp.fare_policy_slabs_details_slab_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab_id_seq OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: fare_policy_slabs_details_slab_id_seq; Type: SEQUENCE OWNED BY; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER SEQUENCE atlas_driver_offer_bpp.fare_policy_slabs_details_slab_id_seq OWNED BY atlas_driver_offer_bpp.fare_policy_slabs_details_slab.id;


--
-- Name: fare_product; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.fare_product (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    fare_policy_id character(36) NOT NULL,
    vehicle_variant character varying(60) NOT NULL,
    area text NOT NULL,
    flow character varying(60) NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.fare_product OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: feedback; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.feedback (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    badge character varying(255) NOT NULL,
    created_at timestamp with time zone NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.feedback OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: feedback_badge; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.feedback_badge (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    badge character varying(255) NOT NULL,
    badge_count integer DEFAULT 0 NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.feedback_badge OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: feedback_form; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.feedback_form (
    category_name character varying(255) NOT NULL,
    id character varying(36) NOT NULL,
    rating integer,
    question character varying(255) NOT NULL,
    answer text[] NOT NULL,
    answer_type character varying(255) NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.feedback_form OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: geometry; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.geometry (
    id character(36) DEFAULT atlas_driver_offer_bpp.uuid_generate_v4() NOT NULL,
    region character varying(255) NOT NULL,
    geom public.geometry(MultiPolygon)
);


ALTER TABLE atlas_driver_offer_bpp.geometry OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: go_home_config; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.go_home_config (
    merchant_id character(36) NOT NULL,
    enable_go_home boolean DEFAULT true NOT NULL,
    start_cnt integer DEFAULT 2 NOT NULL,
    dest_radius_meters integer DEFAULT 3000 NOT NULL,
    active_time integer DEFAULT 1800 NOT NULL,
    update_home_location_after_sec integer DEFAULT 2592000 NOT NULL,
    cancellation_cnt integer DEFAULT 2 NOT NULL,
    num_home_locations integer DEFAULT 5 NOT NULL,
    go_home_from_location_radius integer DEFAULT 7000 NOT NULL,
    go_home_way_point_radius integer DEFAULT 2000 NOT NULL,
    num_drivers_for_dir_check integer DEFAULT 5 NOT NULL,
    go_home_batch_delay integer DEFAULT 4 NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    ignore_waypoints_till integer DEFAULT 3000 NOT NULL,
    add_start_waypoint_at integer DEFAULT 3000 NOT NULL,
    new_loc_allowed_radius integer DEFAULT 20 NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.go_home_config OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: idfy_verification; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.idfy_verification (
    id character(36) NOT NULL,
    driver_id character varying(36) NOT NULL,
    request_id character varying(36) NOT NULL,
    doc_type character varying(36) NOT NULL,
    status character varying(20) NOT NULL,
    idfy_response character(2555),
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    document_image_id1 character varying(36) DEFAULT 'no-id'::character varying NOT NULL,
    document_image_id2 character varying(36),
    issue_date_on_doc timestamp with time zone,
    document_number_encrypted character varying(255) NOT NULL,
    document_number_hash bytea,
    image_extraction_validation character varying(255) NOT NULL,
    driver_date_of_birth timestamp with time zone,
    multiple_r_c boolean,
    dashboard_passed_vehicle_variant character varying(255)
);


ALTER TABLE atlas_driver_offer_bpp.idfy_verification OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: image; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.image (
    id character(36) NOT NULL,
    person_id character varying(36) NOT NULL,
    merchant_id character varying(36) NOT NULL,
    s3_path character varying(255) NOT NULL,
    image_type character varying(36) NOT NULL,
    is_valid boolean NOT NULL,
    created_at timestamp with time zone NOT NULL,
    failure_reason character varying(500)
);


ALTER TABLE atlas_driver_offer_bpp.image OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: invoice; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.invoice (
    id character(36) NOT NULL,
    invoice_short_id text NOT NULL,
    driver_fee_id text NOT NULL,
    invoice_status text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    max_mandate_amount integer,
    payment_mode text DEFAULT 'MANUAL_INVOICE'::text NOT NULL,
    bank_error_message text,
    bank_error_code text,
    bank_error_updated_at timestamp with time zone,
    driver_id text,
    last_status_checked_at timestamp with time zone
);


ALTER TABLE atlas_driver_offer_bpp.invoice OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: issue_category; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.issue_category (
    id character(36) NOT NULL,
    category character varying(255) NOT NULL,
    logo_url character varying(255) NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.issue_category OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: issue_option; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.issue_option (
    id character(36) NOT NULL,
    issue_category_id character(36) NOT NULL,
    option character varying(255) NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.issue_option OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: issue_report; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.issue_report (
    id character varying(255) NOT NULL,
    driver_id character varying(255) NOT NULL,
    ride_id character varying(255),
    description character varying(255) NOT NULL,
    assignee character varying(255),
    status character varying(255) NOT NULL,
    deleted boolean,
    media_files text[],
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    category_id character(36) NOT NULL,
    option_id character(36),
    ticket_id character varying(255)
);


ALTER TABLE atlas_driver_offer_bpp.issue_report OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: issue_translation; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.issue_translation (
    id character(36) NOT NULL,
    sentence character varying(255) NOT NULL,
    translation character varying(255) NOT NULL,
    language character varying(255) NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.issue_translation OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: kiosk_location; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.kiosk_location (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    address text NOT NULL,
    landmark text NOT NULL,
    contact character varying(15),
    longitude double precision NOT NULL,
    latitude double precision NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.kiosk_location OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: kiosk_location_translation; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.kiosk_location_translation (
    kiosk_location_id character(36) NOT NULL,
    language character(36) NOT NULL,
    landmark character varying(255) NOT NULL,
    address text NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.kiosk_location_translation OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: leader_board_configs; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.leader_board_configs (
    id character(36) NOT NULL,
    leader_board_type text NOT NULL,
    number_of_sets integer NOT NULL,
    leader_board_expiry integer NOT NULL,
    z_score_base integer NOT NULL,
    leader_board_length_limit integer NOT NULL,
    merchant_id character(36),
    is_enabled boolean DEFAULT true NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.leader_board_configs OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: location; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.location (
    id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    street text,
    door text,
    city text,
    state text,
    country text,
    building text,
    full_address text,
    area_code text,
    area text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.location OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: location_mapping; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.location_mapping (
    id character(36) NOT NULL,
    location_id character varying(255) NOT NULL,
    tag character varying(255) NOT NULL,
    entity_id character varying(255) NOT NULL,
    "order" integer NOT NULL,
    version character varying(255) NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.location_mapping OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: mandate; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.mandate (
    id text NOT NULL,
    max_amount integer NOT NULL,
    status text NOT NULL,
    payer_vpa text,
    start_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    end_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    payer_app text,
    payer_app_name text,
    mandate_payment_flow text
);


ALTER TABLE atlas_driver_offer_bpp.mandate OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: media_file; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.media_file (
    id character(36) NOT NULL,
    type character(36) NOT NULL,
    url text NOT NULL,
    created_at timestamp without time zone NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.media_file OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: merchant; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.merchant (
    id character(36) NOT NULL,
    name character varying(255),
    subscriber_id character varying(255) NOT NULL,
    gstin character varying(255),
    status character varying(255),
    verified boolean NOT NULL,
    enabled boolean DEFAULT true NOT NULL,
    description text,
    mobile_number text,
    mobile_country_code character varying(255),
    from_time timestamp with time zone,
    to_time timestamp with time zone,
    api_key text,
    head_count bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    info text,
    unique_key_id character varying(255) DEFAULT 'FIXME'::character varying NOT NULL,
    short_id character varying(255) NOT NULL,
    origin_restriction text[] NOT NULL,
    destination_restriction text[] NOT NULL,
    internal_api_key character varying(128) NOT NULL,
    city text DEFAULT 'Kochi'::text NOT NULL,
    geo_hash_precision_value integer DEFAULT 9 NOT NULL,
    country text,
    minimum_driver_rates_count integer DEFAULT 5 NOT NULL,
    registry_url character varying(255) DEFAULT 'http://localhost:8020'::character varying
);


ALTER TABLE atlas_driver_offer_bpp.merchant OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: merchant_message; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.merchant_message (
    merchant_id character(36) NOT NULL,
    message_key character varying(255) NOT NULL,
    message character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.merchant_message OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: merchant_overlay; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.merchant_overlay (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    overlay_key character varying(255) NOT NULL,
    language character varying(255) NOT NULL,
    udf1 character varying(255),
    title text,
    description text,
    image_url text,
    ok_button_text text,
    cancel_button_text text,
    actions text[] DEFAULT '{}'::text[] NOT NULL,
    link text,
    req_body json DEFAULT json_build_object() NOT NULL,
    end_point text,
    method text
);


ALTER TABLE atlas_driver_offer_bpp.merchant_overlay OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: merchant_payment_method; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.merchant_payment_method (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    payment_type character varying(30) NOT NULL,
    payment_instrument character varying(255) NOT NULL,
    collected_by character varying(30) NOT NULL,
    priority integer NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.merchant_payment_method OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: merchant_service_config; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.merchant_service_config (
    merchant_id character(36) NOT NULL,
    service_name character varying(30) NOT NULL,
    config_json json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.merchant_service_config OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: merchant_service_usage_config; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.merchant_service_usage_config (
    merchant_id character(36) NOT NULL,
    get_distances character varying(30) NOT NULL,
    get_routes character varying(30) NOT NULL,
    snap_to_road character varying(30) NOT NULL,
    get_place_name character varying(30) NOT NULL,
    get_place_details character varying(30) NOT NULL,
    auto_complete character varying(30) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    sms_providers_priority_list text[] NOT NULL,
    get_estimated_pickup_distances text NOT NULL,
    whatsapp_providers_priority_list text[] NOT NULL,
    get_pickup_routes text DEFAULT 'Google'::text,
    get_trip_routes text DEFAULT 'Google'::text,
    verification_service character varying(30) NOT NULL,
    initiate_call character varying(30) NOT NULL,
    get_distances_for_cancel_ride text NOT NULL,
    aadhaar_verification_service character varying(30) NOT NULL,
    face_verification_service character varying(30),
    issue_ticket_service character varying(30) DEFAULT 'Kapture'::character varying NOT NULL,
    get_exophone character varying(255) DEFAULT 'Exotel'::character varying NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: message; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.message (
    id character(36) NOT NULL,
    type character(100) NOT NULL,
    title character varying(255) NOT NULL,
    description text NOT NULL,
    media_files text[],
    merchant_id character(36),
    created_at timestamp without time zone NOT NULL,
    label character varying(255),
    like_count integer DEFAULT 0 NOT NULL,
    short_description text DEFAULT ''::text,
    view_count integer DEFAULT 0 NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.message OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: message_report; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.message_report (
    message_id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    delivery_status character(36) NOT NULL,
    read_status boolean NOT NULL,
    reply text,
    message_dynamic_fields json,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    like_status boolean DEFAULT false NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.message_report OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: message_translation; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.message_translation (
    message_id character(36) NOT NULL,
    language character(36) NOT NULL,
    title character varying(255) NOT NULL,
    description text NOT NULL,
    created_at timestamp without time zone NOT NULL,
    label character varying(255),
    short_description text DEFAULT ''::text
);


ALTER TABLE atlas_driver_offer_bpp.message_translation OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: meta_data; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.meta_data (
    driver_id character(36) NOT NULL,
    device text,
    device_o_s text,
    device_date_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    app_permissions text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


ALTER TABLE atlas_driver_offer_bpp.meta_data OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: notification; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.notification (
    id character(36) NOT NULL,
    short_id character varying(255) NOT NULL,
    source_amount numeric(30,2) NOT NULL,
    mandate_id character varying(255) NOT NULL,
    driver_fee_id character varying(255) NOT NULL,
    txn_date timestamp with time zone NOT NULL,
    juspay_provided_id character varying(255) NOT NULL,
    provider_name text,
    notification_type text,
    description text NOT NULL,
    status text NOT NULL,
    date_created timestamp with time zone NOT NULL,
    last_updated timestamp with time zone NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    last_status_checked_at timestamp with time zone
);


ALTER TABLE atlas_driver_offer_bpp.notification OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: onboarding_document_configs; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.onboarding_document_configs (
    merchant_id character(36) NOT NULL,
    document_type text NOT NULL,
    check_extraction boolean NOT NULL,
    check_expiry boolean NOT NULL,
    vehicle_class_check_type text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    rc_number_prefix text DEFAULT 'KA'::text NOT NULL,
    supported_vehicle_classes_json json NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: operating_city; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.operating_city (
    id character(36) NOT NULL,
    merchant_id character varying(255) NOT NULL,
    city_name character varying(255),
    enabled boolean NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.operating_city OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: payment_order; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.payment_order (
    id character(36) NOT NULL,
    short_id character varying(36) NOT NULL,
    person_id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    amount numeric(30,2) NOT NULL,
    currency character varying(30) NOT NULL,
    status character varying(100) NOT NULL,
    web_payment_link text,
    iframe_payment_link text,
    mobile_payment_link text,
    client_auth_token_encrypted character varying(255),
    client_auth_token_hash bytea,
    client_auth_token_expiry timestamp with time zone,
    get_upi_deep_links_option boolean,
    environment character varying(100),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    payment_service_order_id character varying(255) NOT NULL,
    service character varying(255),
    client_id character varying(255),
    description character varying(1024),
    return_url character varying(255),
    action character varying(255),
    request_id character varying(255),
    payment_merchant_id character varying(255),
    create_mandate text,
    mandate_max_amount integer,
    mandate_start_date timestamp with time zone,
    mandate_end_date timestamp with time zone,
    bank_error_message text,
    bank_error_code text
);


ALTER TABLE atlas_driver_offer_bpp.payment_order OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: payment_transaction; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.payment_transaction (
    id character(36) NOT NULL,
    txn_uuid character varying(255),
    payment_method_type character varying(100),
    payment_method character varying(100),
    resp_message character varying(255),
    resp_code character varying(255),
    gateway_reference_id character varying(100),
    order_id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    amount numeric(30,2) NOT NULL,
    currency character varying(30) NOT NULL,
    date_created timestamp with time zone,
    status_id integer NOT NULL,
    status character varying(100) NOT NULL,
    juspay_response text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    mandate_id text,
    mandate_max_amount integer,
    mandate_frequency text,
    mandate_status text,
    mandate_start_date timestamp with time zone,
    mandate_end_date timestamp with time zone,
    bank_error_message text,
    bank_error_code text
);


ALTER TABLE atlas_driver_offer_bpp.payment_transaction OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: person; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.person (
    id character(36) NOT NULL,
    first_name character varying(255) NOT NULL,
    middle_name character varying(255),
    last_name character varying(255),
    role character varying(255) NOT NULL,
    gender character varying(255) NOT NULL,
    identifier_type character varying(255) NOT NULL,
    email character varying(255),
    password_hash bytea,
    mobile_number_encrypted character varying(255),
    mobile_number_hash bytea,
    mobile_country_code character varying(255),
    identifier character varying(255),
    is_new boolean NOT NULL,
    merchant_id character varying(255) NOT NULL,
    device_token character varying(255),
    description character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    rating double precision,
    language character varying(255),
    client_version character(36),
    bundle_version character(36),
    unencrypted_mobile_number character varying(255),
    whatsapp_notification_enroll_status character varying(255),
    unencrypted_alternate_mobile_number character varying(255),
    alternate_mobile_number_encrypted character varying(255),
    alternate_mobile_number_hash bytea,
    hometown character varying(255),
    languages_spoken text[] DEFAULT '{}'::text[],
    onboarded_from_dashboard boolean DEFAULT false,
    face_image_id character(36)
);


ALTER TABLE atlas_driver_offer_bpp.person OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: place_name_cache; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.place_name_cache (
    id character(36) NOT NULL,
    formatted_address text,
    plus_code text,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    place_id text,
    address_components text[] NOT NULL,
    geo_hash text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.place_name_cache OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: plan; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.plan (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    payment_mode text NOT NULL,
    plan_type text NOT NULL,
    name text NOT NULL,
    description text NOT NULL,
    max_amount integer NOT NULL,
    registration_amount integer NOT NULL,
    plan_base_amount text NOT NULL,
    is_offer_applicable boolean NOT NULL,
    max_credit_limit integer NOT NULL,
    free_ride_count integer NOT NULL,
    frequency text NOT NULL,
    cgst_percentage double precision,
    sgst_percentage double precision
);


ALTER TABLE atlas_driver_offer_bpp.plan OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: plan_translation; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.plan_translation (
    plan_id character(36) NOT NULL,
    language character(36) NOT NULL,
    name character varying(255) NOT NULL,
    description text NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.plan_translation OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: quote_special_zone; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.quote_special_zone (
    id character(36) NOT NULL,
    search_request_id character(36) NOT NULL,
    provider_id character(36) NOT NULL,
    distance integer NOT NULL,
    estimated_fare double precision NOT NULL,
    fare_parameters_id character(36) NOT NULL,
    estimated_finish_time timestamp with time zone NOT NULL,
    vehicle_variant character varying(255) NOT NULL,
    valid_till timestamp without time zone NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    special_location_tag text
);


ALTER TABLE atlas_driver_offer_bpp.quote_special_zone OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: rating; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.rating (
    id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    rating_value bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    driver_id character(36) NOT NULL,
    feedback_details character varying(255)
);


ALTER TABLE atlas_driver_offer_bpp.rating OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: registration_token; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.registration_token (
    id character(36) NOT NULL,
    auth_medium character varying(255) NOT NULL,
    auth_type character varying(255) NOT NULL,
    auth_value_hash character varying(1024) NOT NULL,
    token character varying(1024) NOT NULL,
    verified boolean NOT NULL,
    auth_expiry bigint NOT NULL,
    token_expiry bigint NOT NULL,
    attempts bigint NOT NULL,
    entity_id character(36) NOT NULL,
    entity_type character(36) NOT NULL,
    info text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    alternate_number_attempts integer DEFAULT 5 NOT NULL,
    merchant_id text DEFAULT 'favorit0-0000-0000-0000-00000favorit'::text NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.registration_token OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: registry_map_fallback; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.registry_map_fallback (
    subscriber_id character(36) NOT NULL,
    unique_id character(36) NOT NULL,
    registry_url character varying(255) NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.registry_map_fallback OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: ride; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.ride (
    id character(36) NOT NULL,
    booking_id character(36) NOT NULL,
    short_id character varying(36) NOT NULL,
    status character varying(255) NOT NULL,
    driver_id character(36) NOT NULL,
    otp character(4) NOT NULL,
    tracking_url character varying(255) NOT NULL,
    fare integer,
    traveled_distance double precision DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    trip_start_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    trip_end_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    chargeable_distance integer,
    trip_start_lat double precision,
    trip_start_lon double precision,
    trip_end_lat double precision,
    trip_end_lon double precision,
    driver_arrival_time timestamp with time zone,
    fare_parameters_id character(36),
    distance_calculation_failed boolean,
    pickup_drop_outside_of_threshold boolean,
    merchant_id character(36),
    number_of_deviation boolean,
    driver_deviated_from_route boolean,
    number_of_snap_to_road_calls integer,
    driver_go_home_request_id character(36),
    ui_distance_calculation_with_accuracy integer,
    ui_distance_calculation_without_accuracy integer
);


ALTER TABLE atlas_driver_offer_bpp.ride OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: ride_details; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.ride_details (
    id character(36) NOT NULL,
    driver_name character varying(255),
    driver_number_encrypted character varying(255),
    driver_number_hash bytea,
    driver_country_code character varying(255),
    vehicle_number character varying(255) NOT NULL,
    vehicle_color character varying(255),
    vehicle_variant character varying(255),
    vehicle_model character varying(255),
    vehicle_class character varying(255)
);


ALTER TABLE atlas_driver_offer_bpp.ride_details OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: rider_details; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.rider_details (
    id character(36) NOT NULL,
    mobile_country_code character varying(255) NOT NULL,
    mobile_number_encrypted character varying(255) NOT NULL,
    mobile_number_hash bytea,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    referral_code character varying(15),
    referred_by_driver character varying(255),
    referred_at timestamp with time zone,
    has_taken_valid_ride boolean NOT NULL,
    has_taken_valid_ride_at timestamp with time zone,
    merchant_id character(36) DEFAULT 'favorit0-0000-0000-0000-00000favorit'::bpchar NOT NULL,
    otp_code text
);


ALTER TABLE atlas_driver_offer_bpp.rider_details OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: scheduler_job; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.scheduler_job (
    id character varying(255) NOT NULL,
    job_type character varying(255) NOT NULL,
    job_data text NOT NULL,
    scheduled_at timestamp without time zone NOT NULL,
    maximum_delay integer,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    max_errors integer NOT NULL,
    curr_errors integer NOT NULL,
    status character varying(255) NOT NULL,
    shard_id integer,
    parent_job_id character(36)
);


ALTER TABLE atlas_driver_offer_bpp.scheduler_job OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: schema_migrations; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.schema_migrations OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: search_request; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.search_request (
    id character(36) NOT NULL,
    transaction_id character(36) NOT NULL,
    provider_id character varying(255) NOT NULL,
    from_location_id character varying(36),
    to_location_id character varying(36),
    bap_id character varying(255) NOT NULL,
    bap_uri character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    estimated_duration integer NOT NULL,
    estimated_distance integer NOT NULL,
    auto_assign_enabled boolean DEFAULT false,
    device text,
    customer_language character(36),
    special_location_tag text,
    area text,
    bap_city text,
    bap_country text,
    disability_tag text
);


ALTER TABLE atlas_driver_offer_bpp.search_request OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: search_request_for_driver; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.search_request_for_driver (
    id character(36) NOT NULL,
    search_request_id character(36) NOT NULL,
    actual_distance_to_pickup bigint NOT NULL,
    duration_to_pickup bigint NOT NULL,
    vehicle_variant character varying(255) NOT NULL,
    search_request_valid_till timestamp without time zone NOT NULL,
    driver_id character(36) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    start_time timestamp with time zone NOT NULL,
    status character varying(255) NOT NULL,
    lat double precision,
    lon double precision,
    straight_line_distance_to_pickup bigint NOT NULL,
    response character varying(255),
    driver_min_extra_fee double precision,
    driver_max_extra_fee double precision,
    batch_number integer,
    ride_request_popup_delay_duration integer DEFAULT 0 NOT NULL,
    parallel_search_request_count smallint,
    is_part_of_intelligent_pool boolean NOT NULL,
    cancellation_ratio real,
    acceptance_ratio real,
    driver_available_time real,
    driver_speed double precision,
    mode text,
    search_try_id character(36) NOT NULL,
    keep_hidden_for_seconds integer DEFAULT 0 NOT NULL,
    merchant_id character(36),
    go_home_request_id character(36)
);


ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: search_request_location; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.search_request_location (
    id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    city character varying(255),
    state character varying(255),
    country character varying(255),
    street character varying(255),
    building character varying(255),
    area_code character varying(255),
    area character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    full_address character varying(255),
    door character varying(255)
);


ALTER TABLE atlas_driver_offer_bpp.search_request_location OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: search_request_special_zone; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.search_request_special_zone (
    id character(36) NOT NULL,
    transaction_id character(36) NOT NULL,
    message_id character(36) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    valid_till timestamp with time zone NOT NULL,
    provider_id character varying(255) NOT NULL,
    from_location_id character varying(36),
    to_location_id character varying(36),
    bap_id character varying(255) NOT NULL,
    bap_uri character varying(255) NOT NULL,
    estimated_duration integer,
    estimated_distance integer,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    area text
);


ALTER TABLE atlas_driver_offer_bpp.search_request_special_zone OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: search_try; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.search_try (
    id character(36) NOT NULL,
    message_id character(36),
    valid_till timestamp with time zone,
    created_at timestamp with time zone,
    start_time timestamp with time zone,
    vehicle_variant character(255),
    status text,
    updated_at timestamp with time zone,
    search_repeat_counter integer,
    customer_extra_fee integer,
    estimate_id character(36),
    request_id character(36) NOT NULL,
    search_repeat_type character varying(255) NOT NULL,
    base_fare integer NOT NULL,
    merchant_id character(36)
);


ALTER TABLE atlas_driver_offer_bpp.search_try OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: special_location; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.special_location (
    id character(36) NOT NULL,
    location_name character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    gates text[] NOT NULL,
    geom public.geometry(MultiPolygon),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.special_location OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: special_location_priority; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.special_location_priority (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    category character varying(255) NOT NULL,
    pickup_priority integer NOT NULL,
    drop_priority integer NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.special_location_priority OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: tag_category_mapping; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.tag_category_mapping (
    id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.tag_category_mapping OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: transporter_config; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.transporter_config (
    merchant_id character(36) NOT NULL,
    pickup_loc_threshold bigint NOT NULL,
    drop_loc_threshold bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ride_time_estimated_threshold bigint NOT NULL,
    fcm_url text NOT NULL,
    fcm_service_account text NOT NULL,
    fcm_token_key_prefix text NOT NULL,
    referral_link_password text NOT NULL,
    popup_delay_to_add_as_penalty integer,
    threshold_cancellation_score integer,
    min_rides_for_cancellation_score integer,
    onboarding_try_limit integer NOT NULL,
    onboarding_retry_time_in_hours integer NOT NULL,
    check_image_extraction_for_dashboard boolean NOT NULL,
    search_repeat_limit integer NOT NULL,
    default_popup_delay integer NOT NULL,
    media_file_url_pattern text DEFAULT 'http://localhost:8016/ui/<DOMAIN>/media?filePath=<FILE_PATH>'::text NOT NULL,
    media_file_size_upper_limit integer DEFAULT 10000000 NOT NULL,
    include_driver_currently_on_ride boolean DEFAULT true,
    actual_ride_distance_diff_threshold double precision DEFAULT 1200 NOT NULL,
    upwards_recompute_buffer double precision DEFAULT 2000 NOT NULL,
    approx_ride_distance_diff_threshold double precision DEFAULT 1200 NOT NULL,
    min_location_accuracy double precision DEFAULT 50 NOT NULL,
    threshold_cancellation_percentage_to_unlist integer,
    min_rides_to_unlist integer,
    driver_payment_cycle_duration integer DEFAULT 86400 NOT NULL,
    driver_payment_cycle_start_time integer DEFAULT 36000 NOT NULL,
    driver_payment_cycle_buffer integer DEFAULT 14400 NOT NULL,
    driver_payment_reminder_interval integer DEFAULT 1800 NOT NULL,
    time_diff_from_utc integer DEFAULT 19800 NOT NULL,
    subscription boolean DEFAULT false NOT NULL,
    aadhaar_verification_required boolean DEFAULT false NOT NULL,
    rc_limit integer DEFAULT 3 NOT NULL,
    automatic_r_c_activation_cut_off integer DEFAULT 432000 NOT NULL,
    enable_dashboard_sms boolean NOT NULL,
    driver_auto_pay_notification_time bigint DEFAULT 32400,
    driver_auto_pay_execution_time bigint DEFAULT 104400,
    subscription_start_time timestamp with time zone DEFAULT '2023-08-31 00:00:00'::timestamp without time zone NOT NULL,
    mandate_validity integer DEFAULT 5 NOT NULL,
    driver_location_accuracy_buffer integer DEFAULT 10 NOT NULL,
    route_deviation_threshold integer DEFAULT 50 NOT NULL,
    can_downgrade_to_sedan boolean DEFAULT false NOT NULL,
    can_downgrade_to_hatchback boolean DEFAULT false NOT NULL,
    can_downgrade_to_taxi boolean DEFAULT false NOT NULL,
    is_avoid_toll boolean DEFAULT true NOT NULL,
    special_zone_booking_otp_expiry integer DEFAULT 60 NOT NULL,
    aadhaar_image_resize_config json,
    bank_error_expiry bigint DEFAULT 3600 NOT NULL,
    driver_fee_calculation_time bigint,
    driver_fee_calculator_batch_size integer,
    driver_fee_calculator_batch_gap bigint,
    driver_fee_mandate_notification_batch_size integer DEFAULT 20 NOT NULL,
    driver_fee_mandate_execution_batch_size integer DEFAULT 20 NOT NULL,
    mandate_notification_reschedule_interval bigint DEFAULT 60 NOT NULL,
    mandate_execution_reschedule_interval bigint DEFAULT 60 NOT NULL,
    is_plan_mandatory boolean DEFAULT false NOT NULL,
    free_trial_days integer DEFAULT 0 NOT NULL,
    open_market_un_blocked boolean DEFAULT false NOT NULL,
    driver_fee_retry_threshold_config integer DEFAULT 3 NOT NULL,
    update_notification_status_batch_size integer DEFAULT 20 NOT NULL,
    update_order_status_batch_size integer DEFAULT 20 NOT NULL,
    order_and_notification_status_check_time bigint DEFAULT 63000,
    cache_offer_list_by_driver_id boolean DEFAULT false NOT NULL,
    use_offer_list_cache boolean DEFAULT true NOT NULL,
    order_and_notification_status_check_time_limit bigint DEFAULT 345600
);


ALTER TABLE atlas_driver_offer_bpp.transporter_config OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: vehicle; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.vehicle (
    driver_id character(36) NOT NULL,
    capacity bigint,
    category character varying(255),
    make character varying(255),
    model character varying(255) NOT NULL,
    size character varying(255),
    variant character varying(255) NOT NULL,
    color character varying(255) NOT NULL,
    energy_type character varying(255),
    registration_no character varying(255) NOT NULL,
    registration_category character varying(255),
    merchant_id character(36),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    vehicle_class character varying(255) NOT NULL,
    vehicle_name character varying(255),
    fleet_owner_id character varying(36)
);


ALTER TABLE atlas_driver_offer_bpp.vehicle OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: vehicle_registration_certificate; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.vehicle_registration_certificate (
    id character(36) NOT NULL,
    fitness_expiry timestamp with time zone NOT NULL,
    permit_expiry timestamp with time zone,
    vehicle_class character(36),
    insurance_validity timestamp with time zone,
    created_at timestamp with time zone NOT NULL,
    verification_status character varying(20) NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    puc_expiry timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    vehicle_manufacturer character(36),
    failed_rules text[],
    document_image_id character varying(36) DEFAULT 'no-id'::character varying NOT NULL,
    certificate_number_hash bytea,
    certificate_number_encrypted character varying(255),
    vehicle_capacity character varying(255),
    vehicle_model character varying(255),
    vehicle_color character varying(255),
    vehicle_energy_type character varying(255),
    vehicle_variant character varying(255)
);


ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: volunteer; Type: TABLE; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE TABLE atlas_driver_offer_bpp.volunteer (
    id character(36) NOT NULL,
    place character varying(255) NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.volunteer OWNER TO atlas_driver_offer_bpp_user;

--
-- Name: beckn_request; Type: TABLE; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

CREATE TABLE atlas_public_transport.beckn_request (
    id character varying(36) NOT NULL,
    beckn_request text NOT NULL,
    signature_header text NOT NULL,
    time_stamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_public_transport.beckn_request OWNER TO atlas_public_transport_user;

--
-- Name: booking; Type: TABLE; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

CREATE TABLE atlas_public_transport.booking (
    id character(36) NOT NULL,
    search_id character(36) NOT NULL,
    quote_id character(36) NOT NULL,
    bkn_txn_id character(36) NOT NULL,
    requestor_id character(36) NOT NULL,
    quantity integer NOT NULL,
    bpp_id character(36) NOT NULL,
    bpp_url character varying(255) NOT NULL,
    public_transport_support_number character varying(16) NOT NULL,
    description character varying(255) NOT NULL,
    fare numeric(30,2) NOT NULL,
    departure_time timestamp with time zone,
    arrival_time timestamp with time zone,
    departure_station_id character(36) NOT NULL,
    arrival_station_id character(36) NOT NULL,
    status character varying(255) NOT NULL,
    ticket_id character varying(255),
    ticket_created_at timestamp with time zone,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_public_transport.booking OWNER TO atlas_public_transport_user;

--
-- Name: payment_transaction; Type: TABLE; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

CREATE TABLE atlas_public_transport.payment_transaction (
    id character(36) NOT NULL,
    booking_id character(36) NOT NULL,
    bkn_txn_id character(36) NOT NULL,
    payment_gateway_txn_id character varying(255) NOT NULL,
    payment_gateway_txn_status character varying(255) NOT NULL,
    fare numeric(30,2) NOT NULL,
    status character varying(255) NOT NULL,
    payment_url character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_public_transport.payment_transaction OWNER TO atlas_public_transport_user;

--
-- Name: quote; Type: TABLE; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

CREATE TABLE atlas_public_transport.quote (
    id character(36) NOT NULL,
    search_id character(36) NOT NULL,
    bpp_id character(36) NOT NULL,
    bpp_url character varying(255) NOT NULL,
    description character varying(255) NOT NULL,
    fare numeric(30,2) NOT NULL,
    departure_time timestamp with time zone,
    arrival_time timestamp with time zone,
    departure_station_id character(36) NOT NULL,
    arrival_station_id character(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    route_code character varying(255) NOT NULL
);


ALTER TABLE atlas_public_transport.quote OWNER TO atlas_public_transport_user;

--
-- Name: schema_migrations; Type: TABLE; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

CREATE TABLE atlas_public_transport.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_public_transport.schema_migrations OWNER TO atlas_public_transport_user;

--
-- Name: search; Type: TABLE; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

CREATE TABLE atlas_public_transport.search (
    id character(36) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    requestor_id character(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_public_transport.search OWNER TO atlas_public_transport_user;

--
-- Name: transport_station; Type: TABLE; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

CREATE TABLE atlas_public_transport.transport_station (
    id character(36) NOT NULL,
    name character varying(255) NOT NULL,
    station_code character varying(255) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL
);


ALTER TABLE atlas_public_transport.transport_station OWNER TO atlas_public_transport_user;

--
-- Name: schema_migrations; Type: TABLE; Schema: atlas_registry; Owner: atlas_registry_user
--

CREATE TABLE atlas_registry.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_registry.schema_migrations OWNER TO atlas_registry_user;

--
-- Name: subscriber; Type: TABLE; Schema: atlas_registry; Owner: atlas_registry_user
--

CREATE TABLE atlas_registry.subscriber (
    unique_key_id character varying(255) NOT NULL,
    subscriber_id character varying(255) NOT NULL,
    subscriber_url character varying(255) NOT NULL,
    type character varying(255) NOT NULL,
    domain character varying(255) NOT NULL,
    city character varying(255),
    country character varying(255),
    status character varying(255),
    signing_public_key character varying(255) NOT NULL,
    encr_public_key character varying(255),
    valid_from timestamp with time zone,
    valid_until timestamp with time zone,
    created timestamp with time zone DEFAULT now() NOT NULL,
    updated timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_registry.subscriber OWNER TO atlas_registry_user;

--
-- Name: job; Type: TABLE; Schema: atlas_scheduler_example; Owner: atlas_scheduler_example_user
--

CREATE TABLE atlas_scheduler_example.job (
    id character varying(255) NOT NULL,
    job_type character varying(255) NOT NULL,
    job_data text NOT NULL,
    scheduled_at timestamp without time zone NOT NULL,
    maximum_delay integer,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    max_errors integer NOT NULL,
    curr_errors integer NOT NULL,
    status character varying(255) NOT NULL
);


ALTER TABLE atlas_scheduler_example.job OWNER TO atlas_scheduler_example_user;

--
-- Name: schema_migrations; Type: TABLE; Schema: atlas_scheduler_example; Owner: atlas_scheduler_example_user
--

CREATE TABLE atlas_scheduler_example.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_scheduler_example.schema_migrations OWNER TO atlas_scheduler_example_user;

--
-- Name: entry_exit; Type: TABLE; Schema: atlas_special_zone; Owner: atlas_special_zone_user
--

CREATE TABLE atlas_special_zone.entry_exit (
    id character(36) NOT NULL,
    special_zone_id character(36) NOT NULL,
    entry_exit_type character varying(20) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    area character varying(255),
    address character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_special_zone.entry_exit OWNER TO atlas_special_zone_user;

--
-- Name: schema_migrations; Type: TABLE; Schema: atlas_special_zone; Owner: atlas_special_zone_user
--

CREATE TABLE atlas_special_zone.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE atlas_special_zone.schema_migrations OWNER TO atlas_special_zone_user;

--
-- Name: special_zone; Type: TABLE; Schema: atlas_special_zone; Owner: atlas_special_zone_user
--

CREATE TABLE atlas_special_zone.special_zone (
    id character(36) NOT NULL,
    name character varying(255) NOT NULL,
    category_code character varying(30) NOT NULL,
    geom public.geometry(MultiPolygon),
    geo_json text,
    city character varying(20),
    state character varying(20),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT no_overlaps CHECK (atlas_special_zone.no_overlaps_in_special_zone(id, geom))
);


ALTER TABLE atlas_special_zone.special_zone OWNER TO atlas_special_zone_user;

--
-- Name: tag_category_mapping; Type: TABLE; Schema: atlas_special_zone; Owner: atlas_special_zone_user
--

CREATE TABLE atlas_special_zone.tag_category_mapping (
    id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    category_code character varying(30) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_special_zone.tag_category_mapping OWNER TO atlas_special_zone_user;

--
-- Name: fare_policy_driver_extra_fee_bounds id; Type: DEFAULT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ALTER COLUMN id SET DEFAULT nextval('atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds_id_seq'::regclass);


--
-- Name: fare_policy_progressive_details_per_extra_km_rate_section id; Type: DEFAULT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ALTER COLUMN id SET DEFAULT nextval('atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_sectio_id_seq'::regclass);


--
-- Name: fare_policy_slabs_details_slab id; Type: DEFAULT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_policy_slabs_details_slab ALTER COLUMN id SET DEFAULT nextval('atlas_driver_offer_bpp.fare_policy_slabs_details_slab_id_seq'::regclass);


--
-- Name: aadhaar_otp_req aadhaar_otp_req_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.aadhaar_otp_req
    ADD CONSTRAINT aadhaar_otp_req_pkey PRIMARY KEY (id);


--
-- Name: aadhaar_otp_verify aadhaar_otp_verify_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.aadhaar_otp_verify
    ADD CONSTRAINT aadhaar_otp_verify_pkey PRIMARY KEY (id);


--
-- Name: aadhaar_verification aadhaar_verification_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.aadhaar_verification
    ADD CONSTRAINT aadhaar_verification_pkey PRIMARY KEY (person_id);


--
-- Name: app_installs app_installs_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.app_installs
    ADD CONSTRAINT app_installs_pkey PRIMARY KEY (id);


--
-- Name: beckn_request beckn_request_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.beckn_request
    ADD CONSTRAINT beckn_request_pkey PRIMARY KEY (id);


--
-- Name: booking_cancellation_reason booking_cancellation_reason_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.booking_cancellation_reason
    ADD CONSTRAINT booking_cancellation_reason_pkey PRIMARY KEY (booking_id);


--
-- Name: booking_location booking_location_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.booking_location
    ADD CONSTRAINT booking_location_pkey PRIMARY KEY (id);


--
-- Name: call_status call_status_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.call_status
    ADD CONSTRAINT call_status_pkey PRIMARY KEY (id);


--
-- Name: callback_request callback_request_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.callback_request
    ADD CONSTRAINT callback_request_pkey PRIMARY KEY (id);


--
-- Name: cancellation_reason cancellation_reason_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.cancellation_reason
    ADD CONSTRAINT cancellation_reason_pkey PRIMARY KEY (reason_code);


--
-- Name: directions_cache directions_cache_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.directions_cache
    ADD CONSTRAINT directions_cache_pkey PRIMARY KEY (id);


--
-- Name: driver_offer driver_offer_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.driver_offer
    ADD CONSTRAINT driver_offer_pkey PRIMARY KEY (id);


--
-- Name: estimate_breakup estimate_breakup_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.estimate_breakup
    ADD CONSTRAINT estimate_breakup_pkey PRIMARY KEY (id);


--
-- Name: estimate estimate_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.estimate
    ADD CONSTRAINT estimate_pkey PRIMARY KEY (id);


--
-- Name: exophone exophone_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.exophone
    ADD CONSTRAINT exophone_pkey PRIMARY KEY (id);


--
-- Name: exophone exophone_unique_backup_phone; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.exophone
    ADD CONSTRAINT exophone_unique_backup_phone UNIQUE (backup_phone);


--
-- Name: exophone exophone_unique_primary_phone; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.exophone
    ADD CONSTRAINT exophone_unique_primary_phone UNIQUE (primary_phone);


--
-- Name: fare_breakup fare_breakup_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.fare_breakup
    ADD CONSTRAINT fare_breakup_pkey PRIMARY KEY (id);


--
-- Name: feedback_form feedback_form_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.feedback_form
    ADD CONSTRAINT feedback_form_pkey PRIMARY KEY (id);


--
-- Name: search_request idx_16386_primary; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.search_request
    ADD CONSTRAINT idx_16386_primary PRIMARY KEY (id);


--
-- Name: quote idx_16394_primary; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.quote
    ADD CONSTRAINT idx_16394_primary PRIMARY KEY (id);


--
-- Name: search_request_location idx_16434_primary; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.search_request_location
    ADD CONSTRAINT idx_16434_primary PRIMARY KEY (id);


--
-- Name: black_list_org idx_16442_primary; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.black_list_org
    ADD CONSTRAINT idx_16442_primary PRIMARY KEY (id);


--
-- Name: person idx_16451_primary; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.person
    ADD CONSTRAINT idx_16451_primary PRIMARY KEY (id);


--
-- Name: registration_token idx_16467_primary; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.registration_token
    ADD CONSTRAINT idx_16467_primary PRIMARY KEY (id);


--
-- Name: tag idx_16475_primary; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.tag
    ADD CONSTRAINT idx_16475_primary PRIMARY KEY (id);


--
-- Name: saved_location idx_primary_search; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.saved_location
    ADD CONSTRAINT idx_primary_search PRIMARY KEY (id);


--
-- Name: rating idx_rating_primary; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.rating
    ADD CONSTRAINT idx_rating_primary PRIMARY KEY (ride_id);


--
-- Name: location_mapping location_mapping_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.location_mapping
    ADD CONSTRAINT location_mapping_pkey PRIMARY KEY (id);


--
-- Name: location location_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.location
    ADD CONSTRAINT location_pkey PRIMARY KEY (id);


--
-- Name: merchant_config merchant_config_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.merchant_config
    ADD CONSTRAINT merchant_config_pkey PRIMARY KEY (id);


--
-- Name: merchant_message merchant_message_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.merchant_message
    ADD CONSTRAINT merchant_message_pkey PRIMARY KEY (merchant_id, message_key);


--
-- Name: merchant_payment_method merchant_payment_method_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.merchant_payment_method
    ADD CONSTRAINT merchant_payment_method_pkey PRIMARY KEY (id);


--
-- Name: merchant merchant_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.merchant
    ADD CONSTRAINT merchant_pkey PRIMARY KEY (id);


--
-- Name: merchant_service_config merchant_service_config_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.merchant_service_config
    ADD CONSTRAINT merchant_service_config_pkey PRIMARY KEY (merchant_id, service_name);


--
-- Name: merchant_service_usage_config merchant_service_usage_config_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.merchant_service_usage_config
    ADD CONSTRAINT merchant_service_usage_config_pkey PRIMARY KEY (merchant_id);


--
-- Name: merchant merchant_short_id_key; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.merchant
    ADD CONSTRAINT merchant_short_id_key UNIQUE (short_id);


--
-- Name: on_search_event on_search_event_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.on_search_event
    ADD CONSTRAINT on_search_event_pkey PRIMARY KEY (id);


--
-- Name: payment_order payment_order_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.payment_order
    ADD CONSTRAINT payment_order_pkey PRIMARY KEY (id);


--
-- Name: payment_transaction payment_transaction_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.payment_transaction
    ADD CONSTRAINT payment_transaction_pkey PRIMARY KEY (id);


--
-- Name: person_default_emergency_number person_default_emergency_number_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.person_default_emergency_number
    ADD CONSTRAINT person_default_emergency_number_pkey PRIMARY KEY (person_id, mobile_country_code, mobile_number_hash);


--
-- Name: person_disability person_disability_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.person_disability
    ADD CONSTRAINT person_disability_pkey PRIMARY KEY (person_id);


--
-- Name: person person_email_encrypted_key; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.person
    ADD CONSTRAINT person_email_encrypted_key UNIQUE (email_encrypted);


--
-- Name: person person_email_hash_key; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.person
    ADD CONSTRAINT person_email_hash_key UNIQUE (email_hash);


--
-- Name: person_flow_status person_flow_status_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.person_flow_status
    ADD CONSTRAINT person_flow_status_pkey PRIMARY KEY (person_id);


--
-- Name: person_stats person_stats_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.person_stats
    ADD CONSTRAINT person_stats_pkey PRIMARY KEY (person_id);


--
-- Name: place_name_cache place_name_cache_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.place_name_cache
    ADD CONSTRAINT place_name_cache_pkey PRIMARY KEY (id);


--
-- Name: rental_slab rental_slab_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.rental_slab
    ADD CONSTRAINT rental_slab_pkey PRIMARY KEY (id);


--
-- Name: booking ride_booking_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.booking
    ADD CONSTRAINT ride_booking_pkey PRIMARY KEY (id);


--
-- Name: ride ride_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.ride
    ADD CONSTRAINT ride_pkey PRIMARY KEY (id);


--
-- Name: sos sos_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.sos
    ADD CONSTRAINT sos_pkey PRIMARY KEY (id);


--
-- Name: special_location special_location_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.special_location
    ADD CONSTRAINT special_location_pkey PRIMARY KEY (id);


--
-- Name: special_zone_quote special_zone_quote_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.special_zone_quote
    ADD CONSTRAINT special_zone_quote_pkey PRIMARY KEY (id);


--
-- Name: tag_category_mapping tag_category_mapping_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.tag_category_mapping
    ADD CONSTRAINT tag_category_mapping_pkey PRIMARY KEY (tag);


--
-- Name: trip_terms trip_terms_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.trip_terms
    ADD CONSTRAINT trip_terms_pkey PRIMARY KEY (id);


--
-- Name: call_status unique_call_sid; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.call_status
    ADD CONSTRAINT unique_call_sid UNIQUE (call_id);


--
-- Name: person unique_identifier; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.person
    ADD CONSTRAINT unique_identifier UNIQUE (identifier);


--
-- Name: app_installs unique_merchantid_devicetoken_source; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.app_installs
    ADD CONSTRAINT unique_merchantid_devicetoken_source UNIQUE (merchant_id, device_token, source);


--
-- Name: person unique_mobile_number_country_code_merchant_id; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.person
    ADD CONSTRAINT unique_mobile_number_country_code_merchant_id UNIQUE (mobile_country_code, mobile_number_hash, merchant_id);


--
-- Name: directions_cache unique_optimal_path; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.directions_cache
    ADD CONSTRAINT unique_optimal_path UNIQUE (origin_hash, dest_hash, slot);


--
-- Name: black_list_org unique_short_id; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.black_list_org
    ADD CONSTRAINT unique_short_id UNIQUE (subscriber_id);


--
-- Name: webengage webengage_pkey; Type: CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.webengage
    ADD CONSTRAINT webengage_pkey PRIMARY KEY (id);


--
-- Name: access_matrix idx_16402_primary; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.access_matrix
    ADD CONSTRAINT idx_16402_primary PRIMARY KEY (id);


--
-- Name: role idx_16419_primary; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.role
    ADD CONSTRAINT idx_16419_primary PRIMARY KEY (id);


--
-- Name: person idx_16451_primary; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.person
    ADD CONSTRAINT idx_16451_primary PRIMARY KEY (id);


--
-- Name: registration_token idx_16467_primary; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.registration_token
    ADD CONSTRAINT idx_16467_primary PRIMARY KEY (id);


--
-- Name: merchant_access idx_16475_primary; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.merchant_access
    ADD CONSTRAINT idx_16475_primary PRIMARY KEY (id);


--
-- Name: transaction idx_transaction_primary; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.transaction
    ADD CONSTRAINT idx_transaction_primary PRIMARY KEY (id);


--
-- Name: merchant merchant_pkey; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.merchant
    ADD CONSTRAINT merchant_pkey PRIMARY KEY (id);


--
-- Name: person unique_email; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.person
    ADD CONSTRAINT unique_email UNIQUE (email_hash);


--
-- Name: person unique_mobile_number_country_code; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.person
    ADD CONSTRAINT unique_mobile_number_country_code UNIQUE (mobile_country_code, mobile_number_hash);


--
-- Name: role unique_name; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.role
    ADD CONSTRAINT unique_name UNIQUE (name);


--
-- Name: access_matrix unique_role_id_api_entity_user_action_type; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.access_matrix
    ADD CONSTRAINT unique_role_id_api_entity_user_action_type UNIQUE (role_id, api_entity, user_action_type);


--
-- Name: merchant unique_short_id; Type: CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.merchant
    ADD CONSTRAINT unique_short_id UNIQUE (short_id);


--
-- Name: access_matrix idx_16402_primary; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.access_matrix
    ADD CONSTRAINT idx_16402_primary PRIMARY KEY (id);


--
-- Name: role idx_16419_primary; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.role
    ADD CONSTRAINT idx_16419_primary PRIMARY KEY (id);


--
-- Name: person idx_16451_primary; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.person
    ADD CONSTRAINT idx_16451_primary PRIMARY KEY (id);


--
-- Name: registration_token idx_16467_primary; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.registration_token
    ADD CONSTRAINT idx_16467_primary PRIMARY KEY (id);


--
-- Name: merchant_access idx_16475_primary; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.merchant_access
    ADD CONSTRAINT idx_16475_primary PRIMARY KEY (id);


--
-- Name: transaction idx_transaction_primary; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.transaction
    ADD CONSTRAINT idx_transaction_primary PRIMARY KEY (id);


--
-- Name: merchant merchant_pkey; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.merchant
    ADD CONSTRAINT merchant_pkey PRIMARY KEY (id);


--
-- Name: person unique_email; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.person
    ADD CONSTRAINT unique_email UNIQUE (email_hash);


--
-- Name: person unique_mobile_number_country_code; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.person
    ADD CONSTRAINT unique_mobile_number_country_code UNIQUE (mobile_country_code, mobile_number_hash);


--
-- Name: role unique_name; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.role
    ADD CONSTRAINT unique_name UNIQUE (name);


--
-- Name: access_matrix unique_role_id_api_entity_user_action_type; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.access_matrix
    ADD CONSTRAINT unique_role_id_api_entity_user_action_type UNIQUE (role_id, api_entity, user_action_type);


--
-- Name: merchant unique_short_id; Type: CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.merchant
    ADD CONSTRAINT unique_short_id UNIQUE (short_id);


--
-- Name: aadhaar_otp_req aadhaar_otp_req_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.aadhaar_otp_req
    ADD CONSTRAINT aadhaar_otp_req_pkey PRIMARY KEY (id);


--
-- Name: aadhaar_otp_verify aadhaar_otp_verify_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.aadhaar_otp_verify
    ADD CONSTRAINT aadhaar_otp_verify_pkey PRIMARY KEY (id);


--
-- Name: aadhaar_verification aadhaar_verification_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.aadhaar_verification
    ADD CONSTRAINT aadhaar_verification_pkey PRIMARY KEY (driver_id);


--
-- Name: bap_metadata bap_metadata_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.bap_metadata
    ADD CONSTRAINT bap_metadata_pkey PRIMARY KEY (id);


--
-- Name: beckn_request beckn_request_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.beckn_request
    ADD CONSTRAINT beckn_request_pkey PRIMARY KEY (id);


--
-- Name: booking_cancellation_reason booking_cancellation_reason_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.booking_cancellation_reason
    ADD CONSTRAINT booking_cancellation_reason_pkey PRIMARY KEY (booking_id);


--
-- Name: booking_location booking_location_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.booking_location
    ADD CONSTRAINT booking_location_pkey PRIMARY KEY (id);


--
-- Name: business_event business_event_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.business_event
    ADD CONSTRAINT business_event_pkey PRIMARY KEY (id);


--
-- Name: call_status call_status_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.call_status
    ADD CONSTRAINT call_status_pkey PRIMARY KEY (id);


--
-- Name: cancellation_reason cancellation_reason_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.cancellation_reason
    ADD CONSTRAINT cancellation_reason_pkey PRIMARY KEY (reason_code);


--
-- Name: comment comment_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.comment
    ADD CONSTRAINT comment_pkey PRIMARY KEY (id);


--
-- Name: driver_availability driver_availability_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_availability
    ADD CONSTRAINT driver_availability_pkey PRIMARY KEY (id);


--
-- Name: driver_block_reason driver_block_reason_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_block_reason
    ADD CONSTRAINT driver_block_reason_pkey PRIMARY KEY (reason_code);


--
-- Name: driver_fee driver_fee_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_fee
    ADD CONSTRAINT driver_fee_pkey PRIMARY KEY (id);


--
-- Name: driver_flow_status driver_flow_status_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_flow_status
    ADD CONSTRAINT driver_flow_status_pkey PRIMARY KEY (person_id);


--
-- Name: driver_go_home_request driver_go_home_request_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_go_home_request
    ADD CONSTRAINT driver_go_home_request_pkey PRIMARY KEY (id);


--
-- Name: driver_home_location driver_home_location_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_home_location
    ADD CONSTRAINT driver_home_location_pkey PRIMARY KEY (id);


--
-- Name: driver_information driver_information_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_information
    ADD CONSTRAINT driver_information_pkey PRIMARY KEY (driver_id);


--
-- Name: driver_intelligent_pool_config driver_intelligent_pool_config_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_intelligent_pool_config
    ADD CONSTRAINT driver_intelligent_pool_config_pkey PRIMARY KEY (merchant_id);


--
-- Name: driver_location driver_location_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_location
    ADD CONSTRAINT driver_location_pkey PRIMARY KEY (driver_id);


--
-- Name: driver_pool_config driver_pool_config_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_pool_config
    ADD CONSTRAINT driver_pool_config_pkey PRIMARY KEY (merchant_id, trip_distance);


--
-- Name: driver_quote driver_quote_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_quote
    ADD CONSTRAINT driver_quote_pkey PRIMARY KEY (id);


--
-- Name: driver_referral driver_referral_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_referral
    ADD CONSTRAINT driver_referral_pkey PRIMARY KEY (referral_code);


--
-- Name: driver_stats driver_stats_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_stats
    ADD CONSTRAINT driver_stats_pkey PRIMARY KEY (driver_id);


--
-- Name: estimate estimate_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.estimate
    ADD CONSTRAINT estimate_pkey PRIMARY KEY (id);


--
-- Name: exophone exophone_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.exophone
    ADD CONSTRAINT exophone_pkey PRIMARY KEY (id);


--
-- Name: exophone exophone_unique_backup_phone; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.exophone
    ADD CONSTRAINT exophone_unique_backup_phone UNIQUE (backup_phone);


--
-- Name: exophone exophone_unique_primary_phone; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.exophone
    ADD CONSTRAINT exophone_unique_primary_phone UNIQUE (primary_phone);


--
-- Name: fare_parameters fare_parameters_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_parameters
    ADD CONSTRAINT fare_parameters_pkey PRIMARY KEY (id);


--
-- Name: fare_parameters_progressive_details fare_parameters_progressive_details_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_parameters_progressive_details
    ADD CONSTRAINT fare_parameters_progressive_details_pkey PRIMARY KEY (fare_parameters_id);


--
-- Name: fare_parameters_slab_details fare_parameters_slab_details_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_parameters_slab_details
    ADD CONSTRAINT fare_parameters_slab_details_pkey PRIMARY KEY (fare_parameters_id);


--
-- Name: fare_policy_driver_extra_fee_bounds fare_policy_driver_extra_fee_bounds_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds
    ADD CONSTRAINT fare_policy_driver_extra_fee_bounds_pkey PRIMARY KEY (id);


--
-- Name: fare_policy_driver_extra_fee_bounds fare_policy_driver_extra_fee_bounds_unique_start_distance; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds
    ADD CONSTRAINT fare_policy_driver_extra_fee_bounds_unique_start_distance UNIQUE (fare_policy_id, start_distance);


--
-- Name: fare_policy_progressive_details_per_extra_km_rate_section fare_policy_progressive_details_per_extra_km_rate_section_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section
    ADD CONSTRAINT fare_policy_progressive_details_per_extra_km_rate_section_pkey PRIMARY KEY (id);


--
-- Name: fare_policy_progressive_details_per_extra_km_rate_section fare_policy_progressive_details_per_extra_km_rate_section_uniqu; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section
    ADD CONSTRAINT fare_policy_progressive_details_per_extra_km_rate_section_uniqu UNIQUE (fare_policy_id, start_distance);


--
-- Name: fare_policy_progressive_details fare_policy_progressive_details_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_policy_progressive_details
    ADD CONSTRAINT fare_policy_progressive_details_pkey PRIMARY KEY (fare_policy_id);


--
-- Name: fare_policy_slabs_details_slab fare_policy_slabs_details_slab_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_policy_slabs_details_slab
    ADD CONSTRAINT fare_policy_slabs_details_slab_pkey PRIMARY KEY (id);


--
-- Name: fare_product fare_product_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_product
    ADD CONSTRAINT fare_product_pkey PRIMARY KEY (id);


--
-- Name: feedback_form feedback_form_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.feedback_form
    ADD CONSTRAINT feedback_form_pkey PRIMARY KEY (id);


--
-- Name: geometry geometry_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.geometry
    ADD CONSTRAINT geometry_pkey PRIMARY KEY (id);


--
-- Name: go_home_config go_home_config_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.go_home_config
    ADD CONSTRAINT go_home_config_pkey PRIMARY KEY (merchant_id);


--
-- Name: search_request idx_16386_primary; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.search_request
    ADD CONSTRAINT idx_16386_primary PRIMARY KEY (id);


--
-- Name: search_request_location idx_16402_primary; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.search_request_location
    ADD CONSTRAINT idx_16402_primary PRIMARY KEY (id);


--
-- Name: merchant idx_16410_primary; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.merchant
    ADD CONSTRAINT idx_16410_primary PRIMARY KEY (id);


--
-- Name: person idx_16419_primary; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.person
    ADD CONSTRAINT idx_16419_primary PRIMARY KEY (id);


--
-- Name: registration_token idx_16435_primary; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.registration_token
    ADD CONSTRAINT idx_16435_primary PRIMARY KEY (id);


--
-- Name: vehicle idx_16451_primary; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.vehicle
    ADD CONSTRAINT idx_16451_primary PRIMARY KEY (driver_id);


--
-- Name: feedback_badge idx_feedback_badge_primary; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.feedback_badge
    ADD CONSTRAINT idx_feedback_badge_primary PRIMARY KEY (id);


--
-- Name: feedback idx_feedback_primary; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.feedback
    ADD CONSTRAINT idx_feedback_primary PRIMARY KEY (id);


--
-- Name: rating idx_rating_primary; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.rating
    ADD CONSTRAINT idx_rating_primary PRIMARY KEY (id);


--
-- Name: search_request_special_zone idx_search_request_special_zone_primary; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.search_request_special_zone
    ADD CONSTRAINT idx_search_request_special_zone_primary PRIMARY KEY (id);


--
-- Name: invoice invoice_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.invoice
    ADD CONSTRAINT invoice_pkey PRIMARY KEY (id, driver_fee_id);


--
-- Name: issue_category issue_category_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.issue_category
    ADD CONSTRAINT issue_category_pkey PRIMARY KEY (id);


--
-- Name: issue_option issue_option_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.issue_option
    ADD CONSTRAINT issue_option_pkey PRIMARY KEY (id);


--
-- Name: issue_report issue_report_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.issue_report
    ADD CONSTRAINT issue_report_pkey PRIMARY KEY (id);


--
-- Name: issue_translation issue_translation_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.issue_translation
    ADD CONSTRAINT issue_translation_pkey PRIMARY KEY (id);


--
-- Name: scheduler_job job_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.scheduler_job
    ADD CONSTRAINT job_pkey PRIMARY KEY (id);


--
-- Name: kiosk_location kiosk_location_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.kiosk_location
    ADD CONSTRAINT kiosk_location_pkey PRIMARY KEY (id);


--
-- Name: kiosk_location_translation kiosk_location_translation_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.kiosk_location_translation
    ADD CONSTRAINT kiosk_location_translation_pkey PRIMARY KEY (kiosk_location_id, language);


--
-- Name: leader_board_configs leader_board_configs_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.leader_board_configs
    ADD CONSTRAINT leader_board_configs_pkey PRIMARY KEY (id);


--
-- Name: location_mapping location_mapping_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.location_mapping
    ADD CONSTRAINT location_mapping_pkey PRIMARY KEY (id);


--
-- Name: location location_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.location
    ADD CONSTRAINT location_pkey PRIMARY KEY (id);


--
-- Name: mandate mandate_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.mandate
    ADD CONSTRAINT mandate_pkey PRIMARY KEY (id);


--
-- Name: media_file media_file_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.media_file
    ADD CONSTRAINT media_file_pkey PRIMARY KEY (id);


--
-- Name: merchant_message merchant_message_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.merchant_message
    ADD CONSTRAINT merchant_message_pkey PRIMARY KEY (merchant_id, message_key);


--
-- Name: merchant_overlay merchant_overlay_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.merchant_overlay
    ADD CONSTRAINT merchant_overlay_pkey PRIMARY KEY (id);


--
-- Name: merchant_payment_method merchant_payment_method_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.merchant_payment_method
    ADD CONSTRAINT merchant_payment_method_pkey PRIMARY KEY (id);


--
-- Name: merchant_service_config merchant_service_config_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.merchant_service_config
    ADD CONSTRAINT merchant_service_config_pkey PRIMARY KEY (merchant_id, service_name);


--
-- Name: merchant_service_usage_config merchant_service_usage_config_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.merchant_service_usage_config
    ADD CONSTRAINT merchant_service_usage_config_pkey PRIMARY KEY (merchant_id);


--
-- Name: merchant merchant_short_id_key; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.merchant
    ADD CONSTRAINT merchant_short_id_key UNIQUE (short_id);


--
-- Name: message message_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.message
    ADD CONSTRAINT message_pkey PRIMARY KEY (id);


--
-- Name: message_report message_report_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.message_report
    ADD CONSTRAINT message_report_pkey PRIMARY KEY (message_id, driver_id);


--
-- Name: message_translation message_translation_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.message_translation
    ADD CONSTRAINT message_translation_pkey PRIMARY KEY (message_id, language);


--
-- Name: meta_data meta_data_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.meta_data
    ADD CONSTRAINT meta_data_pkey PRIMARY KEY (driver_id);


--
-- Name: notification notification_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.notification
    ADD CONSTRAINT notification_pkey PRIMARY KEY (id);


--
-- Name: merchant organization_unique_mobile_number_country_code; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.merchant
    ADD CONSTRAINT organization_unique_mobile_number_country_code UNIQUE (mobile_country_code, mobile_number);


--
-- Name: payment_order payment_order_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.payment_order
    ADD CONSTRAINT payment_order_pkey PRIMARY KEY (id);


--
-- Name: payment_transaction payment_transaction_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.payment_transaction
    ADD CONSTRAINT payment_transaction_pkey PRIMARY KEY (id);


--
-- Name: person person_unique_mobile_number_country_code; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.person
    ADD CONSTRAINT person_unique_mobile_number_country_code UNIQUE (merchant_id, mobile_country_code, mobile_number_hash);


--
-- Name: onboarding_document_configs pk_onboarding_document_configs; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.onboarding_document_configs
    ADD CONSTRAINT pk_onboarding_document_configs PRIMARY KEY (merchant_id, document_type);


--
-- Name: place_name_cache place_name_cache_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.place_name_cache
    ADD CONSTRAINT place_name_cache_pkey PRIMARY KEY (id);


--
-- Name: plan plan_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.plan
    ADD CONSTRAINT plan_pkey PRIMARY KEY (id, payment_mode);


--
-- Name: plan_translation plan_translation_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.plan_translation
    ADD CONSTRAINT plan_translation_pkey PRIMARY KEY (plan_id, language);


--
-- Name: quote_special_zone quote_special_zone_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.quote_special_zone
    ADD CONSTRAINT quote_special_zone_pkey PRIMARY KEY (id);


--
-- Name: rating rating_product_instance_id_key; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.rating
    ADD CONSTRAINT rating_product_instance_id_key UNIQUE (ride_id);


--
-- Name: registry_map_fallback registry_map_fallback_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.registry_map_fallback
    ADD CONSTRAINT registry_map_fallback_pkey PRIMARY KEY (subscriber_id, unique_id);


--
-- Name: booking ride_booking_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.booking
    ADD CONSTRAINT ride_booking_pkey PRIMARY KEY (id);


--
-- Name: ride_details ride_details_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.ride_details
    ADD CONSTRAINT ride_details_pkey PRIMARY KEY (id);


--
-- Name: rider_details ride_details_unique_mobile_number; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.rider_details
    ADD CONSTRAINT ride_details_unique_mobile_number UNIQUE (merchant_id, mobile_number_hash, mobile_country_code);


--
-- Name: ride ride_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.ride
    ADD CONSTRAINT ride_pkey PRIMARY KEY (id);


--
-- Name: rider_details rider_details_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.rider_details
    ADD CONSTRAINT rider_details_pkey PRIMARY KEY (id);


--
-- Name: search_request_for_driver search_request_for_driver_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.search_request_for_driver
    ADD CONSTRAINT search_request_for_driver_pkey PRIMARY KEY (id);


--
-- Name: search_try search_try_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.search_try
    ADD CONSTRAINT search_try_pkey PRIMARY KEY (id);


--
-- Name: special_location special_location_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.special_location
    ADD CONSTRAINT special_location_pkey PRIMARY KEY (id);


--
-- Name: special_location_priority special_location_priority_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.special_location_priority
    ADD CONSTRAINT special_location_priority_pkey PRIMARY KEY (id);


--
-- Name: tag_category_mapping tag_category_mapping_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.tag_category_mapping
    ADD CONSTRAINT tag_category_mapping_pkey PRIMARY KEY (tag);


--
-- Name: transporter_config transporter_config_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.transporter_config
    ADD CONSTRAINT transporter_config_pkey PRIMARY KEY (merchant_id);


--
-- Name: merchant unique_api_key; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.merchant
    ADD CONSTRAINT unique_api_key UNIQUE (api_key);


--
-- Name: call_status unique_call_sid; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.call_status
    ADD CONSTRAINT unique_call_sid UNIQUE (call_id);


--
-- Name: person unique_email; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.person
    ADD CONSTRAINT unique_email UNIQUE (merchant_id, email);


--
-- Name: fare_policy unique_fare_policy_id; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.fare_policy
    ADD CONSTRAINT unique_fare_policy_id UNIQUE (id);


--
-- Name: person unique_identifier; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.person
    ADD CONSTRAINT unique_identifier UNIQUE (merchant_id, identifier);


--
-- Name: driver_license unique_number; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_license
    ADD CONSTRAINT unique_number UNIQUE (license_number_hash);


--
-- Name: vehicle_registration_certificate unique_rc_id; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.vehicle_registration_certificate
    ADD CONSTRAINT unique_rc_id UNIQUE (certificate_number_hash, fitness_expiry);


--
-- Name: vehicle unique_registration_no; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.vehicle
    ADD CONSTRAINT unique_registration_no UNIQUE (registration_no);


--
-- Name: idfy_verification unique_request_id; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.idfy_verification
    ADD CONSTRAINT unique_request_id UNIQUE (request_id);


--
-- Name: driver_availability unique_row_for_driver_availability; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.driver_availability
    ADD CONSTRAINT unique_row_for_driver_availability UNIQUE (bucket_start_time, bucket_end_time, driver_id, merchant_id);


--
-- Name: merchant unique_short_id; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.merchant
    ADD CONSTRAINT unique_short_id UNIQUE (subscriber_id);


--
-- Name: vehicle_registration_certificate vehicle_registration_certificate_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.vehicle_registration_certificate
    ADD CONSTRAINT vehicle_registration_certificate_pkey PRIMARY KEY (id);


--
-- Name: volunteer volunteer_pkey; Type: CONSTRAINT; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

ALTER TABLE ONLY atlas_driver_offer_bpp.volunteer
    ADD CONSTRAINT volunteer_pkey PRIMARY KEY (id);


--
-- Name: beckn_request beckn_request_pkey; Type: CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.beckn_request
    ADD CONSTRAINT beckn_request_pkey PRIMARY KEY (id);


--
-- Name: booking booking_pkey; Type: CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.booking
    ADD CONSTRAINT booking_pkey PRIMARY KEY (id);


--
-- Name: payment_transaction payment_transaction_booking_id_key; Type: CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.payment_transaction
    ADD CONSTRAINT payment_transaction_booking_id_key UNIQUE (booking_id);


--
-- Name: payment_transaction payment_transaction_pkey; Type: CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.payment_transaction
    ADD CONSTRAINT payment_transaction_pkey PRIMARY KEY (id);


--
-- Name: quote quote_pkey; Type: CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.quote
    ADD CONSTRAINT quote_pkey PRIMARY KEY (id);


--
-- Name: search search_pkey; Type: CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.search
    ADD CONSTRAINT search_pkey PRIMARY KEY (id);


--
-- Name: transport_station transport_station_pkey; Type: CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.transport_station
    ADD CONSTRAINT transport_station_pkey PRIMARY KEY (id);


--
-- Name: subscriber subscriber_pkey; Type: CONSTRAINT; Schema: atlas_registry; Owner: atlas_registry_user
--

ALTER TABLE ONLY atlas_registry.subscriber
    ADD CONSTRAINT subscriber_pkey PRIMARY KEY (unique_key_id, subscriber_id);


--
-- Name: job job_pkey; Type: CONSTRAINT; Schema: atlas_scheduler_example; Owner: atlas_scheduler_example_user
--

ALTER TABLE ONLY atlas_scheduler_example.job
    ADD CONSTRAINT job_pkey PRIMARY KEY (id);


--
-- Name: special_zone special_zone_pkey; Type: CONSTRAINT; Schema: atlas_special_zone; Owner: atlas_special_zone_user
--

ALTER TABLE ONLY atlas_special_zone.special_zone
    ADD CONSTRAINT special_zone_pkey PRIMARY KEY (id);


--
-- Name: tag_category_mapping tag_category_mapping_pkey; Type: CONSTRAINT; Schema: atlas_special_zone; Owner: atlas_special_zone_user
--

ALTER TABLE ONLY atlas_special_zone.tag_category_mapping
    ADD CONSTRAINT tag_category_mapping_pkey PRIMARY KEY (tag);


--
-- Name: idx_16386_requestor; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_16386_requestor ON atlas_app.search_request USING btree (rider_id);


--
-- Name: idx_16394_case_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_16394_case_id ON atlas_app.quote USING btree (request_id);


--
-- Name: idx_16434_city; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_16434_city ON atlas_app.search_request_location USING btree (city);


--
-- Name: idx_16434_state; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_16434_state ON atlas_app.search_request_location USING btree (state);


--
-- Name: idx_16467_entity_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_16467_entity_id ON atlas_app.registration_token USING btree (entity_id);


--
-- Name: idx_16467_entity_type; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_16467_entity_type ON atlas_app.registration_token USING btree (entity_type);


--
-- Name: idx_16475_tag; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_16475_tag ON atlas_app.tag USING btree (tag);


--
-- Name: idx_16475_tag_type; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_16475_tag_type ON atlas_app.tag USING btree (tag_type);


--
-- Name: idx_aadhaar_verification_aadhaar_number_hash; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_aadhaar_verification_aadhaar_number_hash ON atlas_app.aadhaar_verification USING btree (aadhaar_number_hash);


--
-- Name: idx_booking_rider_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_booking_rider_id ON atlas_app.booking USING btree (rider_id);


--
-- Name: idx_booking_rider_id_and_status; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_booking_rider_id_and_status ON atlas_app.booking USING btree (rider_id, status);


--
-- Name: idx_directions_caching; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_directions_caching ON atlas_app.directions_cache USING btree (origin_hash, dest_hash, slot);


--
-- Name: idx_driver_offer_s_req_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_driver_offer_s_req_id ON atlas_app.driver_offer USING btree (estimate_id);


--
-- Name: idx_estimate_request_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_estimate_request_id ON atlas_app.estimate USING btree (request_id);


--
-- Name: idx_fare_breakup_booking_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_fare_breakup_booking_id ON atlas_app.fare_breakup USING btree (booking_id);


--
-- Name: idx_organization_short_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_organization_short_id ON atlas_app.black_list_org USING btree (subscriber_id);


--
-- Name: idx_person_mobile_num; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_person_mobile_num ON atlas_app.person USING btree (mobile_country_code, mobile_number_hash);


--
-- Name: idx_quote_provider_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_quote_provider_id ON atlas_app.quote USING btree (provider_id);


--
-- Name: idx_quote_search_req_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_quote_search_req_id ON atlas_app.quote USING btree (request_id);


--
-- Name: idx_reg_token_token; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_reg_token_token ON atlas_app.registration_token USING btree (token);


--
-- Name: idx_ride_booking_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_ride_booking_id ON atlas_app.ride USING btree (booking_id);


--
-- Name: idx_ride_bpp_ride_id_and_status; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_ride_bpp_ride_id_and_status ON atlas_app.ride USING btree (bpp_ride_id);


--
-- Name: idx_search_req_rider_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_search_req_rider_id ON atlas_app.search_request USING btree (rider_id);


--
-- Name: idx_ticket_id; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX idx_ticket_id ON atlas_app.issue USING btree (ticket_id);


--
-- Name: location_mapping_entity_id_idx; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX location_mapping_entity_id_idx ON atlas_app.location_mapping USING btree (entity_id);


--
-- Name: saved_location_rider_id_idx; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX saved_location_rider_id_idx ON atlas_app.saved_location USING btree (rider_id);


--
-- Name: saved_location_tag_idx; Type: INDEX; Schema: atlas_app; Owner: atlas_app_user
--

CREATE INDEX saved_location_tag_idx ON atlas_app.saved_location USING btree (tag);


--
-- Name: idx_16467_person_id; Type: INDEX; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

CREATE INDEX idx_16467_person_id ON atlas_bap_dashboard.registration_token USING btree (person_id);


--
-- Name: idx_16467_person_id; Type: INDEX; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

CREATE INDEX idx_16467_person_id ON atlas_bpp_dashboard.registration_token USING btree (person_id);


--
-- Name: idx_16402_city; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_16402_city ON atlas_driver_offer_bpp.search_request_location USING btree (city);


--
-- Name: idx_16402_state; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_16402_state ON atlas_driver_offer_bpp.search_request_location USING btree (state);


--
-- Name: idx_16419_organization_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_16419_organization_id ON atlas_driver_offer_bpp.person USING btree (merchant_id);


--
-- Name: idx_16419_role; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_16419_role ON atlas_driver_offer_bpp.person USING btree (role);


--
-- Name: idx_16435_entity_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_16435_entity_id ON atlas_driver_offer_bpp.registration_token USING btree (entity_id);


--
-- Name: idx_16435_entity_type; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_16435_entity_type ON atlas_driver_offer_bpp.registration_token USING btree (entity_type);


--
-- Name: idx_16451_organization_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_16451_organization_id ON atlas_driver_offer_bpp.vehicle USING btree (merchant_id);


--
-- Name: idx_aadhaar_verification_aadhaar_number_hash; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_aadhaar_verification_aadhaar_number_hash ON atlas_driver_offer_bpp.aadhaar_verification USING btree (aadhaar_number_hash);


--
-- Name: idx_booking_provider_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_booking_provider_id ON atlas_driver_offer_bpp.booking USING btree (provider_id);


--
-- Name: idx_driver_feedback; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_driver_feedback ON atlas_driver_offer_bpp.feedback_badge USING btree (driver_id);


--
-- Name: idx_driver_quote_driver_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_driver_quote_driver_id ON atlas_driver_offer_bpp.driver_quote USING btree (driver_id);


--
-- Name: idx_driver_quote_s_try_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_driver_quote_s_try_id ON atlas_driver_offer_bpp.driver_quote USING btree (search_try_id);


--
-- Name: idx_driver_quote_search_request_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_driver_quote_search_request_id ON atlas_driver_offer_bpp.driver_quote USING btree (search_request_id);


--
-- Name: idx_driver_ride_feedback; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_driver_ride_feedback ON atlas_driver_offer_bpp.feedback USING btree (driver_id, ride_id);


--
-- Name: idx_fare_product; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_fare_product ON atlas_driver_offer_bpp.fare_product USING btree (merchant_id, vehicle_variant, area);


--
-- Name: idx_gohome_request; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_gohome_request ON atlas_driver_offer_bpp.ride USING btree (driver_go_home_request_id);


--
-- Name: idx_merchant_overlay_key; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_merchant_overlay_key ON atlas_driver_offer_bpp.merchant_overlay USING btree (merchant_id, overlay_key);


--
-- Name: idx_organization_short_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_organization_short_id ON atlas_driver_offer_bpp.merchant USING btree (subscriber_id);


--
-- Name: idx_reg_token_token; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_reg_token_token ON atlas_driver_offer_bpp.registration_token USING btree (token);


--
-- Name: idx_ride_booking_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_ride_booking_id ON atlas_driver_offer_bpp.ride USING btree (booking_id);


--
-- Name: idx_ride_driver_id_and_status; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_ride_driver_id_and_status ON atlas_driver_offer_bpp.ride USING btree (driver_id, status);


--
-- Name: idx_ride_feedback; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_ride_feedback ON atlas_driver_offer_bpp.feedback USING btree (ride_id);


--
-- Name: idx_search_request_for_driver_driver_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_search_request_for_driver_driver_id ON atlas_driver_offer_bpp.search_request_for_driver USING btree (driver_id);


--
-- Name: idx_search_request_for_driver_s_try_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_search_request_for_driver_s_try_id ON atlas_driver_offer_bpp.search_request_for_driver USING btree (search_try_id);


--
-- Name: idx_search_try_s_req_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_search_try_s_req_id ON atlas_driver_offer_bpp.search_try USING btree (request_id);


--
-- Name: idx_special_location_priority; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_special_location_priority ON atlas_driver_offer_bpp.special_location_priority USING btree (merchant_id, category);


--
-- Name: idx_ticket_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_ticket_id ON atlas_driver_offer_bpp.issue_report USING btree (ticket_id);


--
-- Name: idx_vehicle_fleet_owner_id; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_vehicle_fleet_owner_id ON atlas_driver_offer_bpp.vehicle USING btree (fleet_owner_id);


--
-- Name: idx_vehicle_req_num; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX idx_vehicle_req_num ON atlas_driver_offer_bpp.vehicle USING btree (registration_no);


--
-- Name: location_mapping_entity_id_idx; Type: INDEX; Schema: atlas_driver_offer_bpp; Owner: atlas_driver_offer_bpp_user
--

CREATE INDEX location_mapping_entity_id_idx ON atlas_driver_offer_bpp.location_mapping USING btree (entity_id);


--
-- Name: aadhaar_otp_req aadhaar_otp_req_person_id_fkey; Type: FK CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.aadhaar_otp_req
    ADD CONSTRAINT aadhaar_otp_req_person_id_fkey FOREIGN KEY (person_id) REFERENCES atlas_app.person(id);


--
-- Name: aadhaar_otp_verify aadhaar_otp_verify_person_id_fkey; Type: FK CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.aadhaar_otp_verify
    ADD CONSTRAINT aadhaar_otp_verify_person_id_fkey FOREIGN KEY (person_id) REFERENCES atlas_app.person(id);


--
-- Name: aadhaar_verification aadhaar_verification_person_id_fkey; Type: FK CONSTRAINT; Schema: atlas_app; Owner: atlas_app_user
--

ALTER TABLE ONLY atlas_app.aadhaar_verification
    ADD CONSTRAINT aadhaar_verification_person_id_fkey FOREIGN KEY (person_id) REFERENCES atlas_app.person(id);


--
-- Name: access_matrix access_matrix_role_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.access_matrix
    ADD CONSTRAINT access_matrix_role_id_fkey FOREIGN KEY (role_id) REFERENCES atlas_bap_dashboard.role(id);


--
-- Name: merchant_access merchant_access_merchant_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.merchant_access
    ADD CONSTRAINT merchant_access_merchant_id_fkey FOREIGN KEY (merchant_id) REFERENCES atlas_bap_dashboard.merchant(id);


--
-- Name: person person_role_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.person
    ADD CONSTRAINT person_role_id_fkey FOREIGN KEY (role_id) REFERENCES atlas_bap_dashboard.role(id);


--
-- Name: registration_token registration_token_merchant_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.registration_token
    ADD CONSTRAINT registration_token_merchant_id_fkey FOREIGN KEY (merchant_id) REFERENCES atlas_bap_dashboard.merchant(id);


--
-- Name: registration_token registration_token_person_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.registration_token
    ADD CONSTRAINT registration_token_person_id_fkey FOREIGN KEY (person_id) REFERENCES atlas_bap_dashboard.person(id);


--
-- Name: merchant_access server_access_person_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.merchant_access
    ADD CONSTRAINT server_access_person_id_fkey FOREIGN KEY (person_id) REFERENCES atlas_bap_dashboard.person(id);


--
-- Name: transaction transaction_merchant_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.transaction
    ADD CONSTRAINT transaction_merchant_id_fkey FOREIGN KEY (merchant_id) REFERENCES atlas_bap_dashboard.merchant(id);


--
-- Name: transaction transaction_requestor_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bap_dashboard; Owner: atlas_bap_dashboard_user
--

ALTER TABLE ONLY atlas_bap_dashboard.transaction
    ADD CONSTRAINT transaction_requestor_id_fkey FOREIGN KEY (requestor_id) REFERENCES atlas_bap_dashboard.person(id);


--
-- Name: access_matrix access_matrix_role_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.access_matrix
    ADD CONSTRAINT access_matrix_role_id_fkey FOREIGN KEY (role_id) REFERENCES atlas_bpp_dashboard.role(id);


--
-- Name: merchant_access merchant_access_merchant_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.merchant_access
    ADD CONSTRAINT merchant_access_merchant_id_fkey FOREIGN KEY (merchant_id) REFERENCES atlas_bpp_dashboard.merchant(id);


--
-- Name: person person_role_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.person
    ADD CONSTRAINT person_role_id_fkey FOREIGN KEY (role_id) REFERENCES atlas_bpp_dashboard.role(id);


--
-- Name: registration_token registration_token_merchant_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.registration_token
    ADD CONSTRAINT registration_token_merchant_id_fkey FOREIGN KEY (merchant_id) REFERENCES atlas_bpp_dashboard.merchant(id);


--
-- Name: registration_token registration_token_person_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.registration_token
    ADD CONSTRAINT registration_token_person_id_fkey FOREIGN KEY (person_id) REFERENCES atlas_bpp_dashboard.person(id);


--
-- Name: merchant_access server_access_person_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.merchant_access
    ADD CONSTRAINT server_access_person_id_fkey FOREIGN KEY (person_id) REFERENCES atlas_bpp_dashboard.person(id);


--
-- Name: transaction transaction_merchant_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.transaction
    ADD CONSTRAINT transaction_merchant_id_fkey FOREIGN KEY (merchant_id) REFERENCES atlas_bpp_dashboard.merchant(id);


--
-- Name: transaction transaction_requestor_id_fkey; Type: FK CONSTRAINT; Schema: atlas_bpp_dashboard; Owner: atlas_bpp_dashboard_user
--

ALTER TABLE ONLY atlas_bpp_dashboard.transaction
    ADD CONSTRAINT transaction_requestor_id_fkey FOREIGN KEY (requestor_id) REFERENCES atlas_bpp_dashboard.person(id);


--
-- Name: booking booking_arrival_station_id_fkey; Type: FK CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.booking
    ADD CONSTRAINT booking_arrival_station_id_fkey FOREIGN KEY (arrival_station_id) REFERENCES atlas_public_transport.transport_station(id);


--
-- Name: booking booking_departure_station_id_fkey; Type: FK CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.booking
    ADD CONSTRAINT booking_departure_station_id_fkey FOREIGN KEY (departure_station_id) REFERENCES atlas_public_transport.transport_station(id);


--
-- Name: booking booking_quote_id_fkey; Type: FK CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.booking
    ADD CONSTRAINT booking_quote_id_fkey FOREIGN KEY (quote_id) REFERENCES atlas_public_transport.quote(id);


--
-- Name: booking booking_search_id_fkey; Type: FK CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.booking
    ADD CONSTRAINT booking_search_id_fkey FOREIGN KEY (search_id) REFERENCES atlas_public_transport.search(id);


--
-- Name: payment_transaction payment_transaction_booking_id_fkey; Type: FK CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.payment_transaction
    ADD CONSTRAINT payment_transaction_booking_id_fkey FOREIGN KEY (booking_id) REFERENCES atlas_public_transport.booking(id);


--
-- Name: quote quote_arrival_station_id_fkey; Type: FK CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.quote
    ADD CONSTRAINT quote_arrival_station_id_fkey FOREIGN KEY (arrival_station_id) REFERENCES atlas_public_transport.transport_station(id);


--
-- Name: quote quote_departure_station_id_fkey; Type: FK CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.quote
    ADD CONSTRAINT quote_departure_station_id_fkey FOREIGN KEY (departure_station_id) REFERENCES atlas_public_transport.transport_station(id);


--
-- Name: quote quote_search_id_fkey; Type: FK CONSTRAINT; Schema: atlas_public_transport; Owner: atlas_public_transport_user
--

ALTER TABLE ONLY atlas_public_transport.quote
    ADD CONSTRAINT quote_search_id_fkey FOREIGN KEY (search_id) REFERENCES atlas_public_transport.search(id);


--
-- Name: entry_exit entry_exit_special_zone_id_fkey; Type: FK CONSTRAINT; Schema: atlas_special_zone; Owner: atlas_special_zone_user
--

ALTER TABLE ONLY atlas_special_zone.entry_exit
    ADD CONSTRAINT entry_exit_special_zone_id_fkey FOREIGN KEY (special_zone_id) REFERENCES atlas_special_zone.special_zone(id);


--
-- PostgreSQL database dump complete
--


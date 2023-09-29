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
CREATE TABLE atlas_app.beckn_request (
    id character varying(36) NOT NULL,
    beckn_request text NOT NULL,
    signature_header text NOT NULL,
    time_stamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.black_list_org (
    id character(36) NOT NULL,
    subscriber_id character varying(255) NOT NULL,
    type character varying(255)
);
CREATE TABLE atlas_app.booking (
    id character(36) NOT NULL,
    status character varying(255) NOT NULL,
    provider_id character varying(255) NOT NULL,
    provider_mobile_number character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    rider_id character(36) NOT NULL,
    from_location_id character(36) NOT NULL,
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
CREATE TABLE atlas_app.cancellation_reason (
    reason_code character varying(255) NOT NULL,
    description character varying(255) NOT NULL,
    enabled boolean NOT NULL,
    on_search boolean DEFAULT true NOT NULL,
    on_confirm boolean DEFAULT true NOT NULL,
    on_assign boolean DEFAULT true NOT NULL,
    priority smallint DEFAULT 0 NOT NULL
);
CREATE TABLE atlas_app.directions_cache (
    id character(36) NOT NULL,
    origin_hash text NOT NULL,
    dest_hash text NOT NULL,
    slot integer NOT NULL,
    response text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.disability (
    id character varying(36) NOT NULL,
    tag character varying(255) NOT NULL,
    description character varying(255) NOT NULL
);
CREATE TABLE atlas_app.disability_translation (
    disability_id character varying(36) NOT NULL,
    disability_tag character varying(255) NOT NULL,
    translation character varying(255) NOT NULL,
    language character varying(255) NOT NULL
);
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
CREATE TABLE atlas_app.estimate_breakup (
    id character(36) NOT NULL,
    estimate_id character(36) NOT NULL,
    title character varying(255) NOT NULL,
    price_currency character varying(255) NOT NULL,
    price_value numeric(30,2) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.fare_breakup (
    id character(36) NOT NULL,
    booking_id character(36) NOT NULL,
    description text NOT NULL,
    amount double precision NOT NULL
);
CREATE TABLE atlas_app.feedback_form (
    category_name character varying(255) NOT NULL,
    id character varying(36) NOT NULL,
    rating integer,
    question character varying(255) NOT NULL,
    answer text[] NOT NULL,
    answer_type character varying(255) NOT NULL
);
CREATE TABLE atlas_app.geometry (
    region character varying(255) NOT NULL,
    geom public.geometry(MultiPolygon),
    id character(36) DEFAULT atlas_app.uuid_generate_v4() NOT NULL
);
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
    is_avoid_toll boolean DEFAULT true NOT NULL
);
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
CREATE TABLE atlas_app.merchant_message (
    merchant_id character(36) NOT NULL,
    message_key character varying(255) NOT NULL,
    message character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.merchant_service_config (
    merchant_id character(36) NOT NULL,
    service_name character varying(30) NOT NULL,
    config_json json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
    get_exophone character varying(255) DEFAULT 'Exotel'::character varying NOT NULL
);
CREATE TABLE atlas_app.on_search_event (
    id character(36) NOT NULL,
    bpp_id character varying(255) NOT NULL,
    message_id character varying(255) NOT NULL,
    error_code character varying(255),
    error_type character varying(255),
    error_message text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
    has_disability boolean
);
CREATE TABLE atlas_app.person_default_emergency_number (
    person_id character(36) NOT NULL,
    name character varying(255) NOT NULL,
    mobile_country_code character varying(255) NOT NULL,
    mobile_number_encrypted character varying(255) NOT NULL,
    mobile_number_hash bytea NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.person_disability (
    person_id character(36) NOT NULL,
    disability_id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    description character varying(255),
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.person_flow_status (
    person_id character(36) NOT NULL,
    flow_status json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.quote_terms_bak_1027 (
    id character(36),
    quote_id character(36),
    description character varying(1000)
);
CREATE TABLE atlas_app.rating (
    id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    rating_value integer NOT NULL,
    feedback_details character varying(255),
    rider_id character varying(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.rental_quote_bak_1027 (
    quote_id character(36),
    base_distance integer,
    base_duration_hr integer
);
CREATE TABLE atlas_app.rental_slab (
    id character(36) NOT NULL,
    base_distance integer NOT NULL,
    base_duration integer NOT NULL
);
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
CREATE TABLE atlas_app.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);
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
CREATE TABLE atlas_app.sos (
    id character(36) NOT NULL,
    flow character varying(255),
    status character varying(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    person_id character(36) NOT NULL,
    ride_id character(36) NOT NULL
);
CREATE TABLE atlas_app.special_location (
    id character(36) NOT NULL,
    location_name character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    gates text[] NOT NULL,
    geom public.geometry(MultiPolygon),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.special_zone_quote (
    id character(36) NOT NULL,
    quote_id character(100) NOT NULL
);
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
CREATE TABLE atlas_app.tag_category_mapping (
    id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.trip_terms (
    id character(36) NOT NULL,
    descriptions text NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_req (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_verify (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.bap_metadata (
    id text NOT NULL,
    name text NOT NULL,
    logo_url text NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.beckn_request (
    id character varying(36) NOT NULL,
    beckn_request text NOT NULL,
    signature_header text NOT NULL,
    time_stamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.booking (
    id character(36) NOT NULL,
    status character varying(255) NOT NULL,
    provider_id character(36) NOT NULL,
    bap_id character varying(255) NOT NULL,
    bap_uri character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    rider_id character(36),
    from_location_id character(36) NOT NULL,
    to_location_id character(36) NOT NULL,
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
CREATE TABLE atlas_driver_offer_bpp.cancellation_reason (
    reason_code character varying(255) NOT NULL,
    description character varying(255) NOT NULL,
    enabled boolean NOT NULL,
    priority smallint DEFAULT 0 NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.comment (
    id character varying(255) NOT NULL,
    issue_report_id character varying(255) NOT NULL,
    comment character varying(255) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    author_id character(36) NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.driver_block_reason (
    reason_code text NOT NULL,
    block_reason text,
    block_time_in_hours integer
);
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
    collected_at timestamp without time zone
);
CREATE TABLE atlas_driver_offer_bpp.driver_flow_status (
    person_id character(36) NOT NULL,
    flow_status json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.driver_plan (
    driver_id character(36) NOT NULL,
    plan_id character(36) NOT NULL,
    plan_type text NOT NULL,
    mandate_id text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    mandate_setup_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);
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
CREATE TABLE atlas_driver_offer_bpp.driver_referral (
    referral_code character varying(15) NOT NULL,
    driver_id character varying(255) NOT NULL,
    linked_at timestamp with time zone
);
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
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details (
    fare_parameters_id character(36) NOT NULL,
    dead_km_fare integer NOT NULL,
    extra_km_fare integer
);
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_slab_details (
    fare_parameters_id character(36) NOT NULL,
    platform_fee integer,
    sgst numeric(30,2),
    cgst numeric(30,2)
);
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
CREATE TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds (
    id integer NOT NULL,
    fare_policy_id character(36) NOT NULL,
    start_distance integer NOT NULL,
    min_fee integer NOT NULL,
    max_fee integer NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details (
    fare_policy_id character(36) NOT NULL,
    base_distance integer NOT NULL,
    base_fare integer NOT NULL,
    dead_km_fare integer NOT NULL,
    waiting_charge json,
    night_shift_charge json,
    free_wating_time integer NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section (
    id integer NOT NULL,
    fare_policy_id character(36) NOT NULL,
    start_distance integer NOT NULL,
    per_extra_km_rate numeric(30,2) NOT NULL
);
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
    platform_fee_sgst integer,
    waiting_pickup_charges json
);
CREATE TABLE atlas_driver_offer_bpp.fare_product (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    fare_policy_id character(36) NOT NULL,
    vehicle_variant character varying(60) NOT NULL,
    area text NOT NULL,
    flow character varying(60) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.feedback (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    badge character varying(255) NOT NULL,
    created_at timestamp with time zone NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.feedback_badge (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    badge character varying(255) NOT NULL,
    badge_count integer DEFAULT 0 NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.feedback_form (
    category_name character varying(255) NOT NULL,
    id character varying(36) NOT NULL,
    rating integer,
    question character varying(255) NOT NULL,
    answer text[] NOT NULL,
    answer_type character varying(255) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.geometry (
    id character(36) DEFAULT atlas_driver_offer_bpp.uuid_generate_v4() NOT NULL,
    region character varying(255) NOT NULL,
    geom public.geometry(MultiPolygon)
);
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
    driver_id text
);
CREATE TABLE atlas_driver_offer_bpp.issue_category (
    id character(36) NOT NULL,
    category character varying(255) NOT NULL,
    logo_url character varying(255) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.issue_option (
    id character(36) NOT NULL,
    issue_category_id character(36) NOT NULL,
    option character varying(255) NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.issue_translation (
    id character(36) NOT NULL,
    sentence character varying(255) NOT NULL,
    translation character varying(255) NOT NULL,
    language character varying(255) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.kiosk_location (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    address text NOT NULL,
    landmark text NOT NULL,
    contact character varying(15),
    longitude double precision NOT NULL,
    latitude double precision NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.kiosk_location_translation (
    kiosk_location_id character(36) NOT NULL,
    language character(36) NOT NULL,
    landmark character varying(255) NOT NULL,
    address text NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.media_file (
    id character(36) NOT NULL,
    type character(36) NOT NULL,
    url text NOT NULL,
    created_at timestamp without time zone NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.merchant_message (
    merchant_id character(36) NOT NULL,
    message_key character varying(255) NOT NULL,
    message character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.merchant_service_config (
    merchant_id character(36) NOT NULL,
    service_name character varying(30) NOT NULL,
    config_json json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.message_translation (
    message_id character(36) NOT NULL,
    language character(36) NOT NULL,
    title character varying(255) NOT NULL,
    description text NOT NULL,
    created_at timestamp without time zone NOT NULL,
    label character varying(255),
    short_description text DEFAULT ''::text
);
CREATE TABLE atlas_driver_offer_bpp.meta_data (
    driver_id character(36) NOT NULL,
    device text,
    device_o_s text,
    device_date_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    app_permissions text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);
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
    updated_at timestamp with time zone NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.operating_city (
    id character(36) NOT NULL,
    merchant_id character varying(255) NOT NULL,
    city_name character varying(255),
    enabled boolean NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.payment_order (
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
CREATE TABLE atlas_driver_offer_bpp.plan_translation (
    plan_id character(36) NOT NULL,
    language character(36) NOT NULL,
    name character varying(255) NOT NULL,
    description text NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.rating (
    id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    rating_value bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    driver_id character(36) NOT NULL,
    feedback_details character varying(255)
);
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
CREATE TABLE atlas_driver_offer_bpp.registry_map_fallback (
    subscriber_id character(36) NOT NULL,
    unique_id character(36) NOT NULL,
    registry_url character varying(255) NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.special_location (
    id character(36) NOT NULL,
    location_name character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    gates text[] NOT NULL,
    geom public.geometry(MultiPolygon),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.special_location_priority (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    category character varying(255) NOT NULL,
    pickup_priority integer NOT NULL,
    drop_priority integer NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.tag_category_mapping (
    id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
    open_market_un_blocked boolean DEFAULT false NOT NULL
);
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
    vehicle_name character varying(255)
);
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
CREATE TABLE atlas_app.aadhaar_otp_req (
    id character(36) NOT NULL,
    person_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TABLE atlas_app.aadhaar_otp_verify (
    id character(36) NOT NULL,
    person_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
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
CREATE TABLE atlas_app.beckn_request (
    id character varying(36) NOT NULL,
    beckn_request text NOT NULL,
    signature_header text NOT NULL,
    time_stamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.black_list_org (
    id character(36) NOT NULL,
    subscriber_id character varying(255) NOT NULL,
    type character varying(255)
);
CREATE TABLE atlas_app.booking (
    id character(36) NOT NULL,
    status character varying(255) NOT NULL,
    provider_id character varying(255) NOT NULL,
    provider_mobile_number character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    rider_id character(36) NOT NULL,
    from_location_id character(36) NOT NULL,
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
CREATE TABLE atlas_app.cancellation_reason (
    reason_code character varying(255) NOT NULL,
    description character varying(255) NOT NULL,
    enabled boolean NOT NULL,
    on_search boolean DEFAULT true NOT NULL,
    on_confirm boolean DEFAULT true NOT NULL,
    on_assign boolean DEFAULT true NOT NULL,
    priority smallint DEFAULT 0 NOT NULL
);
CREATE TABLE atlas_app.directions_cache (
    id character(36) NOT NULL,
    origin_hash text NOT NULL,
    dest_hash text NOT NULL,
    slot integer NOT NULL,
    response text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.disability (
    id character varying(36) NOT NULL,
    tag character varying(255) NOT NULL,
    description character varying(255) NOT NULL
);
CREATE TABLE atlas_app.disability_translation (
    disability_id character varying(36) NOT NULL,
    disability_tag character varying(255) NOT NULL,
    translation character varying(255) NOT NULL,
    language character varying(255) NOT NULL
);
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
CREATE TABLE atlas_app.estimate_breakup (
    id character(36) NOT NULL,
    estimate_id character(36) NOT NULL,
    title character varying(255) NOT NULL,
    price_currency character varying(255) NOT NULL,
    price_value numeric(30,2) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.fare_breakup (
    id character(36) NOT NULL,
    booking_id character(36) NOT NULL,
    description text NOT NULL,
    amount double precision NOT NULL
);
CREATE TABLE atlas_app.feedback_form (
    category_name character varying(255) NOT NULL,
    id character varying(36) NOT NULL,
    rating integer,
    question character varying(255) NOT NULL,
    answer text[] NOT NULL,
    answer_type character varying(255) NOT NULL
);
CREATE TABLE atlas_app.geometry (
    region character varying(255) NOT NULL,
    geom public.geometry(MultiPolygon),
    id character(36) DEFAULT atlas_app.uuid_generate_v4() NOT NULL
);
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
CREATE TABLE atlas_app.merchant_message (
    merchant_id character(36) NOT NULL,
    message_key character varying(255) NOT NULL,
    message character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.merchant_service_config (
    merchant_id character(36) NOT NULL,
    service_name character varying(30) NOT NULL,
    config_json json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.on_search_event (
    id character(36) NOT NULL,
    bpp_id character varying(255) NOT NULL,
    message_id character varying(255) NOT NULL,
    error_code character varying(255),
    error_type character varying(255),
    error_message text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.person_default_emergency_number (
    person_id character(36) NOT NULL,
    name character varying(255) NOT NULL,
    mobile_country_code character varying(255) NOT NULL,
    mobile_number_encrypted character varying(255) NOT NULL,
    mobile_number_hash bytea NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.person_disability (
    person_id character(36) NOT NULL,
    disability_id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    description character varying(255),
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.person_flow_status (
    person_id character(36) NOT NULL,
    flow_status json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.quote_terms_bak_1027 (
    id character(36),
    quote_id character(36),
    description character varying(1000)
);
CREATE TABLE atlas_app.rating (
    id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    rating_value integer NOT NULL,
    feedback_details character varying(255),
    rider_id character varying(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.rental_quote_bak_1027 (
    quote_id character(36),
    base_distance integer,
    base_duration_hr integer
);
CREATE TABLE atlas_app.rental_slab (
    id character(36) NOT NULL,
    base_distance integer NOT NULL,
    base_duration integer NOT NULL
);
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
CREATE TABLE atlas_app.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);
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
CREATE TABLE atlas_app.sos (
    id character(36) NOT NULL,
    flow character varying(255),
    status character varying(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    person_id character(36) NOT NULL,
    ride_id character(36) NOT NULL
);
CREATE TABLE atlas_app.special_location (
    id character(36) NOT NULL,
    location_name character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    gates text[] NOT NULL,
    geom public.geometry(MultiPolygon),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.special_zone_quote (
    id character(36) NOT NULL,
    quote_id character(100) NOT NULL
);
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
CREATE TABLE atlas_app.tag_category_mapping (
    id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.trip_terms (
    id character(36) NOT NULL,
    descriptions text NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_req (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_verify (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.bap_metadata (
    id text NOT NULL,
    name text NOT NULL,
    logo_url text NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.beckn_request (
    id character varying(36) NOT NULL,
    beckn_request text NOT NULL,
    signature_header text NOT NULL,
    time_stamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.booking (
    id character(36) NOT NULL,
    status character varying(255) NOT NULL,
    provider_id character(36) NOT NULL,
    bap_id character varying(255) NOT NULL,
    bap_uri character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    rider_id character(36),
    from_location_id character(36) NOT NULL,
    to_location_id character(36) NOT NULL,
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
CREATE TABLE atlas_driver_offer_bpp.cancellation_reason (
    reason_code character varying(255) NOT NULL,
    description character varying(255) NOT NULL,
    enabled boolean NOT NULL,
    priority smallint DEFAULT 0 NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.comment (
    id character varying(255) NOT NULL,
    issue_report_id character varying(255) NOT NULL,
    comment character varying(255) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    author_id character(36) NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.driver_block_reason (
    reason_code text NOT NULL,
    block_reason text,
    block_time_in_hours integer
);
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
CREATE TABLE atlas_driver_offer_bpp.driver_flow_status (
    person_id character(36) NOT NULL,
    flow_status json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.driver_plan (
    driver_id character(36) NOT NULL,
    plan_id character(36) NOT NULL,
    plan_type text NOT NULL,
    mandate_id text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    mandate_setup_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);
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
CREATE TABLE atlas_driver_offer_bpp.driver_referral (
    referral_code character varying(15) NOT NULL,
    driver_id character varying(255) NOT NULL,
    linked_at timestamp with time zone
);
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
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details (
    fare_parameters_id character(36) NOT NULL,
    dead_km_fare integer NOT NULL,
    extra_km_fare integer
);
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_slab_details (
    fare_parameters_id character(36) NOT NULL,
    platform_fee integer,
    sgst numeric(30,2),
    cgst numeric(30,2)
);
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
CREATE TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds (
    id integer NOT NULL,
    fare_policy_id character(36) NOT NULL,
    start_distance integer NOT NULL,
    min_fee integer NOT NULL,
    max_fee integer NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details (
    fare_policy_id character(36) NOT NULL,
    base_distance integer NOT NULL,
    base_fare integer NOT NULL,
    dead_km_fare integer NOT NULL,
    waiting_charge json,
    night_shift_charge json,
    free_wating_time integer NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section (
    id integer NOT NULL,
    fare_policy_id character(36) NOT NULL,
    start_distance integer NOT NULL,
    per_extra_km_rate numeric(30,2) NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.fare_product (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    fare_policy_id character(36) NOT NULL,
    vehicle_variant character varying(60) NOT NULL,
    area text NOT NULL,
    flow character varying(60) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.feedback (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    badge character varying(255) NOT NULL,
    created_at timestamp with time zone NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.feedback_badge (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    badge character varying(255) NOT NULL,
    badge_count integer DEFAULT 0 NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.feedback_form (
    category_name character varying(255) NOT NULL,
    id character varying(36) NOT NULL,
    rating integer,
    question character varying(255) NOT NULL,
    answer text[] NOT NULL,
    answer_type character varying(255) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.geometry (
    id character(36) DEFAULT atlas_driver_offer_bpp.uuid_generate_v4() NOT NULL,
    region character varying(255) NOT NULL,
    geom public.geometry(MultiPolygon)
);
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
CREATE TABLE atlas_driver_offer_bpp.issue_category (
    id character(36) NOT NULL,
    category character varying(255) NOT NULL,
    logo_url character varying(255) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.issue_option (
    id character(36) NOT NULL,
    issue_category_id character(36) NOT NULL,
    option character varying(255) NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.issue_translation (
    id character(36) NOT NULL,
    sentence character varying(255) NOT NULL,
    translation character varying(255) NOT NULL,
    language character varying(255) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.kiosk_location (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    address text NOT NULL,
    landmark text NOT NULL,
    contact character varying(15),
    longitude double precision NOT NULL,
    latitude double precision NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.kiosk_location_translation (
    kiosk_location_id character(36) NOT NULL,
    language character(36) NOT NULL,
    landmark character varying(255) NOT NULL,
    address text NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.media_file (
    id character(36) NOT NULL,
    type character(36) NOT NULL,
    url text NOT NULL,
    created_at timestamp without time zone NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.merchant_message (
    merchant_id character(36) NOT NULL,
    message_key character varying(255) NOT NULL,
    message character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.merchant_service_config (
    merchant_id character(36) NOT NULL,
    service_name character varying(30) NOT NULL,
    config_json json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.message_translation (
    message_id character(36) NOT NULL,
    language character(36) NOT NULL,
    title character varying(255) NOT NULL,
    description text NOT NULL,
    created_at timestamp without time zone NOT NULL,
    label character varying(255),
    short_description text DEFAULT ''::text
);
CREATE TABLE atlas_driver_offer_bpp.meta_data (
    driver_id character(36) NOT NULL,
    device text,
    device_o_s text,
    device_date_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    app_permissions text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);
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
CREATE TABLE atlas_driver_offer_bpp.operating_city (
    id character(36) NOT NULL,
    merchant_id character varying(255) NOT NULL,
    city_name character varying(255),
    enabled boolean NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.payment_order (
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
CREATE TABLE atlas_driver_offer_bpp.plan_translation (
    plan_id character(36) NOT NULL,
    language character(36) NOT NULL,
    name character varying(255) NOT NULL,
    description text NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.rating (
    id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    rating_value bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    driver_id character(36) NOT NULL,
    feedback_details character varying(255)
);
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
CREATE TABLE atlas_driver_offer_bpp.registry_map_fallback (
    subscriber_id character(36) NOT NULL,
    unique_id character(36) NOT NULL,
    registry_url character varying(255) NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.special_location (
    id character(36) NOT NULL,
    location_name character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    gates text[] NOT NULL,
    geom public.geometry(MultiPolygon),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.special_location_priority (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    category character varying(255) NOT NULL,
    pickup_priority integer NOT NULL,
    drop_priority integer NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.tag_category_mapping (
    id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
    order_and_notification_status_check_time bigint DEFAULT 63000
);
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
    vehicle_name character varying(255)
);
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
CREATE TABLE atlas_app.aadhaar_otp_req (
    id character(36) NOT NULL,
    person_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TABLE atlas_app.aadhaar_otp_verify (
    id character(36) NOT NULL,
    person_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
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
CREATE TABLE atlas_app.beckn_request (
    id character varying(36) NOT NULL,
    beckn_request text NOT NULL,
    signature_header text NOT NULL,
    time_stamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.black_list_org (
    id character(36) NOT NULL,
    subscriber_id character varying(255) NOT NULL,
    type character varying(255)
);
CREATE TABLE atlas_app.booking (
    id character(36) NOT NULL,
    status character varying(255) NOT NULL,
    provider_id character varying(255) NOT NULL,
    provider_mobile_number character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    rider_id character(36) NOT NULL,
    from_location_id character(36) NOT NULL,
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
CREATE TABLE atlas_app.cancellation_reason (
    reason_code character varying(255) NOT NULL,
    description character varying(255) NOT NULL,
    enabled boolean NOT NULL,
    on_search boolean DEFAULT true NOT NULL,
    on_confirm boolean DEFAULT true NOT NULL,
    on_assign boolean DEFAULT true NOT NULL,
    priority smallint DEFAULT 0 NOT NULL
);
CREATE TABLE atlas_app.directions_cache (
    id character(36) NOT NULL,
    origin_hash text NOT NULL,
    dest_hash text NOT NULL,
    slot integer NOT NULL,
    response text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.disability (
    id character varying(36) NOT NULL,
    tag character varying(255) NOT NULL,
    description character varying(255) NOT NULL
);
CREATE TABLE atlas_app.disability_translation (
    disability_id character varying(36) NOT NULL,
    disability_tag character varying(255) NOT NULL,
    translation character varying(255) NOT NULL,
    language character varying(255) NOT NULL
);
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
CREATE TABLE atlas_app.estimate_breakup (
    id character(36) NOT NULL,
    estimate_id character(36) NOT NULL,
    title character varying(255) NOT NULL,
    price_currency character varying(255) NOT NULL,
    price_value numeric(30,2) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.fare_breakup (
    id character(36) NOT NULL,
    booking_id character(36) NOT NULL,
    description text NOT NULL,
    amount double precision NOT NULL
);
CREATE TABLE atlas_app.feedback_form (
    category_name character varying(255) NOT NULL,
    id character varying(36) NOT NULL,
    rating integer,
    question character varying(255) NOT NULL,
    answer text[] NOT NULL,
    answer_type character varying(255) NOT NULL
);
CREATE TABLE atlas_app.geometry (
    region character varying(255) NOT NULL,
    geom public.geometry(MultiPolygon),
    id character(36) DEFAULT atlas_app.uuid_generate_v4() NOT NULL
);
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
CREATE TABLE atlas_app.merchant_message (
    merchant_id character(36) NOT NULL,
    message_key character varying(255) NOT NULL,
    message character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.merchant_service_config (
    merchant_id character(36) NOT NULL,
    service_name character varying(30) NOT NULL,
    config_json json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.on_search_event (
    id character(36) NOT NULL,
    bpp_id character varying(255) NOT NULL,
    message_id character varying(255) NOT NULL,
    error_code character varying(255),
    error_type character varying(255),
    error_message text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.person_default_emergency_number (
    person_id character(36) NOT NULL,
    name character varying(255) NOT NULL,
    mobile_country_code character varying(255) NOT NULL,
    mobile_number_encrypted character varying(255) NOT NULL,
    mobile_number_hash bytea NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.person_disability (
    person_id character(36) NOT NULL,
    disability_id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    description character varying(255),
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.person_flow_status (
    person_id character(36) NOT NULL,
    flow_status json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.quote_terms_bak_1027 (
    id character(36),
    quote_id character(36),
    description character varying(1000)
);
CREATE TABLE atlas_app.rating (
    id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    rating_value integer NOT NULL,
    feedback_details character varying(255),
    rider_id character varying(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_app.rental_quote_bak_1027 (
    quote_id character(36),
    base_distance integer,
    base_duration_hr integer
);
CREATE TABLE atlas_app.rental_slab (
    id character(36) NOT NULL,
    base_distance integer NOT NULL,
    base_duration integer NOT NULL
);
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
CREATE TABLE atlas_app.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);
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
CREATE TABLE atlas_app.sos (
    id character(36) NOT NULL,
    flow character varying(255),
    status character varying(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    person_id character(36) NOT NULL,
    ride_id character(36) NOT NULL
);
CREATE TABLE atlas_app.special_location (
    id character(36) NOT NULL,
    location_name character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    gates text[] NOT NULL,
    geom public.geometry(MultiPolygon),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.special_zone_quote (
    id character(36) NOT NULL,
    quote_id character(100) NOT NULL
);
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
CREATE TABLE atlas_app.tag_category_mapping (
    id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_app.trip_terms (
    id character(36) NOT NULL,
    descriptions text NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_req (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_verify (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    request_id text NOT NULL,
    status_code text NOT NULL,
    request_message text NOT NULL,
    transaction_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.bap_metadata (
    id text NOT NULL,
    name text NOT NULL,
    logo_url text NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.beckn_request (
    id character varying(36) NOT NULL,
    beckn_request text NOT NULL,
    signature_header text NOT NULL,
    time_stamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.booking (
    id character(36) NOT NULL,
    status character varying(255) NOT NULL,
    provider_id character(36) NOT NULL,
    bap_id character varying(255) NOT NULL,
    bap_uri character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    rider_id character(36),
    from_location_id character(36) NOT NULL,
    to_location_id character(36) NOT NULL,
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
CREATE TABLE atlas_driver_offer_bpp.cancellation_reason (
    reason_code character varying(255) NOT NULL,
    description character varying(255) NOT NULL,
    enabled boolean NOT NULL,
    priority smallint DEFAULT 0 NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.comment (
    id character varying(255) NOT NULL,
    issue_report_id character varying(255) NOT NULL,
    comment character varying(255) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    author_id character(36) NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.driver_block_reason (
    reason_code text NOT NULL,
    block_reason text,
    block_time_in_hours integer
);
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
CREATE TABLE atlas_driver_offer_bpp.driver_flow_status (
    person_id character(36) NOT NULL,
    flow_status json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.driver_plan (
    driver_id character(36) NOT NULL,
    plan_id character(36) NOT NULL,
    plan_type text NOT NULL,
    mandate_id text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    mandate_setup_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);
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
CREATE TABLE atlas_driver_offer_bpp.driver_referral (
    referral_code character varying(15) NOT NULL,
    driver_id character varying(255) NOT NULL,
    linked_at timestamp with time zone
);
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
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details (
    fare_parameters_id character(36) NOT NULL,
    dead_km_fare integer NOT NULL,
    extra_km_fare integer
);
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_slab_details (
    fare_parameters_id character(36) NOT NULL,
    platform_fee integer,
    sgst numeric(30,2),
    cgst numeric(30,2)
);
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
CREATE TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds (
    id integer NOT NULL,
    fare_policy_id character(36) NOT NULL,
    start_distance integer NOT NULL,
    min_fee integer NOT NULL,
    max_fee integer NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details (
    fare_policy_id character(36) NOT NULL,
    base_distance integer NOT NULL,
    base_fare integer NOT NULL,
    dead_km_fare integer NOT NULL,
    waiting_charge json,
    night_shift_charge json,
    free_wating_time integer NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section (
    id integer NOT NULL,
    fare_policy_id character(36) NOT NULL,
    start_distance integer NOT NULL,
    per_extra_km_rate numeric(30,2) NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.fare_product (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    fare_policy_id character(36) NOT NULL,
    vehicle_variant character varying(60) NOT NULL,
    area text NOT NULL,
    flow character varying(60) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.feedback (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    badge character varying(255) NOT NULL,
    created_at timestamp with time zone NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.feedback_badge (
    id character(36) NOT NULL,
    driver_id character(36) NOT NULL,
    badge character varying(255) NOT NULL,
    badge_count integer DEFAULT 0 NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.feedback_form (
    category_name character varying(255) NOT NULL,
    id character varying(36) NOT NULL,
    rating integer,
    question character varying(255) NOT NULL,
    answer text[] NOT NULL,
    answer_type character varying(255) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.geometry (
    id character(36) DEFAULT atlas_driver_offer_bpp.uuid_generate_v4() NOT NULL,
    region character varying(255) NOT NULL,
    geom public.geometry(MultiPolygon)
);
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
CREATE TABLE atlas_driver_offer_bpp.issue_category (
    id character(36) NOT NULL,
    category character varying(255) NOT NULL,
    logo_url character varying(255) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.issue_option (
    id character(36) NOT NULL,
    issue_category_id character(36) NOT NULL,
    option character varying(255) NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.issue_translation (
    id character(36) NOT NULL,
    sentence character varying(255) NOT NULL,
    translation character varying(255) NOT NULL,
    language character varying(255) NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.kiosk_location (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    address text NOT NULL,
    landmark text NOT NULL,
    contact character varying(15),
    longitude double precision NOT NULL,
    latitude double precision NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.kiosk_location_translation (
    kiosk_location_id character(36) NOT NULL,
    language character(36) NOT NULL,
    landmark character varying(255) NOT NULL,
    address text NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.media_file (
    id character(36) NOT NULL,
    type character(36) NOT NULL,
    url text NOT NULL,
    created_at timestamp without time zone NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.merchant_message (
    merchant_id character(36) NOT NULL,
    message_key character varying(255) NOT NULL,
    message character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.merchant_service_config (
    merchant_id character(36) NOT NULL,
    service_name character varying(30) NOT NULL,
    config_json json NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.message_translation (
    message_id character(36) NOT NULL,
    language character(36) NOT NULL,
    title character varying(255) NOT NULL,
    description text NOT NULL,
    created_at timestamp without time zone NOT NULL,
    label character varying(255),
    short_description text DEFAULT ''::text
);
CREATE TABLE atlas_driver_offer_bpp.meta_data (
    driver_id character(36) NOT NULL,
    device text,
    device_o_s text,
    device_date_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    app_permissions text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);
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
CREATE TABLE atlas_driver_offer_bpp.operating_city (
    id character(36) NOT NULL,
    merchant_id character varying(255) NOT NULL,
    city_name character varying(255),
    enabled boolean NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.payment_order (
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
CREATE TABLE atlas_driver_offer_bpp.plan_translation (
    plan_id character(36) NOT NULL,
    language character(36) NOT NULL,
    name character varying(255) NOT NULL,
    description text NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.rating (
    id character(36) NOT NULL,
    ride_id character varying(36) NOT NULL,
    rating_value bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    driver_id character(36) NOT NULL,
    feedback_details character varying(255)
);
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
CREATE TABLE atlas_driver_offer_bpp.registry_map_fallback (
    subscriber_id character(36) NOT NULL,
    unique_id character(36) NOT NULL,
    registry_url character varying(255) NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);
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
CREATE TABLE atlas_driver_offer_bpp.special_location (
    id character(36) NOT NULL,
    location_name character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    gates text[] NOT NULL,
    geom public.geometry(MultiPolygon),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.special_location_priority (
    id character(36) NOT NULL,
    merchant_id character(36) NOT NULL,
    category character varying(255) NOT NULL,
    pickup_priority integer NOT NULL,
    drop_priority integer NOT NULL
);
CREATE TABLE atlas_driver_offer_bpp.tag_category_mapping (
    id character(36) NOT NULL,
    tag character varying(255) NOT NULL,
    category character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
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
    order_and_notification_status_check_time bigint DEFAULT 63000
);
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
    vehicle_name character varying(255)
);
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

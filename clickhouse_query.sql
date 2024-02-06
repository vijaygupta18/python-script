CREATE TABLE atlas_app_helper.aadhaar_otp_req_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `person_id` Nullable (String),
    `request_id` Nullable (String),
    `status_code` Nullable (String),
    `request_message` Nullable (String),
    `transaction_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.aadhaar_otp_req ON CLUSTER `{cluster}` AS atlas_app_helper.aadhaar_otp_req_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, aadhaar_otp_req_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.aadhaar_otp_req ON CLUSTER `{cluster}` TO atlas_app.aadhaar_otp_req_mv
(
	`id` String,
	`person_id` String,
	`request_id` String,
	`status_code` String,
	`request_message` String,
	`transaction_id` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'status_code'),'') as status_code,
	ifNull(JSONExtractString(message,'request_message'),'') as request_message,
	ifNull(JSONExtractString(message,'transaction_id'),'') as transaction_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'AadhaarOtpReqObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.aadhaar_otp_verify_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `person_id` Nullable (String),
    `request_id` Nullable (String),
    `status_code` Nullable (String),
    `request_message` Nullable (String),
    `transaction_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.aadhaar_otp_verify ON CLUSTER `{cluster}` AS atlas_app_helper.aadhaar_otp_verify_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, aadhaar_otp_verify_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.aadhaar_otp_verify ON CLUSTER `{cluster}` TO atlas_app.aadhaar_otp_verify_mv
(
	`id` String,
	`person_id` String,
	`request_id` String,
	`status_code` String,
	`request_message` String,
	`transaction_id` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'status_code'),'') as status_code,
	ifNull(JSONExtractString(message,'request_message'),'') as request_message,
	ifNull(JSONExtractString(message,'transaction_id'),'') as transaction_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'AadhaarOtpVerifyObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.aadhaar_verification_shard ON CLUSTER `{cluster}`
    (
    `person_id` Nullable (String),
    `person_name` Nullable (String),
    `person_gender` Nullable (String),
    `person_dob` Nullable (String),
    `person_image_path` Nullable (String),
    `aadhaar_number_hash` Nullable (String),
    `is_verified` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (driver_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.aadhaar_verification ON CLUSTER `{cluster}` AS atlas_app_helper.aadhaar_verification_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, aadhaar_verification_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.aadhaar_verification ON CLUSTER `{cluster}` TO atlas_app.aadhaar_verification_mv
(
	`person_id` String,
	`person_name` String,
	`person_gender` String,
	`person_dob` String,
	`person_image_path` String,
	`aadhaar_number_hash` String,
	`is_verified` String,
	`updated_at` DateTime,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'person_name'),'') as person_name,
	ifNull(JSONExtractString(message,'person_gender'),'') as person_gender,
	ifNull(JSONExtractString(message,'person_dob'),'') as person_dob,
	ifNull(JSONExtractString(message,'person_image_path'),'') as person_image_path,
	ifNull(JSONExtractString(message,'aadhaar_number_hash'),'') as aadhaar_number_hash,
	ifNull(JSONExtractString(message,'is_verified'),'') as is_verified,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'AadhaarVerificationObject'
	JSONExtractString(message, 'driver_id') is not null


CREATE TABLE atlas_app_helper.app_installs_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `device_token` Nullable (String),
    `source` Nullable (String),
    `merchant_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `platform` Nullable (String),
    `app_version` Nullable (String),
    `bundle_version` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.app_installs ON CLUSTER `{cluster}` AS atlas_app_helper.app_installs_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, app_installs_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.app_installs ON CLUSTER `{cluster}` TO atlas_app.app_installs_mv
(
	`id` String,
	`device_token` String,
	`source` String,
	`merchant_id` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`platform` String,
	`app_version` String,
	`bundle_version` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'device_token'),'') as device_token,
	ifNull(JSONExtractString(message,'source'),'') as source,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'platform'),'') as platform,
	ifNull(JSONExtractString(message,'app_version'),'') as app_version,
	ifNull(JSONExtractString(message,'bundle_version'),'') as bundle_version,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'AppInstallsObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.beckn_request_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `beckn_request` Nullable (String),
    `signature_header` Nullable (String),
    `time_stamp` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.beckn_request ON CLUSTER `{cluster}` AS atlas_app_helper.beckn_request_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, beckn_request_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.beckn_request ON CLUSTER `{cluster}` TO atlas_app.beckn_request_mv
(
	`id` String,
	`beckn_request` String,
	`signature_header` String,
	`time_stamp` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'beckn_request'),'') as beckn_request,
	ifNull(JSONExtractString(message,'signature_header'),'') as signature_header,
	toDateTime(JSONExtractInt(message,'time_stamp')) as time_stamp,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'BecknRequestObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.black_list_org_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `subscriber_id` Nullable (String),
    `type` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.black_list_org ON CLUSTER `{cluster}` AS atlas_app_helper.black_list_org_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, black_list_org_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.black_list_org ON CLUSTER `{cluster}` TO atlas_app.black_list_org_mv
(
	`id` String,
	`subscriber_id` String,
	`type` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'subscriber_id'),'') as subscriber_id,
	ifNull(JSONExtractString(message,'type'),'') as type,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'BlackListOrgObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.booking_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `status` Nullable (String),
    `provider_id` Nullable (String),
    `provider_mobile_number` Nullable (String),
    `start_time` DateTime DEFAULT now(),
    `rider_id` Nullable (String),
    `from_location_id` Nullable (String),
    `to_location_id` Nullable (String),
    `estimated_fare` Nullable (String),
    `discount` Nullable (String),
    `estimated_total_fare` Nullable (String),
    `distance` Nullable (String),
    `vehicle_variant` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `bpp_ride_booking_id` Nullable (String),
    `provider_name` Nullable (String),
    `provider_url` Nullable (String),
    `reallocations_count` Nullable (Int64),
    `fare_product_type` Nullable (String),
    `trip_terms_id` Nullable (String),
    `rental_slab_id` Nullable (String),
    `merchant_id` Nullable (String),
    `quote_id` Nullable (String),
    `primary_exophone` Nullable (String),
    `otp_code` Nullable (String),
    `transaction_id` Nullable (String),
    `special_location_tag` Nullable (String),
    `payment_method_id` Nullable (String),
    `payment_url` Nullable (String),
    `fulfillment_id` Nullable (String),
    `driver_id` Nullable (String),
    `item_id` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.booking ON CLUSTER `{cluster}` AS atlas_app_helper.booking_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, booking_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.booking ON CLUSTER `{cluster}` TO atlas_app.booking_mv
(
	`id` String,
	`status` String,
	`provider_id` String,
	`provider_mobile_number` String,
	`start_time` DateTime,
	`rider_id` String,
	`from_location_id` String,
	`to_location_id` String,
	`estimated_fare` String,
	`discount` String,
	`estimated_total_fare` String,
	`distance` String,
	`vehicle_variant` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`bpp_ride_booking_id` String,
	`provider_name` String,
	`provider_url` String,
	`reallocations_count` Int64,
	`fare_product_type` String,
	`trip_terms_id` String,
	`rental_slab_id` String,
	`merchant_id` String,
	`quote_id` String,
	`primary_exophone` String,
	`otp_code` String,
	`transaction_id` String,
	`special_location_tag` String,
	`payment_method_id` String,
	`payment_url` String,
	`fulfillment_id` String,
	`driver_id` String,
	`item_id` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	ifNull(JSONExtractString(message,'provider_mobile_number'),'') as provider_mobile_number,
	toDateTime(JSONExtractInt(message,'start_time')) as start_time,
	ifNull(JSONExtractString(message,'rider_id'),'') as rider_id,
	ifNull(JSONExtractString(message,'from_location_id'),'') as from_location_id,
	ifNull(JSONExtractString(message,'to_location_id'),'') as to_location_id,
	ifNull(JSONExtractString(message,'estimated_fare'),'') as estimated_fare,
	ifNull(JSONExtractString(message,'discount'),'') as discount,
	ifNull(JSONExtractString(message,'estimated_total_fare'),'') as estimated_total_fare,
	ifNull(JSONExtractString(message,'distance'),'') as distance,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'bpp_ride_booking_id'),'') as bpp_ride_booking_id,
	ifNull(JSONExtractString(message,'provider_name'),'') as provider_name,
	ifNull(JSONExtractString(message,'provider_url'),'') as provider_url,
	ifNull(JSONExtractInt(message,'reallocations_count'), 0) as reallocations_count,
	ifNull(JSONExtractString(message,'fare_product_type'),'') as fare_product_type,
	ifNull(JSONExtractString(message,'trip_terms_id'),'') as trip_terms_id,
	ifNull(JSONExtractString(message,'rental_slab_id'),'') as rental_slab_id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'quote_id'),'') as quote_id,
	ifNull(JSONExtractString(message,'primary_exophone'),'') as primary_exophone,
	ifNull(JSONExtractString(message,'otp_code'),'') as otp_code,
	ifNull(JSONExtractString(message,'transaction_id'),'') as transaction_id,
	ifNull(JSONExtractString(message,'special_location_tag'),'') as special_location_tag,
	ifNull(JSONExtractString(message,'payment_method_id'),'') as payment_method_id,
	ifNull(JSONExtractString(message,'payment_url'),'') as payment_url,
	ifNull(JSONExtractString(message,'fulfillment_id'),'') as fulfillment_id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'item_id'),'') as item_id,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'BookingObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.booking_cancellation_reason_shard ON CLUSTER `{cluster}`
    (
    `booking_id` String,
    `source` Nullable (String),
    `reason_code` Nullable (String),
    `additional_info` Nullable (String),
    `reason_stage` Nullable (String),
    `ride_id` Nullable (String),
    `driver_cancellation_location_lat` Nullable (Float64),
    `driver_cancellation_location_lon` Nullable (Float64),
    `driver_dist_to_pickup` Nullable (String),
    `merchant_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (booking_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.booking_cancellation_reason ON CLUSTER `{cluster}` AS atlas_app_helper.booking_cancellation_reason_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, booking_cancellation_reason_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.booking_cancellation_reason ON CLUSTER `{cluster}` TO atlas_app.booking_cancellation_reason_mv
(
	`booking_id` String,
	`source` String,
	`reason_code` String,
	`additional_info` String,
	`reason_stage` String,
	`ride_id` String,
	`driver_cancellation_location_lat` Float64,
	`driver_cancellation_location_lon` Float64,
	`driver_dist_to_pickup` String,
	`merchant_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'booking_id'),'') as booking_id,
	ifNull(JSONExtractString(message,'source'),'') as source,
	ifNull(JSONExtractString(message,'reason_code'),'') as reason_code,
	ifNull(JSONExtractString(message,'additional_info'),'') as additional_info,
	ifNull(JSONExtractString(message,'reason_stage'),'') as reason_stage,
	ifNull(JSONExtractString(message,'ride_id'),'') as ride_id,
	ifNull(JSONExtractFloat(message,'driver_cancellation_location_lat'),0.0) as driver_cancellation_location_lat,
	ifNull(JSONExtractFloat(message,'driver_cancellation_location_lon'),0.0) as driver_cancellation_location_lon,
	ifNull(JSONExtractString(message,'driver_dist_to_pickup'),'') as driver_dist_to_pickup,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'BookingCancellationReasonObject'
	JSONExtractString(message, 'booking_id') is not null


CREATE TABLE atlas_app_helper.booking_location_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `city` Nullable (String),
    `state` Nullable (String),
    `country` Nullable (String),
    `street` Nullable (String),
    `door` Nullable (String),
    `building` Nullable (String),
    `area_code` Nullable (String),
    `area` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `ward` Nullable (String),
    `place_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.booking_location ON CLUSTER `{cluster}` AS atlas_app_helper.booking_location_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, booking_location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.booking_location ON CLUSTER `{cluster}` TO atlas_app.booking_location_mv
(
	`id` String,
	`lat` Float64,
	`lon` Float64,
	`city` String,
	`state` String,
	`country` String,
	`street` String,
	`door` String,
	`building` String,
	`area_code` String,
	`area` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`ward` String,
	`place_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'city'),'') as city,
	ifNull(JSONExtractString(message,'state'),'') as state,
	ifNull(JSONExtractString(message,'country'),'') as country,
	ifNull(JSONExtractString(message,'street'),'') as street,
	ifNull(JSONExtractString(message,'door'),'') as door,
	ifNull(JSONExtractString(message,'building'),'') as building,
	ifNull(JSONExtractString(message,'area_code'),'') as area_code,
	ifNull(JSONExtractString(message,'area'),'') as area,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'ward'),'') as ward,
	ifNull(JSONExtractString(message,'place_id'),'') as place_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'BookingLocationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.call_status_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `call_id` Nullable (String),
    `ride_id` Nullable (String),
    `status` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `recording_url` Nullable (String),
    `conversation_duration` Nullable (String),
    `dtmf_number_used` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.call_status ON CLUSTER `{cluster}` AS atlas_app_helper.call_status_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, call_status_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.call_status ON CLUSTER `{cluster}` TO atlas_app.call_status_mv
(
	`id` String,
	`call_id` String,
	`ride_id` String,
	`status` String,
	`created_at` DateTime,
	`recording_url` String,
	`conversation_duration` String,
	`dtmf_number_used` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'call_id'),'') as call_id,
	ifNull(JSONExtractString(message,'ride_id'),'') as ride_id,
	ifNull(JSONExtractString(message,'status'),'') as status,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'recording_url'),'') as recording_url,
	ifNull(JSONExtractString(message,'conversation_duration'),'') as conversation_duration,
	ifNull(JSONExtractString(message,'dtmf_number_used'),'') as dtmf_number_used,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'CallStatusObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.callback_request_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `customer_name` Nullable (String),
    `customer_phone_encrypted` Nullable (String),
    `customer_phone_hash` Nullable (String),
    `customer_mobile_country_code` Nullable (String),
    `status` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.callback_request ON CLUSTER `{cluster}` AS atlas_app_helper.callback_request_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, callback_request_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.callback_request ON CLUSTER `{cluster}` TO atlas_app.callback_request_mv
(
	`id` String,
	`merchant_id` String,
	`customer_name` String,
	`customer_phone_encrypted` String,
	`customer_phone_hash` String,
	`customer_mobile_country_code` String,
	`status` String,
	`updated_at` DateTime,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'customer_name'),'') as customer_name,
	ifNull(JSONExtractString(message,'customer_phone_encrypted'),'') as customer_phone_encrypted,
	ifNull(JSONExtractString(message,'customer_phone_hash'),'') as customer_phone_hash,
	ifNull(JSONExtractString(message,'customer_mobile_country_code'),'') as customer_mobile_country_code,
	ifNull(JSONExtractString(message,'status'),'') as status,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'CallbackRequestObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.cancellation_reason_shard ON CLUSTER `{cluster}`
    (
    `reason_code` String,
    `description` Nullable (String),
    `enabled` Nullable (String),
    `on_search` Nullable (String),
    `on_confirm` Nullable (String),
    `on_assign` Nullable (String),
    `priority` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (reason_code))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.cancellation_reason ON CLUSTER `{cluster}` AS atlas_app_helper.cancellation_reason_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, cancellation_reason_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.cancellation_reason ON CLUSTER `{cluster}` TO atlas_app.cancellation_reason_mv
(
	`reason_code` String,
	`description` String,
	`enabled` String,
	`on_search` String,
	`on_confirm` String,
	`on_assign` String,
	`priority` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'reason_code'),'') as reason_code,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractString(message,'enabled'),'') as enabled,
	ifNull(JSONExtractString(message,'on_search'),'') as on_search,
	ifNull(JSONExtractString(message,'on_confirm'),'') as on_confirm,
	ifNull(JSONExtractString(message,'on_assign'),'') as on_assign,
	ifNull(JSONExtractInt(message,'priority'), 0) as priority,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'CancellationReasonObject'
	JSONExtractString(message, 'reason_code') is not null


CREATE TABLE atlas_app_helper.comment_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `issue_report_id` Nullable (String),
    `author_id` Nullable (String),
    `comment` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.comment ON CLUSTER `{cluster}` AS atlas_app_helper.comment_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, comment_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.comment ON CLUSTER `{cluster}` TO atlas_app.comment_mv
(
	`id` String,
	`issue_report_id` String,
	`author_id` String,
	`comment` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'issue_report_id'),'') as issue_report_id,
	ifNull(JSONExtractString(message,'author_id'),'') as author_id,
	ifNull(JSONExtractString(message,'comment'),'') as comment,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'CommentObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.disability_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `tag` Nullable (String),
    `description` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.disability ON CLUSTER `{cluster}` AS atlas_app_helper.disability_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, disability_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.disability ON CLUSTER `{cluster}` TO atlas_app.disability_mv
(
	`id` String,
	`tag` String,
	`description` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'tag'),'') as tag,
	ifNull(JSONExtractString(message,'description'),'') as description,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'DisabilityObject'


CREATE TABLE atlas_app_helper.disability_translation_shard ON CLUSTER `{cluster}`
    (
    `disability_id` Nullable (String),
    `disability_tag` Nullable (String),
    `translation` Nullable (String),
    `language` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.disability_translation ON CLUSTER `{cluster}` AS atlas_app_helper.disability_translation_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, disability_translation_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.disability_translation ON CLUSTER `{cluster}` TO atlas_app.disability_translation_mv
(
	`disability_id` String,
	`disability_tag` String,
	`translation` String,
	`language` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'disability_id'),'') as disability_id,
	ifNull(JSONExtractString(message,'disability_tag'),'') as disability_tag,
	ifNull(JSONExtractString(message,'translation'),'') as translation,
	ifNull(JSONExtractString(message,'language'),'') as language,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'DisabilityTranslationObject'


CREATE TABLE atlas_app_helper.driver_offer_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `estimate_id` Nullable (String),
    `driver_name` Nullable (String),
    `distance_to_pickup` Nullable (Float64),
    `duration_to_pickup` Nullable (Int64),
    `valid_till` DateTime DEFAULT now(),
    `rating` Nullable (Float64),
    `bpp_quote_id` Nullable (String),
    `merchant_id` Nullable (String),
    `status` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `driver_id` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.driver_offer ON CLUSTER `{cluster}` AS atlas_app_helper.driver_offer_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, driver_offer_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.driver_offer ON CLUSTER `{cluster}` TO atlas_app.driver_offer_mv
(
	`id` String,
	`estimate_id` String,
	`driver_name` String,
	`distance_to_pickup` Float64,
	`duration_to_pickup` Int64,
	`valid_till` DateTime,
	`rating` Float64,
	`bpp_quote_id` String,
	`merchant_id` String,
	`status` String,
	`updated_at` DateTime,
	`driver_id` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'estimate_id'),'') as estimate_id,
	ifNull(JSONExtractString(message,'driver_name'),'') as driver_name,
	ifNull(JSONExtractFloat(message,'distance_to_pickup'),0.0) as distance_to_pickup,
	ifNull(JSONExtractInt(message,'duration_to_pickup'), 0) as duration_to_pickup,
	toDateTime(JSONExtractInt(message,'valid_till')) as valid_till,
	ifNull(JSONExtractFloat(message,'rating'),0.0) as rating,
	ifNull(JSONExtractString(message,'bpp_quote_id'),'') as bpp_quote_id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'status'),'') as status,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverOfferObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.estimate_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `request_id` Nullable (String),
    `estimated_fare` Nullable (String),
    `discount` Nullable (Float64),
    `estimated_total_fare` Nullable (String),
    `provider_id` Nullable (String),
    `provider_url` Nullable (String),
    `provider_name` Nullable (String),
    `provider_mobile_number` Nullable (String),
    `provider_completed_rides_count` Nullable (Int64),
    `vehicle_variant` Nullable (String),
    `trip_terms_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `min_total_fare` Nullable (String),
    `max_total_fare` Nullable (String),
    `night_shift_multiplier` Nullable (String),
    `night_shift_start` DateTime DEFAULT now(),
    `night_shift_end` DateTime DEFAULT now(),
    `drivers_location` Nullable (String),
    `waiting_charge_per_min` Nullable (Float64),
    `status` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `device` Nullable (String),
    `estimated_duration` Nullable (String),
    `estimated_distance` Nullable (String),
    `bpp_estimate_id` Nullable (String),
    `night_shift_charge` Nullable (String),
    `special_location_tag` Nullable (String),
    `merchant_id` Nullable (String),
    `item_id` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.estimate ON CLUSTER `{cluster}` AS atlas_app_helper.estimate_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, estimate_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.estimate ON CLUSTER `{cluster}` TO atlas_app.estimate_mv
(
	`id` String,
	`request_id` String,
	`estimated_fare` String,
	`discount` Float64,
	`estimated_total_fare` String,
	`provider_id` String,
	`provider_url` String,
	`provider_name` String,
	`provider_mobile_number` String,
	`provider_completed_rides_count` Int64,
	`vehicle_variant` String,
	`trip_terms_id` String,
	`created_at` DateTime,
	`min_total_fare` String,
	`max_total_fare` String,
	`night_shift_multiplier` String,
	`night_shift_start` DateTime,
	`night_shift_end` DateTime,
	`drivers_location` String,
	`waiting_charge_per_min` Float64,
	`status` String,
	`updated_at` DateTime,
	`device` String,
	`estimated_duration` String,
	`estimated_distance` String,
	`bpp_estimate_id` String,
	`night_shift_charge` String,
	`special_location_tag` String,
	`merchant_id` String,
	`item_id` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'estimated_fare'),'') as estimated_fare,
	ifNull(JSONExtractFloat(message,'discount'),0.0) as discount,
	ifNull(JSONExtractString(message,'estimated_total_fare'),'') as estimated_total_fare,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	ifNull(JSONExtractString(message,'provider_url'),'') as provider_url,
	ifNull(JSONExtractString(message,'provider_name'),'') as provider_name,
	ifNull(JSONExtractString(message,'provider_mobile_number'),'') as provider_mobile_number,
	ifNull(JSONExtractInt(message,'provider_completed_rides_count'), 0) as provider_completed_rides_count,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	ifNull(JSONExtractString(message,'trip_terms_id'),'') as trip_terms_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'min_total_fare'),'') as min_total_fare,
	ifNull(JSONExtractString(message,'max_total_fare'),'') as max_total_fare,
	ifNull(JSONExtractString(message,'night_shift_multiplier'),'') as night_shift_multiplier,
	toDateTime(JSONExtractInt(message,'night_shift_start')) as night_shift_start,
	toDateTime(JSONExtractInt(message,'night_shift_end')) as night_shift_end,
	ifNull(JSONExtractString(message,'drivers_location'),'') as drivers_location,
	ifNull(JSONExtractFloat(message,'waiting_charge_per_min'),0.0) as waiting_charge_per_min,
	ifNull(JSONExtractString(message,'status'),'') as status,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'device'),'') as device,
	ifNull(JSONExtractString(message,'estimated_duration'),'') as estimated_duration,
	ifNull(JSONExtractString(message,'estimated_distance'),'') as estimated_distance,
	ifNull(JSONExtractString(message,'bpp_estimate_id'),'') as bpp_estimate_id,
	ifNull(JSONExtractString(message,'night_shift_charge'),'') as night_shift_charge,
	ifNull(JSONExtractString(message,'special_location_tag'),'') as special_location_tag,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'item_id'),'') as item_id,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'EstimateObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.estimate_breakup_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `estimate_id` Nullable (String),
    `title` Nullable (String),
    `price_currency` Nullable (String),
    `price_value` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.estimate_breakup ON CLUSTER `{cluster}` AS atlas_app_helper.estimate_breakup_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, estimate_breakup_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.estimate_breakup ON CLUSTER `{cluster}` TO atlas_app.estimate_breakup_mv
(
	`id` String,
	`estimate_id` String,
	`title` String,
	`price_currency` String,
	`price_value` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'estimate_id'),'') as estimate_id,
	ifNull(JSONExtractString(message,'title'),'') as title,
	ifNull(JSONExtractString(message,'price_currency'),'') as price_currency,
	ifNull(JSONExtractString(message,'price_value'),'') as price_value,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'EstimateBreakupObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.exophone_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `primary_phone` Nullable (String),
    `backup_phone` Nullable (String),
    `is_primary_down` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `call_service` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.exophone ON CLUSTER `{cluster}` AS atlas_app_helper.exophone_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, exophone_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.exophone ON CLUSTER `{cluster}` TO atlas_app.exophone_mv
(
	`id` String,
	`merchant_id` String,
	`primary_phone` String,
	`backup_phone` String,
	`is_primary_down` String,
	`updated_at` DateTime,
	`created_at` DateTime,
	`call_service` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'primary_phone'),'') as primary_phone,
	ifNull(JSONExtractString(message,'backup_phone'),'') as backup_phone,
	ifNull(JSONExtractString(message,'is_primary_down'),'') as is_primary_down,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'call_service'),'') as call_service,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'ExophoneObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.fare_breakup_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `booking_id` Nullable (String),
    `description` Nullable (String),
    `amount` Nullable (Float64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.fare_breakup ON CLUSTER `{cluster}` AS atlas_app_helper.fare_breakup_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, fare_breakup_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.fare_breakup ON CLUSTER `{cluster}` TO atlas_app.fare_breakup_mv
(
	`id` String,
	`booking_id` String,
	`description` String,
	`amount` Float64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'booking_id'),'') as booking_id,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractFloat(message,'amount'),0.0) as amount,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'FareBreakupObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.feedback_form_shard ON CLUSTER `{cluster}`
    (
    `category_name` Nullable (String),
    `id` String,
    `rating` Nullable (String),
    `question` Nullable (String),
    `answer` Nullable (Array(String)),
    `answer_type` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.feedback_form ON CLUSTER `{cluster}` AS atlas_app_helper.feedback_form_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, feedback_form_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.feedback_form ON CLUSTER `{cluster}` TO atlas_app.feedback_form_mv
(
	`category_name` String,
	`id` String,
	`rating` String,
	`question` String,
	`answer` Array(String),
	`answer_type` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'category_name'),'') as category_name,
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'rating'),'') as rating,
	ifNull(JSONExtractString(message,'question'),'') as question,
	toDateTime(JSONExtractInt(message,'answer')) as answer,
	ifNull(JSONExtractString(message,'answer_type'),'') as answer_type,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'FeedbackFormObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.geometry_shard ON CLUSTER `{cluster}`
    (
    `region` Nullable (String),
    `geom` Nullable (String),
    `id` String,
    `city` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.geometry ON CLUSTER `{cluster}` AS atlas_app_helper.geometry_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, geometry_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.geometry ON CLUSTER `{cluster}` TO atlas_app.geometry_mv
(
	`region` String,
	`geom` String,
	`id` String,
	`city` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'region'),'') as region,
	ifNull(JSONExtractString(message,'geom'),'') as geom,
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'city'),'') as city,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'GeometryObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.hot_spot_config_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `hot_spot_geo_hash_precision` Nullable (String),
    `nearby_geohash_precision` Nullable (String),
    `block_radius` Nullable (String),
    `min_frequency_of_hot_spot` Nullable (String),
    `weight_of_manual_pickup` Nullable (String),
    `weight_of_manual_saved` Nullable (String),
    `weight_of_auto_pickup` Nullable (String),
    `weight_of_auto_saved` Nullable (String),
    `weight_of_trip_start` Nullable (String),
    `max_num_hot_spots_to_show` Nullable (String),
    `weight_of_trip_end` Nullable (String),
    `weight_of_special_location` Nullable (String),
    `should_take_hot_spot` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.hot_spot_config ON CLUSTER `{cluster}` AS atlas_app_helper.hot_spot_config_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, hot_spot_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.hot_spot_config ON CLUSTER `{cluster}` TO atlas_app.hot_spot_config_mv
(
	`id` String,
	`hot_spot_geo_hash_precision` String,
	`nearby_geohash_precision` String,
	`block_radius` String,
	`min_frequency_of_hot_spot` String,
	`weight_of_manual_pickup` String,
	`weight_of_manual_saved` String,
	`weight_of_auto_pickup` String,
	`weight_of_auto_saved` String,
	`weight_of_trip_start` String,
	`max_num_hot_spots_to_show` String,
	`weight_of_trip_end` String,
	`weight_of_special_location` String,
	`should_take_hot_spot` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'hot_spot_geo_hash_precision'),'') as hot_spot_geo_hash_precision,
	ifNull(JSONExtractString(message,'nearby_geohash_precision'),'') as nearby_geohash_precision,
	ifNull(JSONExtractString(message,'block_radius'),'') as block_radius,
	ifNull(JSONExtractString(message,'min_frequency_of_hot_spot'),'') as min_frequency_of_hot_spot,
	ifNull(JSONExtractString(message,'weight_of_manual_pickup'),'') as weight_of_manual_pickup,
	ifNull(JSONExtractString(message,'weight_of_manual_saved'),'') as weight_of_manual_saved,
	ifNull(JSONExtractString(message,'weight_of_auto_pickup'),'') as weight_of_auto_pickup,
	ifNull(JSONExtractString(message,'weight_of_auto_saved'),'') as weight_of_auto_saved,
	ifNull(JSONExtractString(message,'weight_of_trip_start'),'') as weight_of_trip_start,
	ifNull(JSONExtractString(message,'max_num_hot_spots_to_show'),'') as max_num_hot_spots_to_show,
	ifNull(JSONExtractString(message,'weight_of_trip_end'),'') as weight_of_trip_end,
	ifNull(JSONExtractString(message,'weight_of_special_location'),'') as weight_of_special_location,
	ifNull(JSONExtractString(message,'should_take_hot_spot'),'') as should_take_hot_spot,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'HotSpotConfigObject'


CREATE TABLE atlas_app_helper.issue_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `customer_id` Nullable (String),
    `booking_id` Nullable (String),
    `contact_email` Nullable (String),
    `reason` Nullable (String),
    `description` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `ticket_id` Nullable (String),
    `status` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.issue ON CLUSTER `{cluster}` AS atlas_app_helper.issue_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, issue_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.issue ON CLUSTER `{cluster}` TO atlas_app.issue_mv
(
	`id` String,
	`customer_id` String,
	`booking_id` String,
	`contact_email` String,
	`reason` String,
	`description` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`ticket_id` String,
	`status` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'customer_id'),'') as customer_id,
	ifNull(JSONExtractString(message,'booking_id'),'') as booking_id,
	ifNull(JSONExtractString(message,'contact_email'),'') as contact_email,
	ifNull(JSONExtractString(message,'reason'),'') as reason,
	ifNull(JSONExtractString(message,'description'),'') as description,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'ticket_id'),'') as ticket_id,
	ifNull(JSONExtractString(message,'status'),'') as status,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueObject'


CREATE TABLE atlas_app_helper.issue_category_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `category` Nullable (String),
    `logo_url` Nullable (String),
    `priority` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.issue_category ON CLUSTER `{cluster}` AS atlas_app_helper.issue_category_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, issue_category_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.issue_category ON CLUSTER `{cluster}` TO atlas_app.issue_category_mv
(
	`id` String,
	`category` String,
	`logo_url` String,
	`priority` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'category'),'') as category,
	ifNull(JSONExtractString(message,'logo_url'),'') as logo_url,
	ifNull(JSONExtractInt(message,'priority'), 0) as priority,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueCategoryObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.issue_config_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `auto_mark_issue_closed_duration` Nullable (Float64),
    `on_auto_mark_issue_cls_msgs` Nullable (String),
    `on_create_issue_msgs` Nullable (String),
    `on_issue_reopen_msgs` Nullable (String),
    `on_kapt_mark_issue_res_msgs` Nullable (Array(String)),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.issue_config ON CLUSTER `{cluster}` AS atlas_app_helper.issue_config_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, issue_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.issue_config ON CLUSTER `{cluster}` TO atlas_app.issue_config_mv
(
	`id` String,
	`auto_mark_issue_closed_duration` Float64,
	`on_auto_mark_issue_cls_msgs` String,
	`on_create_issue_msgs` String,
	`on_issue_reopen_msgs` String,
	`on_kapt_mark_issue_res_msgs` Array(String),
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractFloat(message,'auto_mark_issue_closed_duration'),0.0) as auto_mark_issue_closed_duration,
	ifNull(JSONExtractString(message,'on_auto_mark_issue_cls_msgs'),'') as on_auto_mark_issue_cls_msgs,
	ifNull(JSONExtractString(message,'on_create_issue_msgs'),'') as on_create_issue_msgs,
	ifNull(JSONExtractString(message,'on_issue_reopen_msgs'),'') as on_issue_reopen_msgs,
	toDateTime(JSONExtractInt(message,'on_kapt_mark_issue_res_msgs')) as on_kapt_mark_issue_res_msgs,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueConfigObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.issue_message_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `option_id` Nullable (String),
    `category_id` Nullable (String),
    `message` Nullable (String),
    `label` Nullable (String),
    `priority` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.issue_message ON CLUSTER `{cluster}` AS atlas_app_helper.issue_message_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, issue_message_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.issue_message ON CLUSTER `{cluster}` TO atlas_app.issue_message_mv
(
	`id` String,
	`option_id` String,
	`category_id` String,
	`message` String,
	`label` String,
	`priority` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'option_id'),'') as option_id,
	ifNull(JSONExtractString(message,'category_id'),'') as category_id,
	ifNull(JSONExtractString(message,'message'),'') as message,
	ifNull(JSONExtractString(message,'label'),'') as label,
	ifNull(JSONExtractInt(message,'priority'), 0) as priority,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueMessageObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.issue_option_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `issue_category_id` Nullable (String),
    `issue_message_id` Nullable (String),
    `option` Nullable (String),
    `label` Nullable (String),
    `priority` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.issue_option ON CLUSTER `{cluster}` AS atlas_app_helper.issue_option_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, issue_option_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.issue_option ON CLUSTER `{cluster}` TO atlas_app.issue_option_mv
(
	`id` String,
	`issue_category_id` String,
	`issue_message_id` String,
	`option` String,
	`label` String,
	`priority` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'issue_category_id'),'') as issue_category_id,
	ifNull(JSONExtractString(message,'issue_message_id'),'') as issue_message_id,
	ifNull(JSONExtractString(message,'option'),'') as option,
	ifNull(JSONExtractString(message,'label'),'') as label,
	ifNull(JSONExtractInt(message,'priority'), 0) as priority,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueOptionObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.issue_report_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `person_id` Nullable (String),
    `ride_id` Nullable (String),
    `description` Nullable (String),
    `assignee` Nullable (String),
    `status` Nullable (String),
    `category_id` Nullable (String),
    `option_id` Nullable (String),
    `deleted` Nullable (String),
    `media_files` Nullable (String),
    `ticket_id` Nullable (String),
    `chats` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `driver_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.issue_report ON CLUSTER `{cluster}` AS atlas_app_helper.issue_report_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, issue_report_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.issue_report ON CLUSTER `{cluster}` TO atlas_app.issue_report_mv
(
	`id` String,
	`person_id` String,
	`ride_id` String,
	`description` String,
	`assignee` String,
	`status` String,
	`category_id` String,
	`option_id` String,
	`deleted` String,
	`media_files` String,
	`ticket_id` String,
	`chats` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`driver_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'ride_id'),'') as ride_id,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractString(message,'assignee'),'') as assignee,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'category_id'),'') as category_id,
	ifNull(JSONExtractString(message,'option_id'),'') as option_id,
	ifNull(JSONExtractString(message,'deleted'),'') as deleted,
	ifNull(JSONExtractString(message,'media_files'),'') as media_files,
	ifNull(JSONExtractString(message,'ticket_id'),'') as ticket_id,
	ifNull(JSONExtractString(message,'chats'),'') as chats,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueReportObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.issue_translation_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `sentence` Nullable (String),
    `translation` Nullable (String),
    `language` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.issue_translation ON CLUSTER `{cluster}` AS atlas_app_helper.issue_translation_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, issue_translation_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.issue_translation ON CLUSTER `{cluster}` TO atlas_app.issue_translation_mv
(
	`id` String,
	`sentence` String,
	`translation` String,
	`language` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'sentence'),'') as sentence,
	ifNull(JSONExtractString(message,'translation'),'') as translation,
	ifNull(JSONExtractString(message,'language'),'') as language,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueTranslationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.location_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `street` Nullable (String),
    `door` Nullable (String),
    `city` Nullable (String),
    `state` Nullable (String),
    `country` Nullable (String),
    `building` Nullable (String),
    `area_code` Nullable (String),
    `area` Nullable (String),
    `ward` Nullable (String),
    `place_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.location ON CLUSTER `{cluster}` AS atlas_app_helper.location_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.location ON CLUSTER `{cluster}` TO atlas_app.location_mv
(
	`id` String,
	`lat` Float64,
	`lon` Float64,
	`street` String,
	`door` String,
	`city` String,
	`state` String,
	`country` String,
	`building` String,
	`area_code` String,
	`area` String,
	`ward` String,
	`place_id` String,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'street'),'') as street,
	ifNull(JSONExtractString(message,'door'),'') as door,
	ifNull(JSONExtractString(message,'city'),'') as city,
	ifNull(JSONExtractString(message,'state'),'') as state,
	ifNull(JSONExtractString(message,'country'),'') as country,
	ifNull(JSONExtractString(message,'building'),'') as building,
	ifNull(JSONExtractString(message,'area_code'),'') as area_code,
	ifNull(JSONExtractString(message,'area'),'') as area,
	ifNull(JSONExtractString(message,'ward'),'') as ward,
	ifNull(JSONExtractString(message,'place_id'),'') as place_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'LocationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.location_backup_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `location_type` Nullable (String),
    `lat` Nullable (Float64),
    `long` Nullable (Float64),
    `point` Nullable (String),
    `ward` Nullable (String),
    `district` Nullable (String),
    `city` Nullable (String),
    `state` Nullable (String),
    `country` Nullable (String),
    `pincode` Nullable (String),
    `address` Nullable (String),
    `bound` Nullable (String),
    `info` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.location_backup ON CLUSTER `{cluster}` AS atlas_app_helper.location_backup_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, location_backup_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.location_backup ON CLUSTER `{cluster}` TO atlas_app.location_backup_mv
(
	`id` String,
	`location_type` String,
	`lat` Float64,
	`long` Float64,
	`point` String,
	`ward` String,
	`district` String,
	`city` String,
	`state` String,
	`country` String,
	`pincode` String,
	`address` String,
	`bound` String,
	`info` String,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'location_type'),'') as location_type,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'long'),0.0) as long,
	ifNull(JSONExtractString(message,'point'),'') as point,
	ifNull(JSONExtractString(message,'ward'),'') as ward,
	ifNull(JSONExtractString(message,'district'),'') as district,
	ifNull(JSONExtractString(message,'city'),'') as city,
	ifNull(JSONExtractString(message,'state'),'') as state,
	ifNull(JSONExtractString(message,'country'),'') as country,
	ifNull(JSONExtractString(message,'pincode'),'') as pincode,
	ifNull(JSONExtractString(message,'address'),'') as address,
	ifNull(JSONExtractString(message,'bound'),'') as bound,
	ifNull(JSONExtractString(message,'info'),'') as info,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'LocationBackupObject'


CREATE TABLE atlas_app_helper.location_mapping_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `location_id` Nullable (String),
    `tag` Nullable (String),
    `entity_id` Nullable (String),
    `"order"` Nullable (Int64),
    `version` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.location_mapping ON CLUSTER `{cluster}` AS atlas_app_helper.location_mapping_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, location_mapping_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.location_mapping ON CLUSTER `{cluster}` TO atlas_app.location_mapping_mv
(
	`id` String,
	`location_id` String,
	`tag` String,
	`entity_id` String,
	`"order"` Int64,
	`version` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'location_id'),'') as location_id,
	ifNull(JSONExtractString(message,'tag'),'') as tag,
	ifNull(JSONExtractString(message,'entity_id'),'') as entity_id,
	ifNull(JSONExtractInt(message,'"order"'), 0) as "order",
	ifNull(JSONExtractString(message,'version'),'') as version,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'LocationMappingObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.media_file_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `type` Nullable (String),
    `url` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.media_file ON CLUSTER `{cluster}` AS atlas_app_helper.media_file_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, media_file_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.media_file ON CLUSTER `{cluster}` TO atlas_app.media_file_mv
(
	`id` String,
	`type` String,
	`url` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'type'),'') as type,
	ifNull(JSONExtractString(message,'url'),'') as url,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'MediaFileObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.merchant_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `short_id` Nullable (String),
    `origin_restriction` Nullable (String),
    `destination_restriction` Nullable (String),
    `registry_url` Nullable (String),
    `gateway_url` Nullable (String),
    `name` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `driver_offer_base_url` Nullable (String),
    `driver_offer_api_key` Nullable (String),
    `driver_offer_merchant_id` Nullable (String),
    `subscriber_id` Nullable (String),
    `city` Nullable (String),
    `geo_hash_precision_value` Nullable (Int64),
    `signing_public_key` Nullable (String),
    `signature_expiry` Nullable (Int64),
    `cipher_text` Nullable (String),
    `country` Nullable (String),
    `bap_unique_key_id` Nullable (String),
    `bap_id` Nullable (String),
    `time_diff_from_utc` Nullable (Int64),
    `distance_weightage` Nullable (Int64),
    `minimum_driver_rates_count` Nullable (String),
    `is_avoid_toll` Nullable (String),
    `aadhaar_verification_try_limit` Nullable (Int64),
    `aadhaar_key_expiry_time` Nullable (String),
    `media_file_url_pattern` Nullable (String),
    `media_file_size_upper_limit` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.merchant ON CLUSTER `{cluster}` AS atlas_app_helper.merchant_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, merchant_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.merchant ON CLUSTER `{cluster}` TO atlas_app.merchant_mv
(
	`id` String,
	`short_id` String,
	`origin_restriction` String,
	`destination_restriction` String,
	`registry_url` String,
	`gateway_url` String,
	`name` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`driver_offer_base_url` String,
	`driver_offer_api_key` String,
	`driver_offer_merchant_id` String,
	`subscriber_id` String,
	`city` String,
	`geo_hash_precision_value` Int64,
	`signing_public_key` String,
	`signature_expiry` Int64,
	`cipher_text` String,
	`country` String,
	`bap_unique_key_id` String,
	`bap_id` String,
	`time_diff_from_utc` Int64,
	`distance_weightage` Int64,
	`minimum_driver_rates_count` String,
	`is_avoid_toll` String,
	`aadhaar_verification_try_limit` Int64,
	`aadhaar_key_expiry_time` String,
	`media_file_url_pattern` String,
	`media_file_size_upper_limit` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'short_id'),'') as short_id,
	ifNull(JSONExtractString(message,'origin_restriction'),'') as origin_restriction,
	ifNull(JSONExtractString(message,'destination_restriction'),'') as destination_restriction,
	ifNull(JSONExtractString(message,'registry_url'),'') as registry_url,
	ifNull(JSONExtractString(message,'gateway_url'),'') as gateway_url,
	ifNull(JSONExtractString(message,'name'),'') as name,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'driver_offer_base_url'),'') as driver_offer_base_url,
	ifNull(JSONExtractString(message,'driver_offer_api_key'),'') as driver_offer_api_key,
	ifNull(JSONExtractString(message,'driver_offer_merchant_id'),'') as driver_offer_merchant_id,
	ifNull(JSONExtractString(message,'subscriber_id'),'') as subscriber_id,
	ifNull(JSONExtractString(message,'city'),'') as city,
	ifNull(JSONExtractInt(message,'geo_hash_precision_value'), 0) as geo_hash_precision_value,
	ifNull(JSONExtractString(message,'signing_public_key'),'') as signing_public_key,
	ifNull(JSONExtractInt(message,'signature_expiry'), 0) as signature_expiry,
	ifNull(JSONExtractString(message,'cipher_text'),'') as cipher_text,
	ifNull(JSONExtractString(message,'country'),'') as country,
	ifNull(JSONExtractString(message,'bap_unique_key_id'),'') as bap_unique_key_id,
	ifNull(JSONExtractString(message,'bap_id'),'') as bap_id,
	ifNull(JSONExtractInt(message,'time_diff_from_utc'), 0) as time_diff_from_utc,
	ifNull(JSONExtractInt(message,'distance_weightage'), 0) as distance_weightage,
	ifNull(JSONExtractString(message,'minimum_driver_rates_count'),'') as minimum_driver_rates_count,
	ifNull(JSONExtractString(message,'is_avoid_toll'),'') as is_avoid_toll,
	ifNull(JSONExtractInt(message,'aadhaar_verification_try_limit'), 0) as aadhaar_verification_try_limit,
	ifNull(JSONExtractString(message,'aadhaar_key_expiry_time'),'') as aadhaar_key_expiry_time,
	ifNull(JSONExtractString(message,'media_file_url_pattern'),'') as media_file_url_pattern,
	ifNull(JSONExtractInt(message,'media_file_size_upper_limit'), 0) as media_file_size_upper_limit,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.merchant_config_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` Nullable (String),
    `fraud_booking_cancellation_count_threshold` Nullable (Int64),
    `fraud_booking_total_count_threshold` Nullable (Int64),
    `fraud_booking_cancellation_count_window` Nullable (String),
    `fraud_booking_cancelled_by_driver_count_threshold` Nullable (Int64),
    `fraud_booking_cancelled_by_driver_count_window` Nullable (String),
    `fraud_search_count_threshold` Nullable (Int64),
    `fraud_search_count_window` Nullable (String),
    `id` String,
    `enabled` Nullable (String),
    `fraud_ride_count_threshold` Nullable (Int64),
    `fraud_ride_count_window` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.merchant_config ON CLUSTER `{cluster}` AS atlas_app_helper.merchant_config_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, merchant_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.merchant_config ON CLUSTER `{cluster}` TO atlas_app.merchant_config_mv
(
	`merchant_id` String,
	`fraud_booking_cancellation_count_threshold` Int64,
	`fraud_booking_total_count_threshold` Int64,
	`fraud_booking_cancellation_count_window` String,
	`fraud_booking_cancelled_by_driver_count_threshold` Int64,
	`fraud_booking_cancelled_by_driver_count_window` String,
	`fraud_search_count_threshold` Int64,
	`fraud_search_count_window` String,
	`id` String,
	`enabled` String,
	`fraud_ride_count_threshold` Int64,
	`fraud_ride_count_window` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractInt(message,'fraud_booking_cancellation_count_threshold'), 0) as fraud_booking_cancellation_count_threshold,
	ifNull(JSONExtractInt(message,'fraud_booking_total_count_threshold'), 0) as fraud_booking_total_count_threshold,
	ifNull(JSONExtractString(message,'fraud_booking_cancellation_count_window'),'') as fraud_booking_cancellation_count_window,
	ifNull(JSONExtractInt(message,'fraud_booking_cancelled_by_driver_count_threshold'), 0) as fraud_booking_cancelled_by_driver_count_threshold,
	ifNull(JSONExtractString(message,'fraud_booking_cancelled_by_driver_count_window'),'') as fraud_booking_cancelled_by_driver_count_window,
	ifNull(JSONExtractInt(message,'fraud_search_count_threshold'), 0) as fraud_search_count_threshold,
	ifNull(JSONExtractString(message,'fraud_search_count_window'),'') as fraud_search_count_window,
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'enabled'),'') as enabled,
	ifNull(JSONExtractInt(message,'fraud_ride_count_threshold'), 0) as fraud_ride_count_threshold,
	ifNull(JSONExtractString(message,'fraud_ride_count_window'),'') as fraud_ride_count_window,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantConfigObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.merchant_message_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` Nullable (String),
    `message_key` String,
    `message` String,
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `merchant_operating_city_id` String,
    `template_id` Nullable (String),
    `json_data` Nullable (String),
    `contains_url_button` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (merchant_operating_city_id, message_key))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.merchant_message ON CLUSTER `{cluster}` AS atlas_app_helper.merchant_message_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, merchant_message_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.merchant_message ON CLUSTER `{cluster}` TO atlas_app.merchant_message_mv
(
	`merchant_id` String,
	`message_key` String,
	`message` String,
	`updated_at` DateTime,
	`created_at` DateTime,
	`merchant_operating_city_id` String,
	`template_id` String,
	`json_data` String,
	`contains_url_button` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'message_key'),'') as message_key,
	ifNull(JSONExtractString(message,'message'),'') as message,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,
	ifNull(JSONExtractString(message,'template_id'),'') as template_id,
	ifNull(JSONExtractString(message,'json_data'),'') as json_data,
	ifNull(JSONExtractString(message,'contains_url_button'),'') as contains_url_button,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantMessageObject'
	JSONExtractString(message, 'merchant_operating_city_id') is not null
	JSONExtractString(message, ' message_key') is not null


CREATE TABLE atlas_app_helper.merchant_operating_city_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `merchant_short_id` Nullable (String),
    `city` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.merchant_operating_city ON CLUSTER `{cluster}` AS atlas_app_helper.merchant_operating_city_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, merchant_operating_city_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.merchant_operating_city ON CLUSTER `{cluster}` TO atlas_app.merchant_operating_city_mv
(
	`id` String,
	`merchant_id` String,
	`merchant_short_id` String,
	`city` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'merchant_short_id'),'') as merchant_short_id,
	ifNull(JSONExtractString(message,'city'),'') as city,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantOperatingCityObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.merchant_payment_method_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `payment_type` Nullable (String),
    `payment_instrument` Nullable (String),
    `collected_by` Nullable (String),
    `priority` Nullable (Int64),
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.merchant_payment_method ON CLUSTER `{cluster}` AS atlas_app_helper.merchant_payment_method_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, merchant_payment_method_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.merchant_payment_method ON CLUSTER `{cluster}` TO atlas_app.merchant_payment_method_mv
(
	`id` String,
	`merchant_id` String,
	`payment_type` String,
	`payment_instrument` String,
	`collected_by` String,
	`priority` Int64,
	`updated_at` DateTime,
	`created_at` DateTime,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'payment_type'),'') as payment_type,
	ifNull(JSONExtractString(message,'payment_instrument'),'') as payment_instrument,
	ifNull(JSONExtractString(message,'collected_by'),'') as collected_by,
	ifNull(JSONExtractInt(message,'priority'), 0) as priority,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantPaymentMethodObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.merchant_service_config_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` String,
    `service_name` String,
    `config_json` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (merchant_id, service_name))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.merchant_service_config ON CLUSTER `{cluster}` AS atlas_app_helper.merchant_service_config_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, merchant_service_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.merchant_service_config ON CLUSTER `{cluster}` TO atlas_app.merchant_service_config_mv
(
	`merchant_id` String,
	`service_name` String,
	`config_json` String,
	`updated_at` DateTime,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'service_name'),'') as service_name,
	ifNull(JSONExtractString(message,'config_json'),'') as config_json,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantServiceConfigObject'
	JSONExtractString(message, 'merchant_id') is not null
	JSONExtractString(message, ' service_name') is not null


CREATE TABLE atlas_app_helper.merchant_service_usage_config_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` Nullable (String),
    `get_distances` Nullable (String),
    `get_routes` Nullable (String),
    `snap_to_road` Nullable (String),
    `get_place_name` Nullable (String),
    `get_place_details` Nullable (String),
    `auto_complete` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `sms_providers_priority_list` Nullable (Array(String)),
    `whatsapp_providers_priority_list` Nullable (Array(String)),
    `initiate_call` Nullable (String),
    `get_pickup_routes` Nullable (String),
    `get_trip_routes` Nullable (String),
    `use_fraud_detection` Nullable (String),
    `notify_person` Nullable (String),
    `get_distances_for_cancel_ride` Nullable (String),
    `enable_dashboard_sms` Nullable (String),
    `issue_ticket_service` Nullable (String),
    `get_exophone` Nullable (String),
    `aadhaar_verification_service` Nullable (String),
    `merchant_operating_city_id` String,
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (merchant_operating_city_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.merchant_service_usage_config ON CLUSTER `{cluster}` AS atlas_app_helper.merchant_service_usage_config_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, merchant_service_usage_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.merchant_service_usage_config ON CLUSTER `{cluster}` TO atlas_app.merchant_service_usage_config_mv
(
	`merchant_id` String,
	`get_distances` String,
	`get_routes` String,
	`snap_to_road` String,
	`get_place_name` String,
	`get_place_details` String,
	`auto_complete` String,
	`updated_at` DateTime,
	`created_at` DateTime,
	`sms_providers_priority_list` Array(String),
	`whatsapp_providers_priority_list` Array(String),
	`initiate_call` String,
	`get_pickup_routes` String,
	`get_trip_routes` String,
	`use_fraud_detection` String,
	`notify_person` String,
	`get_distances_for_cancel_ride` String,
	`enable_dashboard_sms` String,
	`issue_ticket_service` String,
	`get_exophone` String,
	`aadhaar_verification_service` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'get_distances'),'') as get_distances,
	ifNull(JSONExtractString(message,'get_routes'),'') as get_routes,
	ifNull(JSONExtractString(message,'snap_to_road'),'') as snap_to_road,
	ifNull(JSONExtractString(message,'get_place_name'),'') as get_place_name,
	ifNull(JSONExtractString(message,'get_place_details'),'') as get_place_details,
	ifNull(JSONExtractString(message,'auto_complete'),'') as auto_complete,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'sms_providers_priority_list')) as sms_providers_priority_list,
	toDateTime(JSONExtractInt(message,'whatsapp_providers_priority_list')) as whatsapp_providers_priority_list,
	ifNull(JSONExtractString(message,'initiate_call'),'') as initiate_call,
	ifNull(JSONExtractString(message,'get_pickup_routes'),'') as get_pickup_routes,
	ifNull(JSONExtractString(message,'get_trip_routes'),'') as get_trip_routes,
	ifNull(JSONExtractString(message,'use_fraud_detection'),'') as use_fraud_detection,
	ifNull(JSONExtractString(message,'notify_person'),'') as notify_person,
	ifNull(JSONExtractString(message,'get_distances_for_cancel_ride'),'') as get_distances_for_cancel_ride,
	ifNull(JSONExtractString(message,'enable_dashboard_sms'),'') as enable_dashboard_sms,
	ifNull(JSONExtractString(message,'issue_ticket_service'),'') as issue_ticket_service,
	ifNull(JSONExtractString(message,'get_exophone'),'') as get_exophone,
	ifNull(JSONExtractString(message,'aadhaar_verification_service'),'') as aadhaar_verification_service,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantServiceUsageConfigObject'
	JSONExtractString(message, 'merchant_operating_city_id') is not null


CREATE TABLE atlas_app_helper.on_search_event_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `bpp_id` Nullable (String),
    `message_id` Nullable (String),
    `error_code` Nullable (String),
    `error_type` Nullable (String),
    `error_message` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.on_search_event ON CLUSTER `{cluster}` AS atlas_app_helper.on_search_event_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, on_search_event_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.on_search_event ON CLUSTER `{cluster}` TO atlas_app.on_search_event_mv
(
	`id` String,
	`bpp_id` String,
	`message_id` String,
	`error_code` String,
	`error_type` String,
	`error_message` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'bpp_id'),'') as bpp_id,
	ifNull(JSONExtractString(message,'message_id'),'') as message_id,
	ifNull(JSONExtractString(message,'error_code'),'') as error_code,
	ifNull(JSONExtractString(message,'error_type'),'') as error_type,
	ifNull(JSONExtractString(message,'error_message'),'') as error_message,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'OnSearchEventObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.payment_order_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `short_id` Nullable (String),
    `person_id` Nullable (String),
    `merchant_id` Nullable (String),
    `amount` Nullable (Int64),
    `currency` Nullable (String),
    `status` Nullable (String),
    `web_payment_link` Nullable (String),
    `iframe_payment_link` Nullable (String),
    `mobile_payment_link` Nullable (String),
    `client_auth_token_encrypted` Nullable (String),
    `client_auth_token_hash` Nullable (String),
    `client_auth_token_expiry` DateTime DEFAULT now(),
    `get_upi_deep_links_option` Nullable (String),
    `environment` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `payment_service_order_id` Nullable (String),
    `service` Nullable (String),
    `client_id` Nullable (String),
    `description` Nullable (String),
    `return_url` Nullable (String),
    `action` Nullable (String),
    `request_id` Nullable (String),
    `payment_merchant_id` Nullable (String),
    `create_mandate` Nullable (String),
    `mandate_max_amount` Nullable (String),
    `mandate_start_date` DateTime DEFAULT now(),
    `mandate_end_date` DateTime DEFAULT now(),
    `bank_error_message` Nullable (String),
    `bank_error_code` Nullable (String),
    `is_retried` Nullable (String),
    `is_retargeted` Nullable (String),
    `retarget_link` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.payment_order ON CLUSTER `{cluster}` AS atlas_app_helper.payment_order_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, payment_order_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.payment_order ON CLUSTER `{cluster}` TO atlas_app.payment_order_mv
(
	`id` String,
	`short_id` String,
	`person_id` String,
	`merchant_id` String,
	`amount` Int64,
	`currency` String,
	`status` String,
	`web_payment_link` String,
	`iframe_payment_link` String,
	`mobile_payment_link` String,
	`client_auth_token_encrypted` String,
	`client_auth_token_hash` String,
	`client_auth_token_expiry` DateTime,
	`get_upi_deep_links_option` String,
	`environment` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`payment_service_order_id` String,
	`service` String,
	`client_id` String,
	`description` String,
	`return_url` String,
	`action` String,
	`request_id` String,
	`payment_merchant_id` String,
	`create_mandate` String,
	`mandate_max_amount` String,
	`mandate_start_date` DateTime,
	`mandate_end_date` DateTime,
	`bank_error_message` String,
	`bank_error_code` String,
	`is_retried` String,
	`is_retargeted` String,
	`retarget_link` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'short_id'),'') as short_id,
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractInt(message,'amount'), 0) as amount,
	ifNull(JSONExtractString(message,'currency'),'') as currency,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'web_payment_link'),'') as web_payment_link,
	ifNull(JSONExtractString(message,'iframe_payment_link'),'') as iframe_payment_link,
	ifNull(JSONExtractString(message,'mobile_payment_link'),'') as mobile_payment_link,
	ifNull(JSONExtractString(message,'client_auth_token_encrypted'),'') as client_auth_token_encrypted,
	ifNull(JSONExtractString(message,'client_auth_token_hash'),'') as client_auth_token_hash,
	toDateTime(JSONExtractInt(message,'client_auth_token_expiry')) as client_auth_token_expiry,
	ifNull(JSONExtractString(message,'get_upi_deep_links_option'),'') as get_upi_deep_links_option,
	ifNull(JSONExtractString(message,'environment'),'') as environment,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'payment_service_order_id'),'') as payment_service_order_id,
	ifNull(JSONExtractString(message,'service'),'') as service,
	ifNull(JSONExtractString(message,'client_id'),'') as client_id,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractString(message,'return_url'),'') as return_url,
	ifNull(JSONExtractString(message,'action'),'') as action,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'payment_merchant_id'),'') as payment_merchant_id,
	ifNull(JSONExtractString(message,'create_mandate'),'') as create_mandate,
	ifNull(JSONExtractString(message,'mandate_max_amount'),'') as mandate_max_amount,
	toDateTime(JSONExtractInt(message,'mandate_start_date')) as mandate_start_date,
	toDateTime(JSONExtractInt(message,'mandate_end_date')) as mandate_end_date,
	ifNull(JSONExtractString(message,'bank_error_message'),'') as bank_error_message,
	ifNull(JSONExtractString(message,'bank_error_code'),'') as bank_error_code,
	ifNull(JSONExtractString(message,'is_retried'),'') as is_retried,
	ifNull(JSONExtractString(message,'is_retargeted'),'') as is_retargeted,
	ifNull(JSONExtractString(message,'retarget_link'),'') as retarget_link,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'PaymentOrderObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.payment_transaction_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `txn_uuid` Nullable (String),
    `payment_method_type` Nullable (String),
    `payment_method` Nullable (String),
    `resp_message` Nullable (String),
    `resp_code` Nullable (String),
    `gateway_reference_id` Nullable (String),
    `order_id` Nullable (String),
    `merchant_id` Nullable (String),
    `amount` Nullable (String),
    `currency` Nullable (String),
    `date_created` DateTime DEFAULT now(),
    `status_id` Nullable (Int64),
    `status` Nullable (String),
    `juspay_response` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `mandate_id` Nullable (String),
    `mandate_max_amount` Nullable (String),
    `mandate_frequency` Nullable (String),
    `mandate_status` Nullable (String),
    `mandate_start_date` DateTime DEFAULT now(),
    `mandate_end_date` DateTime DEFAULT now(),
    `bank_error_message` Nullable (String),
    `bank_error_code` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.payment_transaction ON CLUSTER `{cluster}` AS atlas_app_helper.payment_transaction_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, payment_transaction_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.payment_transaction ON CLUSTER `{cluster}` TO atlas_app.payment_transaction_mv
(
	`id` String,
	`txn_uuid` String,
	`payment_method_type` String,
	`payment_method` String,
	`resp_message` String,
	`resp_code` String,
	`gateway_reference_id` String,
	`order_id` String,
	`merchant_id` String,
	`amount` String,
	`currency` String,
	`date_created` DateTime,
	`status_id` Int64,
	`status` String,
	`juspay_response` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`mandate_id` String,
	`mandate_max_amount` String,
	`mandate_frequency` String,
	`mandate_status` String,
	`mandate_start_date` DateTime,
	`mandate_end_date` DateTime,
	`bank_error_message` String,
	`bank_error_code` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'txn_uuid'),'') as txn_uuid,
	ifNull(JSONExtractString(message,'payment_method_type'),'') as payment_method_type,
	ifNull(JSONExtractString(message,'payment_method'),'') as payment_method,
	ifNull(JSONExtractString(message,'resp_message'),'') as resp_message,
	ifNull(JSONExtractString(message,'resp_code'),'') as resp_code,
	ifNull(JSONExtractString(message,'gateway_reference_id'),'') as gateway_reference_id,
	ifNull(JSONExtractString(message,'order_id'),'') as order_id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'amount'),'') as amount,
	ifNull(JSONExtractString(message,'currency'),'') as currency,
	toDateTime(JSONExtractInt(message,'date_created')) as date_created,
	ifNull(JSONExtractInt(message,'status_id'), 0) as status_id,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'juspay_response'),'') as juspay_response,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'mandate_id'),'') as mandate_id,
	ifNull(JSONExtractString(message,'mandate_max_amount'),'') as mandate_max_amount,
	ifNull(JSONExtractString(message,'mandate_frequency'),'') as mandate_frequency,
	ifNull(JSONExtractString(message,'mandate_status'),'') as mandate_status,
	toDateTime(JSONExtractInt(message,'mandate_start_date')) as mandate_start_date,
	toDateTime(JSONExtractInt(message,'mandate_end_date')) as mandate_end_date,
	ifNull(JSONExtractString(message,'bank_error_message'),'') as bank_error_message,
	ifNull(JSONExtractString(message,'bank_error_code'),'') as bank_error_code,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'PaymentTransactionObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.person_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `first_name` Nullable (String),
    `middle_name` Nullable (String),
    `last_name` Nullable (String),
    `role` Nullable (String),
    `gender` Nullable (String),
    `identifier_type` Nullable (String),
    `password_hash` Nullable (String),
    `mobile_number_encrypted` Nullable (String),
    `mobile_number_hash` Nullable (String),
    `mobile_country_code` Nullable (String),
    `identifier` Nullable (String),
    `rating` Nullable (String),
    `is_new` Nullable (String),
    `udf1` Nullable (String),
    `udf2` Nullable (String),
    `device_token` Nullable (String),
    `description` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `merchant_id` Nullable (String),
    `email_encrypted` Nullable (String),
    `email_hash` Nullable (String),
    `enabled` Nullable (String),
    `client_version` Nullable (String),
    `bundle_version` Nullable (String),
    `whatsapp_notification_enroll_status` Nullable (String),
    `unencrypted_mobile_number` Nullable (String),
    `referral_code` Nullable (String),
    `referred_at` DateTime DEFAULT now(),
    `has_taken_valid_ride` Nullable (String),
    `language` Nullable (String),
    `blocked` Nullable (String),
    `blocked_at` DateTime DEFAULT now(),
    `notification_token` Nullable (String),
    `blocked_by_rule_id` Nullable (String),
    `total_ratings` Nullable (Int64),
    `total_rating_score` Nullable (Int64),
    `is_valid_rating` Nullable (String),
    `has_disability` Nullable (String),
    `aadhaar_verified` Nullable (String),
    `current_city` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.person ON CLUSTER `{cluster}` AS atlas_app_helper.person_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, person_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.person ON CLUSTER `{cluster}` TO atlas_app.person_mv
(
	`id` String,
	`first_name` String,
	`middle_name` String,
	`last_name` String,
	`role` String,
	`gender` String,
	`identifier_type` String,
	`password_hash` String,
	`mobile_number_encrypted` String,
	`mobile_number_hash` String,
	`mobile_country_code` String,
	`identifier` String,
	`rating` String,
	`is_new` String,
	`udf1` String,
	`udf2` String,
	`device_token` String,
	`description` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`merchant_id` String,
	`email_encrypted` String,
	`email_hash` String,
	`enabled` String,
	`client_version` String,
	`bundle_version` String,
	`whatsapp_notification_enroll_status` String,
	`unencrypted_mobile_number` String,
	`referral_code` String,
	`referred_at` DateTime,
	`has_taken_valid_ride` String,
	`language` String,
	`blocked` String,
	`blocked_at` DateTime,
	`notification_token` String,
	`blocked_by_rule_id` String,
	`total_ratings` Int64,
	`total_rating_score` Int64,
	`is_valid_rating` String,
	`has_disability` String,
	`aadhaar_verified` String,
	`current_city` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'first_name'),'') as first_name,
	ifNull(JSONExtractString(message,'middle_name'),'') as middle_name,
	ifNull(JSONExtractString(message,'last_name'),'') as last_name,
	ifNull(JSONExtractString(message,'role'),'') as role,
	ifNull(JSONExtractString(message,'gender'),'') as gender,
	ifNull(JSONExtractString(message,'identifier_type'),'') as identifier_type,
	ifNull(JSONExtractString(message,'password_hash'),'') as password_hash,
	ifNull(JSONExtractString(message,'mobile_number_encrypted'),'') as mobile_number_encrypted,
	ifNull(JSONExtractString(message,'mobile_number_hash'),'') as mobile_number_hash,
	ifNull(JSONExtractString(message,'mobile_country_code'),'') as mobile_country_code,
	ifNull(JSONExtractString(message,'identifier'),'') as identifier,
	ifNull(JSONExtractString(message,'rating'),'') as rating,
	ifNull(JSONExtractString(message,'is_new'),'') as is_new,
	ifNull(JSONExtractString(message,'udf1'),'') as udf1,
	ifNull(JSONExtractString(message,'udf2'),'') as udf2,
	ifNull(JSONExtractString(message,'device_token'),'') as device_token,
	ifNull(JSONExtractString(message,'description'),'') as description,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'email_encrypted'),'') as email_encrypted,
	ifNull(JSONExtractString(message,'email_hash'),'') as email_hash,
	ifNull(JSONExtractString(message,'enabled'),'') as enabled,
	ifNull(JSONExtractString(message,'client_version'),'') as client_version,
	ifNull(JSONExtractString(message,'bundle_version'),'') as bundle_version,
	ifNull(JSONExtractString(message,'whatsapp_notification_enroll_status'),'') as whatsapp_notification_enroll_status,
	ifNull(JSONExtractString(message,'unencrypted_mobile_number'),'') as unencrypted_mobile_number,
	ifNull(JSONExtractString(message,'referral_code'),'') as referral_code,
	toDateTime(JSONExtractInt(message,'referred_at')) as referred_at,
	ifNull(JSONExtractString(message,'has_taken_valid_ride'),'') as has_taken_valid_ride,
	ifNull(JSONExtractString(message,'language'),'') as language,
	ifNull(JSONExtractString(message,'blocked'),'') as blocked,
	toDateTime(JSONExtractInt(message,'blocked_at')) as blocked_at,
	ifNull(JSONExtractString(message,'notification_token'),'') as notification_token,
	ifNull(JSONExtractString(message,'blocked_by_rule_id'),'') as blocked_by_rule_id,
	ifNull(JSONExtractInt(message,'total_ratings'), 0) as total_ratings,
	ifNull(JSONExtractInt(message,'total_rating_score'), 0) as total_rating_score,
	ifNull(JSONExtractString(message,'is_valid_rating'),'') as is_valid_rating,
	ifNull(JSONExtractString(message,'has_disability'),'') as has_disability,
	ifNull(JSONExtractString(message,'aadhaar_verified'),'') as aadhaar_verified,
	ifNull(JSONExtractString(message,'current_city'),'') as current_city,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'PersonObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.person_default_emergency_number_shard ON CLUSTER `{cluster}`
    (
    `person_id` String,
    `name` Nullable (String),
    `mobile_country_code` String,
    `mobile_number_encrypted` Nullable (String),
    `mobile_number_hash` String,
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (person_id, mobile_country_code, mobile_number_hash))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.person_default_emergency_number ON CLUSTER `{cluster}` AS atlas_app_helper.person_default_emergency_number_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, person_default_emergency_number_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.person_default_emergency_number ON CLUSTER `{cluster}` TO atlas_app.person_default_emergency_number_mv
(
	`person_id` String,
	`name` String,
	`mobile_country_code` String,
	`mobile_number_encrypted` String,
	`mobile_number_hash` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'name'),'') as name,
	ifNull(JSONExtractString(message,'mobile_country_code'),'') as mobile_country_code,
	ifNull(JSONExtractString(message,'mobile_number_encrypted'),'') as mobile_number_encrypted,
	ifNull(JSONExtractString(message,'mobile_number_hash'),'') as mobile_number_hash,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'PersonDefaultEmergencyNumberObject'
	JSONExtractString(message, 'person_id') is not null
	JSONExtractString(message, ' mobile_country_code') is not null
	JSONExtractString(message, ' mobile_number_hash') is not null


CREATE TABLE atlas_app_helper.person_disability_shard ON CLUSTER `{cluster}`
    (
    `person_id` String,
    `disability_id` Nullable (String),
    `tag` Nullable (String),
    `description` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (person_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.person_disability ON CLUSTER `{cluster}` AS atlas_app_helper.person_disability_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, person_disability_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.person_disability ON CLUSTER `{cluster}` TO atlas_app.person_disability_mv
(
	`person_id` String,
	`disability_id` String,
	`tag` String,
	`description` String,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'disability_id'),'') as disability_id,
	ifNull(JSONExtractString(message,'tag'),'') as tag,
	ifNull(JSONExtractString(message,'description'),'') as description,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'PersonDisabilityObject'
	JSONExtractString(message, 'person_id') is not null


CREATE TABLE atlas_app_helper.person_flow_status_shard ON CLUSTER `{cluster}`
    (
    `person_id` String,
    `flow_status` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (person_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.person_flow_status ON CLUSTER `{cluster}` AS atlas_app_helper.person_flow_status_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, person_flow_status_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.person_flow_status ON CLUSTER `{cluster}` TO atlas_app.person_flow_status_mv
(
	`person_id` String,
	`flow_status` String,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'flow_status'),'') as flow_status,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'PersonFlowStatusObject'
	JSONExtractString(message, 'person_id') is not null


CREATE TABLE atlas_app_helper.person_stats_shard ON CLUSTER `{cluster}`
    (
    `person_id` String,
    `user_cancelled_rides` Nullable (Int64),
    `driver_cancelled_rides` Nullable (Int64),
    `completed_rides` Nullable (Int64),
    `weekend_rides` Nullable (Int64),
    `weekday_rides` Nullable (Int64),
    `off_peak_rides` Nullable (Int64),
    `evening_peak_rides` Nullable (Int64),
    `morning_peak_rides` Nullable (Int64),
    `weekend_peak_rides` Nullable (Int64),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (person_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.person_stats ON CLUSTER `{cluster}` AS atlas_app_helper.person_stats_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, person_stats_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.person_stats ON CLUSTER `{cluster}` TO atlas_app.person_stats_mv
(
	`person_id` String,
	`user_cancelled_rides` Int64,
	`driver_cancelled_rides` Int64,
	`completed_rides` Int64,
	`weekend_rides` Int64,
	`weekday_rides` Int64,
	`off_peak_rides` Int64,
	`evening_peak_rides` Int64,
	`morning_peak_rides` Int64,
	`weekend_peak_rides` Int64,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractInt(message,'user_cancelled_rides'), 0) as user_cancelled_rides,
	ifNull(JSONExtractInt(message,'driver_cancelled_rides'), 0) as driver_cancelled_rides,
	ifNull(JSONExtractInt(message,'completed_rides'), 0) as completed_rides,
	ifNull(JSONExtractInt(message,'weekend_rides'), 0) as weekend_rides,
	ifNull(JSONExtractInt(message,'weekday_rides'), 0) as weekday_rides,
	ifNull(JSONExtractInt(message,'off_peak_rides'), 0) as off_peak_rides,
	ifNull(JSONExtractInt(message,'evening_peak_rides'), 0) as evening_peak_rides,
	ifNull(JSONExtractInt(message,'morning_peak_rides'), 0) as morning_peak_rides,
	ifNull(JSONExtractInt(message,'weekend_peak_rides'), 0) as weekend_peak_rides,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'PersonStatsObject'
	JSONExtractString(message, 'person_id') is not null


CREATE TABLE atlas_app_helper.place_name_cache_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `formatted_address` Nullable (String),
    `plus_code` Nullable (String),
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `place_id` Nullable (String),
    `address_components` Nullable (Array(String)),
    `geo_hash` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.place_name_cache ON CLUSTER `{cluster}` AS atlas_app_helper.place_name_cache_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, place_name_cache_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.place_name_cache ON CLUSTER `{cluster}` TO atlas_app.place_name_cache_mv
(
	`id` String,
	`formatted_address` String,
	`plus_code` String,
	`lat` Float64,
	`lon` Float64,
	`place_id` String,
	`address_components` Array(String),
	`geo_hash` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'formatted_address'),'') as formatted_address,
	ifNull(JSONExtractString(message,'plus_code'),'') as plus_code,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'place_id'),'') as place_id,
	toDateTime(JSONExtractInt(message,'address_components')) as address_components,
	ifNull(JSONExtractString(message,'geo_hash'),'') as geo_hash,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'PlaceNameCacheObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.product_instance_backup_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `case_id` Nullable (String),
    `product_id` Nullable (String),
    `person_id` Nullable (String),
    `person_updated_at` DateTime DEFAULT now(),
    `short_id` Nullable (String),
    `entity_id` Nullable (String),
    `entity_type` Nullable (String),
    `quantity` Nullable (String),
    `price` Nullable (String),
    `type` Nullable (String),
    `status` Nullable (String),
    `start_time` DateTime DEFAULT now(),
    `end_time` DateTime DEFAULT now(),
    `valid_till` DateTime DEFAULT now(),
    `from_location_id` Nullable (String),
    `to_location_id` Nullable (String),
    `organization_id` Nullable (String),
    `parent_id` Nullable (String),
    `info` Nullable (String),
    `udf1` Nullable (String),
    `udf2` Nullable (String),
    `udf3` Nullable (String),
    `udf4` Nullable (String),
    `udf5` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `actual_distance` Nullable (Float64),
    `actual_price` Nullable (Float64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.product_instance_backup ON CLUSTER `{cluster}` AS atlas_app_helper.product_instance_backup_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, product_instance_backup_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.product_instance_backup ON CLUSTER `{cluster}` TO atlas_app.product_instance_backup_mv
(
	`id` String,
	`case_id` String,
	`product_id` String,
	`person_id` String,
	`person_updated_at` DateTime,
	`short_id` String,
	`entity_id` String,
	`entity_type` String,
	`quantity` String,
	`price` String,
	`type` String,
	`status` String,
	`start_time` DateTime,
	`end_time` DateTime,
	`valid_till` DateTime,
	`from_location_id` String,
	`to_location_id` String,
	`organization_id` String,
	`parent_id` String,
	`info` String,
	`udf1` String,
	`udf2` String,
	`udf3` String,
	`udf4` String,
	`udf5` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`actual_distance` Float64,
	`actual_price` Float64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'case_id'),'') as case_id,
	ifNull(JSONExtractString(message,'product_id'),'') as product_id,
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	toDateTime(JSONExtractInt(message,'person_updated_at')) as person_updated_at,
	ifNull(JSONExtractString(message,'short_id'),'') as short_id,
	ifNull(JSONExtractString(message,'entity_id'),'') as entity_id,
	ifNull(JSONExtractString(message,'entity_type'),'') as entity_type,
	ifNull(JSONExtractString(message,'quantity'),'') as quantity,
	ifNull(JSONExtractString(message,'price'),'') as price,
	ifNull(JSONExtractString(message,'type'),'') as type,
	ifNull(JSONExtractString(message,'status'),'') as status,
	toDateTime(JSONExtractInt(message,'start_time')) as start_time,
	toDateTime(JSONExtractInt(message,'end_time')) as end_time,
	toDateTime(JSONExtractInt(message,'valid_till')) as valid_till,
	ifNull(JSONExtractString(message,'from_location_id'),'') as from_location_id,
	ifNull(JSONExtractString(message,'to_location_id'),'') as to_location_id,
	ifNull(JSONExtractString(message,'organization_id'),'') as organization_id,
	ifNull(JSONExtractString(message,'parent_id'),'') as parent_id,
	ifNull(JSONExtractString(message,'info'),'') as info,
	ifNull(JSONExtractString(message,'udf1'),'') as udf1,
	ifNull(JSONExtractString(message,'udf2'),'') as udf2,
	ifNull(JSONExtractString(message,'udf3'),'') as udf3,
	ifNull(JSONExtractString(message,'udf4'),'') as udf4,
	ifNull(JSONExtractString(message,'udf5'),'') as udf5,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractFloat(message,'actual_distance'),0.0) as actual_distance,
	ifNull(JSONExtractFloat(message,'actual_price'),0.0) as actual_price,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'ProductInstanceBackupObject'


CREATE TABLE atlas_app_helper.quote_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `request_id` Nullable (String),
    `estimated_fare` Nullable (String),
    `provider_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `vehicle_variant` Nullable (String),
    `discount` Nullable (String),
    `estimated_total_fare` Nullable (String),
    `total_fare` Nullable (String),
    `provider_mobile_number` Nullable (String),
    `distance_to_nearest_driver` Nullable (String),
    `provider_name` Nullable (String),
    `provider_completed_rides_count` Nullable (Int64),
    `provider_url` Nullable (String),
    `rental_slab_id` Nullable (String),
    `trip_terms_id` Nullable (String),
    `fare_product_type` Nullable (String),
    `driver_offer_id` Nullable (String),
    `merchant_id` Nullable (String),
    `special_zone_quote_id` Nullable (String),
    `special_location_tag` Nullable (String),
    `item_id` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.quote ON CLUSTER `{cluster}` AS atlas_app_helper.quote_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, quote_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.quote ON CLUSTER `{cluster}` TO atlas_app.quote_mv
(
	`id` String,
	`request_id` String,
	`estimated_fare` String,
	`provider_id` String,
	`created_at` DateTime,
	`vehicle_variant` String,
	`discount` String,
	`estimated_total_fare` String,
	`total_fare` String,
	`provider_mobile_number` String,
	`distance_to_nearest_driver` String,
	`provider_name` String,
	`provider_completed_rides_count` Int64,
	`provider_url` String,
	`rental_slab_id` String,
	`trip_terms_id` String,
	`fare_product_type` String,
	`driver_offer_id` String,
	`merchant_id` String,
	`special_zone_quote_id` String,
	`special_location_tag` String,
	`item_id` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'estimated_fare'),'') as estimated_fare,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	ifNull(JSONExtractString(message,'discount'),'') as discount,
	ifNull(JSONExtractString(message,'estimated_total_fare'),'') as estimated_total_fare,
	ifNull(JSONExtractString(message,'total_fare'),'') as total_fare,
	ifNull(JSONExtractString(message,'provider_mobile_number'),'') as provider_mobile_number,
	ifNull(JSONExtractString(message,'distance_to_nearest_driver'),'') as distance_to_nearest_driver,
	ifNull(JSONExtractString(message,'provider_name'),'') as provider_name,
	ifNull(JSONExtractInt(message,'provider_completed_rides_count'), 0) as provider_completed_rides_count,
	ifNull(JSONExtractString(message,'provider_url'),'') as provider_url,
	ifNull(JSONExtractString(message,'rental_slab_id'),'') as rental_slab_id,
	ifNull(JSONExtractString(message,'trip_terms_id'),'') as trip_terms_id,
	ifNull(JSONExtractString(message,'fare_product_type'),'') as fare_product_type,
	ifNull(JSONExtractString(message,'driver_offer_id'),'') as driver_offer_id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'special_zone_quote_id'),'') as special_zone_quote_id,
	ifNull(JSONExtractString(message,'special_location_tag'),'') as special_location_tag,
	ifNull(JSONExtractString(message,'item_id'),'') as item_id,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'QuoteObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.quote_bak_1022_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `request_id` Nullable (String),
    `estimated_fare` Nullable (String),
    `provider_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `vehicle_variant` Nullable (String),
    `discount` Nullable (Float64),
    `estimated_total_fare` Nullable (String),
    `total_fare` Nullable (String),
    `provider_mobile_number` Nullable (String),
    `distance_to_nearest_driver` Nullable (Float64),
    `provider_name` Nullable (String),
    `provider_completed_rides_count` Nullable (String),
    `bpp_quote_id` Nullable (String),
    `provider_url` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.quote_bak_1022 ON CLUSTER `{cluster}` AS atlas_app_helper.quote_bak_1022_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, quote_bak_1022_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.quote_bak_1022 ON CLUSTER `{cluster}` TO atlas_app.quote_bak_1022_mv
(
	`id` String,
	`request_id` String,
	`estimated_fare` String,
	`provider_id` String,
	`created_at` DateTime,
	`vehicle_variant` String,
	`discount` Float64,
	`estimated_total_fare` String,
	`total_fare` String,
	`provider_mobile_number` String,
	`distance_to_nearest_driver` Float64,
	`provider_name` String,
	`provider_completed_rides_count` String,
	`bpp_quote_id` String,
	`provider_url` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'estimated_fare'),'') as estimated_fare,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	ifNull(JSONExtractFloat(message,'discount'),0.0) as discount,
	ifNull(JSONExtractString(message,'estimated_total_fare'),'') as estimated_total_fare,
	ifNull(JSONExtractString(message,'total_fare'),'') as total_fare,
	ifNull(JSONExtractString(message,'provider_mobile_number'),'') as provider_mobile_number,
	ifNull(JSONExtractFloat(message,'distance_to_nearest_driver'),0.0) as distance_to_nearest_driver,
	ifNull(JSONExtractString(message,'provider_name'),'') as provider_name,
	ifNull(JSONExtractString(message,'provider_completed_rides_count'),'') as provider_completed_rides_count,
	ifNull(JSONExtractString(message,'bpp_quote_id'),'') as bpp_quote_id,
	ifNull(JSONExtractString(message,'provider_url'),'') as provider_url,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'QuoteBak1022Object'


CREATE TABLE atlas_app_helper.quote_bak_1026_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `request_id` Nullable (String),
    `estimated_fare` Nullable (String),
    `provider_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `vehicle_variant` Nullable (String),
    `discount` Nullable (Float64),
    `estimated_total_fare` Nullable (String),
    `total_fare` Nullable (String),
    `provider_mobile_number` Nullable (String),
    `distance_to_nearest_driver` Nullable (Float64),
    `provider_name` Nullable (String),
    `provider_completed_rides_count` Nullable (String),
    `bpp_quote_id` Nullable (String),
    `provider_url` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.quote_bak_1026 ON CLUSTER `{cluster}` AS atlas_app_helper.quote_bak_1026_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, quote_bak_1026_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.quote_bak_1026 ON CLUSTER `{cluster}` TO atlas_app.quote_bak_1026_mv
(
	`id` String,
	`request_id` String,
	`estimated_fare` String,
	`provider_id` String,
	`created_at` DateTime,
	`vehicle_variant` String,
	`discount` Float64,
	`estimated_total_fare` String,
	`total_fare` String,
	`provider_mobile_number` String,
	`distance_to_nearest_driver` Float64,
	`provider_name` String,
	`provider_completed_rides_count` String,
	`bpp_quote_id` String,
	`provider_url` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'estimated_fare'),'') as estimated_fare,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	ifNull(JSONExtractFloat(message,'discount'),0.0) as discount,
	ifNull(JSONExtractString(message,'estimated_total_fare'),'') as estimated_total_fare,
	ifNull(JSONExtractString(message,'total_fare'),'') as total_fare,
	ifNull(JSONExtractString(message,'provider_mobile_number'),'') as provider_mobile_number,
	ifNull(JSONExtractFloat(message,'distance_to_nearest_driver'),0.0) as distance_to_nearest_driver,
	ifNull(JSONExtractString(message,'provider_name'),'') as provider_name,
	ifNull(JSONExtractString(message,'provider_completed_rides_count'),'') as provider_completed_rides_count,
	ifNull(JSONExtractString(message,'bpp_quote_id'),'') as bpp_quote_id,
	ifNull(JSONExtractString(message,'provider_url'),'') as provider_url,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'QuoteBak1026Object'


CREATE TABLE atlas_app_helper.quote_terms_bak_1027_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `quote_id` Nullable (String),
    `description` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.quote_terms_bak_1027 ON CLUSTER `{cluster}` AS atlas_app_helper.quote_terms_bak_1027_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, quote_terms_bak_1027_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.quote_terms_bak_1027 ON CLUSTER `{cluster}` TO atlas_app.quote_terms_bak_1027_mv
(
	`id` String,
	`quote_id` String,
	`description` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'quote_id'),'') as quote_id,
	ifNull(JSONExtractString(message,'description'),'') as description,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'QuoteTermsBak1027Object'


CREATE TABLE atlas_app_helper.rating_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `ride_id` Nullable (String),
    `rating_value` Nullable (Int64),
    `feedback_details` Nullable (String),
    `rider_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.rating ON CLUSTER `{cluster}` AS atlas_app_helper.rating_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, rating_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.rating ON CLUSTER `{cluster}` TO atlas_app.rating_mv
(
	`id` String,
	`ride_id` String,
	`rating_value` Int64,
	`feedback_details` String,
	`rider_id` String,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'ride_id'),'') as ride_id,
	ifNull(JSONExtractInt(message,'rating_value'), 0) as rating_value,
	ifNull(JSONExtractString(message,'feedback_details'),'') as feedback_details,
	ifNull(JSONExtractString(message,'rider_id'),'') as rider_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'RatingObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.registration_token_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `auth_medium` Nullable (String),
    `auth_type` Nullable (String),
    `auth_value_hash` Nullable (String),
    `token` Nullable (String),
    `verified` Nullable (String),
    `auth_expiry` Nullable (Int64),
    `token_expiry` Nullable (Int64),
    `attempts` Nullable (Int64),
    `entity_id` Nullable (String),
    `entity_type` Nullable (String),
    `info` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `merchant_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.registration_token ON CLUSTER `{cluster}` AS atlas_app_helper.registration_token_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, registration_token_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.registration_token ON CLUSTER `{cluster}` TO atlas_app.registration_token_mv
(
	`id` String,
	`auth_medium` String,
	`auth_type` String,
	`auth_value_hash` String,
	`token` String,
	`verified` String,
	`auth_expiry` Int64,
	`token_expiry` Int64,
	`attempts` Int64,
	`entity_id` String,
	`entity_type` String,
	`info` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`merchant_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'auth_medium'),'') as auth_medium,
	ifNull(JSONExtractString(message,'auth_type'),'') as auth_type,
	ifNull(JSONExtractString(message,'auth_value_hash'),'') as auth_value_hash,
	ifNull(JSONExtractString(message,'token'),'') as token,
	ifNull(JSONExtractString(message,'verified'),'') as verified,
	ifNull(JSONExtractInt(message,'auth_expiry'), 0) as auth_expiry,
	ifNull(JSONExtractInt(message,'token_expiry'), 0) as token_expiry,
	ifNull(JSONExtractInt(message,'attempts'), 0) as attempts,
	ifNull(JSONExtractString(message,'entity_id'),'') as entity_id,
	ifNull(JSONExtractString(message,'entity_type'),'') as entity_type,
	ifNull(JSONExtractString(message,'info'),'') as info,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'RegistrationTokenObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.rental_quote_bak_1027_shard ON CLUSTER `{cluster}`
    (
    `quote_id` Nullable (String),
    `base_distance` Nullable (String),
    `base_duration_hr` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.rental_quote_bak_1027 ON CLUSTER `{cluster}` AS atlas_app_helper.rental_quote_bak_1027_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, rental_quote_bak_1027_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.rental_quote_bak_1027 ON CLUSTER `{cluster}` TO atlas_app.rental_quote_bak_1027_mv
(
	`quote_id` String,
	`base_distance` String,
	`base_duration_hr` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'quote_id'),'') as quote_id,
	ifNull(JSONExtractString(message,'base_distance'),'') as base_distance,
	ifNull(JSONExtractInt(message,'base_duration_hr'), 0) as base_duration_hr,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'RentalQuoteBak1027Object'


CREATE TABLE atlas_app_helper.rental_slab_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `base_distance` Nullable (Int64),
    `base_duration` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.rental_slab ON CLUSTER `{cluster}` AS atlas_app_helper.rental_slab_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, rental_slab_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.rental_slab ON CLUSTER `{cluster}` TO atlas_app.rental_slab_mv
(
	`id` String,
	`base_distance` Int64,
	`base_duration` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractInt(message,'base_distance'), 0) as base_distance,
	ifNull(JSONExtractInt(message,'base_duration'), 0) as base_duration,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'RentalSlabObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.ride_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `booking_id` Nullable (String),
    `short_id` Nullable (String),
    `status` Nullable (String),
    `driver_name` Nullable (String),
    `driver_rating` Nullable (String),
    `driver_mobile_number` Nullable (String),
    `driver_registered_at` DateTime DEFAULT now(),
    `vehicle_number` Nullable (String),
    `vehicle_model` Nullable (String),
    `vehicle_color` Nullable (String),
    `otp` Nullable (String),
    `tracking_url` Nullable (String),
    `fare` Nullable (String),
    `total_fare` Nullable (String),
    `chargeable_distance` Nullable (String),
    `vehicle_variant` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `bpp_ride_id` Nullable (String),
    `ride_start_time` DateTime DEFAULT now(),
    `ride_end_time` DateTime DEFAULT now(),
    `ride_rating` Nullable (String),
    `driver_arrival_time` DateTime DEFAULT now(),
    `merchant_id` Nullable (String),
    `traveled_distance` Nullable (String),
    `driver_mobile_country_code` Nullable (String),
    `driver_image` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.ride ON CLUSTER `{cluster}` AS atlas_app_helper.ride_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, ride_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.ride ON CLUSTER `{cluster}` TO atlas_app.ride_mv
(
	`id` String,
	`booking_id` String,
	`short_id` String,
	`status` String,
	`driver_name` String,
	`driver_rating` String,
	`driver_mobile_number` String,
	`driver_registered_at` DateTime,
	`vehicle_number` String,
	`vehicle_model` String,
	`vehicle_color` String,
	`otp` String,
	`tracking_url` String,
	`fare` String,
	`total_fare` String,
	`chargeable_distance` String,
	`vehicle_variant` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`bpp_ride_id` String,
	`ride_start_time` DateTime,
	`ride_end_time` DateTime,
	`ride_rating` String,
	`driver_arrival_time` DateTime,
	`merchant_id` String,
	`traveled_distance` String,
	`driver_mobile_country_code` String,
	`driver_image` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'booking_id'),'') as booking_id,
	ifNull(JSONExtractString(message,'short_id'),'') as short_id,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'driver_name'),'') as driver_name,
	ifNull(JSONExtractString(message,'driver_rating'),'') as driver_rating,
	ifNull(JSONExtractString(message,'driver_mobile_number'),'') as driver_mobile_number,
	toDateTime(JSONExtractInt(message,'driver_registered_at')) as driver_registered_at,
	ifNull(JSONExtractString(message,'vehicle_number'),'') as vehicle_number,
	ifNull(JSONExtractString(message,'vehicle_model'),'') as vehicle_model,
	ifNull(JSONExtractString(message,'vehicle_color'),'') as vehicle_color,
	ifNull(JSONExtractString(message,'otp'),'') as otp,
	ifNull(JSONExtractString(message,'tracking_url'),'') as tracking_url,
	ifNull(JSONExtractString(message,'fare'),'') as fare,
	ifNull(JSONExtractString(message,'total_fare'),'') as total_fare,
	ifNull(JSONExtractString(message,'chargeable_distance'),'') as chargeable_distance,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'bpp_ride_id'),'') as bpp_ride_id,
	toDateTime(JSONExtractInt(message,'ride_start_time')) as ride_start_time,
	toDateTime(JSONExtractInt(message,'ride_end_time')) as ride_end_time,
	ifNull(JSONExtractString(message,'ride_rating'),'') as ride_rating,
	toDateTime(JSONExtractInt(message,'driver_arrival_time')) as driver_arrival_time,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'traveled_distance'),'') as traveled_distance,
	ifNull(JSONExtractString(message,'driver_mobile_country_code'),'') as driver_mobile_country_code,
	ifNull(JSONExtractString(message,'driver_image'),'') as driver_image,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'RideObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.ride_booking_bak_1022_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `request_id` Nullable (String),
    `quote_id` Nullable (String),
    `status` Nullable (String),
    `provider_id` Nullable (String),
    `provider_mobile_number` Nullable (String),
    `start_time` DateTime DEFAULT now(),
    `rider_id` Nullable (String),
    `from_location_id` Nullable (String),
    `to_location_id` Nullable (String),
    `estimated_fare` Nullable (Float64),
    `discount` Nullable (Float64),
    `estimated_total_fare` Nullable (String),
    `distance` Nullable (Float64),
    `vehicle_variant` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `bpp_ride_booking_id` Nullable (String),
    `provider_name` Nullable (String),
    `provider_url` Nullable (String),
    `reallocations_count` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.ride_booking_bak_1022 ON CLUSTER `{cluster}` AS atlas_app_helper.ride_booking_bak_1022_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, ride_booking_bak_1022_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.ride_booking_bak_1022 ON CLUSTER `{cluster}` TO atlas_app.ride_booking_bak_1022_mv
(
	`id` String,
	`request_id` String,
	`quote_id` String,
	`status` String,
	`provider_id` String,
	`provider_mobile_number` String,
	`start_time` DateTime,
	`rider_id` String,
	`from_location_id` String,
	`to_location_id` String,
	`estimated_fare` Float64,
	`discount` Float64,
	`estimated_total_fare` String,
	`distance` Float64,
	`vehicle_variant` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`bpp_ride_booking_id` String,
	`provider_name` String,
	`provider_url` String,
	`reallocations_count` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'quote_id'),'') as quote_id,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	ifNull(JSONExtractString(message,'provider_mobile_number'),'') as provider_mobile_number,
	toDateTime(JSONExtractInt(message,'start_time')) as start_time,
	ifNull(JSONExtractString(message,'rider_id'),'') as rider_id,
	ifNull(JSONExtractString(message,'from_location_id'),'') as from_location_id,
	ifNull(JSONExtractString(message,'to_location_id'),'') as to_location_id,
	ifNull(JSONExtractFloat(message,'estimated_fare'),0.0) as estimated_fare,
	ifNull(JSONExtractFloat(message,'discount'),0.0) as discount,
	ifNull(JSONExtractString(message,'estimated_total_fare'),'') as estimated_total_fare,
	ifNull(JSONExtractFloat(message,'distance'),0.0) as distance,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'bpp_ride_booking_id'),'') as bpp_ride_booking_id,
	ifNull(JSONExtractString(message,'provider_name'),'') as provider_name,
	ifNull(JSONExtractString(message,'provider_url'),'') as provider_url,
	ifNull(JSONExtractInt(message,'reallocations_count'), 0) as reallocations_count,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'RideBookingBak1022Object'


CREATE TABLE atlas_app_helper.ride_booking_bak_1026_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `request_id` Nullable (String),
    `quote_id` Nullable (String),
    `status` Nullable (String),
    `provider_id` Nullable (String),
    `provider_mobile_number` Nullable (String),
    `start_time` DateTime DEFAULT now(),
    `rider_id` Nullable (String),
    `from_location_id` Nullable (String),
    `to_location_id` Nullable (String),
    `estimated_fare` Nullable (Float64),
    `discount` Nullable (Float64),
    `estimated_total_fare` Nullable (String),
    `distance` Nullable (Float64),
    `vehicle_variant` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `bpp_ride_booking_id` Nullable (String),
    `provider_name` Nullable (String),
    `provider_url` Nullable (String),
    `reallocations_count` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.ride_booking_bak_1026 ON CLUSTER `{cluster}` AS atlas_app_helper.ride_booking_bak_1026_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, ride_booking_bak_1026_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.ride_booking_bak_1026 ON CLUSTER `{cluster}` TO atlas_app.ride_booking_bak_1026_mv
(
	`id` String,
	`request_id` String,
	`quote_id` String,
	`status` String,
	`provider_id` String,
	`provider_mobile_number` String,
	`start_time` DateTime,
	`rider_id` String,
	`from_location_id` String,
	`to_location_id` String,
	`estimated_fare` Float64,
	`discount` Float64,
	`estimated_total_fare` String,
	`distance` Float64,
	`vehicle_variant` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`bpp_ride_booking_id` String,
	`provider_name` String,
	`provider_url` String,
	`reallocations_count` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'quote_id'),'') as quote_id,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	ifNull(JSONExtractString(message,'provider_mobile_number'),'') as provider_mobile_number,
	toDateTime(JSONExtractInt(message,'start_time')) as start_time,
	ifNull(JSONExtractString(message,'rider_id'),'') as rider_id,
	ifNull(JSONExtractString(message,'from_location_id'),'') as from_location_id,
	ifNull(JSONExtractString(message,'to_location_id'),'') as to_location_id,
	ifNull(JSONExtractFloat(message,'estimated_fare'),0.0) as estimated_fare,
	ifNull(JSONExtractFloat(message,'discount'),0.0) as discount,
	ifNull(JSONExtractString(message,'estimated_total_fare'),'') as estimated_total_fare,
	ifNull(JSONExtractFloat(message,'distance'),0.0) as distance,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'bpp_ride_booking_id'),'') as bpp_ride_booking_id,
	ifNull(JSONExtractString(message,'provider_name'),'') as provider_name,
	ifNull(JSONExtractString(message,'provider_url'),'') as provider_url,
	ifNull(JSONExtractInt(message,'reallocations_count'), 0) as reallocations_count,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'RideBookingBak1026Object'


CREATE TABLE atlas_app_helper.saved_location_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `city` Nullable (String),
    `state` Nullable (String),
    `country` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `street` Nullable (String),
    `door` Nullable (String),
    `building` Nullable (String),
    `area_code` Nullable (String),
    `area` Nullable (String),
    `tag` Nullable (String),
    `rider_id` Nullable (String),
    `place_id` Nullable (String),
    `ward` Nullable (String),
    `is_moved` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.saved_location ON CLUSTER `{cluster}` AS atlas_app_helper.saved_location_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, saved_location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.saved_location ON CLUSTER `{cluster}` TO atlas_app.saved_location_mv
(
	`id` String,
	`lat` Float64,
	`lon` Float64,
	`city` String,
	`state` String,
	`country` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`street` String,
	`door` String,
	`building` String,
	`area_code` String,
	`area` String,
	`tag` String,
	`rider_id` String,
	`place_id` String,
	`ward` String,
	`is_moved` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'city'),'') as city,
	ifNull(JSONExtractString(message,'state'),'') as state,
	ifNull(JSONExtractString(message,'country'),'') as country,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'street'),'') as street,
	ifNull(JSONExtractString(message,'door'),'') as door,
	ifNull(JSONExtractString(message,'building'),'') as building,
	ifNull(JSONExtractString(message,'area_code'),'') as area_code,
	ifNull(JSONExtractString(message,'area'),'') as area,
	ifNull(JSONExtractString(message,'tag'),'') as tag,
	ifNull(JSONExtractString(message,'rider_id'),'') as rider_id,
	ifNull(JSONExtractString(message,'place_id'),'') as place_id,
	ifNull(JSONExtractString(message,'ward'),'') as ward,
	ifNull(JSONExtractString(message,'is_moved'),'') as is_moved,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'SavedLocationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.schema_migrations_shard ON CLUSTER `{cluster}`
    (
    `filename` Nullable (String),
    `checksum` Nullable (String),
    `executed_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.schema_migrations ON CLUSTER `{cluster}` AS atlas_app_helper.schema_migrations_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, schema_migrations_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.schema_migrations ON CLUSTER `{cluster}` TO atlas_app.schema_migrations_mv
(
	`filename` String,
	`checksum` String,
	`executed_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'filename'),'') as filename,
	ifNull(JSONExtractString(message,'checksum'),'') as checksum,
	toDateTime(JSONExtractInt(message,'executed_at')) as executed_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'SchemaMigrationsObject'


CREATE TABLE atlas_app_helper.search_request_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `start_time` DateTime DEFAULT now(),
    `valid_till` DateTime DEFAULT now(),
    `rider_id` Nullable (String),
    `from_location_id` Nullable (String),
    `to_location_id` Nullable (String),
    `distance` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `merchant_id` Nullable (String),
    `bundle_version` Nullable (String),
    `client_version` Nullable (String),
    `language` Nullable (String),
    `max_distance` Nullable (Float64),
    `device` Nullable (String),
    `estimated_ride_duration` Nullable (String),
    `customer_extra_fee` Nullable (String),
    `auto_assign_enabled` Nullable (String),
    `auto_assign_enabled_v2` Nullable (String),
    `available_payment_methods` Nullable (String),
    `selected_payment_method_id` Nullable (String),
    `disability_tag` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.search_request ON CLUSTER `{cluster}` AS atlas_app_helper.search_request_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, search_request_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.search_request ON CLUSTER `{cluster}` TO atlas_app.search_request_mv
(
	`id` String,
	`start_time` DateTime,
	`valid_till` DateTime,
	`rider_id` String,
	`from_location_id` String,
	`to_location_id` String,
	`distance` String,
	`created_at` DateTime,
	`merchant_id` String,
	`bundle_version` String,
	`client_version` String,
	`language` String,
	`max_distance` Float64,
	`device` String,
	`estimated_ride_duration` String,
	`customer_extra_fee` String,
	`auto_assign_enabled` String,
	`auto_assign_enabled_v2` String,
	`available_payment_methods` String,
	`selected_payment_method_id` String,
	`disability_tag` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	toDateTime(JSONExtractInt(message,'start_time')) as start_time,
	toDateTime(JSONExtractInt(message,'valid_till')) as valid_till,
	ifNull(JSONExtractString(message,'rider_id'),'') as rider_id,
	ifNull(JSONExtractString(message,'from_location_id'),'') as from_location_id,
	ifNull(JSONExtractString(message,'to_location_id'),'') as to_location_id,
	ifNull(JSONExtractString(message,'distance'),'') as distance,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'bundle_version'),'') as bundle_version,
	ifNull(JSONExtractString(message,'client_version'),'') as client_version,
	ifNull(JSONExtractString(message,'language'),'') as language,
	ifNull(JSONExtractFloat(message,'max_distance'),0.0) as max_distance,
	ifNull(JSONExtractString(message,'device'),'') as device,
	ifNull(JSONExtractString(message,'estimated_ride_duration'),'') as estimated_ride_duration,
	ifNull(JSONExtractString(message,'customer_extra_fee'),'') as customer_extra_fee,
	ifNull(JSONExtractString(message,'auto_assign_enabled'),'') as auto_assign_enabled,
	ifNull(JSONExtractString(message,'auto_assign_enabled_v2'),'') as auto_assign_enabled_v2,
	ifNull(JSONExtractString(message,'available_payment_methods'),'') as available_payment_methods,
	ifNull(JSONExtractString(message,'selected_payment_method_id'),'') as selected_payment_method_id,
	ifNull(JSONExtractString(message,'disability_tag'),'') as disability_tag,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'SearchRequestObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.search_request_bak_1022_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `start_time` DateTime DEFAULT now(),
    `valid_till` DateTime DEFAULT now(),
    `rider_id` Nullable (String),
    `from_location_id` Nullable (String),
    `to_location_id` Nullable (String),
    `distance` Nullable (Float64),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.search_request_bak_1022 ON CLUSTER `{cluster}` AS atlas_app_helper.search_request_bak_1022_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, search_request_bak_1022_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.search_request_bak_1022 ON CLUSTER `{cluster}` TO atlas_app.search_request_bak_1022_mv
(
	`id` String,
	`start_time` DateTime,
	`valid_till` DateTime,
	`rider_id` String,
	`from_location_id` String,
	`to_location_id` String,
	`distance` Float64,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	toDateTime(JSONExtractInt(message,'start_time')) as start_time,
	toDateTime(JSONExtractInt(message,'valid_till')) as valid_till,
	ifNull(JSONExtractString(message,'rider_id'),'') as rider_id,
	ifNull(JSONExtractString(message,'from_location_id'),'') as from_location_id,
	ifNull(JSONExtractString(message,'to_location_id'),'') as to_location_id,
	ifNull(JSONExtractFloat(message,'distance'),0.0) as distance,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'SearchRequestBak1022Object'


CREATE TABLE atlas_app_helper.search_request_location_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `city` Nullable (String),
    `state` Nullable (String),
    `country` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `street` Nullable (String),
    `door` Nullable (String),
    `building` Nullable (String),
    `area_code` Nullable (String),
    `area` Nullable (String),
    `ward` Nullable (String),
    `place_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.search_request_location ON CLUSTER `{cluster}` AS atlas_app_helper.search_request_location_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, search_request_location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.search_request_location ON CLUSTER `{cluster}` TO atlas_app.search_request_location_mv
(
	`id` String,
	`lat` Float64,
	`lon` Float64,
	`city` String,
	`state` String,
	`country` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`street` String,
	`door` String,
	`building` String,
	`area_code` String,
	`area` String,
	`ward` String,
	`place_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'city'),'') as city,
	ifNull(JSONExtractString(message,'state'),'') as state,
	ifNull(JSONExtractString(message,'country'),'') as country,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'street'),'') as street,
	ifNull(JSONExtractString(message,'door'),'') as door,
	ifNull(JSONExtractString(message,'building'),'') as building,
	ifNull(JSONExtractString(message,'area_code'),'') as area_code,
	ifNull(JSONExtractString(message,'area'),'') as area,
	ifNull(JSONExtractString(message,'ward'),'') as ward,
	ifNull(JSONExtractString(message,'place_id'),'') as place_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'SearchRequestLocationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.search_request_location_1026_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `city` Nullable (String),
    `state` Nullable (String),
    `country` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `street` Nullable (String),
    `door` Nullable (String),
    `building` Nullable (String),
    `area_code` Nullable (String),
    `area` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.search_request_location_1026 ON CLUSTER `{cluster}` AS atlas_app_helper.search_request_location_1026_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, search_request_location_1026_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.search_request_location_1026 ON CLUSTER `{cluster}` TO atlas_app.search_request_location_1026_mv
(
	`id` String,
	`lat` Float64,
	`lon` Float64,
	`city` String,
	`state` String,
	`country` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`street` String,
	`door` String,
	`building` String,
	`area_code` String,
	`area` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'city'),'') as city,
	ifNull(JSONExtractString(message,'state'),'') as state,
	ifNull(JSONExtractString(message,'country'),'') as country,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'street'),'') as street,
	ifNull(JSONExtractString(message,'door'),'') as door,
	ifNull(JSONExtractString(message,'building'),'') as building,
	ifNull(JSONExtractString(message,'area_code'),'') as area_code,
	ifNull(JSONExtractString(message,'area'),'') as area,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'SearchRequestLocation1026Object'


CREATE TABLE atlas_app_helper.sos_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `flow` Nullable (String),
    `status` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `person_id` Nullable (String),
    `ride_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.sos ON CLUSTER `{cluster}` AS atlas_app_helper.sos_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, sos_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.sos ON CLUSTER `{cluster}` TO atlas_app.sos_mv
(
	`id` String,
	`flow` String,
	`status` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`person_id` String,
	`ride_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'flow'),'') as flow,
	ifNull(JSONExtractString(message,'status'),'') as status,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'ride_id'),'') as ride_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'SosObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.special_location_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `location_name` Nullable (String),
    `category` Nullable (String),
    `gates` Nullable (Array(String)),
    `geom` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.special_location ON CLUSTER `{cluster}` AS atlas_app_helper.special_location_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, special_location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.special_location ON CLUSTER `{cluster}` TO atlas_app.special_location_mv
(
	`id` String,
	`location_name` String,
	`category` String,
	`gates` Array(String),
	`geom` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'location_name'),'') as location_name,
	ifNull(JSONExtractString(message,'category'),'') as category,
	toDateTime(JSONExtractInt(message,'gates')) as gates,
	ifNull(JSONExtractString(message,'geom'),'') as geom,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'SpecialLocationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.special_zone_quote_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `quote_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.special_zone_quote ON CLUSTER `{cluster}` AS atlas_app_helper.special_zone_quote_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, special_zone_quote_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.special_zone_quote ON CLUSTER `{cluster}` TO atlas_app.special_zone_quote_mv
(
	`id` String,
	`quote_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'quote_id'),'') as quote_id,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'SpecialZoneQuoteObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.tag_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `created_by` Nullable (String),
    `created_by_entity_type` Nullable (String),
    `tag_type` Nullable (String),
    `tag` Nullable (String),
    `info` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.tag ON CLUSTER `{cluster}` AS atlas_app_helper.tag_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, tag_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.tag ON CLUSTER `{cluster}` TO atlas_app.tag_mv
(
	`id` String,
	`created_by` String,
	`created_by_entity_type` String,
	`tag_type` String,
	`tag` String,
	`info` String,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'created_by'),'') as created_by,
	ifNull(JSONExtractString(message,'created_by_entity_type'),'') as created_by_entity_type,
	ifNull(JSONExtractString(message,'tag_type'),'') as tag_type,
	ifNull(JSONExtractString(message,'tag'),'') as tag,
	ifNull(JSONExtractString(message,'info'),'') as info,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'TagObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.tag_category_mapping_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `tag` String,
    `category` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (tag))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.tag_category_mapping ON CLUSTER `{cluster}` AS atlas_app_helper.tag_category_mapping_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, tag_category_mapping_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.tag_category_mapping ON CLUSTER `{cluster}` TO atlas_app.tag_category_mapping_mv
(
	`id` String,
	`tag` String,
	`category` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'tag'),'') as tag,
	ifNull(JSONExtractString(message,'category'),'') as category,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'TagCategoryMappingObject'
	JSONExtractString(message, 'tag') is not null


CREATE TABLE atlas_app_helper.ticket_booking_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `short_id` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `ticket_place_id` Nullable (String),
    `person_id` Nullable (String),
    `amount` Nullable (String),
    `visit_date` DateTime DEFAULT now(),
    `status` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.ticket_booking ON CLUSTER `{cluster}` AS atlas_app_helper.ticket_booking_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, ticket_booking_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.ticket_booking ON CLUSTER `{cluster}` TO atlas_app.ticket_booking_mv
(
	`id` String,
	`short_id` String,
	`merchant_operating_city_id` String,
	`ticket_place_id` String,
	`person_id` String,
	`amount` String,
	`visit_date` DateTime,
	`status` String,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'short_id'),'') as short_id,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,
	ifNull(JSONExtractString(message,'ticket_place_id'),'') as ticket_place_id,
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'amount'),'') as amount,
	toDateTime(JSONExtractInt(message,'visit_date')) as visit_date,
	ifNull(JSONExtractString(message,'status'),'') as status,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'TicketBookingObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.ticket_booking_service_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `short_id` Nullable (String),
    `ticket_booking_id` Nullable (String),
    `ticket_service_id` Nullable (String),
    `amount` Nullable (String),
    `status` Nullable (String),
    `verification_count` Nullable (Int64),
    `expiry_date` DateTime DEFAULT now(),
    `merchant_operating_city_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.ticket_booking_service ON CLUSTER `{cluster}` AS atlas_app_helper.ticket_booking_service_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, ticket_booking_service_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.ticket_booking_service ON CLUSTER `{cluster}` TO atlas_app.ticket_booking_service_mv
(
	`id` String,
	`short_id` String,
	`ticket_booking_id` String,
	`ticket_service_id` String,
	`amount` String,
	`status` String,
	`verification_count` Int64,
	`expiry_date` DateTime,
	`merchant_operating_city_id` String,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'short_id'),'') as short_id,
	ifNull(JSONExtractString(message,'ticket_booking_id'),'') as ticket_booking_id,
	ifNull(JSONExtractString(message,'ticket_service_id'),'') as ticket_service_id,
	ifNull(JSONExtractString(message,'amount'),'') as amount,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractInt(message,'verification_count'), 0) as verification_count,
	toDateTime(JSONExtractInt(message,'expiry_date')) as expiry_date,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'TicketBookingServiceObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.ticket_booking_service_price_breakup_shard ON CLUSTER `{cluster}`
    (
    `ticket_booking_service_id` Nullable (String),
    `attendee_type` Nullable (String),
    `number_of_units` Nullable (Int64),
    `price_per_unit` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.ticket_booking_service_price_breakup ON CLUSTER `{cluster}` AS atlas_app_helper.ticket_booking_service_price_breakup_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, ticket_booking_service_price_breakup_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.ticket_booking_service_price_breakup ON CLUSTER `{cluster}` TO atlas_app.ticket_booking_service_price_breakup_mv
(
	`ticket_booking_service_id` String,
	`attendee_type` String,
	`number_of_units` Int64,
	`price_per_unit` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'ticket_booking_service_id'),'') as ticket_booking_service_id,
	ifNull(JSONExtractString(message,'attendee_type'),'') as attendee_type,
	ifNull(JSONExtractInt(message,'number_of_units'), 0) as number_of_units,
	ifNull(JSONExtractString(message,'price_per_unit'),'') as price_per_unit,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'TicketBookingServicePriceBreakupObject'


CREATE TABLE atlas_app_helper.ticket_place_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_operating_city_id` Nullable (String),
    `name` Nullable (String),
    `description` Nullable (String),
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `gallery` Nullable (String),
    `open_timings` DateTime DEFAULT now(),
    `close_timings` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.ticket_place ON CLUSTER `{cluster}` AS atlas_app_helper.ticket_place_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, ticket_place_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.ticket_place ON CLUSTER `{cluster}` TO atlas_app.ticket_place_mv
(
	`id` String,
	`merchant_operating_city_id` String,
	`name` String,
	`description` String,
	`lat` Float64,
	`lon` Float64,
	`gallery` String,
	`open_timings` DateTime,
	`close_timings` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,
	ifNull(JSONExtractString(message,'name'),'') as name,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'gallery'),'') as gallery,
	toDateTime(JSONExtractInt(message,'open_timings')) as open_timings,
	toDateTime(JSONExtractInt(message,'close_timings')) as close_timings,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'TicketPlaceObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.ticket_service_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `places_id` Nullable (String),
    `service` Nullable (String),
    `max_verification` Nullable (Int64),
    `open_timings` DateTime DEFAULT now(),
    `close_timings` DateTime DEFAULT now(),
    `validity_timings` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.ticket_service ON CLUSTER `{cluster}` AS atlas_app_helper.ticket_service_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, ticket_service_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.ticket_service ON CLUSTER `{cluster}` TO atlas_app.ticket_service_mv
(
	`id` String,
	`places_id` String,
	`service` String,
	`max_verification` Int64,
	`open_timings` DateTime,
	`close_timings` DateTime,
	`validity_timings` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'places_id'),'') as places_id,
	ifNull(JSONExtractString(message,'service'),'') as service,
	ifNull(JSONExtractInt(message,'max_verification'), 0) as max_verification,
	toDateTime(JSONExtractInt(message,'open_timings')) as open_timings,
	toDateTime(JSONExtractInt(message,'close_timings')) as close_timings,
	toDateTime(JSONExtractInt(message,'validity_timings')) as validity_timings,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'TicketServiceObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.ticket_service_price_shard ON CLUSTER `{cluster}`
    (
    `ticket_service_id` String,
    `attendee_type` String,
    `price_per_unit` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (ticket_service_id, attendee_type))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.ticket_service_price ON CLUSTER `{cluster}` AS atlas_app_helper.ticket_service_price_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, ticket_service_price_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.ticket_service_price ON CLUSTER `{cluster}` TO atlas_app.ticket_service_price_mv
(
	`ticket_service_id` String,
	`attendee_type` String,
	`price_per_unit` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'ticket_service_id'),'') as ticket_service_id,
	ifNull(JSONExtractString(message,'attendee_type'),'') as attendee_type,
	ifNull(JSONExtractString(message,'price_per_unit'),'') as price_per_unit,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'TicketServicePriceObject'
	JSONExtractString(message, 'ticket_service_id') is not null
	JSONExtractString(message, ' attendee_type') is not null


CREATE TABLE atlas_app_helper.trip_terms_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `descriptions` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.trip_terms ON CLUSTER `{cluster}` AS atlas_app_helper.trip_terms_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, trip_terms_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.trip_terms ON CLUSTER `{cluster}` TO atlas_app.trip_terms_mv
(
	`id` String,
	`descriptions` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'descriptions'),'') as descriptions,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'TripTermsObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_app_helper.webengage_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `version` Nullable (String),
    `content_template_id` Nullable (String),
    `principal_entity_id` Nullable (String),
    `info_message_id` Nullable (String),
    `web_message_id` Nullable (String),
    `to_number` Nullable (String),
    `status` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_app.webengage ON CLUSTER `{cluster}` AS atlas_app_helper.webengage_shard
ENGINE = Distributed(`{cluster}`, atlas_app_helper, webengage_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_app.webengage ON CLUSTER `{cluster}` TO atlas_app.webengage_mv
(
	`id` String,
	`version` String,
	`content_template_id` String,
	`principal_entity_id` String,
	`info_message_id` String,
	`web_message_id` String,
	`to_number` String,
	`status` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'version'),'') as version,
	ifNull(JSONExtractString(message,'content_template_id'),'') as content_template_id,
	ifNull(JSONExtractString(message,'principal_entity_id'),'') as principal_entity_id,
	ifNull(JSONExtractString(message,'info_message_id'),'') as info_message_id,
	ifNull(JSONExtractString(message,'web_message_id'),'') as web_message_id,
	ifNull(JSONExtractString(message,'to_number'),'') as to_number,
	ifNull(JSONExtractString(message,'status'),'') as status,

	FROM atlas_app.bap_main_queue
	where JSONExtractString(message,'tag') = 'WebengageObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.aadhaar_otp_req_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_id` Nullable (String),
    `request_id` Nullable (String),
    `status_code` Nullable (String),
    `request_message` Nullable (String),
    `transaction_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_req ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.aadhaar_otp_req_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, aadhaar_otp_req_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.aadhaar_otp_req ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.aadhaar_otp_req_mv
(
	`id` String,
	`driver_id` String,
	`request_id` String,
	`status_code` String,
	`request_message` String,
	`transaction_id` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'status_code'),'') as status_code,
	ifNull(JSONExtractString(message,'request_message'),'') as request_message,
	ifNull(JSONExtractString(message,'transaction_id'),'') as transaction_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'AadhaarOtpReqObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.aadhaar_otp_verify_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_id` Nullable (String),
    `request_id` Nullable (String),
    `status_code` Nullable (String),
    `request_message` Nullable (String),
    `transaction_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_verify ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.aadhaar_otp_verify_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, aadhaar_otp_verify_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.aadhaar_otp_verify ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.aadhaar_otp_verify_mv
(
	`id` String,
	`driver_id` String,
	`request_id` String,
	`status_code` String,
	`request_message` String,
	`transaction_id` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'status_code'),'') as status_code,
	ifNull(JSONExtractString(message,'request_message'),'') as request_message,
	ifNull(JSONExtractString(message,'transaction_id'),'') as transaction_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'AadhaarOtpVerifyObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.aadhaar_verification_shard ON CLUSTER `{cluster}`
    (
    `driver_id` String,
    `driver_name` Nullable (String),
    `driver_gender` Nullable (String),
    `driver_dob` Nullable (String),
    `driver_image` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `aadhaar_number_hash` Nullable (String),
    `is_verified` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `driver_image_path` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (driver_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.aadhaar_verification ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.aadhaar_verification_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, aadhaar_verification_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.aadhaar_verification ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.aadhaar_verification_mv
(
	`driver_id` String,
	`driver_name` String,
	`driver_gender` String,
	`driver_dob` String,
	`driver_image` String,
	`created_at` DateTime,
	`aadhaar_number_hash` String,
	`is_verified` String,
	`updated_at` DateTime,
	`driver_image_path` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'driver_name'),'') as driver_name,
	ifNull(JSONExtractString(message,'driver_gender'),'') as driver_gender,
	ifNull(JSONExtractString(message,'driver_dob'),'') as driver_dob,
	ifNull(JSONExtractString(message,'driver_image'),'') as driver_image,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'aadhaar_number_hash'),'') as aadhaar_number_hash,
	ifNull(JSONExtractString(message,'is_verified'),'') as is_verified,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'driver_image_path'),'') as driver_image_path,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'AadhaarVerificationObject'
	JSONExtractString(message, 'driver_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.bap_metadata_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `name` Nullable (String),
    `logo_url` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.bap_metadata ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.bap_metadata_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, bap_metadata_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.bap_metadata ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.bap_metadata_mv
(
	`id` String,
	`name` String,
	`logo_url` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'name'),'') as name,
	ifNull(JSONExtractString(message,'logo_url'),'') as logo_url,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'BapMetadataObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.beckn_request_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `beckn_request` Nullable (String),
    `signature_header` Nullable (String),
    `time_stamp` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.beckn_request ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.beckn_request_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, beckn_request_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.beckn_request ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.beckn_request_mv
(
	`id` String,
	`beckn_request` String,
	`signature_header` String,
	`time_stamp` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'beckn_request'),'') as beckn_request,
	ifNull(JSONExtractString(message,'signature_header'),'') as signature_header,
	toDateTime(JSONExtractInt(message,'time_stamp')) as time_stamp,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'BecknRequestObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.booking_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `status` Nullable (String),
    `provider_id` Nullable (String),
    `bap_id` Nullable (String),
    `bap_uri` Nullable (String),
    `start_time` DateTime DEFAULT now(),
    `rider_id` Nullable (String),
    `from_location_id` Nullable (String),
    `to_location_id` Nullable (String),
    `vehicle_variant` Nullable (String),
    `estimated_distance` Nullable (Int64),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `quote_id` Nullable (String),
    `fare_parameters_id` Nullable (String),
    `estimated_fare` Nullable (Float64),
    `rider_name` Nullable (String),
    `estimated_duration` Nullable (Int64),
    `primary_exophone` Nullable (String),
    `booking_type` Nullable (String),
    `special_zone_otp_code` Nullable (String),
    `transaction_id` Nullable (String),
    `max_estimated_distance` Nullable (Float64),
    `area` Nullable (String),
    `special_location_tag` Nullable (String),
    `payment_method_id` Nullable (String),
    `bap_city` Nullable (String),
    `bap_country` Nullable (String),
    `payment_url` Nullable (String),
    `disability_tag` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.booking ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.booking_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, booking_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.booking ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.booking_mv
(
	`id` String,
	`status` String,
	`provider_id` String,
	`bap_id` String,
	`bap_uri` String,
	`start_time` DateTime,
	`rider_id` String,
	`from_location_id` String,
	`to_location_id` String,
	`vehicle_variant` String,
	`estimated_distance` Int64,
	`created_at` DateTime,
	`updated_at` DateTime,
	`quote_id` String,
	`fare_parameters_id` String,
	`estimated_fare` Float64,
	`rider_name` String,
	`estimated_duration` Int64,
	`primary_exophone` String,
	`booking_type` String,
	`special_zone_otp_code` String,
	`transaction_id` String,
	`max_estimated_distance` Float64,
	`area` String,
	`special_location_tag` String,
	`payment_method_id` String,
	`bap_city` String,
	`bap_country` String,
	`payment_url` String,
	`disability_tag` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	ifNull(JSONExtractString(message,'bap_id'),'') as bap_id,
	ifNull(JSONExtractString(message,'bap_uri'),'') as bap_uri,
	toDateTime(JSONExtractInt(message,'start_time')) as start_time,
	ifNull(JSONExtractString(message,'rider_id'),'') as rider_id,
	ifNull(JSONExtractString(message,'from_location_id'),'') as from_location_id,
	ifNull(JSONExtractString(message,'to_location_id'),'') as to_location_id,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	ifNull(JSONExtractInt(message,'estimated_distance'), 0) as estimated_distance,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'quote_id'),'') as quote_id,
	ifNull(JSONExtractString(message,'fare_parameters_id'),'') as fare_parameters_id,
	ifNull(JSONExtractFloat(message,'estimated_fare'),0.0) as estimated_fare,
	ifNull(JSONExtractString(message,'rider_name'),'') as rider_name,
	ifNull(JSONExtractInt(message,'estimated_duration'), 0) as estimated_duration,
	ifNull(JSONExtractString(message,'primary_exophone'),'') as primary_exophone,
	ifNull(JSONExtractString(message,'booking_type'),'') as booking_type,
	ifNull(JSONExtractString(message,'special_zone_otp_code'),'') as special_zone_otp_code,
	ifNull(JSONExtractString(message,'transaction_id'),'') as transaction_id,
	ifNull(JSONExtractFloat(message,'max_estimated_distance'),0.0) as max_estimated_distance,
	ifNull(JSONExtractString(message,'area'),'') as area,
	ifNull(JSONExtractString(message,'special_location_tag'),'') as special_location_tag,
	ifNull(JSONExtractString(message,'payment_method_id'),'') as payment_method_id,
	ifNull(JSONExtractString(message,'bap_city'),'') as bap_city,
	ifNull(JSONExtractString(message,'bap_country'),'') as bap_country,
	ifNull(JSONExtractString(message,'payment_url'),'') as payment_url,
	ifNull(JSONExtractString(message,'disability_tag'),'') as disability_tag,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'BookingObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.booking_cancellation_reason_shard ON CLUSTER `{cluster}`
    (
    `driver_id` Nullable (String),
    `booking_id` String,
    `ride_id` Nullable (String),
    `source` Nullable (String),
    `reason_code` Nullable (String),
    `additional_info` Nullable (String),
    `driver_cancellation_location_lat` Nullable (Float64),
    `driver_cancellation_location_lon` Nullable (Float64),
    `driver_dist_to_pickup` Nullable (String),
    `merchant_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (booking_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.booking_cancellation_reason ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.booking_cancellation_reason_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, booking_cancellation_reason_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.booking_cancellation_reason ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.booking_cancellation_reason_mv
(
	`driver_id` String,
	`booking_id` String,
	`ride_id` String,
	`source` String,
	`reason_code` String,
	`additional_info` String,
	`driver_cancellation_location_lat` Float64,
	`driver_cancellation_location_lon` Float64,
	`driver_dist_to_pickup` String,
	`merchant_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'booking_id'),'') as booking_id,
	ifNull(JSONExtractString(message,'ride_id'),'') as ride_id,
	ifNull(JSONExtractString(message,'source'),'') as source,
	ifNull(JSONExtractString(message,'reason_code'),'') as reason_code,
	ifNull(JSONExtractString(message,'additional_info'),'') as additional_info,
	ifNull(JSONExtractFloat(message,'driver_cancellation_location_lat'),0.0) as driver_cancellation_location_lat,
	ifNull(JSONExtractFloat(message,'driver_cancellation_location_lon'),0.0) as driver_cancellation_location_lon,
	ifNull(JSONExtractString(message,'driver_dist_to_pickup'),'') as driver_dist_to_pickup,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'BookingCancellationReasonObject'
	JSONExtractString(message, 'booking_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.booking_location_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `city` Nullable (String),
    `state` Nullable (String),
    `country` Nullable (String),
    `street` Nullable (String),
    `building` Nullable (String),
    `area_code` Nullable (String),
    `area` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `door` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.booking_location ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.booking_location_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, booking_location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.booking_location ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.booking_location_mv
(
	`id` String,
	`lat` Float64,
	`lon` Float64,
	`city` String,
	`state` String,
	`country` String,
	`street` String,
	`building` String,
	`area_code` String,
	`area` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`door` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'city'),'') as city,
	ifNull(JSONExtractString(message,'state'),'') as state,
	ifNull(JSONExtractString(message,'country'),'') as country,
	ifNull(JSONExtractString(message,'street'),'') as street,
	ifNull(JSONExtractString(message,'building'),'') as building,
	ifNull(JSONExtractString(message,'area_code'),'') as area_code,
	ifNull(JSONExtractString(message,'area'),'') as area,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'door'),'') as door,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'BookingLocationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.business_event_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_id` Nullable (String),
    `event_type` Nullable (String),
    `time_stamp` DateTime DEFAULT now(),
    `booking_id` Nullable (String),
    `when_pool_was_computed` Nullable (String),
    `vehicle_variant` Nullable (String),
    `distance` Nullable (Float64),
    `duration` Nullable (Float64),
    `ride_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.business_event ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.business_event_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, business_event_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.business_event ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.business_event_mv
(
	`id` String,
	`driver_id` String,
	`event_type` String,
	`time_stamp` DateTime,
	`booking_id` String,
	`when_pool_was_computed` String,
	`vehicle_variant` String,
	`distance` Float64,
	`duration` Float64,
	`ride_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'event_type'),'') as event_type,
	toDateTime(JSONExtractInt(message,'time_stamp')) as time_stamp,
	ifNull(JSONExtractString(message,'booking_id'),'') as booking_id,
	ifNull(JSONExtractString(message,'when_pool_was_computed'),'') as when_pool_was_computed,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	ifNull(JSONExtractFloat(message,'distance'),0.0) as distance,
	ifNull(JSONExtractFloat(message,'duration'),0.0) as duration,
	ifNull(JSONExtractString(message,'ride_id'),'') as ride_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'BusinessEventObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.call_status_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `call_id` Nullable (String),
    `recording_url` Nullable (String),
    `status` Nullable (String),
    `conversation_duration` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `dtmf_number_used` Nullable (String),
    `entity_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.call_status ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.call_status_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, call_status_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.call_status ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.call_status_mv
(
	`id` String,
	`call_id` String,
	`recording_url` String,
	`status` String,
	`conversation_duration` String,
	`created_at` DateTime,
	`dtmf_number_used` String,
	`entity_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'call_id'),'') as call_id,
	ifNull(JSONExtractString(message,'recording_url'),'') as recording_url,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'conversation_duration'),'') as conversation_duration,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'dtmf_number_used'),'') as dtmf_number_used,
	ifNull(JSONExtractString(message,'entity_id'),'') as entity_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'CallStatusObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.cancellation_reason_shard ON CLUSTER `{cluster}`
    (
    `reason_code` String,
    `description` Nullable (String),
    `enabled` Nullable (String),
    `priority` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (reason_code))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.cancellation_reason ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.cancellation_reason_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, cancellation_reason_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.cancellation_reason ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.cancellation_reason_mv
(
	`reason_code` String,
	`description` String,
	`enabled` String,
	`priority` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'reason_code'),'') as reason_code,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractString(message,'enabled'),'') as enabled,
	ifNull(JSONExtractInt(message,'priority'), 0) as priority,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'CancellationReasonObject'
	JSONExtractString(message, 'reason_code') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.comment_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `issue_report_id` Nullable (String),
    `comment` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `author_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.comment ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.comment_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, comment_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.comment ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.comment_mv
(
	`id` String,
	`issue_report_id` String,
	`comment` String,
	`created_at` DateTime,
	`author_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'issue_report_id'),'') as issue_report_id,
	ifNull(JSONExtractString(message,'comment'),'') as comment,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'author_id'),'') as author_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'CommentObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_availability_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_id` Nullable (String),
    `merchant_id` Nullable (String),
    `total_available_time` Nullable (Int64),
    `last_available_time` DateTime DEFAULT now(),
    `bucket_start_time` DateTime DEFAULT now(),
    `bucket_end_time` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_availability ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_availability_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_availability_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_availability ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_availability_mv
(
	`id` String,
	`driver_id` String,
	`merchant_id` String,
	`total_available_time` Int64,
	`last_available_time` DateTime,
	`bucket_start_time` DateTime,
	`bucket_end_time` DateTime,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractInt(message,'total_available_time'), 0) as total_available_time,
	toDateTime(JSONExtractInt(message,'last_available_time')) as last_available_time,
	toDateTime(JSONExtractInt(message,'bucket_start_time')) as bucket_start_time,
	toDateTime(JSONExtractInt(message,'bucket_end_time')) as bucket_end_time,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverAvailabilityObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_block_reason_shard ON CLUSTER `{cluster}`
    (
    `reason_code` String,
    `block_reason` Nullable (String),
    `block_time_in_hours` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (reason_code))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_block_reason ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_block_reason_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_block_reason_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_block_reason ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_block_reason_mv
(
	`reason_code` String,
	`block_reason` String,
	`block_time_in_hours` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'reason_code'),'') as reason_code,
	ifNull(JSONExtractString(message,'block_reason'),'') as block_reason,
	ifNull(JSONExtractInt(message,'block_time_in_hours'), 0) as block_time_in_hours,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverBlockReasonObject'
	JSONExtractString(message, 'reason_code') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_fee_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_id` Nullable (String),
    `total_earnings` Nullable (Int64),
    `num_rides` Nullable (Int64),
    `govt_charges` Nullable (Int64),
    `platform_fee` Nullable (String),
    `cgst` Nullable (String),
    `sgst` Nullable (String),
    `pay_by` DateTime DEFAULT now(),
    `start_time` DateTime DEFAULT now(),
    `end_time` DateTime DEFAULT now(),
    `status` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `collected_by` Nullable (String),
    `fee_type` Nullable (String),
    `merchant_id` Nullable (String),
    `offer_id` Nullable (String),
    `plan_offer_title` Nullable (String),
    `stage_updated_at` DateTime DEFAULT now(),
    `bill_number` Nullable (String),
    `autopay_payment_stage` Nullable (String),
    `fee_without_discount` Nullable (String),
    `collected_at` DateTime DEFAULT now(),
    `scheduler_try_count` Nullable (Int64),
    `overlay_sent` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_fee ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_fee_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_fee_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_fee ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_fee_mv
(
	`id` String,
	`driver_id` String,
	`total_earnings` Int64,
	`num_rides` Int64,
	`govt_charges` Int64,
	`platform_fee` String,
	`cgst` String,
	`sgst` String,
	`pay_by` DateTime,
	`start_time` DateTime,
	`end_time` DateTime,
	`status` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`collected_by` String,
	`fee_type` String,
	`merchant_id` String,
	`offer_id` String,
	`plan_offer_title` String,
	`stage_updated_at` DateTime,
	`bill_number` String,
	`autopay_payment_stage` String,
	`fee_without_discount` String,
	`collected_at` DateTime,
	`scheduler_try_count` Int64,
	`overlay_sent` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractInt(message,'total_earnings'), 0) as total_earnings,
	ifNull(JSONExtractInt(message,'num_rides'), 0) as num_rides,
	ifNull(JSONExtractInt(message,'govt_charges'), 0) as govt_charges,
	ifNull(JSONExtractString(message,'platform_fee'),'') as platform_fee,
	ifNull(JSONExtractString(message,'cgst'),'') as cgst,
	ifNull(JSONExtractString(message,'sgst'),'') as sgst,
	toDateTime(JSONExtractInt(message,'pay_by')) as pay_by,
	toDateTime(JSONExtractInt(message,'start_time')) as start_time,
	toDateTime(JSONExtractInt(message,'end_time')) as end_time,
	ifNull(JSONExtractString(message,'status'),'') as status,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'collected_by'),'') as collected_by,
	ifNull(JSONExtractString(message,'fee_type'),'') as fee_type,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'offer_id'),'') as offer_id,
	ifNull(JSONExtractString(message,'plan_offer_title'),'') as plan_offer_title,
	toDateTime(JSONExtractInt(message,'stage_updated_at')) as stage_updated_at,
	ifNull(JSONExtractString(message,'bill_number'),'') as bill_number,
	ifNull(JSONExtractString(message,'autopay_payment_stage'),'') as autopay_payment_stage,
	ifNull(JSONExtractString(message,'fee_without_discount'),'') as fee_without_discount,
	toDateTime(JSONExtractInt(message,'collected_at')) as collected_at,
	ifNull(JSONExtractInt(message,'scheduler_try_count'), 0) as scheduler_try_count,
	ifNull(JSONExtractString(message,'overlay_sent'),'') as overlay_sent,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverFeeObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_flow_status_shard ON CLUSTER `{cluster}`
    (
    `person_id` String,
    `flow_status` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (person_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_flow_status ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_flow_status_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_flow_status_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_flow_status ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_flow_status_mv
(
	`person_id` String,
	`flow_status` String,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'flow_status'),'') as flow_status,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverFlowStatusObject'
	JSONExtractString(message, 'person_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_go_home_request_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_id` Nullable (String),
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `point` Nullable (String),
    `status` Nullable (String),
    `num_cancellation` Nullable (Int64),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `reached_home` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_go_home_request ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_go_home_request_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_go_home_request_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_go_home_request ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_go_home_request_mv
(
	`id` String,
	`driver_id` String,
	`lat` Float64,
	`lon` Float64,
	`point` String,
	`status` String,
	`num_cancellation` Int64,
	`created_at` DateTime,
	`updated_at` DateTime,
	`reached_home` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'point'),'') as point,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractInt(message,'num_cancellation'), 0) as num_cancellation,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'reached_home'),'') as reached_home,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverGoHomeRequestObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_home_location_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_id` Nullable (String),
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `home_address` Nullable (String),
    `tag` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_home_location ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_home_location_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_home_location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_home_location ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_home_location_mv
(
	`id` String,
	`driver_id` String,
	`lat` Float64,
	`lon` Float64,
	`home_address` String,
	`tag` String,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'home_address'),'') as home_address,
	ifNull(JSONExtractString(message,'tag'),'') as tag,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverHomeLocationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_information_shard ON CLUSTER `{cluster}`
    (
    `driver_id` String,
    `active` Nullable (String),
    `on_ride` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `enabled` Nullable (String),
    `verified` Nullable (String),
    `referral_code` Nullable (String),
    `admin_id` Nullable (String),
    `blocked` Nullable (String),
    `last_enabled_on` DateTime DEFAULT now(),
    `can_downgrade_to_hatchback` Nullable (String),
    `can_downgrade_to_sedan` Nullable (String),
    `can_downgrade_to_taxi` Nullable (String),
    `mode` Nullable (String),
    `merchant_id` Nullable (String),
    `num_of_locks` Nullable (Int64),
    `aadhaar_verified` Nullable (String),
    `subscribed` Nullable (String),
    `payment_pending` Nullable (String),
    `blocked_reason` Nullable (String),
    `block_expiry_time` DateTime DEFAULT now(),
    `auto_pay_status` Nullable (String),
    `comp_aadhaar_image_path` Nullable (String),
    `available_upi_apps` Nullable (String),
    `payer_vpa` Nullable (String),
    `enabled_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (driver_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_information ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_information_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_information_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_information ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_information_mv
(
	`driver_id` String,
	`active` String,
	`on_ride` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`enabled` String,
	`verified` String,
	`referral_code` String,
	`admin_id` String,
	`blocked` String,
	`last_enabled_on` DateTime,
	`can_downgrade_to_hatchback` String,
	`can_downgrade_to_sedan` String,
	`can_downgrade_to_taxi` String,
	`mode` String,
	`merchant_id` String,
	`num_of_locks` Int64,
	`aadhaar_verified` String,
	`subscribed` String,
	`payment_pending` String,
	`blocked_reason` String,
	`block_expiry_time` DateTime,
	`auto_pay_status` String,
	`comp_aadhaar_image_path` String,
	`available_upi_apps` String,
	`payer_vpa` String,
	`enabled_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'active'),'') as active,
	ifNull(JSONExtractString(message,'on_ride'),'') as on_ride,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'enabled'),'') as enabled,
	ifNull(JSONExtractString(message,'verified'),'') as verified,
	ifNull(JSONExtractString(message,'referral_code'),'') as referral_code,
	ifNull(JSONExtractString(message,'admin_id'),'') as admin_id,
	ifNull(JSONExtractString(message,'blocked'),'') as blocked,
	toDateTime(JSONExtractInt(message,'last_enabled_on')) as last_enabled_on,
	ifNull(JSONExtractString(message,'can_downgrade_to_hatchback'),'') as can_downgrade_to_hatchback,
	ifNull(JSONExtractString(message,'can_downgrade_to_sedan'),'') as can_downgrade_to_sedan,
	ifNull(JSONExtractString(message,'can_downgrade_to_taxi'),'') as can_downgrade_to_taxi,
	ifNull(JSONExtractString(message,'mode'),'') as mode,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractInt(message,'num_of_locks'), 0) as num_of_locks,
	ifNull(JSONExtractString(message,'aadhaar_verified'),'') as aadhaar_verified,
	ifNull(JSONExtractString(message,'subscribed'),'') as subscribed,
	ifNull(JSONExtractString(message,'payment_pending'),'') as payment_pending,
	ifNull(JSONExtractString(message,'blocked_reason'),'') as blocked_reason,
	toDateTime(JSONExtractInt(message,'block_expiry_time')) as block_expiry_time,
	ifNull(JSONExtractString(message,'auto_pay_status'),'') as auto_pay_status,
	ifNull(JSONExtractString(message,'comp_aadhaar_image_path'),'') as comp_aadhaar_image_path,
	ifNull(JSONExtractString(message,'available_upi_apps'),'') as available_upi_apps,
	ifNull(JSONExtractString(message,'payer_vpa'),'') as payer_vpa,
	toDateTime(JSONExtractInt(message,'enabled_at')) as enabled_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverInformationObject'
	JSONExtractString(message, 'driver_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_intelligent_pool_config_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` Nullable (String),
    `availability_time_weightage` Nullable (Int64),
    `acceptance_ratio_weightage` Nullable (Int64),
    `cancellation_ratio_weightage` Nullable (Int64),
    `availability_time_window_option` Nullable (String),
    `acceptance_ratio_window_option` Nullable (String),
    `cancellation_ratio_window_option` Nullable (String),
    `min_quotes_to_qualify_for_intelligent_pool` Nullable (Int64),
    `min_quotes_to_qualify_for_intelligent_pool_window_option` Nullable (String),
    `intelligent_pool_percentage` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `speed_normalizer` Nullable (Float64),
    `driver_speed_weightage` Nullable (Int64),
    `location_update_sample_time` Nullable (Int64),
    `min_location_updates` Nullable (Int64),
    `default_driver_speed` Nullable (Float64),
    `actual_pickup_distance_weightage` Nullable (Int64),
    `merchant_operating_city_id` String,
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (merchant_operating_city_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_intelligent_pool_config_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_intelligent_pool_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_intelligent_pool_config ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_intelligent_pool_config_mv
(
	`merchant_id` String,
	`availability_time_weightage` Int64,
	`acceptance_ratio_weightage` Int64,
	`cancellation_ratio_weightage` Int64,
	`availability_time_window_option` String,
	`acceptance_ratio_window_option` String,
	`cancellation_ratio_window_option` String,
	`min_quotes_to_qualify_for_intelligent_pool` Int64,
	`min_quotes_to_qualify_for_intelligent_pool_window_option` String,
	`intelligent_pool_percentage` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`speed_normalizer` Float64,
	`driver_speed_weightage` Int64,
	`location_update_sample_time` Int64,
	`min_location_updates` Int64,
	`default_driver_speed` Float64,
	`actual_pickup_distance_weightage` Int64,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractInt(message,'availability_time_weightage'), 0) as availability_time_weightage,
	ifNull(JSONExtractInt(message,'acceptance_ratio_weightage'), 0) as acceptance_ratio_weightage,
	ifNull(JSONExtractInt(message,'cancellation_ratio_weightage'), 0) as cancellation_ratio_weightage,
	ifNull(JSONExtractString(message,'availability_time_window_option'),'') as availability_time_window_option,
	ifNull(JSONExtractString(message,'acceptance_ratio_window_option'),'') as acceptance_ratio_window_option,
	ifNull(JSONExtractString(message,'cancellation_ratio_window_option'),'') as cancellation_ratio_window_option,
	ifNull(JSONExtractInt(message,'min_quotes_to_qualify_for_intelligent_pool'), 0) as min_quotes_to_qualify_for_intelligent_pool,
	ifNull(JSONExtractString(message,'min_quotes_to_qualify_for_intelligent_pool_window_option'),'') as min_quotes_to_qualify_for_intelligent_pool_window_option,
	ifNull(JSONExtractString(message,'intelligent_pool_percentage'),'') as intelligent_pool_percentage,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractFloat(message,'speed_normalizer'),0.0) as speed_normalizer,
	ifNull(JSONExtractInt(message,'driver_speed_weightage'), 0) as driver_speed_weightage,
	ifNull(JSONExtractInt(message,'location_update_sample_time'), 0) as location_update_sample_time,
	ifNull(JSONExtractInt(message,'min_location_updates'), 0) as min_location_updates,
	ifNull(JSONExtractFloat(message,'default_driver_speed'),0.0) as default_driver_speed,
	ifNull(JSONExtractInt(message,'actual_pickup_distance_weightage'), 0) as actual_pickup_distance_weightage,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverIntelligentPoolConfigObject'
	JSONExtractString(message, 'merchant_operating_city_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_license_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `driver_id` Nullable (String),
    `driver_dob` DateTime DEFAULT now(),
    `license_expiry` DateTime DEFAULT now(),
    `class_of_vehicles` Nullable (String),
    `verification_status` Nullable (String),
    `consent` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `consent_timestamp` DateTime DEFAULT now(),
    `failed_rules` Nullable (String),
    `driver_name` Nullable (String),
    `document_image_id1` Nullable (String),
    `document_image_id2` Nullable (String),
    `license_number_hash` Nullable (String),
    `license_number_encrypted` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_license ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_license_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_license_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_license ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_license_mv
(
	`id` String,
	`driver_id` String,
	`driver_dob` DateTime,
	`license_expiry` DateTime,
	`class_of_vehicles` String,
	`verification_status` String,
	`consent` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`consent_timestamp` DateTime,
	`failed_rules` String,
	`driver_name` String,
	`document_image_id1` String,
	`document_image_id2` String,
	`license_number_hash` String,
	`license_number_encrypted` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	toDateTime(JSONExtractInt(message,'driver_dob')) as driver_dob,
	toDateTime(JSONExtractInt(message,'license_expiry')) as license_expiry,
	ifNull(JSONExtractString(message,'class_of_vehicles'),'') as class_of_vehicles,
	ifNull(JSONExtractString(message,'verification_status'),'') as verification_status,
	ifNull(JSONExtractString(message,'consent'),'') as consent,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'consent_timestamp')) as consent_timestamp,
	ifNull(JSONExtractString(message,'failed_rules'),'') as failed_rules,
	ifNull(JSONExtractString(message,'driver_name'),'') as driver_name,
	ifNull(JSONExtractString(message,'document_image_id1'),'') as document_image_id1,
	ifNull(JSONExtractString(message,'document_image_id2'),'') as document_image_id2,
	ifNull(JSONExtractString(message,'license_number_hash'),'') as license_number_hash,
	ifNull(JSONExtractString(message,'license_number_encrypted'),'') as license_number_encrypted,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverLicenseObject'


CREATE TABLE atlas_driver_offer_bpp_helper.driver_location_shard ON CLUSTER `{cluster}`
    (
    `driver_id` String,
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `point` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `coordinates_calculated_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `merchant_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (driver_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_location ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_location_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_location ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_location_mv
(
	`driver_id` String,
	`lat` Float64,
	`lon` Float64,
	`point` String,
	`created_at` DateTime,
	`coordinates_calculated_at` DateTime,
	`updated_at` DateTime,
	`merchant_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'point'),'') as point,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'coordinates_calculated_at')) as coordinates_calculated_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverLocationObject'
	JSONExtractString(message, 'driver_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_plan_shard ON CLUSTER `{cluster}`
    (
    `driver_id` Nullable (String),
    `plan_id` Nullable (String),
    `plan_type` Nullable (String),
    `mandate_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `mandate_setup_date` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_plan ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_plan_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_plan_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_plan ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_plan_mv
(
	`driver_id` String,
	`plan_id` String,
	`plan_type` String,
	`mandate_id` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`mandate_setup_date` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'plan_id'),'') as plan_id,
	ifNull(JSONExtractString(message,'plan_type'),'') as plan_type,
	ifNull(JSONExtractString(message,'mandate_id'),'') as mandate_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'mandate_setup_date')) as mandate_setup_date,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverPlanObject'


CREATE TABLE atlas_driver_offer_bpp_helper.driver_pool_config_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` Nullable (String),
    `min_radius_of_search` Nullable (Int64),
    `max_radius_of_search` Nullable (Int64),
    `radius_step_size` Nullable (Int64),
    `driver_position_info_expiry` Nullable (String),
    `actual_distance_threshold` Nullable (String),
    `max_driver_quotes_required` Nullable (String),
    `driver_quote_limit` Nullable (String),
    `driver_request_count_limit` Nullable (String),
    `driver_batch_size` Nullable (Int64),
    `max_number_of_batches` Nullable (Int64),
    `max_parallel_search_requests` Nullable (Int64),
    `pool_sorting_type` Nullable (String),
    `single_batch_process_time` Nullable (String),
    `trip_distance` Nullable (Int64),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `radius_shrink_value_for_drivers_on_ride` Nullable (Int64),
    `driver_to_destination_distance_threshold` Nullable (Int64),
    `driver_to_destination_duration` Nullable (Int64),
    `distance_based_batch_split` Nullable (Array(String)),
    `vehicle_variant` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `id` String,
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_pool_config ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_pool_config_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_pool_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_pool_config ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_pool_config_mv
(
	`merchant_id` String,
	`min_radius_of_search` Int64,
	`max_radius_of_search` Int64,
	`radius_step_size` Int64,
	`driver_position_info_expiry` String,
	`actual_distance_threshold` String,
	`max_driver_quotes_required` String,
	`driver_quote_limit` String,
	`driver_request_count_limit` String,
	`driver_batch_size` Int64,
	`max_number_of_batches` Int64,
	`max_parallel_search_requests` Int64,
	`pool_sorting_type` String,
	`single_batch_process_time` String,
	`trip_distance` Int64,
	`created_at` DateTime,
	`updated_at` DateTime,
	`radius_shrink_value_for_drivers_on_ride` Int64,
	`driver_to_destination_distance_threshold` Int64,
	`driver_to_destination_duration` Int64,
	`distance_based_batch_split` Array(String),
	`vehicle_variant` String,
	`merchant_operating_city_id` String,
	`id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractInt(message,'min_radius_of_search'), 0) as min_radius_of_search,
	ifNull(JSONExtractInt(message,'max_radius_of_search'), 0) as max_radius_of_search,
	ifNull(JSONExtractInt(message,'radius_step_size'), 0) as radius_step_size,
	ifNull(JSONExtractString(message,'driver_position_info_expiry'),'') as driver_position_info_expiry,
	ifNull(JSONExtractString(message,'actual_distance_threshold'),'') as actual_distance_threshold,
	ifNull(JSONExtractString(message,'max_driver_quotes_required'),'') as max_driver_quotes_required,
	ifNull(JSONExtractString(message,'driver_quote_limit'),'') as driver_quote_limit,
	ifNull(JSONExtractString(message,'driver_request_count_limit'),'') as driver_request_count_limit,
	ifNull(JSONExtractInt(message,'driver_batch_size'), 0) as driver_batch_size,
	ifNull(JSONExtractInt(message,'max_number_of_batches'), 0) as max_number_of_batches,
	ifNull(JSONExtractInt(message,'max_parallel_search_requests'), 0) as max_parallel_search_requests,
	ifNull(JSONExtractString(message,'pool_sorting_type'),'') as pool_sorting_type,
	ifNull(JSONExtractString(message,'single_batch_process_time'),'') as single_batch_process_time,
	ifNull(JSONExtractInt(message,'trip_distance'), 0) as trip_distance,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractInt(message,'radius_shrink_value_for_drivers_on_ride'), 0) as radius_shrink_value_for_drivers_on_ride,
	ifNull(JSONExtractInt(message,'driver_to_destination_distance_threshold'), 0) as driver_to_destination_distance_threshold,
	ifNull(JSONExtractInt(message,'driver_to_destination_duration'), 0) as driver_to_destination_duration,
	toDateTime(JSONExtractInt(message,'distance_based_batch_split')) as distance_based_batch_split,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,
	ifNull(JSONExtractString(message,'id'),'') as id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverPoolConfigObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_quote_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `status` Nullable (String),
    `search_request_id` Nullable (String),
    `driver_id` Nullable (String),
    `distance_to_pickup` Nullable (Int64),
    `duration_to_pickup` Nullable (Int64),
    `vehicle_variant` Nullable (String),
    `valid_till` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `driver_name` Nullable (String),
    `driver_rating` Nullable (Float64),
    `distance` Nullable (Int64),
    `fare_parameters_id` Nullable (String),
    `estimated_fare` Nullable (Float64),
    `search_request_for_driver_id` Nullable (String),
    `provider_id` Nullable (String),
    `search_try_id` Nullable (String),
    `special_location_tag` Nullable (String),
    `estimate_id` Nullable (String),
    `go_home_request_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_quote ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_quote_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_quote_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_quote ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_quote_mv
(
	`id` String,
	`status` String,
	`search_request_id` String,
	`driver_id` String,
	`distance_to_pickup` Int64,
	`duration_to_pickup` Int64,
	`vehicle_variant` String,
	`valid_till` DateTime,
	`created_at` DateTime,
	`updated_at` DateTime,
	`driver_name` String,
	`driver_rating` Float64,
	`distance` Int64,
	`fare_parameters_id` String,
	`estimated_fare` Float64,
	`search_request_for_driver_id` String,
	`provider_id` String,
	`search_try_id` String,
	`special_location_tag` String,
	`estimate_id` String,
	`go_home_request_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'search_request_id'),'') as search_request_id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractInt(message,'distance_to_pickup'), 0) as distance_to_pickup,
	ifNull(JSONExtractInt(message,'duration_to_pickup'), 0) as duration_to_pickup,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	toDateTime(JSONExtractInt(message,'valid_till')) as valid_till,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'driver_name'),'') as driver_name,
	ifNull(JSONExtractFloat(message,'driver_rating'),0.0) as driver_rating,
	ifNull(JSONExtractInt(message,'distance'), 0) as distance,
	ifNull(JSONExtractString(message,'fare_parameters_id'),'') as fare_parameters_id,
	ifNull(JSONExtractFloat(message,'estimated_fare'),0.0) as estimated_fare,
	ifNull(JSONExtractString(message,'search_request_for_driver_id'),'') as search_request_for_driver_id,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	ifNull(JSONExtractString(message,'search_try_id'),'') as search_try_id,
	ifNull(JSONExtractString(message,'special_location_tag'),'') as special_location_tag,
	ifNull(JSONExtractString(message,'estimate_id'),'') as estimate_id,
	ifNull(JSONExtractString(message,'go_home_request_id'),'') as go_home_request_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverQuoteObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_rc_association_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `driver_id` Nullable (String),
    `rc_id` Nullable (String),
    `associated_on` DateTime DEFAULT now(),
    `associated_till` DateTime DEFAULT now(),
    `consent` Nullable (String),
    `consent_timestamp` DateTime DEFAULT now(),
    `is_rc_active` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_rc_association ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_rc_association_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_rc_association_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_rc_association ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_rc_association_mv
(
	`id` String,
	`driver_id` String,
	`rc_id` String,
	`associated_on` DateTime,
	`associated_till` DateTime,
	`consent` String,
	`consent_timestamp` DateTime,
	`is_rc_active` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'rc_id'),'') as rc_id,
	toDateTime(JSONExtractInt(message,'associated_on')) as associated_on,
	toDateTime(JSONExtractInt(message,'associated_till')) as associated_till,
	ifNull(JSONExtractString(message,'consent'),'') as consent,
	toDateTime(JSONExtractInt(message,'consent_timestamp')) as consent_timestamp,
	ifNull(JSONExtractString(message,'is_rc_active'),'') as is_rc_active,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverRcAssociationObject'


CREATE TABLE atlas_driver_offer_bpp_helper.driver_referral_shard ON CLUSTER `{cluster}`
    (
    `referral_code` String,
    `driver_id` Nullable (String),
    `linked_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (referral_code))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_referral ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_referral_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_referral_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_referral ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_referral_mv
(
	`referral_code` String,
	`driver_id` String,
	`linked_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'referral_code'),'') as referral_code,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	toDateTime(JSONExtractInt(message,'linked_at')) as linked_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverReferralObject'
	JSONExtractString(message, 'referral_code') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.driver_stats_shard ON CLUSTER `{cluster}`
    (
    `driver_id` String,
    `idle_since` DateTime DEFAULT now(),
    `total_rides` Nullable (Int64),
    `total_distance` Nullable (Float64),
    `rides_cancelled` Nullable (String),
    `total_rides_assigned` Nullable (String),
    `total_earnings` Nullable (Int64),
    `bonus_earned` Nullable (Int64),
    `late_night_trips` Nullable (Int64),
    `earnings_missed` Nullable (Int64),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (driver_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.driver_stats ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.driver_stats_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, driver_stats_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.driver_stats ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.driver_stats_mv
(
	`driver_id` String,
	`idle_since` DateTime,
	`total_rides` Int64,
	`total_distance` Float64,
	`rides_cancelled` String,
	`total_rides_assigned` String,
	`total_earnings` Int64,
	`bonus_earned` Int64,
	`late_night_trips` Int64,
	`earnings_missed` Int64,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	toDateTime(JSONExtractInt(message,'idle_since')) as idle_since,
	ifNull(JSONExtractInt(message,'total_rides'), 0) as total_rides,
	ifNull(JSONExtractFloat(message,'total_distance'),0.0) as total_distance,
	ifNull(JSONExtractString(message,'rides_cancelled'),'') as rides_cancelled,
	ifNull(JSONExtractString(message,'total_rides_assigned'),'') as total_rides_assigned,
	ifNull(JSONExtractInt(message,'total_earnings'), 0) as total_earnings,
	ifNull(JSONExtractInt(message,'bonus_earned'), 0) as bonus_earned,
	ifNull(JSONExtractInt(message,'late_night_trips'), 0) as late_night_trips,
	ifNull(JSONExtractInt(message,'earnings_missed'), 0) as earnings_missed,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'DriverStatsObject'
	JSONExtractString(message, 'driver_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.estimate_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `vehicle_variant` Nullable (String),
    `min_fare` Nullable (Int64),
    `max_fare` Nullable (Int64),
    `estimate_breakup_list` Nullable (Array(String)),
    `night_shift_multiplier` Nullable (String),
    `night_shift_start` Nullable (String),
    `night_shift_end` Nullable (String),
    `waiting_charge_per_min` Nullable (String),
    `waiting_or_pickup_charges` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `night_shift_charge` Nullable (String),
    `request_id` Nullable (String),
    `special_location_tag` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.estimate ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.estimate_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, estimate_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.estimate ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.estimate_mv
(
	`id` String,
	`vehicle_variant` String,
	`min_fare` Int64,
	`max_fare` Int64,
	`estimate_breakup_list` Array(String),
	`night_shift_multiplier` String,
	`night_shift_start` String,
	`night_shift_end` String,
	`waiting_charge_per_min` String,
	`waiting_or_pickup_charges` String,
	`created_at` DateTime,
	`night_shift_charge` String,
	`request_id` String,
	`special_location_tag` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	ifNull(JSONExtractInt(message,'min_fare'), 0) as min_fare,
	ifNull(JSONExtractInt(message,'max_fare'), 0) as max_fare,
	toDateTime(JSONExtractInt(message,'estimate_breakup_list')) as estimate_breakup_list,
	ifNull(JSONExtractString(message,'night_shift_multiplier'),'') as night_shift_multiplier,
	ifNull(JSONExtractString(message,'night_shift_start'),'') as night_shift_start,
	ifNull(JSONExtractString(message,'night_shift_end'),'') as night_shift_end,
	ifNull(JSONExtractString(message,'waiting_charge_per_min'),'') as waiting_charge_per_min,
	ifNull(JSONExtractString(message,'waiting_or_pickup_charges'),'') as waiting_or_pickup_charges,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'night_shift_charge'),'') as night_shift_charge,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'special_location_tag'),'') as special_location_tag,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'EstimateObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.exophone_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `primary_phone` Nullable (String),
    `backup_phone` Nullable (String),
    `is_primary_down` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `exophone_type` Nullable (String),
    `call_service` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.exophone ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.exophone_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, exophone_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.exophone ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.exophone_mv
(
	`id` String,
	`merchant_id` String,
	`primary_phone` String,
	`backup_phone` String,
	`is_primary_down` String,
	`updated_at` DateTime,
	`created_at` DateTime,
	`exophone_type` String,
	`call_service` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'primary_phone'),'') as primary_phone,
	ifNull(JSONExtractString(message,'backup_phone'),'') as backup_phone,
	ifNull(JSONExtractString(message,'is_primary_down'),'') as is_primary_down,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'exophone_type'),'') as exophone_type,
	ifNull(JSONExtractString(message,'call_service'),'') as call_service,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'ExophoneObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.fare_parameters_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_selected_fare` Nullable (String),
    `base_fare` Nullable (Int64),
    `service_charge` Nullable (String),
    `customer_extra_fee` Nullable (String),
    `fare_parameters_type` Nullable (String),
    `govt_charges` Nullable (String),
    `waiting_charge` Nullable (String),
    `night_shift_charge` Nullable (String),
    `night_shift_rate_if_applies` Nullable (Float64),
    `ride_extra_time_fare` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.fare_parameters ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.fare_parameters_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, fare_parameters_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.fare_parameters ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.fare_parameters_mv
(
	`id` String,
	`driver_selected_fare` String,
	`base_fare` Int64,
	`service_charge` String,
	`customer_extra_fee` String,
	`fare_parameters_type` String,
	`govt_charges` String,
	`waiting_charge` String,
	`night_shift_charge` String,
	`night_shift_rate_if_applies` Float64,
	`ride_extra_time_fare` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_selected_fare'),'') as driver_selected_fare,
	ifNull(JSONExtractInt(message,'base_fare'), 0) as base_fare,
	ifNull(JSONExtractString(message,'service_charge'),'') as service_charge,
	ifNull(JSONExtractString(message,'customer_extra_fee'),'') as customer_extra_fee,
	ifNull(JSONExtractString(message,'fare_parameters_type'),'') as fare_parameters_type,
	ifNull(JSONExtractString(message,'govt_charges'),'') as govt_charges,
	ifNull(JSONExtractString(message,'waiting_charge'),'') as waiting_charge,
	ifNull(JSONExtractString(message,'night_shift_charge'),'') as night_shift_charge,
	ifNull(JSONExtractFloat(message,'night_shift_rate_if_applies'),0.0) as night_shift_rate_if_applies,
	ifNull(JSONExtractInt(message,'ride_extra_time_fare'), 0) as ride_extra_time_fare,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FareParametersObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.fare_parameters_progressive_details_shard ON CLUSTER `{cluster}`
    (
    `fare_parameters_id` String,
    `dead_km_fare` Nullable (Int64),
    `extra_km_fare` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (fare_parameters_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.fare_parameters_progressive_details_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, fare_parameters_progressive_details_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.fare_parameters_progressive_details ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.fare_parameters_progressive_details_mv
(
	`fare_parameters_id` String,
	`dead_km_fare` Int64,
	`extra_km_fare` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'fare_parameters_id'),'') as fare_parameters_id,
	ifNull(JSONExtractInt(message,'dead_km_fare'), 0) as dead_km_fare,
	ifNull(JSONExtractInt(message,'extra_km_fare'), 0) as extra_km_fare,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FareParametersProgressiveDetailsObject'
	JSONExtractString(message, 'fare_parameters_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.fare_parameters_slab_details_shard ON CLUSTER `{cluster}`
    (
    `fare_parameters_id` String,
    `platform_fee` Nullable (String),
    `sgst` Nullable (String),
    `cgst` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (fare_parameters_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.fare_parameters_slab_details ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.fare_parameters_slab_details_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, fare_parameters_slab_details_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.fare_parameters_slab_details ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.fare_parameters_slab_details_mv
(
	`fare_parameters_id` String,
	`platform_fee` String,
	`sgst` String,
	`cgst` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'fare_parameters_id'),'') as fare_parameters_id,
	ifNull(JSONExtractString(message,'platform_fee'),'') as platform_fee,
	ifNull(JSONExtractString(message,'sgst'),'') as sgst,
	ifNull(JSONExtractString(message,'cgst'),'') as cgst,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FareParametersSlabDetailsObject'
	JSONExtractString(message, 'fare_parameters_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.fare_policy_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `night_shift_start` DateTime DEFAULT now(),
    `night_shift_end` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `min_allowed_trip_distance` Nullable (String),
    `max_allowed_trip_distance` Nullable (String),
    `service_charge` Nullable (String),
    `govt_charges` Nullable (Float64),
    `fare_policy_type` Nullable (String),
    `description` Nullable (String),
    `per_minute_ride_extra_time_charge` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.fare_policy ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.fare_policy_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, fare_policy_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.fare_policy ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.fare_policy_mv
(
	`id` String,
	`night_shift_start` DateTime,
	`night_shift_end` DateTime,
	`created_at` DateTime,
	`updated_at` DateTime,
	`min_allowed_trip_distance` String,
	`max_allowed_trip_distance` String,
	`service_charge` String,
	`govt_charges` Float64,
	`fare_policy_type` String,
	`description` String,
	`per_minute_ride_extra_time_charge` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	toDateTime(JSONExtractInt(message,'night_shift_start')) as night_shift_start,
	toDateTime(JSONExtractInt(message,'night_shift_end')) as night_shift_end,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'min_allowed_trip_distance'),'') as min_allowed_trip_distance,
	ifNull(JSONExtractString(message,'max_allowed_trip_distance'),'') as max_allowed_trip_distance,
	ifNull(JSONExtractString(message,'service_charge'),'') as service_charge,
	ifNull(JSONExtractFloat(message,'govt_charges'),0.0) as govt_charges,
	ifNull(JSONExtractString(message,'fare_policy_type'),'') as fare_policy_type,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractInt(message,'per_minute_ride_extra_time_charge'), 0) as per_minute_ride_extra_time_charge,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FarePolicyObject'


CREATE TABLE atlas_driver_offer_bpp_helper.fare_policy_27_07_bak_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `organization_id` Nullable (String),
    `fare_for_pickup` Nullable (Float64),
    `night_shift_start` DateTime DEFAULT now(),
    `night_shift_end` DateTime DEFAULT now(),
    `night_shift_rate` Nullable (Float64),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `fare_per_km` Nullable (Float64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.fare_policy_27_07_bak ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.fare_policy_27_07_bak_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, fare_policy_27_07_bak_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.fare_policy_27_07_bak ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.fare_policy_27_07_bak_mv
(
	`id` String,
	`organization_id` String,
	`fare_for_pickup` Float64,
	`night_shift_start` DateTime,
	`night_shift_end` DateTime,
	`night_shift_rate` Float64,
	`created_at` DateTime,
	`updated_at` DateTime,
	`fare_per_km` Float64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'organization_id'),'') as organization_id,
	ifNull(JSONExtractFloat(message,'fare_for_pickup'),0.0) as fare_for_pickup,
	toDateTime(JSONExtractInt(message,'night_shift_start')) as night_shift_start,
	toDateTime(JSONExtractInt(message,'night_shift_end')) as night_shift_end,
	ifNull(JSONExtractFloat(message,'night_shift_rate'),0.0) as night_shift_rate,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractFloat(message,'fare_per_km'),0.0) as fare_per_km,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FarePolicy2707BakObject'


CREATE TABLE atlas_driver_offer_bpp_helper.fare_policy_driver_extra_fee_bounds_shard ON CLUSTER `{cluster}`
    (
    `id` Int64,
    `fare_policy_id` Nullable (String),
    `start_distance` Nullable (Int64),
    `min_fee` Nullable (Int64),
    `max_fee` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.fare_policy_driver_extra_fee_bounds_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, fare_policy_driver_extra_fee_bounds_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds_mv
(
	`id` Int64,
	`fare_policy_id` String,
	`start_distance` Int64,
	`min_fee` Int64,
	`max_fee` Int64,
)
	AS SELECT
	ifNull(JSONExtractInt(message,'id'), 0) as id,
	ifNull(JSONExtractString(message,'fare_policy_id'),'') as fare_policy_id,
	ifNull(JSONExtractInt(message,'start_distance'), 0) as start_distance,
	ifNull(JSONExtractInt(message,'min_fee'), 0) as min_fee,
	ifNull(JSONExtractInt(message,'max_fee'), 0) as max_fee,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FarePolicyDriverExtraFeeBoundsObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.fare_policy_progressive_details_shard ON CLUSTER `{cluster}`
    (
    `fare_policy_id` String,
    `base_distance` Nullable (Int64),
    `base_fare` Nullable (Int64),
    `dead_km_fare` Nullable (Int64),
    `waiting_charge` Nullable (String),
    `night_shift_charge` Nullable (String),
    `free_wating_time` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (fare_policy_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.fare_policy_progressive_details_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, fare_policy_progressive_details_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.fare_policy_progressive_details ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.fare_policy_progressive_details_mv
(
	`fare_policy_id` String,
	`base_distance` Int64,
	`base_fare` Int64,
	`dead_km_fare` Int64,
	`waiting_charge` String,
	`night_shift_charge` String,
	`free_wating_time` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'fare_policy_id'),'') as fare_policy_id,
	ifNull(JSONExtractInt(message,'base_distance'), 0) as base_distance,
	ifNull(JSONExtractInt(message,'base_fare'), 0) as base_fare,
	ifNull(JSONExtractInt(message,'dead_km_fare'), 0) as dead_km_fare,
	ifNull(JSONExtractString(message,'waiting_charge'),'') as waiting_charge,
	ifNull(JSONExtractString(message,'night_shift_charge'),'') as night_shift_charge,
	ifNull(JSONExtractInt(message,'free_wating_time'), 0) as free_wating_time,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FarePolicyProgressiveDetailsObject'
	JSONExtractString(message, 'fare_policy_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.fare_policy_progressive_details_per_extra_km_rate_section_shard ON CLUSTER `{cluster}`
    (
    `id` Int64,
    `fare_policy_id` Nullable (String),
    `start_distance` Nullable (Int64),
    `per_extra_km_rate` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.fare_policy_progressive_details_per_extra_km_rate_section_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, fare_policy_progressive_details_per_extra_km_rate_section_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section_mv
(
	`id` Int64,
	`fare_policy_id` String,
	`start_distance` Int64,
	`per_extra_km_rate` String,
)
	AS SELECT
	ifNull(JSONExtractInt(message,'id'), 0) as id,
	ifNull(JSONExtractString(message,'fare_policy_id'),'') as fare_policy_id,
	ifNull(JSONExtractInt(message,'start_distance'), 0) as start_distance,
	ifNull(JSONExtractString(message,'per_extra_km_rate'),'') as per_extra_km_rate,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FarePolicyProgressiveDetailsPerExtraKmRateSectionObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.fare_policy_slabs_details_slab_shard ON CLUSTER `{cluster}`
    (
    `id` Int64,
    `fare_policy_id` Nullable (String),
    `start_distance` Nullable (Int64),
    `base_fare` Nullable (Int64),
    `waiting_charge` Nullable (String),
    `night_shift_charge` Nullable (String),
    `free_wating_time` Nullable (Int64),
    `platform_fee_charge` Nullable (String),
    `platform_fee_cgst` Nullable (String),
    `platform_fee_sgst` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.fare_policy_slabs_details_slab_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, fare_policy_slabs_details_slab_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.fare_policy_slabs_details_slab ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.fare_policy_slabs_details_slab_mv
(
	`id` Int64,
	`fare_policy_id` String,
	`start_distance` Int64,
	`base_fare` Int64,
	`waiting_charge` String,
	`night_shift_charge` String,
	`free_wating_time` Int64,
	`platform_fee_charge` String,
	`platform_fee_cgst` String,
	`platform_fee_sgst` Int64,
)
	AS SELECT
	ifNull(JSONExtractInt(message,'id'), 0) as id,
	ifNull(JSONExtractString(message,'fare_policy_id'),'') as fare_policy_id,
	ifNull(JSONExtractInt(message,'start_distance'), 0) as start_distance,
	ifNull(JSONExtractInt(message,'base_fare'), 0) as base_fare,
	ifNull(JSONExtractString(message,'waiting_charge'),'') as waiting_charge,
	ifNull(JSONExtractString(message,'night_shift_charge'),'') as night_shift_charge,
	ifNull(JSONExtractInt(message,'free_wating_time'), 0) as free_wating_time,
	ifNull(JSONExtractString(message,'platform_fee_charge'),'') as platform_fee_charge,
	ifNull(JSONExtractString(message,'platform_fee_cgst'),'') as platform_fee_cgst,
	ifNull(JSONExtractInt(message,'platform_fee_sgst'), 0) as platform_fee_sgst,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FarePolicySlabsDetailsSlabObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.fare_product_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `fare_policy_id` Nullable (String),
    `vehicle_variant` Nullable (String),
    `area` Nullable (String),
    `flow` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.fare_product ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.fare_product_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, fare_product_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.fare_product ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.fare_product_mv
(
	`id` String,
	`merchant_id` String,
	`fare_policy_id` String,
	`vehicle_variant` String,
	`area` String,
	`flow` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'fare_policy_id'),'') as fare_policy_id,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	ifNull(JSONExtractString(message,'area'),'') as area,
	ifNull(JSONExtractString(message,'flow'),'') as flow,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FareProductObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.feedback_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_id` Nullable (String),
    `ride_id` Nullable (String),
    `badge` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.feedback ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.feedback_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, feedback_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.feedback ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.feedback_mv
(
	`id` String,
	`driver_id` String,
	`ride_id` String,
	`badge` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'ride_id'),'') as ride_id,
	ifNull(JSONExtractString(message,'badge'),'') as badge,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FeedbackObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.feedback_badge_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_id` Nullable (String),
    `badge` Nullable (String),
    `badge_count` Nullable (Int64),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.feedback_badge ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.feedback_badge_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, feedback_badge_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.feedback_badge ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.feedback_badge_mv
(
	`id` String,
	`driver_id` String,
	`badge` String,
	`badge_count` Int64,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'badge'),'') as badge,
	ifNull(JSONExtractInt(message,'badge_count'), 0) as badge_count,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FeedbackBadgeObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.feedback_form_shard ON CLUSTER `{cluster}`
    (
    `category_name` Nullable (String),
    `id` String,
    `rating` Nullable (String),
    `question` Nullable (String),
    `answer` Nullable (Array(String)),
    `answer_type` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.feedback_form ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.feedback_form_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, feedback_form_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.feedback_form ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.feedback_form_mv
(
	`category_name` String,
	`id` String,
	`rating` String,
	`question` String,
	`answer` Array(String),
	`answer_type` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'category_name'),'') as category_name,
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'rating'),'') as rating,
	ifNull(JSONExtractString(message,'question'),'') as question,
	toDateTime(JSONExtractInt(message,'answer')) as answer,
	ifNull(JSONExtractString(message,'answer_type'),'') as answer_type,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FeedbackFormObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.fleet_driver_association_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_id` Nullable (String),
    `fleet_owner_id` Nullable (String),
    `is_active` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.fleet_driver_association ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.fleet_driver_association_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, fleet_driver_association_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.fleet_driver_association ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.fleet_driver_association_mv
(
	`id` String,
	`driver_id` String,
	`fleet_owner_id` String,
	`is_active` String,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'fleet_owner_id'),'') as fleet_owner_id,
	ifNull(JSONExtractString(message,'is_active'),'') as is_active,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'FleetDriverAssociationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.geometry_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `region` Nullable (String),
    `geom` Nullable (String),
    `city` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.geometry ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.geometry_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, geometry_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.geometry ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.geometry_mv
(
	`id` String,
	`region` String,
	`geom` String,
	`city` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'region'),'') as region,
	ifNull(JSONExtractString(message,'geom'),'') as geom,
	ifNull(JSONExtractString(message,'city'),'') as city,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'GeometryObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.go_home_config_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` Nullable (String),
    `enable_go_home` Nullable (String),
    `start_cnt` Nullable (Int64),
    `dest_radius_meters` Nullable (Int64),
    `active_time` Nullable (Int64),
    `update_home_location_after_sec` Nullable (Int64),
    `cancellation_cnt` Nullable (Int64),
    `num_home_locations` Nullable (Int64),
    `go_home_from_location_radius` Nullable (Int64),
    `go_home_way_point_radius` Nullable (Int64),
    `num_drivers_for_dir_check` Nullable (Int64),
    `go_home_batch_delay` Nullable (Int64),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `ignore_waypoints_till` Nullable (Int64),
    `add_start_waypoint_at` Nullable (Int64),
    `new_loc_allowed_radius` Nullable (Int64),
    `merchant_operating_city_id` String,
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (merchant_operating_city_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.go_home_config ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.go_home_config_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, go_home_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.go_home_config ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.go_home_config_mv
(
	`merchant_id` String,
	`enable_go_home` String,
	`start_cnt` Int64,
	`dest_radius_meters` Int64,
	`active_time` Int64,
	`update_home_location_after_sec` Int64,
	`cancellation_cnt` Int64,
	`num_home_locations` Int64,
	`go_home_from_location_radius` Int64,
	`go_home_way_point_radius` Int64,
	`num_drivers_for_dir_check` Int64,
	`go_home_batch_delay` Int64,
	`created_at` DateTime,
	`updated_at` DateTime,
	`ignore_waypoints_till` Int64,
	`add_start_waypoint_at` Int64,
	`new_loc_allowed_radius` Int64,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'enable_go_home'),'') as enable_go_home,
	ifNull(JSONExtractInt(message,'start_cnt'), 0) as start_cnt,
	ifNull(JSONExtractInt(message,'dest_radius_meters'), 0) as dest_radius_meters,
	ifNull(JSONExtractInt(message,'active_time'), 0) as active_time,
	ifNull(JSONExtractInt(message,'update_home_location_after_sec'), 0) as update_home_location_after_sec,
	ifNull(JSONExtractInt(message,'cancellation_cnt'), 0) as cancellation_cnt,
	ifNull(JSONExtractInt(message,'num_home_locations'), 0) as num_home_locations,
	ifNull(JSONExtractInt(message,'go_home_from_location_radius'), 0) as go_home_from_location_radius,
	ifNull(JSONExtractInt(message,'go_home_way_point_radius'), 0) as go_home_way_point_radius,
	ifNull(JSONExtractInt(message,'num_drivers_for_dir_check'), 0) as num_drivers_for_dir_check,
	ifNull(JSONExtractInt(message,'go_home_batch_delay'), 0) as go_home_batch_delay,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractInt(message,'ignore_waypoints_till'), 0) as ignore_waypoints_till,
	ifNull(JSONExtractInt(message,'add_start_waypoint_at'), 0) as add_start_waypoint_at,
	ifNull(JSONExtractInt(message,'new_loc_allowed_radius'), 0) as new_loc_allowed_radius,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'GoHomeConfigObject'
	JSONExtractString(message, 'merchant_operating_city_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.idfy_verification_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `driver_id` Nullable (String),
    `request_id` Nullable (String),
    `doc_type` Nullable (String),
    `status` Nullable (String),
    `idfy_response` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `document_image_id1` Nullable (String),
    `document_image_id2` Nullable (String),
    `issue_date_on_doc` DateTime DEFAULT now(),
    `document_number_encrypted` Nullable (String),
    `document_number_hash` Nullable (String),
    `image_extraction_validation` Nullable (String),
    `driver_date_of_birth` DateTime DEFAULT now(),
    `multiple_r_c` Nullable (String),
    `dashboard_passed_vehicle_variant` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.idfy_verification ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.idfy_verification_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, idfy_verification_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.idfy_verification ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.idfy_verification_mv
(
	`id` String,
	`driver_id` String,
	`request_id` String,
	`doc_type` String,
	`status` String,
	`idfy_response` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`document_image_id1` String,
	`document_image_id2` String,
	`issue_date_on_doc` DateTime,
	`document_number_encrypted` String,
	`document_number_hash` String,
	`image_extraction_validation` String,
	`driver_date_of_birth` DateTime,
	`multiple_r_c` String,
	`dashboard_passed_vehicle_variant` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'doc_type'),'') as doc_type,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'idfy_response'),'') as idfy_response,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'document_image_id1'),'') as document_image_id1,
	ifNull(JSONExtractString(message,'document_image_id2'),'') as document_image_id2,
	toDateTime(JSONExtractInt(message,'issue_date_on_doc')) as issue_date_on_doc,
	ifNull(JSONExtractString(message,'document_number_encrypted'),'') as document_number_encrypted,
	ifNull(JSONExtractString(message,'document_number_hash'),'') as document_number_hash,
	ifNull(JSONExtractString(message,'image_extraction_validation'),'') as image_extraction_validation,
	toDateTime(JSONExtractInt(message,'driver_date_of_birth')) as driver_date_of_birth,
	ifNull(JSONExtractString(message,'multiple_r_c'),'') as multiple_r_c,
	ifNull(JSONExtractString(message,'dashboard_passed_vehicle_variant'),'') as dashboard_passed_vehicle_variant,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'IdfyVerificationObject'


CREATE TABLE atlas_driver_offer_bpp_helper.image_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `person_id` Nullable (String),
    `merchant_id` Nullable (String),
    `s3_path` Nullable (String),
    `image_type` Nullable (String),
    `is_valid` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `failure_reason` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.image ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.image_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, image_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.image ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.image_mv
(
	`id` String,
	`person_id` String,
	`merchant_id` String,
	`s3_path` String,
	`image_type` String,
	`is_valid` String,
	`created_at` DateTime,
	`failure_reason` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'s3_path'),'') as s3_path,
	ifNull(JSONExtractString(message,'image_type'),'') as image_type,
	ifNull(JSONExtractString(message,'is_valid'),'') as is_valid,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'failure_reason'),'') as failure_reason,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'ImageObject'


CREATE TABLE atlas_driver_offer_bpp_helper.invoice_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `invoice_short_id` Nullable (String),
    `driver_fee_id` String,
    `invoice_status` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `max_mandate_amount` Nullable (String),
    `payment_mode` Nullable (String),
    `bank_error_message` Nullable (String),
    `bank_error_code` Nullable (String),
    `bank_error_updated_at` DateTime DEFAULT now(),
    `driver_id` Nullable (String),
    `last_status_checked_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id, driver_fee_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.invoice ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.invoice_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, invoice_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.invoice ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.invoice_mv
(
	`id` String,
	`invoice_short_id` String,
	`driver_fee_id` String,
	`invoice_status` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`max_mandate_amount` String,
	`payment_mode` String,
	`bank_error_message` String,
	`bank_error_code` String,
	`bank_error_updated_at` DateTime,
	`driver_id` String,
	`last_status_checked_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'invoice_short_id'),'') as invoice_short_id,
	ifNull(JSONExtractString(message,'driver_fee_id'),'') as driver_fee_id,
	ifNull(JSONExtractString(message,'invoice_status'),'') as invoice_status,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'max_mandate_amount'),'') as max_mandate_amount,
	ifNull(JSONExtractString(message,'payment_mode'),'') as payment_mode,
	ifNull(JSONExtractString(message,'bank_error_message'),'') as bank_error_message,
	ifNull(JSONExtractString(message,'bank_error_code'),'') as bank_error_code,
	toDateTime(JSONExtractInt(message,'bank_error_updated_at')) as bank_error_updated_at,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	toDateTime(JSONExtractInt(message,'last_status_checked_at')) as last_status_checked_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'InvoiceObject'
	JSONExtractString(message, 'id') is not null
	JSONExtractString(message, ' driver_fee_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.issue_category_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `category` Nullable (String),
    `logo_url` Nullable (String),
    `priority` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.issue_category ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.issue_category_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, issue_category_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.issue_category ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.issue_category_mv
(
	`id` String,
	`category` String,
	`logo_url` String,
	`priority` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'category'),'') as category,
	ifNull(JSONExtractString(message,'logo_url'),'') as logo_url,
	ifNull(JSONExtractInt(message,'priority'), 0) as priority,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueCategoryObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.issue_config_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `auto_mark_issue_closed_duration` Nullable (Float64),
    `on_auto_mark_issue_cls_msgs` Nullable (String),
    `on_create_issue_msgs` Nullable (String),
    `on_issue_reopen_msgs` Nullable (String),
    `on_kapt_mark_issue_res_msgs` Nullable (Array(String)),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.issue_config ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.issue_config_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, issue_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.issue_config ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.issue_config_mv
(
	`id` String,
	`auto_mark_issue_closed_duration` Float64,
	`on_auto_mark_issue_cls_msgs` String,
	`on_create_issue_msgs` String,
	`on_issue_reopen_msgs` String,
	`on_kapt_mark_issue_res_msgs` Array(String),
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractFloat(message,'auto_mark_issue_closed_duration'),0.0) as auto_mark_issue_closed_duration,
	ifNull(JSONExtractString(message,'on_auto_mark_issue_cls_msgs'),'') as on_auto_mark_issue_cls_msgs,
	ifNull(JSONExtractString(message,'on_create_issue_msgs'),'') as on_create_issue_msgs,
	ifNull(JSONExtractString(message,'on_issue_reopen_msgs'),'') as on_issue_reopen_msgs,
	toDateTime(JSONExtractInt(message,'on_kapt_mark_issue_res_msgs')) as on_kapt_mark_issue_res_msgs,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueConfigObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.issue_message_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `option_id` Nullable (String),
    `category_id` Nullable (String),
    `message` Nullable (String),
    `label` Nullable (String),
    `priority` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.issue_message ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.issue_message_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, issue_message_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.issue_message ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.issue_message_mv
(
	`id` String,
	`option_id` String,
	`category_id` String,
	`message` String,
	`label` String,
	`priority` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'option_id'),'') as option_id,
	ifNull(JSONExtractString(message,'category_id'),'') as category_id,
	ifNull(JSONExtractString(message,'message'),'') as message,
	ifNull(JSONExtractString(message,'label'),'') as label,
	ifNull(JSONExtractInt(message,'priority'), 0) as priority,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueMessageObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.issue_option_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `issue_category_id` Nullable (String),
    `option` Nullable (String),
    `priority` Nullable (Int64),
    `label` Nullable (String),
    `issue_message_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.issue_option ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.issue_option_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, issue_option_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.issue_option ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.issue_option_mv
(
	`id` String,
	`issue_category_id` String,
	`option` String,
	`priority` Int64,
	`label` String,
	`issue_message_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'issue_category_id'),'') as issue_category_id,
	ifNull(JSONExtractString(message,'option'),'') as option,
	ifNull(JSONExtractInt(message,'priority'), 0) as priority,
	ifNull(JSONExtractString(message,'label'),'') as label,
	ifNull(JSONExtractString(message,'issue_message_id'),'') as issue_message_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueOptionObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.issue_report_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_id` Nullable (String),
    `ride_id` Nullable (String),
    `description` Nullable (String),
    `assignee` Nullable (String),
    `status` Nullable (String),
    `deleted` Nullable (String),
    `media_files` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `category_id` Nullable (String),
    `option_id` Nullable (String),
    `ticket_id` Nullable (String),
    `person_id` Nullable (String),
    `chats` Nullable (Array(String)),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.issue_report ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.issue_report_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, issue_report_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.issue_report ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.issue_report_mv
(
	`id` String,
	`driver_id` String,
	`ride_id` String,
	`description` String,
	`assignee` String,
	`status` String,
	`deleted` String,
	`media_files` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`category_id` String,
	`option_id` String,
	`ticket_id` String,
	`person_id` String,
	`chats` Array(String),
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'ride_id'),'') as ride_id,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractString(message,'assignee'),'') as assignee,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'deleted'),'') as deleted,
	ifNull(JSONExtractString(message,'media_files'),'') as media_files,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'category_id'),'') as category_id,
	ifNull(JSONExtractString(message,'option_id'),'') as option_id,
	ifNull(JSONExtractString(message,'ticket_id'),'') as ticket_id,
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	toDateTime(JSONExtractInt(message,'chats')) as chats,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueReportObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.issue_translation_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `sentence` Nullable (String),
    `translation` Nullable (String),
    `language` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.issue_translation ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.issue_translation_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, issue_translation_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.issue_translation ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.issue_translation_mv
(
	`id` String,
	`sentence` String,
	`translation` String,
	`language` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'sentence'),'') as sentence,
	ifNull(JSONExtractString(message,'translation'),'') as translation,
	ifNull(JSONExtractString(message,'language'),'') as language,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'IssueTranslationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.kiosk_location_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `address` Nullable (String),
    `landmark` Nullable (String),
    `contact` Nullable (String),
    `longitude` Nullable (Float64),
    `latitude` Nullable (Float64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.kiosk_location ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.kiosk_location_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, kiosk_location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.kiosk_location ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.kiosk_location_mv
(
	`id` String,
	`merchant_id` String,
	`address` String,
	`landmark` String,
	`contact` String,
	`longitude` Float64,
	`latitude` Float64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'address'),'') as address,
	ifNull(JSONExtractString(message,'landmark'),'') as landmark,
	ifNull(JSONExtractString(message,'contact'),'') as contact,
	ifNull(JSONExtractFloat(message,'longitude'),0.0) as longitude,
	ifNull(JSONExtractFloat(message,'latitude'),0.0) as latitude,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'KioskLocationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.kiosk_location_translation_shard ON CLUSTER `{cluster}`
    (
    `kiosk_location_id` String,
    `language` String,
    `landmark` Nullable (String),
    `address` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (kiosk_location_id, language))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.kiosk_location_translation ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.kiosk_location_translation_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, kiosk_location_translation_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.kiosk_location_translation ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.kiosk_location_translation_mv
(
	`kiosk_location_id` String,
	`language` String,
	`landmark` String,
	`address` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'kiosk_location_id'),'') as kiosk_location_id,
	ifNull(JSONExtractString(message,'language'),'') as language,
	ifNull(JSONExtractString(message,'landmark'),'') as landmark,
	ifNull(JSONExtractString(message,'address'),'') as address,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'KioskLocationTranslationObject'
	JSONExtractString(message, 'kiosk_location_id') is not null
	JSONExtractString(message, ' language') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.leader_board_configs_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `leader_board_type` Nullable (String),
    `number_of_sets` Nullable (Int64),
    `leader_board_expiry` Nullable (Int64),
    `z_score_base` Nullable (Int64),
    `leader_board_length_limit` Nullable (Int64),
    `merchant_id` Nullable (String),
    `is_enabled` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.leader_board_configs ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.leader_board_configs_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, leader_board_configs_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.leader_board_configs ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.leader_board_configs_mv
(
	`id` String,
	`leader_board_type` String,
	`number_of_sets` Int64,
	`leader_board_expiry` Int64,
	`z_score_base` Int64,
	`leader_board_length_limit` Int64,
	`merchant_id` String,
	`is_enabled` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'leader_board_type'),'') as leader_board_type,
	ifNull(JSONExtractInt(message,'number_of_sets'), 0) as number_of_sets,
	ifNull(JSONExtractInt(message,'leader_board_expiry'), 0) as leader_board_expiry,
	ifNull(JSONExtractInt(message,'z_score_base'), 0) as z_score_base,
	ifNull(JSONExtractInt(message,'leader_board_length_limit'), 0) as leader_board_length_limit,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'is_enabled'),'') as is_enabled,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'LeaderBoardConfigsObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.location_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `street` Nullable (String),
    `door` Nullable (String),
    `city` Nullable (String),
    `state` Nullable (String),
    `country` Nullable (String),
    `building` Nullable (String),
    `full_address` Nullable (String),
    `area_code` Nullable (String),
    `area` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.location ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.location_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.location ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.location_mv
(
	`id` String,
	`lat` Float64,
	`lon` Float64,
	`street` String,
	`door` String,
	`city` String,
	`state` String,
	`country` String,
	`building` String,
	`full_address` String,
	`area_code` String,
	`area` String,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'street'),'') as street,
	ifNull(JSONExtractString(message,'door'),'') as door,
	ifNull(JSONExtractString(message,'city'),'') as city,
	ifNull(JSONExtractString(message,'state'),'') as state,
	ifNull(JSONExtractString(message,'country'),'') as country,
	ifNull(JSONExtractString(message,'building'),'') as building,
	ifNull(JSONExtractString(message,'full_address'),'') as full_address,
	ifNull(JSONExtractString(message,'area_code'),'') as area_code,
	ifNull(JSONExtractString(message,'area'),'') as area,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'LocationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.location_mapping_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `location_id` Nullable (String),
    `tag` Nullable (String),
    `entity_id` Nullable (String),
    `"order"` Nullable (Int64),
    `version` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.location_mapping ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.location_mapping_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, location_mapping_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.location_mapping ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.location_mapping_mv
(
	`id` String,
	`location_id` String,
	`tag` String,
	`entity_id` String,
	`"order"` Int64,
	`version` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'location_id'),'') as location_id,
	ifNull(JSONExtractString(message,'tag'),'') as tag,
	ifNull(JSONExtractString(message,'entity_id'),'') as entity_id,
	ifNull(JSONExtractInt(message,'"order"'), 0) as "order",
	ifNull(JSONExtractString(message,'version'),'') as version,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'LocationMappingObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.mandate_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `max_amount` Nullable (Int64),
    `status` Nullable (String),
    `payer_vpa` Nullable (String),
    `start_date` DateTime DEFAULT now(),
    `end_date` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `payer_app` Nullable (String),
    `payer_app_name` Nullable (String),
    `mandate_payment_flow` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.mandate ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.mandate_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, mandate_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.mandate ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.mandate_mv
(
	`id` String,
	`max_amount` Int64,
	`status` String,
	`payer_vpa` String,
	`start_date` DateTime,
	`end_date` DateTime,
	`created_at` DateTime,
	`updated_at` DateTime,
	`payer_app` String,
	`payer_app_name` String,
	`mandate_payment_flow` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractInt(message,'max_amount'), 0) as max_amount,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'payer_vpa'),'') as payer_vpa,
	toDateTime(JSONExtractInt(message,'start_date')) as start_date,
	toDateTime(JSONExtractInt(message,'end_date')) as end_date,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'payer_app'),'') as payer_app,
	ifNull(JSONExtractString(message,'payer_app_name'),'') as payer_app_name,
	ifNull(JSONExtractString(message,'mandate_payment_flow'),'') as mandate_payment_flow,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MandateObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.media_file_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `type` Nullable (String),
    `url` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.media_file ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.media_file_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, media_file_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.media_file ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.media_file_mv
(
	`id` String,
	`type` String,
	`url` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'type'),'') as type,
	ifNull(JSONExtractString(message,'url'),'') as url,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MediaFileObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.merchant_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `name` Nullable (String),
    `subscriber_id` Nullable (String),
    `gstin` Nullable (String),
    `status` Nullable (String),
    `verified` Nullable (String),
    `enabled` Nullable (String),
    `description` Nullable (String),
    `mobile_number` Nullable (String),
    `mobile_country_code` Nullable (String),
    `from_time` DateTime DEFAULT now(),
    `to_time` DateTime DEFAULT now(),
    `api_key` Nullable (String),
    `head_count` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `info` Nullable (String),
    `unique_key_id` Nullable (String),
    `short_id` Nullable (String),
    `origin_restriction` Nullable (Array(String)),
    `destination_restriction` Nullable (Array(String)),
    `internal_api_key` Nullable (String),
    `city` Nullable (String),
    `geo_hash_precision_value` Nullable (Int64),
    `country` Nullable (String),
    `minimum_driver_rates_count` Nullable (Int64),
    `registry_url` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.merchant ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.merchant_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, merchant_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.merchant ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.merchant_mv
(
	`id` String,
	`name` String,
	`subscriber_id` String,
	`gstin` String,
	`status` String,
	`verified` String,
	`enabled` String,
	`description` String,
	`mobile_number` String,
	`mobile_country_code` String,
	`from_time` DateTime,
	`to_time` DateTime,
	`api_key` String,
	`head_count` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`info` String,
	`unique_key_id` String,
	`short_id` String,
	`origin_restriction` Array(String),
	`destination_restriction` Array(String),
	`internal_api_key` String,
	`city` String,
	`geo_hash_precision_value` Int64,
	`country` String,
	`minimum_driver_rates_count` Int64,
	`registry_url` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'name'),'') as name,
	ifNull(JSONExtractString(message,'subscriber_id'),'') as subscriber_id,
	ifNull(JSONExtractString(message,'gstin'),'') as gstin,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'verified'),'') as verified,
	ifNull(JSONExtractString(message,'enabled'),'') as enabled,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractString(message,'mobile_number'),'') as mobile_number,
	ifNull(JSONExtractString(message,'mobile_country_code'),'') as mobile_country_code,
	toDateTime(JSONExtractInt(message,'from_time')) as from_time,
	toDateTime(JSONExtractInt(message,'to_time')) as to_time,
	ifNull(JSONExtractString(message,'api_key'),'') as api_key,
	ifNull(JSONExtractString(message,'head_count'),'') as head_count,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'info'),'') as info,
	ifNull(JSONExtractString(message,'unique_key_id'),'') as unique_key_id,
	ifNull(JSONExtractString(message,'short_id'),'') as short_id,
	toDateTime(JSONExtractInt(message,'origin_restriction')) as origin_restriction,
	toDateTime(JSONExtractInt(message,'destination_restriction')) as destination_restriction,
	ifNull(JSONExtractString(message,'internal_api_key'),'') as internal_api_key,
	ifNull(JSONExtractString(message,'city'),'') as city,
	ifNull(JSONExtractInt(message,'geo_hash_precision_value'), 0) as geo_hash_precision_value,
	ifNull(JSONExtractString(message,'country'),'') as country,
	ifNull(JSONExtractInt(message,'minimum_driver_rates_count'), 0) as minimum_driver_rates_count,
	ifNull(JSONExtractString(message,'registry_url'),'') as registry_url,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.merchant_message_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` Nullable (String),
    `message_key` String,
    `message` String,
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `template_id` Nullable (String),
    `json_data` Nullable (String),
    `contains_url_button` Nullable (String),
    `merchant_operating_city_id` String,
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (merchant_operating_city_id, message_key))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.merchant_message ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.merchant_message_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, merchant_message_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.merchant_message ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.merchant_message_mv
(
	`merchant_id` String,
	`message_key` String,
	`message` String,
	`updated_at` DateTime,
	`created_at` DateTime,
	`template_id` String,
	`json_data` String,
	`contains_url_button` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'message_key'),'') as message_key,
	ifNull(JSONExtractString(message,'message'),'') as message,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'template_id'),'') as template_id,
	ifNull(JSONExtractString(message,'json_data'),'') as json_data,
	ifNull(JSONExtractString(message,'contains_url_button'),'') as contains_url_button,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantMessageObject'
	JSONExtractString(message, 'merchant_operating_city_id') is not null
	JSONExtractString(message, ' message_key') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.merchant_operating_city_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `merchant_short_id` Nullable (String),
    `city` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.merchant_operating_city ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.merchant_operating_city_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, merchant_operating_city_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.merchant_operating_city ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.merchant_operating_city_mv
(
	`id` String,
	`merchant_id` String,
	`merchant_short_id` String,
	`city` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'merchant_short_id'),'') as merchant_short_id,
	ifNull(JSONExtractString(message,'city'),'') as city,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantOperatingCityObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.merchant_overlay_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `overlay_key` Nullable (String),
    `language` Nullable (String),
    `udf1` Nullable (String),
    `title` Nullable (String),
    `description` Nullable (String),
    `image_url` Nullable (String),
    `ok_button_text` Nullable (String),
    `cancel_button_text` Nullable (String),
    `actions` Nullable (Array(String)),
    `link` Nullable (String),
    `req_body` Nullable (String),
    `end_point` Nullable (String),
    `method` Nullable (String),
    `delay` Nullable (String),
    `contact_support_number` Nullable (String),
    `toast_message` Nullable (String),
    `secondary_actions` Nullable (String),
    `social_media_links` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.merchant_overlay ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.merchant_overlay_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, merchant_overlay_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.merchant_overlay ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.merchant_overlay_mv
(
	`id` String,
	`merchant_id` String,
	`overlay_key` String,
	`language` String,
	`udf1` String,
	`title` String,
	`description` String,
	`image_url` String,
	`ok_button_text` String,
	`cancel_button_text` String,
	`actions` Array(String),
	`link` String,
	`req_body` String,
	`end_point` String,
	`method` String,
	`delay` String,
	`contact_support_number` String,
	`toast_message` String,
	`secondary_actions` String,
	`social_media_links` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'overlay_key'),'') as overlay_key,
	ifNull(JSONExtractString(message,'language'),'') as language,
	ifNull(JSONExtractString(message,'udf1'),'') as udf1,
	ifNull(JSONExtractString(message,'title'),'') as title,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractString(message,'image_url'),'') as image_url,
	ifNull(JSONExtractString(message,'ok_button_text'),'') as ok_button_text,
	ifNull(JSONExtractString(message,'cancel_button_text'),'') as cancel_button_text,
	toDateTime(JSONExtractInt(message,'actions')) as actions,
	ifNull(JSONExtractString(message,'link'),'') as link,
	ifNull(JSONExtractString(message,'req_body'),'') as req_body,
	ifNull(JSONExtractString(message,'end_point'),'') as end_point,
	ifNull(JSONExtractString(message,'method'),'') as method,
	ifNull(JSONExtractString(message,'delay'),'') as delay,
	ifNull(JSONExtractString(message,'contact_support_number'),'') as contact_support_number,
	ifNull(JSONExtractString(message,'toast_message'),'') as toast_message,
	ifNull(JSONExtractString(message,'secondary_actions'),'') as secondary_actions,
	ifNull(JSONExtractString(message,'social_media_links'),'') as social_media_links,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantOverlayObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.merchant_payment_method_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `payment_type` Nullable (String),
    `payment_instrument` Nullable (String),
    `collected_by` Nullable (String),
    `priority` Nullable (Int64),
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.merchant_payment_method ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.merchant_payment_method_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, merchant_payment_method_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.merchant_payment_method ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.merchant_payment_method_mv
(
	`id` String,
	`merchant_id` String,
	`payment_type` String,
	`payment_instrument` String,
	`collected_by` String,
	`priority` Int64,
	`updated_at` DateTime,
	`created_at` DateTime,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'payment_type'),'') as payment_type,
	ifNull(JSONExtractString(message,'payment_instrument'),'') as payment_instrument,
	ifNull(JSONExtractString(message,'collected_by'),'') as collected_by,
	ifNull(JSONExtractInt(message,'priority'), 0) as priority,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantPaymentMethodObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.merchant_service_config_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` String,
    `service_name` String,
    `config_json` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (merchant_id, service_name))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.merchant_service_config ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.merchant_service_config_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, merchant_service_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.merchant_service_config ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.merchant_service_config_mv
(
	`merchant_id` String,
	`service_name` String,
	`config_json` String,
	`updated_at` DateTime,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'service_name'),'') as service_name,
	ifNull(JSONExtractString(message,'config_json'),'') as config_json,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantServiceConfigObject'
	JSONExtractString(message, 'merchant_id') is not null
	JSONExtractString(message, ' service_name') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.merchant_service_usage_config_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` Nullable (String),
    `get_distances` Nullable (String),
    `get_routes` Nullable (String),
    `snap_to_road` Nullable (String),
    `get_place_name` Nullable (String),
    `get_place_details` Nullable (String),
    `auto_complete` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `sms_providers_priority_list` Nullable (Array(String)),
    `get_estimated_pickup_distances` Nullable (String),
    `whatsapp_providers_priority_list` Nullable (Array(String)),
    `get_pickup_routes` Nullable (String),
    `get_trip_routes` Nullable (String),
    `verification_service` Nullable (String),
    `initiate_call` Nullable (String),
    `get_distances_for_cancel_ride` Nullable (String),
    `aadhaar_verification_service` Nullable (String),
    `face_verification_service` Nullable (String),
    `issue_ticket_service` Nullable (String),
    `get_exophone` Nullable (String),
    `merchant_operating_city_id` String,
    `snap_to_road_providers_list` Nullable (Array(String)),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (merchant_operating_city_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.merchant_service_usage_config ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.merchant_service_usage_config_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, merchant_service_usage_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.merchant_service_usage_config ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.merchant_service_usage_config_mv
(
	`merchant_id` String,
	`get_distances` String,
	`get_routes` String,
	`snap_to_road` String,
	`get_place_name` String,
	`get_place_details` String,
	`auto_complete` String,
	`updated_at` DateTime,
	`created_at` DateTime,
	`sms_providers_priority_list` Array(String),
	`get_estimated_pickup_distances` String,
	`whatsapp_providers_priority_list` Array(String),
	`get_pickup_routes` String,
	`get_trip_routes` String,
	`verification_service` String,
	`initiate_call` String,
	`get_distances_for_cancel_ride` String,
	`aadhaar_verification_service` String,
	`face_verification_service` String,
	`issue_ticket_service` String,
	`get_exophone` String,
	`merchant_operating_city_id` String,
	`snap_to_road_providers_list` Array(String),
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'get_distances'),'') as get_distances,
	ifNull(JSONExtractString(message,'get_routes'),'') as get_routes,
	ifNull(JSONExtractString(message,'snap_to_road'),'') as snap_to_road,
	ifNull(JSONExtractString(message,'get_place_name'),'') as get_place_name,
	ifNull(JSONExtractString(message,'get_place_details'),'') as get_place_details,
	ifNull(JSONExtractString(message,'auto_complete'),'') as auto_complete,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'sms_providers_priority_list')) as sms_providers_priority_list,
	ifNull(JSONExtractString(message,'get_estimated_pickup_distances'),'') as get_estimated_pickup_distances,
	toDateTime(JSONExtractInt(message,'whatsapp_providers_priority_list')) as whatsapp_providers_priority_list,
	ifNull(JSONExtractString(message,'get_pickup_routes'),'') as get_pickup_routes,
	ifNull(JSONExtractString(message,'get_trip_routes'),'') as get_trip_routes,
	ifNull(JSONExtractString(message,'verification_service'),'') as verification_service,
	ifNull(JSONExtractString(message,'initiate_call'),'') as initiate_call,
	ifNull(JSONExtractString(message,'get_distances_for_cancel_ride'),'') as get_distances_for_cancel_ride,
	ifNull(JSONExtractString(message,'aadhaar_verification_service'),'') as aadhaar_verification_service,
	ifNull(JSONExtractString(message,'face_verification_service'),'') as face_verification_service,
	ifNull(JSONExtractString(message,'issue_ticket_service'),'') as issue_ticket_service,
	ifNull(JSONExtractString(message,'get_exophone'),'') as get_exophone,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,
	toDateTime(JSONExtractInt(message,'snap_to_road_providers_list')) as snap_to_road_providers_list,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MerchantServiceUsageConfigObject'
	JSONExtractString(message, 'merchant_operating_city_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.message_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `type` Nullable (String),
    `title` Nullable (String),
    `description` Nullable (String),
    `media_files` Nullable (String),
    `merchant_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `label` Nullable (String),
    `like_count` Nullable (Int64),
    `short_description` Nullable (String),
    `view_count` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.message ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.message_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, message_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.message ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.message_mv
(
	`id` String,
	`type` String,
	`title` String,
	`description` String,
	`media_files` String,
	`merchant_id` String,
	`created_at` DateTime,
	`label` String,
	`like_count` Int64,
	`short_description` String,
	`view_count` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'type'),'') as type,
	ifNull(JSONExtractString(message,'title'),'') as title,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractString(message,'media_files'),'') as media_files,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'label'),'') as label,
	ifNull(JSONExtractInt(message,'like_count'), 0) as like_count,
	ifNull(JSONExtractString(message,'short_description'),'') as short_description,
	ifNull(JSONExtractInt(message,'view_count'), 0) as view_count,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MessageObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.message_report_shard ON CLUSTER `{cluster}`
    (
    `message_id` String,
    `driver_id` String,
    `delivery_status` Nullable (String),
    `read_status` Nullable (String),
    `reply` Nullable (String),
    `message_dynamic_fields` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `like_status` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (message_id, driver_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.message_report ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.message_report_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, message_report_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.message_report ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.message_report_mv
(
	`message_id` String,
	`driver_id` String,
	`delivery_status` String,
	`read_status` String,
	`reply` String,
	`message_dynamic_fields` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`like_status` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'message_id'),'') as message_id,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'delivery_status'),'') as delivery_status,
	ifNull(JSONExtractString(message,'read_status'),'') as read_status,
	ifNull(JSONExtractString(message,'reply'),'') as reply,
	ifNull(JSONExtractString(message,'message_dynamic_fields'),'') as message_dynamic_fields,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'like_status'),'') as like_status,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MessageReportObject'
	JSONExtractString(message, 'message_id') is not null
	JSONExtractString(message, ' driver_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.message_translation_shard ON CLUSTER `{cluster}`
    (
    `message_id` String,
    `language` String,
    `title` Nullable (String),
    `description` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `label` Nullable (String),
    `short_description` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (message_id, language))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.message_translation ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.message_translation_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, message_translation_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.message_translation ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.message_translation_mv
(
	`message_id` String,
	`language` String,
	`title` String,
	`description` String,
	`created_at` DateTime,
	`label` String,
	`short_description` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'message_id'),'') as message_id,
	ifNull(JSONExtractString(message,'language'),'') as language,
	ifNull(JSONExtractString(message,'title'),'') as title,
	ifNull(JSONExtractString(message,'description'),'') as description,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractString(message,'label'),'') as label,
	ifNull(JSONExtractString(message,'short_description'),'') as short_description,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MessageTranslationObject'
	JSONExtractString(message, 'message_id') is not null
	JSONExtractString(message, ' language') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.meta_data_shard ON CLUSTER `{cluster}`
    (
    `driver_id` String,
    `device` Nullable (String),
    `device_o_s` Nullable (String),
    `device_date_time` DateTime DEFAULT now(),
    `app_permissions` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (driver_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.meta_data ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.meta_data_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, meta_data_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.meta_data ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.meta_data_mv
(
	`driver_id` String,
	`device` String,
	`device_o_s` String,
	`device_date_time` DateTime,
	`app_permissions` String,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'device'),'') as device,
	ifNull(JSONExtractString(message,'device_o_s'),'') as device_o_s,
	toDateTime(JSONExtractInt(message,'device_date_time')) as device_date_time,
	ifNull(JSONExtractString(message,'app_permissions'),'') as app_permissions,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'MetaDataObject'
	JSONExtractString(message, 'driver_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.notification_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `short_id` Nullable (String),
    `source_amount` Nullable (String),
    `mandate_id` Nullable (String),
    `driver_fee_id` Nullable (String),
    `txn_date` DateTime DEFAULT now(),
    `juspay_provided_id` Nullable (String),
    `provider_name` Nullable (String),
    `notification_type` Nullable (String),
    `description` Nullable (String),
    `status` Nullable (String),
    `date_created` DateTime DEFAULT now(),
    `last_updated` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `last_status_checked_at` DateTime DEFAULT now(),
    `response_code` Nullable (String),
    `response_message` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.notification ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.notification_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, notification_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.notification ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.notification_mv
(
	`id` String,
	`short_id` String,
	`source_amount` String,
	`mandate_id` String,
	`driver_fee_id` String,
	`txn_date` DateTime,
	`juspay_provided_id` String,
	`provider_name` String,
	`notification_type` String,
	`description` String,
	`status` String,
	`date_created` DateTime,
	`last_updated` DateTime,
	`created_at` DateTime,
	`updated_at` DateTime,
	`last_status_checked_at` DateTime,
	`response_code` String,
	`response_message` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'short_id'),'') as short_id,
	ifNull(JSONExtractString(message,'source_amount'),'') as source_amount,
	ifNull(JSONExtractString(message,'mandate_id'),'') as mandate_id,
	ifNull(JSONExtractString(message,'driver_fee_id'),'') as driver_fee_id,
	toDateTime(JSONExtractInt(message,'txn_date')) as txn_date,
	ifNull(JSONExtractString(message,'juspay_provided_id'),'') as juspay_provided_id,
	ifNull(JSONExtractString(message,'provider_name'),'') as provider_name,
	ifNull(JSONExtractString(message,'notification_type'),'') as notification_type,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractString(message,'status'),'') as status,
	toDateTime(JSONExtractInt(message,'date_created')) as date_created,
	toDateTime(JSONExtractInt(message,'last_updated')) as last_updated,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'last_status_checked_at')) as last_status_checked_at,
	ifNull(JSONExtractString(message,'response_code'),'') as response_code,
	ifNull(JSONExtractString(message,'response_message'),'') as response_message,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'NotificationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.onboarding_document_configs_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` Nullable (String),
    `document_type` String,
    `check_extraction` Nullable (String),
    `check_expiry` Nullable (String),
    `vehicle_class_check_type` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `rc_number_prefix` Nullable (String),
    `supported_vehicle_classes_json` Nullable (String),
    `merchant_operating_city_id` String,
    `rc_number_prefix_list` Nullable (Array(String)),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (merchant_operating_city_id, document_type))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.onboarding_document_configs ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.onboarding_document_configs_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, onboarding_document_configs_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.onboarding_document_configs ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.onboarding_document_configs_mv
(
	`merchant_id` String,
	`document_type` String,
	`check_extraction` String,
	`check_expiry` String,
	`vehicle_class_check_type` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`rc_number_prefix` String,
	`supported_vehicle_classes_json` String,
	`merchant_operating_city_id` String,
	`rc_number_prefix_list` Array(String),
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'document_type'),'') as document_type,
	ifNull(JSONExtractString(message,'check_extraction'),'') as check_extraction,
	ifNull(JSONExtractString(message,'check_expiry'),'') as check_expiry,
	ifNull(JSONExtractString(message,'vehicle_class_check_type'),'') as vehicle_class_check_type,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'rc_number_prefix'),'') as rc_number_prefix,
	ifNull(JSONExtractString(message,'supported_vehicle_classes_json'),'') as supported_vehicle_classes_json,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,
	toDateTime(JSONExtractInt(message,'rc_number_prefix_list')) as rc_number_prefix_list,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'OnboardingDocumentConfigsObject'
	JSONExtractString(message, 'merchant_operating_city_id') is not null
	JSONExtractString(message, ' document_type') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.operating_city_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `merchant_id` Nullable (String),
    `city_name` Nullable (String),
    `enabled` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.operating_city ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.operating_city_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, operating_city_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.operating_city ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.operating_city_mv
(
	`id` String,
	`merchant_id` String,
	`city_name` String,
	`enabled` String,
	`created_at` DateTime,
	`updated_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'city_name'),'') as city_name,
	ifNull(JSONExtractString(message,'enabled'),'') as enabled,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'OperatingCityObject'


CREATE TABLE atlas_driver_offer_bpp_helper.payment_order_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `short_id` Nullable (String),
    `person_id` Nullable (String),
    `merchant_id` Nullable (String),
    `amount` Nullable (String),
    `currency` Nullable (String),
    `status` Nullable (String),
    `web_payment_link` Nullable (String),
    `iframe_payment_link` Nullable (String),
    `mobile_payment_link` Nullable (String),
    `client_auth_token_encrypted` Nullable (String),
    `client_auth_token_hash` Nullable (String),
    `client_auth_token_expiry` DateTime DEFAULT now(),
    `get_upi_deep_links_option` Nullable (String),
    `environment` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `payment_service_order_id` Nullable (String),
    `service` Nullable (String),
    `client_id` Nullable (String),
    `description` Nullable (String),
    `return_url` Nullable (String),
    `action` Nullable (String),
    `request_id` Nullable (String),
    `payment_merchant_id` Nullable (String),
    `create_mandate` Nullable (String),
    `mandate_max_amount` Nullable (String),
    `mandate_start_date` DateTime DEFAULT now(),
    `mandate_end_date` DateTime DEFAULT now(),
    `bank_error_message` Nullable (String),
    `bank_error_code` Nullable (String),
    `is_retried` Nullable (String),
    `is_retargeted` Nullable (String),
    `retarget_link` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.payment_order ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.payment_order_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, payment_order_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.payment_order ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.payment_order_mv
(
	`id` String,
	`short_id` String,
	`person_id` String,
	`merchant_id` String,
	`amount` String,
	`currency` String,
	`status` String,
	`web_payment_link` String,
	`iframe_payment_link` String,
	`mobile_payment_link` String,
	`client_auth_token_encrypted` String,
	`client_auth_token_hash` String,
	`client_auth_token_expiry` DateTime,
	`get_upi_deep_links_option` String,
	`environment` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`payment_service_order_id` String,
	`service` String,
	`client_id` String,
	`description` String,
	`return_url` String,
	`action` String,
	`request_id` String,
	`payment_merchant_id` String,
	`create_mandate` String,
	`mandate_max_amount` String,
	`mandate_start_date` DateTime,
	`mandate_end_date` DateTime,
	`bank_error_message` String,
	`bank_error_code` String,
	`is_retried` String,
	`is_retargeted` String,
	`retarget_link` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'short_id'),'') as short_id,
	ifNull(JSONExtractString(message,'person_id'),'') as person_id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'amount'),'') as amount,
	ifNull(JSONExtractString(message,'currency'),'') as currency,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'web_payment_link'),'') as web_payment_link,
	ifNull(JSONExtractString(message,'iframe_payment_link'),'') as iframe_payment_link,
	ifNull(JSONExtractString(message,'mobile_payment_link'),'') as mobile_payment_link,
	ifNull(JSONExtractString(message,'client_auth_token_encrypted'),'') as client_auth_token_encrypted,
	ifNull(JSONExtractString(message,'client_auth_token_hash'),'') as client_auth_token_hash,
	toDateTime(JSONExtractInt(message,'client_auth_token_expiry')) as client_auth_token_expiry,
	ifNull(JSONExtractString(message,'get_upi_deep_links_option'),'') as get_upi_deep_links_option,
	ifNull(JSONExtractString(message,'environment'),'') as environment,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'payment_service_order_id'),'') as payment_service_order_id,
	ifNull(JSONExtractString(message,'service'),'') as service,
	ifNull(JSONExtractString(message,'client_id'),'') as client_id,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractString(message,'return_url'),'') as return_url,
	ifNull(JSONExtractString(message,'action'),'') as action,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'payment_merchant_id'),'') as payment_merchant_id,
	ifNull(JSONExtractString(message,'create_mandate'),'') as create_mandate,
	ifNull(JSONExtractString(message,'mandate_max_amount'),'') as mandate_max_amount,
	toDateTime(JSONExtractInt(message,'mandate_start_date')) as mandate_start_date,
	toDateTime(JSONExtractInt(message,'mandate_end_date')) as mandate_end_date,
	ifNull(JSONExtractString(message,'bank_error_message'),'') as bank_error_message,
	ifNull(JSONExtractString(message,'bank_error_code'),'') as bank_error_code,
	ifNull(JSONExtractString(message,'is_retried'),'') as is_retried,
	ifNull(JSONExtractString(message,'is_retargeted'),'') as is_retargeted,
	ifNull(JSONExtractString(message,'retarget_link'),'') as retarget_link,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'PaymentOrderObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.payment_transaction_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `txn_uuid` Nullable (String),
    `payment_method_type` Nullable (String),
    `payment_method` Nullable (String),
    `resp_message` Nullable (String),
    `resp_code` Nullable (String),
    `gateway_reference_id` Nullable (String),
    `order_id` Nullable (String),
    `merchant_id` Nullable (String),
    `amount` Nullable (String),
    `currency` Nullable (String),
    `date_created` DateTime DEFAULT now(),
    `status_id` Nullable (Int64),
    `status` Nullable (String),
    `juspay_response` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `mandate_id` Nullable (String),
    `mandate_max_amount` Nullable (String),
    `mandate_frequency` Nullable (String),
    `mandate_status` Nullable (String),
    `mandate_start_date` DateTime DEFAULT now(),
    `mandate_end_date` DateTime DEFAULT now(),
    `bank_error_message` Nullable (String),
    `bank_error_code` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.payment_transaction ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.payment_transaction_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, payment_transaction_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.payment_transaction ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.payment_transaction_mv
(
	`id` String,
	`txn_uuid` String,
	`payment_method_type` String,
	`payment_method` String,
	`resp_message` String,
	`resp_code` String,
	`gateway_reference_id` String,
	`order_id` String,
	`merchant_id` String,
	`amount` String,
	`currency` String,
	`date_created` DateTime,
	`status_id` Int64,
	`status` String,
	`juspay_response` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`mandate_id` String,
	`mandate_max_amount` String,
	`mandate_frequency` String,
	`mandate_status` String,
	`mandate_start_date` DateTime,
	`mandate_end_date` DateTime,
	`bank_error_message` String,
	`bank_error_code` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'txn_uuid'),'') as txn_uuid,
	ifNull(JSONExtractString(message,'payment_method_type'),'') as payment_method_type,
	ifNull(JSONExtractString(message,'payment_method'),'') as payment_method,
	ifNull(JSONExtractString(message,'resp_message'),'') as resp_message,
	ifNull(JSONExtractString(message,'resp_code'),'') as resp_code,
	ifNull(JSONExtractString(message,'gateway_reference_id'),'') as gateway_reference_id,
	ifNull(JSONExtractString(message,'order_id'),'') as order_id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'amount'),'') as amount,
	ifNull(JSONExtractString(message,'currency'),'') as currency,
	toDateTime(JSONExtractInt(message,'date_created')) as date_created,
	ifNull(JSONExtractInt(message,'status_id'), 0) as status_id,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'juspay_response'),'') as juspay_response,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'mandate_id'),'') as mandate_id,
	ifNull(JSONExtractString(message,'mandate_max_amount'),'') as mandate_max_amount,
	ifNull(JSONExtractString(message,'mandate_frequency'),'') as mandate_frequency,
	ifNull(JSONExtractString(message,'mandate_status'),'') as mandate_status,
	toDateTime(JSONExtractInt(message,'mandate_start_date')) as mandate_start_date,
	toDateTime(JSONExtractInt(message,'mandate_end_date')) as mandate_end_date,
	ifNull(JSONExtractString(message,'bank_error_message'),'') as bank_error_message,
	ifNull(JSONExtractString(message,'bank_error_code'),'') as bank_error_code,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'PaymentTransactionObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.person_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `first_name` Nullable (String),
    `middle_name` Nullable (String),
    `last_name` Nullable (String),
    `role` Nullable (String),
    `gender` Nullable (String),
    `identifier_type` Nullable (String),
    `email` Nullable (String),
    `password_hash` Nullable (String),
    `mobile_number_encrypted` Nullable (String),
    `mobile_number_hash` Nullable (String),
    `mobile_country_code` Nullable (String),
    `identifier` Nullable (String),
    `is_new` Nullable (String),
    `merchant_id` Nullable (String),
    `device_token` Nullable (String),
    `description` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `rating` Nullable (Float64),
    `language` Nullable (String),
    `client_version` Nullable (String),
    `bundle_version` Nullable (String),
    `unencrypted_mobile_number` Nullable (String),
    `whatsapp_notification_enroll_status` Nullable (String),
    `unencrypted_alternate_mobile_number` Nullable (String),
    `alternate_mobile_number_encrypted` Nullable (String),
    `alternate_mobile_number_hash` Nullable (String),
    `hometown` Nullable (String),
    `languages_spoken` Nullable (Array(String)),
    `onboarded_from_dashboard` Nullable (String),
    `face_image_id` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.person ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.person_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, person_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.person ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.person_mv
(
	`id` String,
	`first_name` String,
	`middle_name` String,
	`last_name` String,
	`role` String,
	`gender` String,
	`identifier_type` String,
	`email` String,
	`password_hash` String,
	`mobile_number_encrypted` String,
	`mobile_number_hash` String,
	`mobile_country_code` String,
	`identifier` String,
	`is_new` String,
	`merchant_id` String,
	`device_token` String,
	`description` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`rating` Float64,
	`language` String,
	`client_version` String,
	`bundle_version` String,
	`unencrypted_mobile_number` String,
	`whatsapp_notification_enroll_status` String,
	`unencrypted_alternate_mobile_number` String,
	`alternate_mobile_number_encrypted` String,
	`alternate_mobile_number_hash` String,
	`hometown` String,
	`languages_spoken` Array(String),
	`onboarded_from_dashboard` String,
	`face_image_id` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'first_name'),'') as first_name,
	ifNull(JSONExtractString(message,'middle_name'),'') as middle_name,
	ifNull(JSONExtractString(message,'last_name'),'') as last_name,
	ifNull(JSONExtractString(message,'role'),'') as role,
	ifNull(JSONExtractString(message,'gender'),'') as gender,
	ifNull(JSONExtractString(message,'identifier_type'),'') as identifier_type,
	ifNull(JSONExtractString(message,'email'),'') as email,
	ifNull(JSONExtractString(message,'password_hash'),'') as password_hash,
	ifNull(JSONExtractString(message,'mobile_number_encrypted'),'') as mobile_number_encrypted,
	ifNull(JSONExtractString(message,'mobile_number_hash'),'') as mobile_number_hash,
	ifNull(JSONExtractString(message,'mobile_country_code'),'') as mobile_country_code,
	ifNull(JSONExtractString(message,'identifier'),'') as identifier,
	ifNull(JSONExtractString(message,'is_new'),'') as is_new,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'device_token'),'') as device_token,
	ifNull(JSONExtractString(message,'description'),'') as description,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractFloat(message,'rating'),0.0) as rating,
	ifNull(JSONExtractString(message,'language'),'') as language,
	ifNull(JSONExtractString(message,'client_version'),'') as client_version,
	ifNull(JSONExtractString(message,'bundle_version'),'') as bundle_version,
	ifNull(JSONExtractString(message,'unencrypted_mobile_number'),'') as unencrypted_mobile_number,
	ifNull(JSONExtractString(message,'whatsapp_notification_enroll_status'),'') as whatsapp_notification_enroll_status,
	ifNull(JSONExtractString(message,'unencrypted_alternate_mobile_number'),'') as unencrypted_alternate_mobile_number,
	ifNull(JSONExtractString(message,'alternate_mobile_number_encrypted'),'') as alternate_mobile_number_encrypted,
	ifNull(JSONExtractString(message,'alternate_mobile_number_hash'),'') as alternate_mobile_number_hash,
	ifNull(JSONExtractString(message,'hometown'),'') as hometown,
	toDateTime(JSONExtractInt(message,'languages_spoken')) as languages_spoken,
	ifNull(JSONExtractString(message,'onboarded_from_dashboard'),'') as onboarded_from_dashboard,
	ifNull(JSONExtractString(message,'face_image_id'),'') as face_image_id,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'PersonObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.place_name_cache_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `formatted_address` Nullable (String),
    `plus_code` Nullable (String),
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `place_id` Nullable (String),
    `address_components` Nullable (Array(String)),
    `geo_hash` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.place_name_cache ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.place_name_cache_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, place_name_cache_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.place_name_cache ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.place_name_cache_mv
(
	`id` String,
	`formatted_address` String,
	`plus_code` String,
	`lat` Float64,
	`lon` Float64,
	`place_id` String,
	`address_components` Array(String),
	`geo_hash` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'formatted_address'),'') as formatted_address,
	ifNull(JSONExtractString(message,'plus_code'),'') as plus_code,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'place_id'),'') as place_id,
	toDateTime(JSONExtractInt(message,'address_components')) as address_components,
	ifNull(JSONExtractString(message,'geo_hash'),'') as geo_hash,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'PlaceNameCacheObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.plan_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `payment_mode` String,
    `plan_type` Nullable (String),
    `name` Nullable (String),
    `description` Nullable (String),
    `max_amount` Nullable (Int64),
    `registration_amount` Nullable (Int64),
    `plan_base_amount` Nullable (String),
    `is_offer_applicable` Nullable (String),
    `max_credit_limit` Nullable (Int64),
    `free_ride_count` Nullable (Int64),
    `frequency` Nullable (String),
    `cgst_percentage` Nullable (Float64),
    `sgst_percentage` Nullable (Float64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id, payment_mode))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.plan ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.plan_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, plan_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.plan ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.plan_mv
(
	`id` String,
	`merchant_id` String,
	`payment_mode` String,
	`plan_type` String,
	`name` String,
	`description` String,
	`max_amount` Int64,
	`registration_amount` Int64,
	`plan_base_amount` String,
	`is_offer_applicable` String,
	`max_credit_limit` Int64,
	`free_ride_count` Int64,
	`frequency` String,
	`cgst_percentage` Float64,
	`sgst_percentage` Float64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'payment_mode'),'') as payment_mode,
	ifNull(JSONExtractString(message,'plan_type'),'') as plan_type,
	ifNull(JSONExtractString(message,'name'),'') as name,
	ifNull(JSONExtractString(message,'description'),'') as description,
	ifNull(JSONExtractInt(message,'max_amount'), 0) as max_amount,
	ifNull(JSONExtractInt(message,'registration_amount'), 0) as registration_amount,
	ifNull(JSONExtractString(message,'plan_base_amount'),'') as plan_base_amount,
	ifNull(JSONExtractString(message,'is_offer_applicable'),'') as is_offer_applicable,
	ifNull(JSONExtractInt(message,'max_credit_limit'), 0) as max_credit_limit,
	ifNull(JSONExtractInt(message,'free_ride_count'), 0) as free_ride_count,
	ifNull(JSONExtractString(message,'frequency'),'') as frequency,
	ifNull(JSONExtractFloat(message,'cgst_percentage'),0.0) as cgst_percentage,
	ifNull(JSONExtractFloat(message,'sgst_percentage'),0.0) as sgst_percentage,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'PlanObject'
	JSONExtractString(message, 'id') is not null
	JSONExtractString(message, ' payment_mode') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.plan_translation_shard ON CLUSTER `{cluster}`
    (
    `plan_id` String,
    `language` String,
    `name` Nullable (String),
    `description` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (plan_id, language))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.plan_translation ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.plan_translation_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, plan_translation_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.plan_translation ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.plan_translation_mv
(
	`plan_id` String,
	`language` String,
	`name` String,
	`description` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'plan_id'),'') as plan_id,
	ifNull(JSONExtractString(message,'language'),'') as language,
	ifNull(JSONExtractString(message,'name'),'') as name,
	ifNull(JSONExtractString(message,'description'),'') as description,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'PlanTranslationObject'
	JSONExtractString(message, 'plan_id') is not null
	JSONExtractString(message, ' language') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.quote_special_zone_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `search_request_id` Nullable (String),
    `provider_id` Nullable (String),
    `distance` Nullable (Int64),
    `estimated_fare` Nullable (Float64),
    `fare_parameters_id` Nullable (String),
    `estimated_finish_time` DateTime DEFAULT now(),
    `vehicle_variant` Nullable (String),
    `valid_till` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `special_location_tag` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.quote_special_zone ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.quote_special_zone_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, quote_special_zone_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.quote_special_zone ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.quote_special_zone_mv
(
	`id` String,
	`search_request_id` String,
	`provider_id` String,
	`distance` Int64,
	`estimated_fare` Float64,
	`fare_parameters_id` String,
	`estimated_finish_time` DateTime,
	`vehicle_variant` String,
	`valid_till` DateTime,
	`created_at` DateTime,
	`updated_at` DateTime,
	`special_location_tag` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'search_request_id'),'') as search_request_id,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	ifNull(JSONExtractInt(message,'distance'), 0) as distance,
	ifNull(JSONExtractFloat(message,'estimated_fare'),0.0) as estimated_fare,
	ifNull(JSONExtractString(message,'fare_parameters_id'),'') as fare_parameters_id,
	toDateTime(JSONExtractInt(message,'estimated_finish_time')) as estimated_finish_time,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	toDateTime(JSONExtractInt(message,'valid_till')) as valid_till,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'special_location_tag'),'') as special_location_tag,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'QuoteSpecialZoneObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.rating_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `ride_id` Nullable (String),
    `rating_value` Nullable (Int64),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `driver_id` Nullable (String),
    `feedback_details` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.rating ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.rating_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, rating_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.rating ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.rating_mv
(
	`id` String,
	`ride_id` String,
	`rating_value` Int64,
	`created_at` DateTime,
	`updated_at` DateTime,
	`driver_id` String,
	`feedback_details` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'ride_id'),'') as ride_id,
	ifNull(JSONExtractInt(message,'rating_value'), 0) as rating_value,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'feedback_details'),'') as feedback_details,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'RatingObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.registration_token_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `auth_medium` Nullable (String),
    `auth_type` Nullable (String),
    `auth_value_hash` Nullable (String),
    `token` Nullable (String),
    `verified` Nullable (String),
    `auth_expiry` Nullable (Int64),
    `token_expiry` Nullable (Int64),
    `attempts` Nullable (Int64),
    `entity_id` Nullable (String),
    `entity_type` Nullable (String),
    `info` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `alternate_number_attempts` Nullable (Int64),
    `merchant_id` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.registration_token ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.registration_token_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, registration_token_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.registration_token ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.registration_token_mv
(
	`id` String,
	`auth_medium` String,
	`auth_type` String,
	`auth_value_hash` String,
	`token` String,
	`verified` String,
	`auth_expiry` Int64,
	`token_expiry` Int64,
	`attempts` Int64,
	`entity_id` String,
	`entity_type` String,
	`info` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`alternate_number_attempts` Int64,
	`merchant_id` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'auth_medium'),'') as auth_medium,
	ifNull(JSONExtractString(message,'auth_type'),'') as auth_type,
	ifNull(JSONExtractString(message,'auth_value_hash'),'') as auth_value_hash,
	ifNull(JSONExtractString(message,'token'),'') as token,
	ifNull(JSONExtractString(message,'verified'),'') as verified,
	ifNull(JSONExtractInt(message,'auth_expiry'), 0) as auth_expiry,
	ifNull(JSONExtractInt(message,'token_expiry'), 0) as token_expiry,
	ifNull(JSONExtractInt(message,'attempts'), 0) as attempts,
	ifNull(JSONExtractString(message,'entity_id'),'') as entity_id,
	ifNull(JSONExtractString(message,'entity_type'),'') as entity_type,
	ifNull(JSONExtractString(message,'info'),'') as info,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractInt(message,'alternate_number_attempts'), 0) as alternate_number_attempts,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'RegistrationTokenObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.registry_map_fallback_shard ON CLUSTER `{cluster}`
    (
    `subscriber_id` String,
    `unique_id` String,
    `registry_url` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (subscriber_id, unique_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.registry_map_fallback ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.registry_map_fallback_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, registry_map_fallback_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.registry_map_fallback ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.registry_map_fallback_mv
(
	`subscriber_id` String,
	`unique_id` String,
	`registry_url` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'subscriber_id'),'') as subscriber_id,
	ifNull(JSONExtractString(message,'unique_id'),'') as unique_id,
	ifNull(JSONExtractString(message,'registry_url'),'') as registry_url,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'RegistryMapFallbackObject'
	JSONExtractString(message, 'subscriber_id') is not null
	JSONExtractString(message, ' unique_id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.ride_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `booking_id` Nullable (String),
    `short_id` Nullable (String),
    `status` Nullable (String),
    `driver_id` Nullable (String),
    `otp` Nullable (String),
    `tracking_url` Nullable (String),
    `fare` Nullable (String),
    `traveled_distance` Nullable (Float64),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `trip_start_time` DateTime DEFAULT now(),
    `trip_end_time` DateTime DEFAULT now(),
    `chargeable_distance` Nullable (String),
    `trip_start_lat` Nullable (Float64),
    `trip_start_lon` Nullable (Float64),
    `trip_end_lat` Nullable (Float64),
    `trip_end_lon` Nullable (Float64),
    `driver_arrival_time` DateTime DEFAULT now(),
    `fare_parameters_id` Nullable (String),
    `distance_calculation_failed` Nullable (String),
    `pickup_drop_outside_of_threshold` Nullable (String),
    `merchant_id` Nullable (String),
    `number_of_deviation` Nullable (String),
    `driver_deviated_from_route` Nullable (String),
    `number_of_snap_to_road_calls` Nullable (String),
    `driver_go_home_request_id` Nullable (String),
    `ui_distance_calculation_with_accuracy` Nullable (String),
    `ui_distance_calculation_without_accuracy` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `number_of_osrm_snap_to_road_calls` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.ride ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.ride_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, ride_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.ride ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.ride_mv
(
	`id` String,
	`booking_id` String,
	`short_id` String,
	`status` String,
	`driver_id` String,
	`otp` String,
	`tracking_url` String,
	`fare` String,
	`traveled_distance` Float64,
	`created_at` DateTime,
	`updated_at` DateTime,
	`trip_start_time` DateTime,
	`trip_end_time` DateTime,
	`chargeable_distance` String,
	`trip_start_lat` Float64,
	`trip_start_lon` Float64,
	`trip_end_lat` Float64,
	`trip_end_lon` Float64,
	`driver_arrival_time` DateTime,
	`fare_parameters_id` String,
	`distance_calculation_failed` String,
	`pickup_drop_outside_of_threshold` String,
	`merchant_id` String,
	`number_of_deviation` String,
	`driver_deviated_from_route` String,
	`number_of_snap_to_road_calls` String,
	`driver_go_home_request_id` String,
	`ui_distance_calculation_with_accuracy` String,
	`ui_distance_calculation_without_accuracy` String,
	`merchant_operating_city_id` String,
	`number_of_osrm_snap_to_road_calls` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'booking_id'),'') as booking_id,
	ifNull(JSONExtractString(message,'short_id'),'') as short_id,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	ifNull(JSONExtractString(message,'otp'),'') as otp,
	ifNull(JSONExtractString(message,'tracking_url'),'') as tracking_url,
	ifNull(JSONExtractString(message,'fare'),'') as fare,
	ifNull(JSONExtractFloat(message,'traveled_distance'),0.0) as traveled_distance,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	toDateTime(JSONExtractInt(message,'trip_start_time')) as trip_start_time,
	toDateTime(JSONExtractInt(message,'trip_end_time')) as trip_end_time,
	ifNull(JSONExtractString(message,'chargeable_distance'),'') as chargeable_distance,
	ifNull(JSONExtractFloat(message,'trip_start_lat'),0.0) as trip_start_lat,
	ifNull(JSONExtractFloat(message,'trip_start_lon'),0.0) as trip_start_lon,
	ifNull(JSONExtractFloat(message,'trip_end_lat'),0.0) as trip_end_lat,
	ifNull(JSONExtractFloat(message,'trip_end_lon'),0.0) as trip_end_lon,
	toDateTime(JSONExtractInt(message,'driver_arrival_time')) as driver_arrival_time,
	ifNull(JSONExtractString(message,'fare_parameters_id'),'') as fare_parameters_id,
	ifNull(JSONExtractString(message,'distance_calculation_failed'),'') as distance_calculation_failed,
	ifNull(JSONExtractString(message,'pickup_drop_outside_of_threshold'),'') as pickup_drop_outside_of_threshold,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'number_of_deviation'),'') as number_of_deviation,
	ifNull(JSONExtractString(message,'driver_deviated_from_route'),'') as driver_deviated_from_route,
	ifNull(JSONExtractString(message,'number_of_snap_to_road_calls'),'') as number_of_snap_to_road_calls,
	ifNull(JSONExtractString(message,'driver_go_home_request_id'),'') as driver_go_home_request_id,
	ifNull(JSONExtractString(message,'ui_distance_calculation_with_accuracy'),'') as ui_distance_calculation_with_accuracy,
	ifNull(JSONExtractString(message,'ui_distance_calculation_without_accuracy'),'') as ui_distance_calculation_without_accuracy,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,
	ifNull(JSONExtractInt(message,'number_of_osrm_snap_to_road_calls'), 0) as number_of_osrm_snap_to_road_calls,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'RideObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.ride_details_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `driver_name` Nullable (String),
    `driver_number_encrypted` Nullable (String),
    `driver_number_hash` Nullable (String),
    `driver_country_code` Nullable (String),
    `vehicle_number` Nullable (String),
    `vehicle_color` Nullable (String),
    `vehicle_variant` Nullable (String),
    `vehicle_model` Nullable (String),
    `vehicle_class` Nullable (String),
    `fleet_owner_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.ride_details ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.ride_details_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, ride_details_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.ride_details ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.ride_details_mv
(
	`id` String,
	`driver_name` String,
	`driver_number_encrypted` String,
	`driver_number_hash` String,
	`driver_country_code` String,
	`vehicle_number` String,
	`vehicle_color` String,
	`vehicle_variant` String,
	`vehicle_model` String,
	`vehicle_class` String,
	`fleet_owner_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'driver_name'),'') as driver_name,
	ifNull(JSONExtractString(message,'driver_number_encrypted'),'') as driver_number_encrypted,
	ifNull(JSONExtractString(message,'driver_number_hash'),'') as driver_number_hash,
	ifNull(JSONExtractString(message,'driver_country_code'),'') as driver_country_code,
	ifNull(JSONExtractString(message,'vehicle_number'),'') as vehicle_number,
	ifNull(JSONExtractString(message,'vehicle_color'),'') as vehicle_color,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	ifNull(JSONExtractString(message,'vehicle_model'),'') as vehicle_model,
	ifNull(JSONExtractString(message,'vehicle_class'),'') as vehicle_class,
	ifNull(JSONExtractString(message,'fleet_owner_id'),'') as fleet_owner_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'RideDetailsObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.rider_details_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `mobile_country_code` Nullable (String),
    `mobile_number_encrypted` Nullable (String),
    `mobile_number_hash` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `referral_code` Nullable (String),
    `referred_by_driver` Nullable (String),
    `referred_at` DateTime DEFAULT now(),
    `has_taken_valid_ride` Nullable (String),
    `has_taken_valid_ride_at` DateTime DEFAULT now(),
    `merchant_id` Nullable (String),
    `otp_code` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.rider_details ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.rider_details_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, rider_details_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.rider_details ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.rider_details_mv
(
	`id` String,
	`mobile_country_code` String,
	`mobile_number_encrypted` String,
	`mobile_number_hash` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`referral_code` String,
	`referred_by_driver` String,
	`referred_at` DateTime,
	`has_taken_valid_ride` String,
	`has_taken_valid_ride_at` DateTime,
	`merchant_id` String,
	`otp_code` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'mobile_country_code'),'') as mobile_country_code,
	ifNull(JSONExtractString(message,'mobile_number_encrypted'),'') as mobile_number_encrypted,
	ifNull(JSONExtractString(message,'mobile_number_hash'),'') as mobile_number_hash,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'referral_code'),'') as referral_code,
	ifNull(JSONExtractString(message,'referred_by_driver'),'') as referred_by_driver,
	toDateTime(JSONExtractInt(message,'referred_at')) as referred_at,
	ifNull(JSONExtractString(message,'has_taken_valid_ride'),'') as has_taken_valid_ride,
	toDateTime(JSONExtractInt(message,'has_taken_valid_ride_at')) as has_taken_valid_ride_at,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'otp_code'),'') as otp_code,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'RiderDetailsObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.scheduler_job_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `job_type` Nullable (String),
    `job_data` Nullable (String),
    `scheduled_at` DateTime DEFAULT now(),
    `maximum_delay` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `max_errors` Nullable (Int64),
    `curr_errors` Nullable (Int64),
    `status` Nullable (String),
    `shard_id` Nullable (String),
    `parent_job_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.scheduler_job ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.scheduler_job_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, scheduler_job_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.scheduler_job ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.scheduler_job_mv
(
	`id` String,
	`job_type` String,
	`job_data` String,
	`scheduled_at` DateTime,
	`maximum_delay` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`max_errors` Int64,
	`curr_errors` Int64,
	`status` String,
	`shard_id` String,
	`parent_job_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'job_type'),'') as job_type,
	ifNull(JSONExtractString(message,'job_data'),'') as job_data,
	toDateTime(JSONExtractInt(message,'scheduled_at')) as scheduled_at,
	ifNull(JSONExtractString(message,'maximum_delay'),'') as maximum_delay,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractInt(message,'max_errors'), 0) as max_errors,
	ifNull(JSONExtractInt(message,'curr_errors'), 0) as curr_errors,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractString(message,'shard_id'),'') as shard_id,
	ifNull(JSONExtractString(message,'parent_job_id'),'') as parent_job_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'SchedulerJobObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.schema_migrations_shard ON CLUSTER `{cluster}`
    (
    `filename` Nullable (String),
    `checksum` Nullable (String),
    `executed_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.schema_migrations ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.schema_migrations_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, schema_migrations_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.schema_migrations ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.schema_migrations_mv
(
	`filename` String,
	`checksum` String,
	`executed_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'filename'),'') as filename,
	ifNull(JSONExtractString(message,'checksum'),'') as checksum,
	toDateTime(JSONExtractInt(message,'executed_at')) as executed_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'SchemaMigrationsObject'


CREATE TABLE atlas_driver_offer_bpp_helper.search_request_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `transaction_id` Nullable (String),
    `provider_id` Nullable (String),
    `from_location_id` Nullable (String),
    `to_location_id` Nullable (String),
    `bap_id` Nullable (String),
    `bap_uri` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `estimated_duration` Nullable (Int64),
    `estimated_distance` Nullable (Int64),
    `auto_assign_enabled` Nullable (String),
    `device` Nullable (String),
    `customer_language` Nullable (String),
    `special_location_tag` Nullable (String),
    `area` Nullable (String),
    `bap_city` Nullable (String),
    `bap_country` Nullable (String),
    `disability_tag` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.search_request ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.search_request_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, search_request_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.search_request ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.search_request_mv
(
	`id` String,
	`transaction_id` String,
	`provider_id` String,
	`from_location_id` String,
	`to_location_id` String,
	`bap_id` String,
	`bap_uri` String,
	`created_at` DateTime,
	`estimated_duration` Int64,
	`estimated_distance` Int64,
	`auto_assign_enabled` String,
	`device` String,
	`customer_language` String,
	`special_location_tag` String,
	`area` String,
	`bap_city` String,
	`bap_country` String,
	`disability_tag` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'transaction_id'),'') as transaction_id,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	ifNull(JSONExtractString(message,'from_location_id'),'') as from_location_id,
	ifNull(JSONExtractString(message,'to_location_id'),'') as to_location_id,
	ifNull(JSONExtractString(message,'bap_id'),'') as bap_id,
	ifNull(JSONExtractString(message,'bap_uri'),'') as bap_uri,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	ifNull(JSONExtractInt(message,'estimated_duration'), 0) as estimated_duration,
	ifNull(JSONExtractInt(message,'estimated_distance'), 0) as estimated_distance,
	ifNull(JSONExtractString(message,'auto_assign_enabled'),'') as auto_assign_enabled,
	ifNull(JSONExtractString(message,'device'),'') as device,
	ifNull(JSONExtractString(message,'customer_language'),'') as customer_language,
	ifNull(JSONExtractString(message,'special_location_tag'),'') as special_location_tag,
	ifNull(JSONExtractString(message,'area'),'') as area,
	ifNull(JSONExtractString(message,'bap_city'),'') as bap_city,
	ifNull(JSONExtractString(message,'bap_country'),'') as bap_country,
	ifNull(JSONExtractString(message,'disability_tag'),'') as disability_tag,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'SearchRequestObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.search_request_for_driver_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `search_request_id` Nullable (String),
    `actual_distance_to_pickup` Nullable (Int64),
    `duration_to_pickup` Nullable (Int64),
    `vehicle_variant` Nullable (String),
    `search_request_valid_till` DateTime DEFAULT now(),
    `driver_id` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `start_time` DateTime DEFAULT now(),
    `status` Nullable (String),
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `straight_line_distance_to_pickup` Nullable (Int64),
    `response` Nullable (String),
    `driver_min_extra_fee` Nullable (Float64),
    `driver_max_extra_fee` Nullable (Float64),
    `batch_number` Nullable (String),
    `ride_request_popup_delay_duration` Nullable (Int64),
    `parallel_search_request_count` Nullable (String),
    `is_part_of_intelligent_pool` Nullable (String),
    `cancellation_ratio` Nullable (String),
    `acceptance_ratio` Nullable (String),
    `driver_available_time` Nullable (String),
    `driver_speed` Nullable (Float64),
    `mode` Nullable (String),
    `search_try_id` Nullable (String),
    `keep_hidden_for_seconds` Nullable (Int64),
    `merchant_id` Nullable (String),
    `go_home_request_id` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.search_request_for_driver ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.search_request_for_driver_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, search_request_for_driver_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.search_request_for_driver ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.search_request_for_driver_mv
(
	`id` String,
	`search_request_id` String,
	`actual_distance_to_pickup` Int64,
	`duration_to_pickup` Int64,
	`vehicle_variant` String,
	`search_request_valid_till` DateTime,
	`driver_id` String,
	`created_at` DateTime,
	`start_time` DateTime,
	`status` String,
	`lat` Float64,
	`lon` Float64,
	`straight_line_distance_to_pickup` Int64,
	`response` String,
	`driver_min_extra_fee` Float64,
	`driver_max_extra_fee` Float64,
	`batch_number` String,
	`ride_request_popup_delay_duration` Int64,
	`parallel_search_request_count` String,
	`is_part_of_intelligent_pool` String,
	`cancellation_ratio` String,
	`acceptance_ratio` String,
	`driver_available_time` String,
	`driver_speed` Float64,
	`mode` String,
	`search_try_id` String,
	`keep_hidden_for_seconds` Int64,
	`merchant_id` String,
	`go_home_request_id` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'search_request_id'),'') as search_request_id,
	ifNull(JSONExtractInt(message,'actual_distance_to_pickup'), 0) as actual_distance_to_pickup,
	ifNull(JSONExtractInt(message,'duration_to_pickup'), 0) as duration_to_pickup,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	toDateTime(JSONExtractInt(message,'search_request_valid_till')) as search_request_valid_till,
	ifNull(JSONExtractString(message,'driver_id'),'') as driver_id,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'start_time')) as start_time,
	ifNull(JSONExtractString(message,'status'),'') as status,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractInt(message,'straight_line_distance_to_pickup'), 0) as straight_line_distance_to_pickup,
	ifNull(JSONExtractString(message,'response'),'') as response,
	ifNull(JSONExtractFloat(message,'driver_min_extra_fee'),0.0) as driver_min_extra_fee,
	ifNull(JSONExtractFloat(message,'driver_max_extra_fee'),0.0) as driver_max_extra_fee,
	ifNull(JSONExtractString(message,'batch_number'),'') as batch_number,
	ifNull(JSONExtractInt(message,'ride_request_popup_delay_duration'), 0) as ride_request_popup_delay_duration,
	ifNull(JSONExtractString(message,'parallel_search_request_count'),'') as parallel_search_request_count,
	ifNull(JSONExtractString(message,'is_part_of_intelligent_pool'),'') as is_part_of_intelligent_pool,
	ifNull(JSONExtractString(message,'cancellation_ratio'),'') as cancellation_ratio,
	ifNull(JSONExtractString(message,'acceptance_ratio'),'') as acceptance_ratio,
	ifNull(JSONExtractString(message,'driver_available_time'),'') as driver_available_time,
	ifNull(JSONExtractFloat(message,'driver_speed'),0.0) as driver_speed,
	ifNull(JSONExtractString(message,'mode'),'') as mode,
	ifNull(JSONExtractString(message,'search_try_id'),'') as search_try_id,
	ifNull(JSONExtractInt(message,'keep_hidden_for_seconds'), 0) as keep_hidden_for_seconds,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'go_home_request_id'),'') as go_home_request_id,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'SearchRequestForDriverObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.search_request_location_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `lat` Nullable (Float64),
    `lon` Nullable (Float64),
    `city` Nullable (String),
    `state` Nullable (String),
    `country` Nullable (String),
    `street` Nullable (String),
    `building` Nullable (String),
    `area_code` Nullable (String),
    `area` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `full_address` Nullable (String),
    `door` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.search_request_location ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.search_request_location_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, search_request_location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.search_request_location ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.search_request_location_mv
(
	`id` String,
	`lat` Float64,
	`lon` Float64,
	`city` String,
	`state` String,
	`country` String,
	`street` String,
	`building` String,
	`area_code` String,
	`area` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`full_address` String,
	`door` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractFloat(message,'lat'),0.0) as lat,
	ifNull(JSONExtractFloat(message,'lon'),0.0) as lon,
	ifNull(JSONExtractString(message,'city'),'') as city,
	ifNull(JSONExtractString(message,'state'),'') as state,
	ifNull(JSONExtractString(message,'country'),'') as country,
	ifNull(JSONExtractString(message,'street'),'') as street,
	ifNull(JSONExtractString(message,'building'),'') as building,
	ifNull(JSONExtractString(message,'area_code'),'') as area_code,
	ifNull(JSONExtractString(message,'area'),'') as area,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'full_address'),'') as full_address,
	ifNull(JSONExtractString(message,'door'),'') as door,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'SearchRequestLocationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.search_request_special_zone_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `transaction_id` Nullable (String),
    `message_id` Nullable (String),
    `start_time` DateTime DEFAULT now(),
    `valid_till` DateTime DEFAULT now(),
    `provider_id` Nullable (String),
    `from_location_id` Nullable (String),
    `to_location_id` Nullable (String),
    `bap_id` Nullable (String),
    `bap_uri` Nullable (String),
    `estimated_duration` Nullable (String),
    `estimated_distance` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `area` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.search_request_special_zone ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.search_request_special_zone_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, search_request_special_zone_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.search_request_special_zone ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.search_request_special_zone_mv
(
	`id` String,
	`transaction_id` String,
	`message_id` String,
	`start_time` DateTime,
	`valid_till` DateTime,
	`provider_id` String,
	`from_location_id` String,
	`to_location_id` String,
	`bap_id` String,
	`bap_uri` String,
	`estimated_duration` String,
	`estimated_distance` String,
	`created_at` DateTime,
	`updated_at` DateTime,
	`area` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'transaction_id'),'') as transaction_id,
	ifNull(JSONExtractString(message,'message_id'),'') as message_id,
	toDateTime(JSONExtractInt(message,'start_time')) as start_time,
	toDateTime(JSONExtractInt(message,'valid_till')) as valid_till,
	ifNull(JSONExtractString(message,'provider_id'),'') as provider_id,
	ifNull(JSONExtractString(message,'from_location_id'),'') as from_location_id,
	ifNull(JSONExtractString(message,'to_location_id'),'') as to_location_id,
	ifNull(JSONExtractString(message,'bap_id'),'') as bap_id,
	ifNull(JSONExtractString(message,'bap_uri'),'') as bap_uri,
	ifNull(JSONExtractString(message,'estimated_duration'),'') as estimated_duration,
	ifNull(JSONExtractString(message,'estimated_distance'),'') as estimated_distance,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'area'),'') as area,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'SearchRequestSpecialZoneObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.search_try_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `message_id` Nullable (String),
    `valid_till` DateTime DEFAULT now(),
    `created_at` DateTime DEFAULT now(),
    `start_time` DateTime DEFAULT now(),
    `vehicle_variant` Nullable (String),
    `status` Nullable (String),
    `updated_at` DateTime DEFAULT now(),
    `search_repeat_counter` Nullable (String),
    `customer_extra_fee` Nullable (String),
    `estimate_id` Nullable (String),
    `request_id` Nullable (String),
    `search_repeat_type` Nullable (String),
    `base_fare` Nullable (Int64),
    `merchant_id` Nullable (String),
    `merchant_operating_city_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.search_try ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.search_try_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, search_try_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.search_try ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.search_try_mv
(
	`id` String,
	`message_id` String,
	`valid_till` DateTime,
	`created_at` DateTime,
	`start_time` DateTime,
	`vehicle_variant` String,
	`status` String,
	`updated_at` DateTime,
	`search_repeat_counter` String,
	`customer_extra_fee` String,
	`estimate_id` String,
	`request_id` String,
	`search_repeat_type` String,
	`base_fare` Int64,
	`merchant_id` String,
	`merchant_operating_city_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'message_id'),'') as message_id,
	toDateTime(JSONExtractInt(message,'valid_till')) as valid_till,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,
	toDateTime(JSONExtractInt(message,'start_time')) as start_time,
	ifNull(JSONExtractString(message,'vehicle_variant'),'') as vehicle_variant,
	ifNull(JSONExtractString(message,'status'),'') as status,
	toDateTime(JSONExtractInt(message,'updated_at')) as updated_at,
	ifNull(JSONExtractString(message,'search_repeat_counter'),'') as search_repeat_counter,
	ifNull(JSONExtractString(message,'customer_extra_fee'),'') as customer_extra_fee,
	ifNull(JSONExtractString(message,'estimate_id'),'') as estimate_id,
	ifNull(JSONExtractString(message,'request_id'),'') as request_id,
	ifNull(JSONExtractString(message,'search_repeat_type'),'') as search_repeat_type,
	ifNull(JSONExtractInt(message,'base_fare'), 0) as base_fare,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'merchant_operating_city_id'),'') as merchant_operating_city_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'SearchTryObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.special_location_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `location_name` Nullable (String),
    `category` Nullable (String),
    `gates` Nullable (Array(String)),
    `geom` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.special_location ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.special_location_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, special_location_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.special_location ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.special_location_mv
(
	`id` String,
	`location_name` String,
	`category` String,
	`gates` Array(String),
	`geom` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'location_name'),'') as location_name,
	ifNull(JSONExtractString(message,'category'),'') as category,
	toDateTime(JSONExtractInt(message,'gates')) as gates,
	ifNull(JSONExtractString(message,'geom'),'') as geom,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'SpecialLocationObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.special_location_priority_shard ON CLUSTER `{cluster}`
    (
    `id` String,
    `merchant_id` Nullable (String),
    `category` Nullable (String),
    `pickup_priority` Nullable (Int64),
    `drop_priority` Nullable (Int64),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.special_location_priority ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.special_location_priority_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, special_location_priority_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.special_location_priority ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.special_location_priority_mv
(
	`id` String,
	`merchant_id` String,
	`category` String,
	`pickup_priority` Int64,
	`drop_priority` Int64,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,
	ifNull(JSONExtractString(message,'category'),'') as category,
	ifNull(JSONExtractInt(message,'pickup_priority'), 0) as pickup_priority,
	ifNull(JSONExtractInt(message,'drop_priority'), 0) as drop_priority,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'SpecialLocationPriorityObject'
	JSONExtractString(message, 'id') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.tag_category_mapping_shard ON CLUSTER `{cluster}`
    (
    `id` Nullable (String),
    `tag` String,
    `category` Nullable (String),
    `created_at` DateTime DEFAULT now(),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (tag))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.tag_category_mapping ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.tag_category_mapping_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, tag_category_mapping_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.tag_category_mapping ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.tag_category_mapping_mv
(
	`id` String,
	`tag` String,
	`category` String,
	`created_at` DateTime,
)
	AS SELECT
	ifNull(JSONExtractString(message,'id'),'') as id,
	ifNull(JSONExtractString(message,'tag'),'') as tag,
	ifNull(JSONExtractString(message,'category'),'') as category,
	toDateTime(JSONExtractInt(message,'created_at')) as created_at,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'TagCategoryMappingObject'
	JSONExtractString(message, 'tag') is not null


CREATE TABLE atlas_driver_offer_bpp_helper.transporter_config_shard ON CLUSTER `{cluster}`
    (
    `merchant_id` Nullable (String),
    `date` DateTime DEFAULT now()
	)

ENGINE = ReplicatedReplacingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', date)
PARTITION BY toStartOfWeek(created_at)
PRIMARY KEY (created_at)
ORDER BY (created_at, (merchant_operating_city_id))
TTL created_at + toIntervalDay(365)
SETTINGS index_granularity = 8192

CREATE TABLE atlas_driver_offer_bpp.transporter_config ON CLUSTER `{cluster}` AS atlas_driver_offer_bpp_helper.transporter_config_shard
ENGINE = Distributed(`{cluster}`, atlas_driver_offer_bpp_helper, transporter_config_shard, xxHash32(id))

CREATE MATERIALIZED VIEW atlas_driver_offer_bpp.transporter_config ON CLUSTER `{cluster}` TO atlas_driver_offer_bpp.transporter_config_mv
(
	`merchant_id` String,
)
	AS SELECT
	ifNull(JSONExtractString(message,'merchant_id'),'') as merchant_id,

	FROM atlas_driver_offer_bpp.bap_main_queue
	where JSONExtractString(message,'tag') = 'TransporterConfigObject'
	JSONExtractString(message, 'merchant_operating_city_id') is not null



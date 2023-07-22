#!/bin/bash

driver_tables="search_request_for_driver estimate geometry registration_token search_try driver_fee driver_referral driver_stats driver_location exophone quote_special_zone ride_details fare_policy onboarding_document_configs meta_data fare_product leader_board_configs person booking_cancellation_reason driver_quote driver_information cancellation_reason merchant rider_details call_status business_event fare_parameters vehicle bap_metadata booking search_request rating search_request_special_zone media_file place_name_cache search_request_location fare_parameters_progressive_details fare_parameters_slab_details booking_location driver_flow_status ride message_translation message_report message fare_policy_progressive_details fare_policy_driver_extra_fee_bounds restricted_extra_fare fare_policy_slabs_details_slab fare_policy_progressive_details_per_extra_km_rate_section issue_option issue_category comment issue_report issue_translation driver_license driver_rc_association aadhaar_otp_req aadhaar_verification image vehicle_registration_certificate aadhaar_otp_verify idfy_verification operating_city driver_intelligent_pool_config transporter_config merchant_service_usage_config onboarding_document_configs merchant_service_config leader_board_configs merchant_payment_method driver_pool_config merchant_message"
rider_tables="saved_location estimate geometry callback_request registration_token on_search_event sos webengage exophone merchant_config ride special_zone_quote black_list_org issue rental_slab person booking_cancellation_reason cancellation_reason app_installs merchant call_status estimate_breakup trip_terms driver_offer booking search_request quote directions_cache place_name_cache search_request_location booking_location person_default_emergency_number person_flow_status payment_transaction payment_order fare_breakup merchant_service_usage_config merchant_service_config merchant_payment_method merchant_message "

# driver_tables="person ride"

# Connection details here
host="localhost"
username="postgres"
dbname="atlas_dev"
password="root"
schemaName="atlas_app"

export PGPASSWORD="$password"

# for table in $driver_tables; do
#     psql "host=$host port=5434 dbname=$dbname user=$username" -c "\copy (SELECT * FROM $schemaName.$table LIMIT 1) TO STDOUT WITH (FORMAT csv , HEADER);" > "dumps/$table.csv"
#     echo "Dumped $table"
# done

for table in $rider_tables; do
    psql --host localhost --username postgres --dbname atlas_dev --port 5434 -c "\copy $schemaName.person  FROM 'Users/vijay.gupta/Desktop/rdumps/$table.csv' DELIMITER ',' CSV HEADER;"
    echo "created entries from  $table"
done

unset PGPASSWORD

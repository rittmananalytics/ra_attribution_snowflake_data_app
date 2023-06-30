create schema ra_development.attribution_data_app

create or replace table ra_development.attribution_data_app.ga4_daily_export(v variant);

copy into "RA_DEVELOPMENT"."ATTRIBUTION_DATA_APP"."GA4_DAILY_EXPORT"
from @stage_ga4_export pattern='daily/.*' file_format = (type='PARQUET');
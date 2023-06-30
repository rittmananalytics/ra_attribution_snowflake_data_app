CREATE STORAGE INTEGRATION integration_ga4_export
TYPE = EXTERNAL_STAGE
STORAGE_PROVIDER = 'GCS'
ENABLED = TRUE
STORAGE_ALLOWED_LOCATIONS = ('gcs://ga4_exports/');
    
show integrations;
    
create stage stage_ga4_export
url = 'gcs://ga4_exports/'
storage_integration = integration_ga4_export
file_format = (type = parquet);


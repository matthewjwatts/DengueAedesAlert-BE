# Project Notes: Aedes-Dengue-Belgium-Dashboard

### How to deploy project locally

renv::init()

## *Aedes* surveillance data notes

-   Note that column names should remain consistent and no spaces in column names. This can cause IT and programming related issues.

-   Data should be presented in long format, one row per observation.

-   A unique ID must be generated per observation, this is to ensure that when we update data in the dashboard dataset, we do not upload it more than once.

-   Note that rather than supplying Year and Month of detection, supply a "Detection_date" column.

-   Combination of Latitude, Longitude and Detection_date. All symbols removed, combination of numerical data only.

    ### Data file format:

| Observation_ID | Site_codes | Municipality | Postcode                  | Latitude             | Longitude            | Site_type     | Detection_date       |
|---------|---------|---------|---------|---------|---------|---------|---------|
| Numeric        | String     | String       | Four-digit numerical code | Decimal format, between -90 and 90 degrees | Decimal format between -180 and 180 degrees | String format | `YYYY-MM-DD` format |

## *Dengue* imports surveillance data notes:

There ares ome inconsistencies in this dataset, including labeling of source countries and postcodes in Belgium.

[Solution]{.underline}

-   Choose one language to track information (at the moment there is a list of dutch and english)

-   Choose standard 4 digit Belgium postcode format (see list at end of document)

-   Label source countries (choose a standard naming convention)

    Note there should be no spaces in column names, this can cause IT and programming related issues later.

### Data file format:

| Sample_ID | Source_country | Postcode       | Report_date                 |
|-----------|---------|-------------------|----------------------|
| Numeric   | String  | Numeric, 4 digits | `YYYY-MM-DD` format. |

## Post code / shape file data notes:

Recommended naming conventions:

List of post codes: <https://www.geo.be/catalog/details/9738c7c0-5255-11ea-8895-34e12d0f0423?l=en>.

Country names: It could be useful to start tracking regional information, consider using this naming convention

<https://unstats.un.org/unsd/methodology/m49/>

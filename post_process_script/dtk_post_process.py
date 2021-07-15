#!/usr/bin/python

from __future__ import print_function

import json
import os
import math
import numpy as np
import pandas as pd
import time


class UndefinedChannelException(Exception):
    pass


class UnknownSourceException(Exception):
    pass


by_age_and_gender_filename = "ReportHIVByAgeAndGender.csv"
relationships_at_infection_filename = 'ReportRelationshipsAtInfection.csv'

AGGREGATED_NODE = 0  # reserved node number for aggregated aka 'National' processing

OUTPUT_DIRECTORY = 'output'

MALE = 0
FEMALE = 1
BOTH = 2
GENDER_MAP = {'Male': MALE, 'Female': FEMALE, 'Both': BOTH}  # str to int
GENDER_MAP_SHORT = {'M': MALE, 'F': FEMALE, 'Both': BOTH}  # str to int
GENDER_STR = {k: v for v, k in GENDER_MAP.items()}  # int to str

PREVALENCE = 'prevalence'
INCIDENCE = 'incidence'
RELATIONSHIP = 'relationship'


# load file post_channel_config.json
ref_config = json.load(open(os.path.join("Assets", "post_channel_config.json"), 'rb'))


def get_year(channel):
    return ref_config[channel]['Year']


def get_age_bin(channel):
    """
    tuple(15, 20) got written to [15, 20] in file post_channel_config.json
    here we just change [15, 20] back to (15, 20)
    """

    age_bins = ref_config[channel]['AgeBin']
    age_bins = [(g[0], g[1]) for g in age_bins]
    return age_bins


def get_gender(channel):
    genders = ref_config[channel]['Gender']
    genders = [GENDER_MAP[g] for g in genders]
    return genders


def get_reference_channels():
    return list(ref_config.keys())


def get_reports(data, report_source):
    channels_ref = get_reference_channels()

    # add details for each channel
    entries = []

    first_year = int(math.ceil(data.Year.min()))
    last_prevalence_year = int(data.Year.max())
    last_incidence_year = last_prevalence_year - 1

    if report_source == 'ReportHIVByAgeAndGender':
        channel = 'Prevalence'
        if channel in channels_ref:
            entry = {'Name': channel,
                     'Type': PREVALENCE,
                     'Year': set(list(range(first_year, last_prevalence_year + 1)) + get_year(channel=channel)),
                     'AgeBins': get_age_bin(channel=channel),
                     'Gender': get_gender(channel=channel),
                     'ByNode': 'Both',
                     'Map': lambda rows: rows.sum(),
                     'Reduce': lambda row: float(row.Infected) / float(row.Population) if row.Population > 0 else 0}
            entries.append(entry)

        channel = 'Population'
        if channel in channels_ref:
            entry = {'Name': channel,
                     'Type': PREVALENCE,
                     'Year': set(list(range(first_year, last_prevalence_year + 1)) + get_year(channel=channel)),
                     'AgeBins': get_age_bin(channel=channel),
                     'Gender': get_gender(channel=channel),
                     'ByNode': 'Both',
                     'Map': lambda rows: rows.sum(),
                     'Reduce': lambda row: row.Population}
            entries.append(entry)

        channel = 'OnART'
        if channel in channels_ref:
            entry = {'Name': channel,
                     'Type': PREVALENCE,
                     'Year': set([x + 0.5 for x in range(2000,   last_prevalence_year)] + get_year(channel=channel)),
                     'AgeBins': get_age_bin(channel=channel),
                     'Gender': get_gender(channel=channel),
                     'ByNode': 'Both',
                     'Map': lambda rows: rows.sum(),
                     'Reduce': lambda row: row.On_ART}
            entries.append(entry)

        # channel = 'Incidence'
        # if channel in channels_ref:
        #     entry = {'Name': channel,
        #              'Type': INCIDENCE,
        #              'Year': set(list(range(first_year, last_incidence_year + 1)) + get_year(channel=channel)),  # must be integers
        #              'AgeBins': get_age_bin(channel=channel),
        #              'Gender': get_gender(channel=channel),
        #              'ByNode': 'Both',
        #              'Map': lambda rows: rows.sum(),
        #              'Reduce': compute_incidence}
        #     entries.append(entry)

        # channel = 'ARTCoverage'
        # if channel in channels_ref:
        #     entry = {'Name': channel,
        #              'Type': PREVALENCE,
        #              'Year': set(list(range(first_year, last_prevalence_year + 1)) + get_year(channel=channel)),
        #              'AgeBins': get_age_bin(channel=channel),
        #              'Gender': get_gender(channel=channel),
        #              'ByNode': 'Both',
        #              'Map': lambda rows: rows.sum(),
        #              'Reduce': lambda row: float(row.On_ART) / float(row.Infected) if row.Infected > 0 else 0}
        #     entries.append(entry)

        channel = 'PrevDiscordMalePositive'
        if channel in channels_ref:
            entry = {'Name': channel,
                     'Type': PREVALENCE,
                     'Year': set(get_year(channel=channel)),
                     'AgeBins': get_age_bin(channel=channel),
                     'Gender': get_gender(channel=channel),
                     'ByNode': True,
                     'Map': PrevDiscordMalePositive_mapper, #lambda rows: rows.sum(),
                     'Reduce': compute_prevalence_serodiscordancy}
            entries.append(entry)

        channel = 'PrevDiscordFemalePositive'
        if channel in channels_ref:
            entry = {'Name': channel,
                     'Type': PREVALENCE,
                     'Year': set(get_year(channel=channel)),
                     'AgeBins': get_age_bin(channel=channel),
                     'Gender': get_gender(channel=channel),
                     'ByNode': 'Both',
                     'Map': PrevDiscordFemalePositive_mapper,
                     'Reduce': compute_prevalence_serodiscordancy}
            entries.append(entry)

    elif report_source == 'ReportRelationshipsAtInfection':
        channel = 'ExternalStableDiscordantConversionRate'
        if channel in channels_ref:
            entry = {'Name': channel,
                     'Type': RELATIONSHIP,
                     'Year': set(get_year(channel=channel)),
                     'AgeBins': get_age_bin(channel=channel),
                     'Gender': get_gender(channel=channel),
                     'ByNode': 'Both',
                     'Map': lambda rows: rows.sum(),
                     'Reduce': compute_external_relationship_infection_rate}
            entries.append(entry)
    else:
        raise UnknownSourceException(f'Unknown report data source: {report_source}')

    return entries


def compute_external_relationship_infection_rate(row):
    """
    Computes the conversion rate of discordant, stable relationships to concordancy by an external source
    :param row: a sim data row
    :return: the given conversion rate for one sim row of data
    """
    total_stable_infections = row.n_external_infections + row.n_internal_infections
    if total_stable_infections > 0:
        rate = row.n_external_infections / total_stable_infections
    else:
        rate = 0
    return rate


def verify_all_channels_supported(channel_sets):
    channels_ref = get_reference_channels()
    channels_supported = [channel['Name'] for channel_set in channel_sets for channel in channel_set]
    channels_not_supported = list(set(channels_ref) - set(channels_supported))
    if(len(channels_not_supported)) > 0:
        raise UndefinedChannelException('No definition for how to post-process channel: %s' % channels_not_supported[0])


def PrevDiscordMalePositive_mapper(rows):
    value = PrevDiscord_mapper(rows=rows, gender_positive=MALE)
    return value


def PrevDiscordFemalePositive_mapper(rows):
    value = PrevDiscord_mapper(rows=rows, gender_positive=FEMALE)
    return value


def PrevDiscord_mapper(rows, gender_positive):
    # first pre-filter the incoming rows based on HasHIV status (not a stratifier, but a critical selection criteria)
    if gender_positive is MALE:
        has_hiv_selector = 1
    elif gender_positive is FEMALE:
        has_hiv_selector = 0
    else:
        raise Exception('Unable to process discordant relationship prevalence for gender: %s' % gender_positive)
    rows = rows[rows['HasHIV'] == has_hiv_selector]
    return rows.sum()


def compute_incidence(row):
    newly_infected_annualized = float(row.newly_infected_annualized)
    if row.Population > 0:
        incidence = newly_infected_annualized / float(row.Population - row.Infected - row.Newly_Infected)
    else:
        incidence = 0
    print('%s = %s / (%s - %s - %s)' % (incidence, newly_infected_annualized, row.Population, row.Infected, row.Newly_Infected))
    return incidence


def compute_prevalence_serodiscordancy(row):
    num_marital = float(row.Num_MARITAL)
    num_discordant = num_marital - float(row.Num_Concordant_MARITAL)
    if num_marital > 0:
        prev_discordancy = num_discordant/num_marital
    else:
        prev_discordancy = 0
    return prev_discordancy


def add_year_in(df):
    """
    Determine the year a particular date/timeframe occurred in (assumes tail-end dates).
    For use in 'E'-type sheets.
    """
    year_in = np.ceil(df['Year']) - 1
    return df.assign(year_in=year_in)


def preprocess_for_incidence(all_data):
    input_stratifiers = ['Year', 'NodeId', 'Gender', 'Age', 'IsCircumcised', 'IP_Key:Risk']
    grouping_stratifiers = ['NodeId', 'Gender', 'Age', 'year_in', 'IsCircumcised', 'IP_Key:Risk']

    # add the year each row is in
    data = add_year_in(all_data)
    data = data[list(set(input_stratifiers + ['year_in', 'Year', 'Newly_Infected', 'Infected', 'Population']))]

    # yearly incidence count, reported on 0.5 year to line up with the data it will be combined with in a calculation
    summed = data.drop('Year', axis=1)
    summed = summed.groupby(grouping_stratifiers).sum().reset_index()[grouping_stratifiers + ['Newly_Infected']]
    summed = summed.assign(year_in=summed['year_in'] + 0.5)
    summed = summed.rename(columns={'year_in': 'Year', 'Newly_Infected': 'newly_infected_annualized'})

    # merge into original dataframe
    summed = summed.set_index(input_stratifiers)
    data = data.set_index(input_stratifiers)
    data = data.merge(summed, how='inner', left_index=True, right_index=True).reset_index()

    # convert back to integer year; this is needed to report results on integer years as requested
    data = data.drop('Year', axis=1)
    data = data.rename(columns={'year_in': 'Year'})

    # drop circum. column; only because we added it
    data = data.drop('IsCircumcised', axis=1)
    data = data.drop('IP_Key:Risk', axis=1)
    return data


def get_blank_dataframe():
    output_stratifiers = ['Year', 'Node', 'Gender', 'AgeBin']
    output = pd.DataFrame(columns=(output_stratifiers + ['Result']))
    return output


def process_nodes(data, output, year, gender, min_age, max_age, report, node_ids, allow_incomplete=False):
    print('channel: %s year: %s node_ids: %s' % (report['Name'], year, node_ids))
    for node_id in node_ids:
        # determine which data lines need to be included in this calculation
        conditions = (data.Year == year) & (data.Age >= min_age) & (data.Age < max_age)
        if gender != BOTH:
            conditions = conditions & (data.Gender == gender)
        if node_id != AGGREGATED_NODE:
            conditions = conditions & (data.NodeId == node_id)

        rows = data[conditions]

        if len(rows) == 0 and not allow_incomplete:
            raise Exception('Channel %s data is empty for year: %s. Something went wrong, ask a developer.' %
                            (report['Name'], year))

        mapping = report['Map'](rows)
        try:
            result = report['Reduce'](mapping)
        except AttributeError:
            print(' -- FAILED!')
            return output
        new_row = {'Year': year, 'Node': node_id, 'Gender': GENDER_STR[gender], 'AgeBin': '[%d:%d)' % (min_age, max_age), 'Result': result}
        output = output.append(new_row, ignore_index=True)
    return output


def preprocess_for_relationship(df):
    # There is no AgeBin in this data; add ALL_AGES to every row... or NOT.
    # The Year column is floating point. Change to integer (sum by year?)
    stratifiers = ['Year', 'NodeId', 'Age', 'Gender']

    # convert 'Year' from a floating point (by month) to integer
    data = add_year_in(df=df)
    data = data.drop('Year', axis=1)
    data = data.rename(columns={'year_in': 'Year'})

    # data column name normalization
    data = data.rename(columns={'InfectedAgeYears': 'Age'})
    data = data.rename(columns={'InfectedGender': 'Gender'})

    # normalize the genders from M/F/B -> 0/1/2
    avail_genders = data['Gender'].unique()
    for gender in avail_genders:
        data.loc[data['Gender'] == gender, 'Gender'] = GENDER_MAP_SHORT[gender]

    # trim off any unnecessary data
    data = data[list(set(stratifiers + ['WasInMaritalRelationship', 'WasInfectedByMaritalPartner']))]

    # partition the number of hiv status conversions in discordant stable relationships and add to the dataframe

    # in stable relationship, infected by stable partner
    conditions = (data['WasInMaritalRelationship'] == 1) & (data['WasInfectedByMaritalPartner'] == 1)
    n_internal_infected = data[conditions].groupby(stratifiers).size().reset_index()\
        .rename(columns={0: 'n_internal_infections'}).set_index(['Year', 'NodeId', 'Age', 'Gender'])

    # in stable relationship, infected by other than stable partner
    conditions = (data['WasInMaritalRelationship'] == 1) & (data['WasInfectedByMaritalPartner'] == 0)
    n_external_infected = data[conditions].groupby(stratifiers).size().reset_index()\
        .rename(columns={0: 'n_external_infections'}).set_index(['Year', 'NodeId', 'Age', 'Gender'])

    annual_data = n_internal_infected.merge(n_external_infected,
                                            how='outer', left_index=True, right_index=True).reset_index()
    return annual_data


def process_report(report, all_data, node_ids):
    # preprocessing for incidence types
    allow_incomplete = False
    if report['Type'] == INCIDENCE:
        data = preprocess_for_incidence(all_data)
    elif report['Type'] == RELATIONSHIP:
        data = preprocess_for_relationship(all_data)
        allow_incomplete = True  # data was constructed; it is likely not year/node/gender/agebin complete
    else:
        data = all_data

    # determine which type(s) of processing to do
    process_by_node = True if report['ByNode'] in [True, 'Both'] else False
    process_aggregated = True if report['ByNode'] in [False, 'Both'] else False

    # Create output data
    output = get_blank_dataframe()
    for year in report['Year']:
        for gender in report['Gender']:
            for min_age, max_age in report['AgeBins']:
                if process_by_node:
                    output = process_nodes(data, output, year, gender, min_age, max_age, report,
                                           node_ids=node_ids, allow_incomplete=allow_incomplete)

                if process_aggregated:
                    output = process_nodes(data, output, year, gender, min_age, max_age, report,
                                           node_ids=[AGGREGATED_NODE], allow_incomplete=allow_incomplete)

    output.set_index(['Year', 'Node', 'Gender', 'AgeBin'], inplace=True)
    return output


def read_report_file(filename, node_id_column):
    print('Loading file: %s' % filename)
    data = pd.read_csv(filename)
    if node_id_column != 'NodeId':
        data = data.rename(columns={node_id_column: 'NodeId'})
    data.columns = map(lambda s: s.strip().replace(' ', '_').replace('(', '').replace(')', ''), data.columns)
    node_ids = sorted([int(node_id) for node_id in data['NodeId'].unique()])
    return data, node_ids


def main(output_dir):
    print("Hello from Python!")
    print("Started Python post processing  @ " + time.asctime())
    print("Current working directory is: " + os.getcwd())

    # This is a required report result file
    filename = os.path.join(output_dir, by_age_and_gender_filename)
    if os.path.exists(filename):
        data, node_ids = read_report_file(filename=filename, node_id_column='NodeId')
        report_source = os.path.splitext(os.path.basename(filename))[0]
        reports = get_reports(data, report_source=report_source)
    else:
        raise Exception(f'Expected report file does not exist: {filename}')

    # This is an optional report result file
    filename = os.path.join(output_dir, relationships_at_infection_filename)
    if os.path.exists(filename):
        relationships_df, _ = read_report_file(filename=filename, node_id_column='NodeID')
        # relationships_df = pd.read_csv(filename)
        report_source = os.path.splitext(os.path.basename(filename))[0]
        relationship_reports = get_reports(relationships_df, report_source=report_source)
    else:
        relationships_df = None
        relationship_reports = {}

    verify_all_channels_supported(channel_sets=[reports, relationship_reports])

    post_process_dir = 'post_process'
    directory = os.path.join(output_dir, post_process_dir)
    if not os.path.exists(directory):
        os.makedirs(directory)

    print('Processing ReportHIVByAgeAndGender.csv ...')
    for report in reports:
        result = process_report(report, data, node_ids)
        result.to_csv(os.path.join(output_dir, post_process_dir, '%s.csv' % report['Name']))

    if relationships_df is not None:
        print('Processing ReportRelationshipsAtInfection.csv ...')
        for report in relationship_reports:
            result = process_report(report=report, all_data=relationships_df, node_ids=node_ids)
            result.to_csv(os.path.join(output_dir, post_process_dir, '%s.csv' % report['Name']))

    print("Finished Python post processing @ " + time.asctime())


application = main

if __name__ == '__main__':
    main(OUTPUT_DIRECTORY)
#!/usr/bin/env python

import collections
import os
import sys

import boto3

orgs = boto3.client("organizations")
    
ACTIVE = "ACTIVE"
regions = ["us-west-2"]
roles = [
    ("ad", "admin"),
    ("ro", "readonly"),
    ("og", "OrganizationAccountAccessRole"),
]

def main(args, env):
    config_file = os.environ["AWS_CONFIG_FILE"]
    org, user = args[1:]
    
    accounts = orgs.list_accounts()["Accounts"]

    account_ids = active_account_ids(org, accounts)

    config = []
    for name, account in account_ids:
        for nick, role in roles:
            for region in regions:
                config.append(stanza(org, nick, role, name, account, region, user))

    with open(config_file, 'w') as f:
        f.write("\n".join(config))

def active_account_ids(org, accounts):
    for item in accounts:
        if item["Status"] != ACTIVE:
            continue
        if item["Id"] == org:
            item["Name"] = "org"
        yield (item["Name"], item["Id"])

def stanza(org, nick, role, name, account, region, user):
    return template.format(
        org=org,
        nick=nick,
        role=role,
        name=name,
        account=account,
        region=region,
        user=user,
    )

template = r"""
[profile {name}-{nick}]
source_profile = default
role_arn = arn:aws:iam::{account}:role/{region}-{role}
mfa_serial = arn:aws:iam::{org}:mfa/{user}
"""

if __name__ == "__main__":
    sys.exit(main(sys.argv, os.environ))

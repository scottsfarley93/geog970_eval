import pandas
import json

def flatten_json(y):
    out = {}

    def flatten(x, name=''):
        if type(x) is dict:
            for a in x:
                flatten(x[a], name + a + '.')
        elif type(x) is list:
            i = 0
            for a in x:
                flatten(a, name + str(i) + '.')
                i += 1
        else:
            out[name[:-1]] = x

    flatten(y)
    return out

def configJSONToDF(data):
    configurations = []
    for configuration in data:
        flat = flatten_json(configuration)
        configurations.append(flat)

    df = pandas.DataFrame.from_records(configurations)

    return df
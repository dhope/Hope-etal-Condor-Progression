#!/usr/bin/python3
import lisreadpy as lr
import numpy as np
import pandas as pd
from siteibm import WESA
import time
import itertools

def expand_grid(data_dict):
    rows = itertools.product(*data_dict.values())
    return pd.DataFrame.from_records(rows, columns=data_dict.keys())

import migrateexample as me




def runSimSingleSite(LoS, ArrivalDate, nbirds, seed, propWint, propA, variables):
    print(LoS, ArrivalDate, nbirds, seed, propWint, propA)
    variables["Proportion_A"] = float(propA[0])
    variables['outfile_name'] = "./powanal/singleSimulation_" + str(seed) + "_" + str(LoS) + \
    "_" +str(ArrivalDate) +"_"+ str(nbirds) + "_" + str(propWint)  + str(propA) + ".csv"
    variables['A_arrival_time'] = int(ArrivalDate[0])
    variables['A_lengthofstay'] = int(LoS[0])
    variables['totalbirds'] = int(nbirds[0])
    variables['proportionWinterResidents'] = propWint[0]
    np.random.seed(seed)
    me.sim_migration_w_WinteringBirds(variables)


runs = expand_grid(
    {
    "LoS":[1,2,3,4,5],
    "ArrivalDate":[1,2,4,8],
    "nbirds": [1000, 10000],
    "seed": [573, 5779, 778,882], #range(1,10),
    "propWint": [0.,0.25, 0.5, 0.75],
    "propA": [1.0, 0.75, 0.5, 0.25]
    }


    )

rundict = runs.to_dict()


if __name__ == '__main__':
    import time
    import numpy as np
    variables = lr.lisreadpy('siteibm.ctl', False)
    for i in range(0,len(runs)):
        samplerun = runs.loc[[i]].to_dict(orient="list")
        runSimSingleSite(**samplerun, variables = variables)

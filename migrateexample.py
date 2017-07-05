#!/usr/bin/python3
import lisreadpy as lr
import numpy as np
import pandas as pd
from siteibm import WESA
import time

def main():
    '''Simple model with only one bird'''
    a = lr.lisreadpy('siteibm.ctl')
    if a['print_out']:
        outfile = open(a['outfile_name'], 'w')
        print("Bird\ttime\tfuel\tsite\tgroup\tLoS", file=outfile)
    bird = WESA('A', **a)
    print(bird.get_variable('_fuel'))
    print(bird.get_variable('_time'))

    bird.migrate(outfile,bird_id = 1,  **a)

    if a['print_out']:
        outfile.close()


def sim_migration():
    '''Simulate movement through a single site'''
    variables = lr.lisreadpy('siteibm.ctl', False)
    if variables['print_out']:
        outfile = open(variables['outfile_name'], 'w')
        print("Bird\ttime\tsite\tgroup \
            \tLoS\tPrSkip\tGroupSize\tArrival\tWinterRes", file=outfile) #\tfuel
    birds = []
    i = 0
    output = []
    groups = ['A', 'B']
    for j in groups:
        i = 0
        print(j)
        variables['Proportion_B'] = 1 - variables['Proportion_A']
        while i < variables['totalbirds'] * variables['Proportion_{}'.format(j)]:
            bird = WESA(j, i,  **variables)
            bird.migrate(outfile, bird_id = i,  **variables)
            if i % 1000 == 0:
                print("Bird", i," in group", j)
            i += 1
    if variables['print_out']:
            outfile.close()
            print('ALLL Done!')
    # import rpy2.robjects as ro
    # r = ro.r

    # r.source('histogram.r')


def sim_migration_w_WinteringBirds(variables):
    '''Simulate movement through a single site'''
    # variables = lr.lisreadpy('siteibm.ctl', False)
    if variables['print_out']:
        outfile = open(variables['outfile_name'], 'w')
        print("Bird\ttime\tsite\tgroup \
            \tLoS\tPrSkip\tGroupSize\tArrival\tWinterRes", file=outfile) #\tfuel
    birds = []
    i = 0
    output = []
    groups = ['A', 'B']
    variables['Proportion_B'] = 1 - variables['Proportion_A']
    for j in groups:
        i = 0
        print(variables['Proportion_{}'.format(j)])
        numberOfmigratingBirds = variables['Proportion_{}'.format(j)] * (1-variables['proportionWinterResidents']) * variables['totalbirds']
        while i < numberOfmigratingBirds:
            bird = WESA(j, i, winteringBird = False, **variables)
            bird.migrate(outfile, bird_id = i,  **variables)
            if i % 1000 == 0:
                print("Bird", i," in group", j)
            i += 1
        while i >= numberOfmigratingBirds and i < variables['totalbirds'] * variables['Proportion_{}'.format(j)]:
            bird = WESA(j, i, winteringBird = True,  **variables)
            bird.migrate(outfile, bird_id = i,  **variables)
            if i % 1000 == 0:
                print("Wintering Bird", i," in group", j)
            i += 1


    if variables['print_out']:
            outfile.close()
            print('ALLL Done!')
    # import rpy2.robjects as ro
    # r = ro.r

    # r.source('histogram.r')


def sim_migration_by_time():
    """Simulate as above, but step through all birds each step"""

    timeA = time.time()
    variables = lr.lisreadpy('siteibm.ctl', False)
    columns = ['bird_id', 'Time', 'Site']
    # Initialize Birds 
    totalnumberofBirds = variables['{}_numbirds'.format('A')]  +\
                            variables['{}_numbirds'.format('A')]
    birds = [None] * totalnumberofBirds
    i = 0
    # output = []
    def step_through_birds(focal_bird, t , variables = variables):
        conditions = (t >= focal_bird.true_arrivaldate 
                and focal_bird._dead is False)
        if conditions:
            focal_bird.migrate_step(t, **variables)
        else:
            pass
    groups = ['A', 'B']
    timeB = time.time()

    numberA = variables['{}_numbirds'.format('A')]
    birds = [WESA(groups[j], (i + j * numberA) , **variables) for j in range(len(groups)) for i in range(variables['{}_numbirds'.format(groups[j])]) ]
    timeC = time.time()
    t = variables['baseline_arrival_date']
    while t < variables['finalDate']:

        [step_through_birds(focal_bird, t, variables) for focal_bird in birds]
        t += 1
    timeD = time.time()
    bird_id_ = 0
    dates_final = []
    sites_final = []
    outfile = open('counts2.csv', 'w')
    print('time\tsite\tgroup\tBird', file = outfile) 
    def append_results(focal_bird, outfile = outfile):
        dates, site_hist,  group = focal_bird.migrate_step(time = t, **variables)
        
        idout = [focal_bird.ident] * len(dates)
        group_out = [group] * len(dates)
        output = [ dates, site_hist, group_out, idout ]
        def print_fun(list_i, outfile):
            # print(list_i)
            [print(item, end = "\t", file = outfile) for item in list_i]
            print('', file = outfile)
        
        for i in range(len(output[0])):
            list_i = [sublist[i] for sublist in output ]
            
            print_fun(list_i, outfile)

    output = [append_results(focal_bird) for focal_bird in birds]

    outfile.close()
    timeE = time.time()
    timeF = time.time()
    print(timeB - timeA, timeC - timeB, timeD - timeC, timeE - timeD, timeF - timeE)




if __name__ == '__main__':
    import time
    import numpy as np
    np.random.seed(911)
    variables = lr.lisreadpy('siteibm.ctl', False)
    time1 = time.time()
    # sim_migration()
    sim_migration_w_WinteringBirds()
    time2 = time.time()
    print('Model by individual took ', time2 - time1, " seconds")
    # time2 = time.time()
    # sim_migration_by_time()
    # time3 = time.time()
    # print('Model by timestep took ', time3 - time2, " seconds")
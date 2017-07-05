#!/bin/python3
'''
Model of individual sandpiper movement through a site.
An indivudal based model, where migrants arrive at a given date,
and fuel load. They then stay for a given time and depart.
The distribution of the resulting population movement
is the final output.
'''

import numpy as np
from math import *

class WESA:
    # minMigrantArrival = 500
    ''' A wesa class which moves through the site '''
    def __init__(self, group,ident,winteringBird,  **kwargs):
        '''Initialize bird with variables '''
        ##### Group Variable Names ########
        # self.name_fuel_mean = '{}_fuel_arrival'.format(group) 
        # self.name_fuel_sigma = '{}_fuel_sigma'.format(group)
        self.name_arrival_mean = '{}_arrival_time'.format(group)
        self.name_arrival_sigma = '{}_arrival_sigma'.format(group)
        self.name_lengthofstay = '{}_lengthofstay'.format(group)
        self.name_hide_prob = '{}_hideProbability'.format(group)
        self.name_prob_skip = '{}_prob_skip'.format(group)
        self.name_groupsize = '{}_numbirds'.format(group)
        self.winterResident = winteringBird
        
        if self.winterResident is True:
            # Arrival Date is baseline_arrival_date
            # print(ident)
            # LOS is RANDOM FAKE arrival Date + RANDOM FAKE LOS

            self._time_arrival = kwargs.get('baseline_arrival_date')#WESA.minMigrantArrival#int(np.random.lognormal(log(kwargs.get(self.name_arrival_mean)),.3)) # Set the FAKE ARRIVAL DATE
            self.true_arrivaldate = kwargs.get('baseline_arrival_date')#WESA.minMigrantArrival # Date of first migrant arrival
            self._time = self.true_arrivaldate # Set the time to date of first migrant arrival
            self.length_stay = np.random.lognormal(log(kwargs.get(self.name_lengthofstay)), .1) # Set Los After migrants arrive
            self.depart_date = self._time + self.length_stay  # Set TRUE departure date
            self.site = kwargs.get('winteringSite') # Set the site they spend the winter at
            

        else:
            self.site = 0 # Site number
            # Time 
            self._time_arrival = int(np.random.lognormal(log(kwargs.get(self.name_arrival_mean)),kwargs.get(self.name_arrival_sigma)))
            #np.random.poisson(kwargs.get(self.name_arrival_mean)))
            self.true_arrivaldate = self._time_arrival + kwargs.get('baseline_arrival_date')
            self._time = self.true_arrivaldate
            # self.LoS = []
            self.setLengthofStay(kwargs) # Set the length of stay
            self.depart_date = self.length_stay + self._time # Departure date
   

        self._dead = False
        self._departed = False # Has the bird departed
        # self._error = kwargs.get(self.name_hide_prob) # Not currently in use
        self.set_prob_skip(kwargs, self.site) # Set the probability of skipping each site
        
        self.group = group # Set the group
        self.groupsize = kwargs.get(self.name_groupsize)
        self.skip = False # Did the bird skip the previous site?
        self.site_hist = []
        self.dates = []
        self.ident = ident





    # Functions to call and change variables

    def get_variable(self, _var):
        '''Function to call a variable by name'''
        out_ = eval('self.{}'.format(_var))
        return out_

    # def set_fuel(self,value_):
    #     '''Set the fuel load (obsolete)'''
    #     self._fuel += value_

    def advance_time(self, times = 1):
        '''Tick the time forward'''
        self._time += times

    # ------ Functions to migrate ----- # 
    def die(self, mortrisk, finalDate):
        '''If random number is above mortality risk or after final date
        bird dies '''
        if (#self._fuel <= 0 or
        np.random.uniform(0.0, 1.0) < mortrisk):
            self._dead = True
        if self._time >= finalDate:
            self._dead = True
            print('dead bird' ,self.ident,  self.site )

    # def feed(self, food):
    #     '''Forage (obsolete)'''
    #     if self._dead is False:
    #         self.set_fuel(food)

    def estmigtime(self, kwargs):
        '''Set the time in flight between sites'''
        self.timetonext = np.random.lognormal(log(kwargs.get('time_between_sites')), 0.2)
        self.timetonext = int(round(self.timetonext))

    def setLengthofStay(self, kwargs):
        '''Set the length of stay at each site upon arrival'''
        if self.site == 3 and kwargs.get('separateCRD_LoS') is True:
            self.length_stay = np.random.lognormal(log(kwargs.get('CRD_LOS')), .1)
        else:
            self.length_stay = np.random.lognormal(log(kwargs.get(self.name_lengthofstay)), .1)#,
                                               # kwargs.get('sigma_stay'))
        # print(self.length_stay)
        # self.LoS = self.LoS.append(self.length_stay)
    def set_prob_skip(self, kwargs, site):
        '''Set the probability of skipping a site'''
        # Adjust Kachemak Bay Skip Probability
        self.prob_skip = kwargs.get(self.name_prob_skip)[site]
        if kwargs.get('biasKachemak') and site == (kwargs.get('breedingsite') - 1):
                        prob_skip_max = 1#self.prob_skip
                        prob_adj = prob_skip_max * (1-(self._time/kwargs.get('critical_date'))**35)
                        self.prob_skip = max(prob_adj, 0)
        # if site == kwargs.get('breedingsite'):
        #     print(site, self.prob_skip,  kwargs.get(self.name_prob_skip),self._dead)


    def depart(self, kwargs):
        '''Adjust bird's variables upon departure from a site'''
        if self._departed is False: # If bird has not already departed
            if self._time >= self.depart_date: # If time is after or at departure date
                self._departed = True # Depart
                self.site += 1 # Set the next site
                self.flighttime = 0 # Set the flight counter to zero
                self.estmigtime(kwargs) # Set the time in flight to next site
                self.arrival_time = self._time + self.timetonext
                self.set_prob_skip(kwargs, self.site) # Set the probability of skipping that site

    def hide(self):
        '''Hide from counts. Not in use'''
        if self._error < np.random.uniform():
            self.advance_time()

    def print_to_file(self, _id, _file):
        '''Print output at each step for each bird'''
        print(_id, '\t', self._time, '\t',
                #self._fuel,'\t', 
                self.site,'\t',
                self.group,'\t',
                self.length_stay,'\t',
                self.prob_skip,'\t',
                self.groupsize, '\t',
                self._time_arrival,'\t',
                self.winterResident, file = _file)

    def migrate(self, outfile,bird_id = 1, **kwargs):
        '''Function for bird to migrate through each site until final breeding site'''

        if kwargs.get('output_results') is True:
            dates = [self._time]
            sites = [self.site]
        

        while (self._dead is False and self.site < kwargs.get('breedingsite')):
            # As long as the bird is not dead or at the breeding grounds perform the following functions
            self.die(kwargs.get('risk'), kwargs.get('finalDate'))
            if self._departed is False:
                # If the bird is at a site, print conditions,
                # then try to depart
                if kwargs.get('print_out') is True:
                    self.print_to_file(bird_id, outfile)
                if kwargs.get('output_results') is True:
                    dates.append(self._time)
                    sites.append(self.site)
                # self.feed(kwargs.get('food'))
                self.depart(kwargs)
                
                self.advance_time()
                # self.hide()
            else:
                # If migrant has departed, fly, land, or skip a site
                # total_time_for_flight = self.timetonext
                if self.flighttime < self.timetonext:
                    # Continue flying
                    self.advance_time(self.timetonext)
                    self.flighttime += self.timetonext

                else:
                    # Otherwise land or skip site
                    if np.random.uniform(0,1) < self.prob_skip:# and self.skip is False:
                        # Skip site
                        self.flighttime = 0 # Reset time in flight
                        self.site += 1 # Set goal site to next
                        self.estmigtime(kwargs) # Estimate time to next site
                        self.skip = True
                        self.set_prob_skip(kwargs, self.site) # Set the probability of skipping that site

                    else:
                        # Otherwise land at site
                        # If earlier than previous migrants put date into first arrival
                        if self._time < WESA.minMigrantArrival and self.winterResident is False:
                            WESA.minMigrantArrival = self._time
                        self._departed = False
                        self.setLengthofStay(kwargs)
                        self.depart_date = self.length_stay + self._time
                        self.skip = False
                        if self.site == kwargs.get('breedingsite'):
                            self.dead = True

                    self.advance_time()

        if self.site >= kwargs.get('breedingsite'):
            if kwargs.get('print_out') is True:
                self.print_to_file(bird_id, outfile) # Print final arrival time at breeding ground
            if kwargs.get('output_results') is True:
                        dates.append(self._time)
                        sites.append(self.site)
                        resident_ls = [self.winterResident] * len(dates)
                        # Here is the problem
                        # You need to have this read as bool
                        # print(resident_ls)
                        # print(catdogs)
                        return dates, sites, resident_ls
        else:
            return self._time # Return arrival time



    def migrate_step(self,time, **kwargs):  
        if self._dead is False:
            self.die(kwargs.get('risk'), kwargs.get('finalDate'))
            self._time = time
            # If you are at a site, stay or depart
            if self._departed is False:
                
                # If the bird is at a site, print conditions,
                # then try to depart
                if self.winterResident == False or self.site != kwargs.get('winteringSite'):
                    self.dates.append(self._time)
                    self.site_hist.append(self.site)
                if self.site == kwargs.get('breedingsite'):
                            print('Error:Trying to forage at breeding site')
                            print(self._time, self.site, self.ident. self._departed) 
                self.depart(kwargs)
             
            elif self._departed is True:
                # If migrant has departed, fly, land, or skip a site
                # If you have departed a site, migrate onwards
                if self._time < self.arrival_time:
                    # Continue flying
                    pass

                # If you are arriving at a site, skip it or land
                else:
                    # Otherwise land or skip site
                    rndm_roll = np.random.uniform(0,1)
                    if  rndm_roll < self.prob_skip:# and self.skip is False:
                        # Skip site
                        if self.site == kwargs.get('breedingsite'):
                            print('Error:Trying to skip breeding site')
                            print(self._dead,self._time, self.site, kwargs.get('breedingsite'),  self.ident, self.prob_skip, rndm_roll )  
                        self.site += 1 # Set goal site to next
                        self.estmigtime(kwargs) # Estimate time to next site
                        self.skip = True
                        self.arrival_time = self._time + self.timetonext
                        self.set_prob_skip(kwargs, self.site) # Set the probability of skipping that site

                    else:
                        # Otherwise land at site
                        self._departed = False
                        self.setLengthofStay(kwargs)
                        self.depart_date = self.length_stay + self._time
                        self.skip = False
                        self.dates.append(self._time)
                        self.site_hist.append(self.site)
                        if self.site == kwargs.get('breedingsite'):
                            self._dead = True
            else:
                raise TypeError("Departure status not set")
        else:
                # print(len(self.dates), len(self.site_hist))
                outvardates = self.dates
                outvarsites = self.site_hist
                return outvardates, outvarsites, self.group











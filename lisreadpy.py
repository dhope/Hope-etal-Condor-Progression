"""
Function to import variables into a dictionary from a 
control file. File must be formated with '##' for comments,
'#'' for variable names and values on the following line
Updated April 13 to import dictionaries if formatted correctly
Created by David Hope based on 'lisread.r' by Dr. Sean Cox 
"""
import numpy as np
from ast import literal_eval
from math import *
# Goal is to read a control file and import a dictionary with all variables


def lisreadpy(fname, _print = True):
    """Function to import variables from control file"""
    import re # Regular Expression Module
    _letters = re.compile('[a-z]', re.IGNORECASE)
    _digits = re.compile('\d')

    def contains_digits(d, value=_digits):
        return bool(value.search(d))
    values_dsvm = dict()
    DataFileName = fname
    # Open file
    configFile = open(DataFileName, 'r')
    x = 0

    in_dict = False
    for index, Line in enumerate(configFile):
        """scan through each line, skipping if is a comment
           if line is a variable, assign to key"""

        if not Line.startswith('##') and not Line.startswith('\n'):
            if Line.startswith('#!# Begin Dictionary - '):
                # Initiate Dictionary
                dict_ = {}
                dict_name = Line.strip('#!# Begin Dictionary - ')
                dict_name = dict_name.strip('\n')
                in_dict = True
                # print('In Dictionary? ' ,in_dict)

            elif Line.startswith('#!# End Dictionary'):
                # Export Dictionary to variables
                values_dsvm[dict_name] = dict_
                # print(dict_name)
                if _print: print(dict_name, ' = ', dict_)
                x = 0
                in_dict = False

            elif Line.startswith("#"):
                name = Line.replace('# ', '')
                name = name.strip('\n')
                if in_dict:
                    # print("Good boy  " , in_dict)
                    dict_[name] = []
                else:
                    values_dsvm[name] = []
                    # print(name, in_dict)
                x = 1
            elif Line.startswith('list_of_list '):
                values = Line.strip('list_of_list ')
                values = values.strip('\n')
                values = values.split('; ')
                out = []
                for item in values:
                    out.append(eval(item))
                if in_dict:
                    dict_[name] = out
                else:
                    values_dsvm[name] = out
                x = 0

            elif Line.startswith('{'):
                values = Line.strip('\n')
                values = values.split("; ")
                for dicts_ in values:
                    tmp_dict = literal_eval(dicts_)
                    if in_dict:
                        dict_[name].append(tmp_dict)
                    else:
                        values_dsvm[name].append(tmp_dict)
            elif x == 1:
                _line = Line.strip('\n')
                if contains_digits(_line, _letters) is True:
                    try:
                        _line = eval(_line)
                    except:
                        _line = _line
                        if contains_digits(_line, _digits) is True:
                               _line = _line.split(sep = " ")

                               print("Mixed String", name, ' = ', _line)


                else:
                    try:
                        _line = np.fromstring(_line, dtype=float, sep=' ')
                        float(_line)
                        inNumberint = int(_line)
                        if float(_line) == inNumberint:
                            _line = inNumberint
                        else:
                            _line = float(_line)
                    except ValueError:
                        if contains_digits(_line) == True:
                            if len(_line) ==1:
                                _line = float(_line[0])
                        else:
                                _line = _line
                                if bool(_print): print('The variable here is ', name,": ", _line)
                    except TypeError:
                        if bool(_print): print('Variable ', name, ' has an error:', TypeError)

                        try:
                            _h = _line.astype(np.int)
                            if (_h == _line).all():
                                _line = _h
                        except:
                            if bool(_print): print("PROBLEM HERE: Name is ", name, 'Value is ',_line)
                if in_dict:
                    dict_[name] = _line
                else:
                    values_dsvm[name] = _line
                    if _print: print(name, " = ", _line)    
                x = 0
    return values_dsvm          


def dictsep(_dict, _variables):
    mydic = {k: _dict[k] for k in _variables}
    return   mydic

def kwargs_var(kwargs):
    for name, value in kwargs.items():
        print(name)
        exec('{0} = {1}'.format(name, value))

    #for key in mydic.keys():
     #    return exec('{0} = {1}'.format(key, mydic[key]))

if __name__ == '__main__':
    a = lisreadpy('DSVM.ctl', False)
    print(a['sensitivity_variables'])
    #print(a)

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 24 08:11:28 2018

@author: ber0061
"""

import pandas as pd
from gen_dat_soubor import Generator
my_gen=Generator(60,5,15,0.6,0.7,0.1,0,0.05,0.5,0.1,4,(0.2,0.35),1000)



my_gen.generate_datasets_and_export(1,201,'../DU_LS20_prez','ukol_')

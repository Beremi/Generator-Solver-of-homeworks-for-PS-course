#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 24 08:11:28 2018

@author: ber0061
"""

from gen_dat_soubor import Generator

my_gen = Generator(column_names=['svítivost při teplotě 22 °C (lm)',
                                 'svítivost při teplotě 5 °C (lm)', 'výrobce'],
                   TF_answers=['ano', 'ne'],
                   group_names=['Amber', 'Bright', 'Clear', 'Dim'],
                   mean_size=70,
                   std_size=5,
                   mean_values=801,
                   std_values=10,
                   mean_std=40,
                   std_std=5,
                   mean_change_mean=1,
                   std_change_mean=0.1,
                   mean_change_std=5,
                   std_change_std=0.8,
                   data_set_count=4,
                   fail_prob=(0.2,0.35),
                   rounding=1,
                   seed=9)


my_gen.generate_datasets_and_export(start_idx=1, end_idx=301,
                                    location='../DU_LS21_prez',
                                    prefix_name='ukol_')

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 24 08:11:28 2018

@author: ber0061
"""
import numpy as np
import pandas as pd
import random as rnd
import os



class Generator(object):
    def __init__(self, mean_size,std_size,mean_values,std_values,mean_std,std_std,mean_change_mean,std_change_mean,mean_change_std,std_change_std,data_set_count, fail_prob,seed):
        self.mean_size = mean_size
        self.std_size = std_size
        self.seed = seed
        self.mean_values = mean_values
        self.std_values = std_values

        self.mean_std = mean_std
        self.std_std = std_std

        self.mean_change_mean = mean_change_mean
        self.std_change_mean = std_change_mean

        self.mean_change_std = mean_change_std
        self.std_change_std = std_change_std

        self.data_set_count=data_set_count
        self.fail_prob=fail_prob

    def ensure_dir(self,directory):
        if not os.path.exists(directory):
            os.makedirs(directory)

    def generate_dataset(self,seed):
        rnd.seed(a=seed+self.seed)
        type_DU=rnd.randint(0, 3)
        n=np.zeros(self.data_set_count,dtype=np.int32)
        val_means=np.zeros(self.data_set_count)
        val_stds=np.zeros(self.data_set_count)
        change_means=np.zeros(self.data_set_count)
        change_stds=np.zeros(self.data_set_count)
        fail_probability=np.zeros(self.data_set_count)

        for i in range(0,self.data_set_count):
            n[i]=np.floor(rnd.normalvariate(mu=self.mean_size,sigma=self.std_size))
            val_means[i]=rnd.normalvariate(mu=self.mean_values,sigma=self.std_values)
            val_stds[i]=rnd.normalvariate(mu=self.mean_std,sigma=self.std_std)
            change_means[i]=rnd.normalvariate(mu=self.mean_change_mean,sigma=self.std_change_mean)
            change_stds[i]=rnd.normalvariate(mu=self.mean_change_std,sigma=self.std_change_std)
            fail_probability[i]=rnd.uniform(self.fail_prob[0],self.fail_prob[1])


        dataset_standard = pd.DataFrame(index=range(0,int(np.sum(n))), columns=['automaticke mereni', 'manualni mereni', 'typ abraziva','vysledek chemickeho testu'])
        idx=0;
        if type_DU==0:
            for i in range(0,self.data_set_count):
                op_id=rnd.randint(0, n[i]-1)
                for j in range(0,n[i]):
                    tmp1=rnd.normalvariate(mu=val_means[i],sigma=val_stds[i])
                    if j==op_id:
                        tmp_pm=rnd.randint(0, 1)
                        tmp1=tmp1+tmp_pm*rnd.normalvariate(mu=3,sigma=0.3)-(1-tmp_pm)*rnd.normalvariate(mu=3,sigma=0.3)
                    tmp2=tmp1-rnd.normalvariate(mu=change_means[i],sigma=change_stds[i])
                    if rnd.random()<0.015:
                        tmp2=tmp2-rnd.normalvariate(mu=0,sigma=3)
                    tmp3='A'+str(int(i+1))
                    if rnd.uniform(0,1)>fail_probability[i]:
                        tmp4='prosel'
                    else:
                        tmp4='neprosel'
                    dataset_standard.loc[idx]=[tmp1,tmp2,tmp3,tmp4]
                    idx=idx+1

        if type_DU==1:
            for i in range(0,self.data_set_count):
                op_id=rnd.randint(0, n[i]-1)
                for j in range(0,n[i]):
                    tmp1=rnd.normalvariate(mu=val_means[i],sigma=val_stds[i])
                    if j==op_id:
                        tmp_pm=rnd.randint(0, 1)
                        tmp1=tmp1+tmp_pm*rnd.normalvariate(mu=3,sigma=0.3)-(1-tmp_pm)*rnd.normalvariate(mu=3,sigma=0.3)
                    tmp2=tmp1-rnd.expovariate(lambd=1.0/change_means[i])
                    if rnd.random()<0.015:
                        tmp2=tmp2-rnd.normalvariate(mu=0,sigma=3)
                    tmp3='A'+str(int(i+1))
                    if rnd.uniform(0,1)>fail_probability[i]:
                        tmp4='prosel'
                    else:
                        tmp4='neprosel'
                    dataset_standard.loc[idx]=[tmp1,tmp2,tmp3,tmp4]
                    idx=idx+1

        if type_DU==2:
            for i in range(0,self.data_set_count):
                op_id=rnd.randint(0, n[i]-1)
                for j in range(0,n[i]):
                    tmp1=rnd.uniform(a=(val_means[i]-val_stds[i]),b=(val_means[i]+val_stds[i]))
                    if j==op_id:
                        tmp_pm=rnd.randint(0, 1)
                        tmp1=tmp1+tmp_pm*rnd.normalvariate(mu=3,sigma=0.3)-(1-tmp_pm)*rnd.normalvariate(mu=3,sigma=0.3)
                    tmp2=tmp1-rnd.expovariate(lambd=1.0/change_means[i])
                    if rnd.random()<0.015:
                        tmp2=tmp2-rnd.normalvariate(mu=0,sigma=3)
                    tmp3='A'+str(int(i+1))
                    if rnd.uniform(0,1)>fail_probability[i]:
                        tmp4='prosel'
                    else:
                        tmp4='neprosel'
                    dataset_standard.loc[idx]=[tmp1,tmp2,tmp3,tmp4]
                    idx=idx+1

        if type_DU==3:
            for i in range(0,self.data_set_count):
                op_id=rnd.randint(0, n[i]-1)
                for j in range(0,n[i]):
                    tmp1=rnd.uniform(a=(val_means[i]-val_stds[i]),b=(val_means[i]+val_stds[i]))
                    if j==op_id:
                        tmp_pm=rnd.randint(0, 1)
                        tmp1=tmp1+tmp_pm*rnd.normalvariate(mu=3,sigma=0.3)-(1-tmp_pm)*rnd.normalvariate(mu=3,sigma=0.3)
                    tmp2=tmp1-rnd.normalvariate(mu=change_means[i],sigma=change_stds[i])
                    if rnd.random()<0.015:
                        tmp2=tmp2-rnd.normalvariate(mu=0,sigma=3)
                    tmp3='A'+str(int(i+1))
                    if rnd.uniform(0,1)>fail_probability[i]:
                        tmp4='prosel'
                    else:
                        tmp4='neprosel'
                    dataset_standard.loc[idx]=[tmp1,tmp2,tmp3,tmp4]
                    idx=idx+1
        return dataset_standard

    def generate_datasets_and_export(self,start_idx,end_idx,location,prefix_name):
        self.ensure_dir(location)
        os.chdir(location)
        for i in range(start_idx,end_idx):
            df=self.generate_dataset(i)
            self.ensure_dir('standard_dat_format')
            os.chdir('standard_dat_format')
            writer = pd.ExcelWriter(prefix_name+str(int(i)) + '.xlsx')
            df.to_excel(writer,'Vysledky mereni')
            writer.save()
            os.chdir('..')
            type_save=rnd.randint(0, 1)
            writer = pd.ExcelWriter(prefix_name+str(int(i)) + '.xlsx')
            if type_save==0:
                for j in range(1,self.data_set_count+1):
                    df_tmp=df.loc[df['typ abraziva'] == ('A'+str(int(j)))]
                    df_tmp.pop('typ abraziva')
                    df_tmp.to_excel(writer,'A'+str(int(j)))

            if type_save==1:
                tmp=list()
                for j in range(1,self.data_set_count+1):
                    df_tmp=df.loc[df['typ abraziva'] == ('A'+str(int(j)))]
                    df_tmp.pop('typ abraziva')
                    df_tmp=df_tmp.rename(index=str, columns={'automaticke mereni': ('A'+str(int(j))+' automaticke mereni'), 'manualni mereni': ('A'+str(int(j))+' manualni mereni'),'vysledek chemickeho testu': ('A'+str(int(j))+' vysledek chemickeho testu')})
                    df_tmp=df_tmp.reset_index(drop=True)
                    tmp.append(df_tmp)
                result = pd.concat(tmp, axis=1, sort=False)
                result.to_excel(writer,'Vysledky mereni')
            writer.save()

        os.chdir('..')
        return


# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd


# In[2]:


cars_dt = pd.read_csv('veh3SAS_pipe.csv', sep = '\|')
cars_dt.head(20)


# In[4]:


cars_dt[cars_dt.manufacturer == 'ferrari']


# In[3]:


cars_dt.cylinders.unique()


# In[3]:


cars_dt.info()


# In[4]:


cars_dt['cylinders'] = cars_dt['cylinders'].str.slice(stop = 2)


# In[5]:


#Converting "other" and null entries to "nan"
cars_dt['cylinders'] = cars_dt['cylinders'].replace(['ot'], [999])
cars_dt['cylinders'] = cars_dt['cylinders'].replace([np.nan], [999])
cars_dt.cylinders = cars_dt.cylinders.astype(float)
cars_dt.cylinders.dtype


# In[6]:


cars_dt['cylinders'] = cars_dt['cylinders'].astype(str)


# In[7]:


#Converting null and other - Manufacturer
cars_dt['manufacturer'] = cars_dt['manufacturer'].replace([np.nan], [999])


# In[8]:


#Converting null and other - Model
cars_dt['model'] = cars_dt['model'].replace([np.nan], [999])


# ## GroupBy

# In[9]:


#Creating GroupBy DataFrame
cyl_grpby = cars_dt[['manufacturer','model','cylinders']]


# In[10]:


#Converting Cylinders Column to String
cyl_grpby.cylinders = cyl_grpby.cylinders.astype(str)


# ## GroupBy - Mode

# In[11]:


cyl_grpby = cyl_grpby.groupby(["manufacturer", "model"], as_index = False)["cylinders"].agg(pd.Series.mode)


# In[12]:


cyl_grpby.cylinders = cyl_grpby.cylinders.astype(str)


# In[13]:


null_lkup_cyl_cor1 = cyl_grpby.where(cyl_grpby['cylinders'] != '999.0') 


# In[14]:


#List of Manufacturers
manu_list = ['acura', 'alfa-romeo', 'aston-martin', 'audi', 'bmw',
       'buick', 'cadillac', 'chevrolet', 'chrysler', 'dodge', 'ferrari',
       'fiat', 'ford', 'gmc', 'harley-davids', 'hennessey', 'honda',
       'hyundai', 'infiniti', 'jaguar', 'jeep', 'kia', 'land rover',
       'lexus', 'lincoln', 'mazda', 'mercedes-benz', 'mercury', 'mini',
       'mitsubishi', 'nissan', 'pontiac', 'porche', 'ram', 'rover',
       'saturn', 'subaru', 'toyota', 'volkswagen', 'volvo']


# In[15]:


cyl_grpby.manufacturer = cyl_grpby.manufacturer.astype(str)
cyl_grpby.model = cyl_grpby.model.astype(str)
cyl_grpby.cylinders = cyl_grpby.cylinders.astype(str)


# In[16]:


null_lkup_cyl_cor1 = null_lkup_cyl_cor1[null_lkup_cyl_cor1.manufacturer.isin(manu_list)]


# In[17]:


#null_lkup_cyl_cor2 = null_lkup_cyl_cor1.where(null_lkup_cyl_cor1['cylinders'].str.len() < 6) 
null_lkup_cyl_cor2 = null_lkup_cyl_cor1 


# In[18]:


#Cylinders Approved Values List
cyl_list = ['4.0', '5.0', '6.0', '8.0', '10.0', '12.0', '16.0']


# In[19]:


null_lkup_cyl_cor2 = null_lkup_cyl_cor2[null_lkup_cyl_cor2.cylinders.isin(cyl_list)]


# ## Final LookUp Table

# In[20]:


null_lkup_cyl_cor = null_lkup_cyl_cor2


# ## Mode Imputation - Cylinders Column

# In[21]:


#Sorting Both Tables
#Sort Main Table
cars_dt = cars_dt.sort_values(by=['manufacturer', 'model'], ascending = True)
#Sort LookUp Table
null_lkup_cyl_cor = null_lkup_cyl_cor.sort_values(by=['manufacturer', 'model'], ascending = True)


# In[22]:


#Joining the two dataframes
join_view = pd.merge(cars_dt, null_lkup_cyl_cor, on = ['manufacturer', 'model'])


# In[23]:


#Dropping records where Model is null
join_view = join_view[join_view['model'] != 999]


# In[24]:


#Dropping records where Manufacturer is null
join_view = join_view[join_view['manufacturer'] != 999]


# In[25]:


valid_df = join_view[join_view['cylinders_x'].isin(cyl_list)]


# In[26]:


imp_df = join_view[join_view['cylinders_x'] == '999.0']


# In[27]:


del valid_df['cylinders_y']
del imp_df['cylinders_x']


# In[28]:


valid_df['cylinders'] = ''
imp_df['cylinders'] = ''


# In[29]:


valid_df['cylinders'] = valid_df['cylinders_x']
imp_df['cylinders'] = imp_df['cylinders_y']


# In[30]:


del valid_df['cylinders_x']
del imp_df['cylinders_y']


# In[31]:


FINAL_CYL_IMPUTE = pd.concat([valid_df, imp_df])


# In[32]:


FINAL_CYL_IMPUTE


# ## Label Encoding

# In[33]:


FINAL_CYL_IMPUTE.info()


# In[34]:


cat_data = FINAL_CYL_IMPUTE[['region','manufacturer','model','condition','fuel','title_status',
                            'transmission','drive','type','paint_color']]


# In[35]:


cat_data


# In[36]:


from sklearn.preprocessing import LabelEncoder
#Region
label_encode = LabelEncoder()
label_encode.fit(cat_data['region'])
label_encode.classes_
cat_data['region'] = label_encode.transform(cat_data['region'])


# In[37]:


#manufacturer
label_encode = LabelEncoder()
label_encode.fit(cat_data['manufacturer'])
label_encode.classes_
cat_data['manufacturer'] = label_encode.transform(cat_data['manufacturer'])


# In[38]:


#model
label_encode = LabelEncoder()
label_encode.fit(cat_data['model'])
label_encode.classes_
cat_data['model'] = label_encode.transform(cat_data['model'])


# In[39]:


cat_data['condition'] = cat_data['condition'].astype(str)


# In[40]:


#condition
label_encode = LabelEncoder()
label_encode.fit(cat_data['condition'])
label_encode.classes_
cat_data['condition'] = label_encode.transform(cat_data['condition'])


# In[41]:


cat_data['fuel'] = cat_data['fuel'].astype(str)


# In[42]:


#fuel
label_encode = LabelEncoder()
label_encode.fit(cat_data['fuel'])
label_encode.classes_
cat_data['fuel'] = label_encode.transform(cat_data['fuel'])


# In[43]:


cat_data['title_status'] = cat_data['title_status'].astype(str)


# In[44]:


#title_status
label_encode = LabelEncoder()
label_encode.fit(cat_data['title_status'])
label_encode.classes_
cat_data['title_status'] = label_encode.transform(cat_data['title_status'])


# In[45]:


cat_data['transmission'] = cat_data['transmission'].astype(str)


# In[46]:


#transmission
label_encode = LabelEncoder()
label_encode.fit(cat_data['transmission'])
label_encode.classes_
cat_data['transmission'] = label_encode.transform(cat_data['transmission'])


# In[47]:


cat_data['drive'] = cat_data['drive'].astype(str)


# In[48]:


#drive
label_encode = LabelEncoder()
label_encode.fit(cat_data['drive'])
label_encode.classes_
cat_data['drive'] = label_encode.transform(cat_data['drive'])


# In[49]:


cat_data['type'] = cat_data['type'].astype(str)


# In[50]:


#type
label_encode = LabelEncoder()
label_encode.fit(cat_data['type'])
label_encode.classes_
cat_data['type'] = label_encode.transform(cat_data['type'])


# In[51]:


cat_data['paint_color'] = cat_data['paint_color'].astype(str)


# In[52]:


#paint_color
label_encode = LabelEncoder()
label_encode.fit(cat_data['paint_color'])
label_encode.classes_
cat_data['paint_color'] = label_encode.transform(cat_data['paint_color'])


# In[53]:


#Updated Table with Encoded Values
cat_data


# In[54]:


cate_conv_key = FINAL_CYL_IMPUTE[['region','manufacturer','model','condition','fuel','title_status',
                            'transmission','drive','type','paint_color']]
cate_conv_key


# In[55]:


rohith_reg_data = pd.concat([FINAL_CYL_IMPUTE, cat_data], axis = 1)


# In[56]:


rohith_reg_data


# ## Writing File to Dataset CSV

# In[57]:


rohith_reg_data.to_csv(r'C:\Users\srohi\Desktop\result\rohith_reg_data.csv', index = False, header = True)


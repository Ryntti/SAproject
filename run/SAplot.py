from matplotlib import pyplot as plt, patches
import numpy as np
plt.rcParams["figure.figsize"] = [8.00, 5.0]


sysconfig_initial = np.loadtxt('sysconfig_initial.txt') # Read the data of the initial system configuration into an array
sysconfig_fp = np.loadtxt('sys_fp_properties.txt') 
sysincost = sysconfig_fp[0][2]*sysconfig_fp[0][3] # the cost is the width * height of the footprint rec.
# First, the plot of the initial configuration, as a subplot
fig, sys_initial = plt.subplots()
ax1 = plt.gca()
# Set the grid ax1es to be appropriate
sys_initial.set_ylim(-20,120)
sys_initial.set_xlim(-20,120)
sys_initial.set_title(label = f'System initial configuration \n {sysincost:7.2f}')


for i in range(0,len(sysconfig_initial)):
    # The position coordinates of each system rectangle
    # here x and y must be defined as the bottom left corner coordinates of each rectangle
    x = sysconfig_initial[i][0]-0.5*sysconfig_initial[i][2]
    y = sysconfig_initial[i][1]-0.5*sysconfig_initial[i][3]
    # The width and length parameters of each system rectangle
    width = sysconfig_initial[i][2]
    length = sysconfig_initial[i][3]
    sys_initial.add_patch(patches.Rectangle((x, y), width, length, edgecolor='black',facecolor = 'lightseagreen', linewidth=2))

fpxi = sysconfig_fp[0][0]
fpyi = sysconfig_fp[0][1]
fpwi = sysconfig_fp[0][2]
fphi = sysconfig_fp[0][3]
sys_initial.add_patch(patches.Rectangle((fpxi, fpyi), fpwi, fphi, edgecolor='blue', facecolor ='none', linewidth=3.0, linestyle='--'))


sysconfig_end = np.loadtxt('sysconfend.txt')
#Now the final config plot
fig, sys_final = plt.subplots()

#Set the grid ax1es to be suitable
sys_final.set_ylim(-20,120)
sys_final.set_xlim(-20,120)

# # #Hide the ax1es, we don't need them
# ax1.get_xaxis().set_visible(False)
# ax1.get_yaxis().set_visible(False)

for i in range(0,len(sysconfig_end)):
    # The position coordinates of each rectangle in the system
    x = sysconfig_end[i][0]-0.5*sysconfig_end[i][2]
    y = sysconfig_end[i][1]-0.5*sysconfig_end[i][3]
    # The width and height of each rectangle in the system
    width = sysconfig_end[i][2]
    length = sysconfig_end[i][3]
    sys_final.add_patch(patches.Rectangle((x, y), width, length, edgecolor='black',facecolor = 'red', linewidth=1.5))
# Now add the footprint contour 
fpxf = sysconfig_fp[1][0]
fpyf = sysconfig_fp[1][1]
fpwf = sysconfig_fp[1][2]
fphf = sysconfig_fp[1][3]
sysficost = sysconfig_fp[1][2]*sysconfig_fp[1][3]

sys_final.add_patch(patches.Rectangle((fpxf, fpyf), fpwf, fphf, edgecolor='blue', facecolor ='none', linewidth=3.0, linestyle='--'))
sys_final.set_title(label = f'System final configuration \n {sysficost:7.2f}')

plt.show()
# Then the cost function as a function of control parameter c
sys_ctgraph = plt.plot()
ctdata = np.loadtxt('ctgraph_data.txt')
costs = ctdata[:,0]
temps = ctdata[:,1]

plt.title('Footprint cost as a function of c')
plt.plot(temps, costs, color = 'red', linewidth = 3.1)
plt.ylim(0,ctdata[0][0] + 800)
plt.xlim(0,40)
plt.ylabel('Footprint cost')
plt.xlabel('Control parameter c')

plt.show()
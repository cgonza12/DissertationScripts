#===============
# Import modules
#===============

import os                           # for file/folder operations
import numpy.random as rnd          # for random number generators
import pandas                       # for reading csv treat list diles
from psychopy import visual, event, core, gui, data


#==============================================
# Settings that we might want to tweak later on
#==============================================

datapath = 'data'                   # directory to save data in
impath = 'images'                   # directory where images can be found
treatpath = 'treatlists/'             # directory where treatment lists can be found
asfx = 'a.jpg'                      # suffix for the first image
bsfx = 'b.jpg'                      # suffix for the second image
blank = 'blank.jpg'                 # suffix for the blank image
inst = 'inst.jpg'                   # suffix for instruction image
scrsize = (900,600)                # screen size (pixels)
timelimit = 60                      # image freezes after this time (seconds)
changetime = .4                     # how often images are changing (seconds)
maskingtime = .2                    # how often images are changing (seconds)
waitingtime = 30                    # how long to wait for mouse response

#========================================
# Store info about the experiment session
#========================================

# Show a dialog box to enter session information
exp_name = 'CB_study1'
exp_info = {
            'participant': '',  
            'cueCond': 1
            
            }
dlg = gui.DlgFromDict(dictionary=exp_info, title=exp_name)

# If 'Cancel' is pressed, quit
if dlg.OK == False:
    core.quit()

# Add the date and the experiment name
exp_info['date'] = data.getDateStr()
exp_info['exp_name'] = exp_name

# Set up filename for saving data
if not os.path.isdir(datapath):
    os.makedirs(datapath)
data_fname = exp_info['participant'] + '_' + exp_info['date']
data_fname = os.path.join(datapath, data_fname)

# read in pre made treat list for this participant number
treatfile = treatpath+exp_info['participant']+'_treatout.csv'
treatlist = pandas.DataFrame.from_csv(treatfile)

imlist = map(str, treatlist['imglist'])
n_imgs = len(imlist)                # total number of images

print imlist
print n_imgs
#=========================
# Prepare conditions lists
#=========================
        

# Make the orientations list, in random order
# Half upright (0), half inverted (1)
orilist = [0]*(len(imlist))
rnd.shuffle(orilist)


#===============================
# Creation of window and stimuli
#===============================

# Open a window
win = visual.Window(size=scrsize, color='white', units='pix', fullscr=True)

# Create the start message
start_message = visual.TextStim(win,
                                text="Press space to start the experiment",
                                color='red', height=20)

# Create the bitmap object; its contents can still change
bitmap = visual.SimpleImageStim(win, 
                                image=os.path.join(impath, imlist[0]+asfx))


trialImgList = [asfx,blank,bsfx,blank]*60

#=========================
# Creation of TrialHandler
#=========================

stim_order = []
for im, ori in zip(imlist, orilist):
    stim_order.append({'im': im, 'ori': ori})

trials = data.TrialHandler(stim_order, nReps=1, extraInfo=exp_info,
                           method='sequential', originPath=datapath)


#=====================
# Start the experiment
#=====================

# Display the start message
start_message.draw()
win.flip()

# Initialize two clocks:
#   - for image change time
#   - for response time
change_clock = core.Clock()
rt_clock = core.Clock()
acc_clock = core.Clock()

nTrial = 0
# Start the main loop that goes through all trials
for trial in trials:
    mouse = event.Mouse() 
    mouse
    # Wait for spacebar press to start (or escape to quit)
    keys = event.waitKeys(keyList=['space', 'escape'])
    
    if 'escape' in keys:
        trials.saveAsWideText(data_fname + '.csv', delim=',')
        core.quit()
        
    
    
    # Fetch the image path (minus the suffix)
    im_fname = os.path.join(impath, trial['im'])
    
    # Flip image upside down if necessary    
    bitmap.setFlipHoriz(trial['ori'])

    # Set the reaction time clock to 0
    rt_clock.reset()
    
    # set pre message to display participants progress
    nTrial = nTrial+1
    preMessage = visual.TextStim(win,
                                text=(str(nTrial)+'/'+str(n_imgs)),
                                color='red', height=20)
    preMessage.draw()
    win.flip()
    core.wait(1)
    
 #map(str, imlist[nTrial-1])
    curImg = int(imlist[nTrial-1])
    
    if 10< nTrial <= 27 and exp_info['cueCond'] ==1:
        if 1<=curImg<=10:
            thisChange='color'
        elif 11<=curImg<=70:
            thisChange='size'
        elif 71<=curImg<=130:
            thisChange='dissappear'
        elif 131<=curImg<=190:
            thisChange='move'
        
        condMessage = visual.TextStim(win,
                                    text=(thisChange),
                                    color='red', height=30)
        condMessage.draw()
        win.flip()
        core.wait(1)
        
        if 10< nTrial > n_imgs-((n_imgs-10)/2) and exp_info['cueCond'] ==2:
            if 1<=curImg<=10:
                thisChange='color'
            elif 11<=curImg<=70:
                thisChange='size'
            elif 71<=curImg<=130:
                thisChange='dissappear'
            elif 131<=curImg<=190:
                thisChange='move'
            
            condMessage = visual.TextStim(win,
                                        text=('Change in '+thisChange),
                                        color='red', height=20)
            condMessage.draw()
            win.flip()
            core.wait(1)
    
    nflips = 0
    # Start the trial
    keys = []  # this list will fill up once a participant responds
    while len(keys) == 0 and rt_clock.getTime() < timelimit:
        
        bitmap.setImage(im_fname + trialImgList[nflips])#draw the image from the list A blank B blank etc.
        bitmap.draw()
        win.flip()
        nflips = nflips +1
        # For 500 ms, do nothing except wait for the participant's response
        
        if trialImgList[nflips]=='blank.jpg':
            core.wait(maskingtime)# don't collect responses during blank 
        else:
                change_clock.reset()
                while change_clock.getTime() <= changetime:
                    
                    # Register spacebar or escape press
                    keys = event.getKeys(keyList=['space','escape'])
                    if len(keys) > 0:
                        break
                        
                # Check if participant responded
                if len(keys) > 0:
                
                    # Was it an escape button?
                    if 'escape' in keys:
                        trials.saveAsWideText(data_fname + '.csv', delim=',')
                        core.quit()
                        
                    else:
                        # Take only the last key press
                        
                        print keys
                        print rt_clock.getTime()
                        last_key = keys[-1]
                        rt = rt_clock.getTime()-1# remove the time wasted with preMessage
                        acc = keys[-1][0]
                        
                else:
                    rt = timelimit
                    acc = 0  # no response is an incorrect response
                    
                # Add the current trial's data to the TrialHandler
                trials.addData('rt', rt)
                trials.addData('acc', acc)
        
    
    if acc != 0: 
        # show A image with instructions to click on the change if they hit the space bar
        bitmap.setImage(im_fname + inst)
        bitmap.draw()
        win.flip()
            
        
        mouse.clickReset()
        
        acc_clock.reset()
        while acc_clock.getTime() <= waitingtime:
            if sum(mouse.getPressed()) > 0:
                mousepos =  mouse.getPos()#get mouse position once its been pressed
                print mousepos
                break
        
        trials.addData('mousepos', mousepos) 
        
        advanceMessage = visual.TextStim(win,
                                text="Press space to continue",
                                color='red', height=20)
        advanceMessage.draw()
        win.flip()
    else:
        # if it timed out just tell them to hit space in order to continue
        advanceMessage = visual.TextStim(win,
                                text="Press space to continue",
                                color='red', height=20)
        advanceMessage.draw()
        win.flip()
   
    
  
#======================
# End of the experiment
#======================

# Save all data to a file
trials.saveAsWideText(data_fname + '.csv', delim=',')

# Quit the experiment
core.quit()
# RSFT Gain Loss
This folder holds the "RSFT gain loss" experiment. The following lines introduce you to running and changing the experiment. To run the experiment, follow the instruction for the prerequisites in "1. Prerequesites", and then run the experiment as described in "2. Running the experiment". To change the experiment, read "3. Modifying th experiment"

## 1. Prerequesites
You need oTree. Download it from [https://otree.readthedocs.io](https://otree.readthedocs.io). It requires Python; and we use Python 3.7.

You also need to copy-paste the contents of the folder *_global* from the current folder one folder upwards into your general folder *oTree* that contains all apps, assuming you saved this experiment under *oTree/...*:
* files here in *_global/_static* go one folder up into *oTree/_static*
* files here in  *_global/_templates* go one folder up into *oTree/_templates*

## 2. Running the experiment
Run the experiment by starting the oTree devserver from the command line as follows 
1. Open the command line, navigate to the oTree folder, type `otree devserver`
2. Open the link in your browser that is provided by otree, usually: [http://localhost:8000/demo/](http://localhost:8000/demo/)
3. Click "RSFT Gain Loss" followed by Session-wide link to begin the experiment.

## 3. Modifying the experiment
### Explanation to change parts experiment
  * `static/stimuli` you add new stimuli as comma-separated csv files.
  * `exp.py` contains the set up of the experiment and you will change it if you use new stimuli.






## Folders and Files
### Folders
  - /locale			Language files
  - /static			picture files and stimuli csv files
  - /templates		html pages and their layout

### FILES
  - pages.py		which pages to show?
  - exp.py			set up the experiment: which stimuli?
  - models.py		set up the experiment II
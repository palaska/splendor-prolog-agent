#Splendor playing intelligent agent
##implementation by Baris Palaska


This is the implementation of an agent that considers and handles the states just as humans does. It checks the cards on the board, nobles, its owned coins and development cards, then takes an action.


##How it works
The actions that agent takes depends on the priorities. The priorities are decided based on the current state of the board and owned development cards.

On every step, the agent calculates the development card priorities by checking the nobles and its own development cards. The factors in this process are as followed;

•	Calculation of the distance to nobles (Development cards required to obtain a noble)
•	The commonness of the gem color in nobles (if a color is present in multiple nobles, it is more valuable)
•	Negative feedback caused by having too many development cards from single color (generally it is better to have a more balanced development hand rather than being strong at a certain color)

At the end of these 3 processes and some operations, the agent obtains a priority list of development cards at that state. This priority list is later used in the calculation of the reward by buying each development card.


![alt text](https://github.com/palaska/splendor-prolog-agent/blob/master/results.png "Results")
<center>Results of 100 Games vs Random agent</center>

# PairingAlgorithm
Pairing Algorithm for encounter with restriction on pairing, preference on pairing and round continuation

# Principles
## Summary
This algorithm aim at pairing N subscribers of an event around tables of size S with R rotation.
It is specific for LOTR LCG encounters.
It should be generic, and/or propose a list of specific encounters.
It should manage drop out during rotations.
## Notions
The code use the following definition for this notions:
### Subscriber
A participant of the encounter, which need to be paired.
### Constraints
A list of constraints a participant brings, making him impossible to be paired with participant with __at least__ one constraints in common.
### Table
A pairing of subscribers for a round.
### Round
A rotation during the encounter. Round are supposed to be completed by all table before the next round start.
## Pseudo Algorithm
The algorithm will call all those algorithm each round:

1. **REDUCE**: Reduce the complexity to find a solution if it exist. Return in error if it has explored all possible solution without finding one solution, or return the first solution encountered. 
2. **GREEDY**: Find a better local solution by liberating a seat, free all possible candidate for this seat, and prevent the original subscriber to seat back at the same place, then run the **REDUCE** algorithm.
3. **SHUFFLE**: Try another starting configuration to run the **GREEDY** algorithm and find other local solution.    
4. TO BE IMPLEMENTED: **HEURISTIC**: Try to find a better solution by randomly apply modifications on the pairing. Then run the **SHUFFLE** / **GREEDY** algorithm.

### Reduce
Start from the list of subscribers, then try to seat them one by one to find a solution. 
Will ban impossible solution and revert to surely find a solution if there is at least one.
Will stop at the first solution to not explore all possible solutions.
Will always return the same pairing for same ordered list of subscribers.

1. get all subscribers, define them as non-seated
2. while not all are seated:
  1. if there is a non complete table, try to complete it:
    1. find out who can still seat at the table, with the least possible matching partner. 
    2. if no one match, then dismiss the table and ban the configuration.
    3. if the situation is banned, then ban the configuration.
    4. seat him.
  2. else, create a new table:
    1. select the subscribers with the least possible matching partner. If not possible, then the algorithm end with no possible solution.
    2. select his partner with the least possible matching partner. If no one exist, then break all the tables and ban the configuration.
    3. seat them both.
3. terminate and return the pairing.

### Greedy
Start from an initial solution.
Will always return the same pairing for same ordered list of subscribers.

1. mark all score for each subscriber as 0.
2. while not all score are different than 0:
  1. free a seat with a 0 score.
  2. free all subscribers who could take his place.
  3. ban his place (pairing with a random other subscriber from the table).
  4. run the **REDUCE** algorithm.
  5. calculate score. Impossible solution are given a score of -1. 
  6. if score is better than other:
    1. store the pairing.
    2. set all other score at 0.
    3. use the new pairing.
3. return the stored pairing.

### Shuffle
Start with an initial solution.
Will not return the same pairing from an ordered list of subscribers.

1. for a predetermine number of times _(default=10)_:
  1. shuffle the list of subscriber.
  2. run the **GREEDY** algorithm.
  3. store the pairing if better than the previously stored.
2. return the stored pairing.

### Heuristic
NOT IMPLEMENTED.

# Arguments
For now, this algorithm can take one argument: a file with the description of the subscribers.
In the future, it should take the specification of the type of encounter, along/within the subscribers file.

# Output
For now, this algorithm generate a file with the same name as the input suffixed by .out. Format can be specified.
In the future, format should be passed as an argument and more format should be supported. 

# Parameters

__sizeOfTable__: the size S of subscribers per table _(default=3)_
__numberOfRound__: the number R of rotation during the encounter _(default=3)_
__score group__: the score penalty to prevent group of subscriber to be paired together _(default=50)_ 
__score alreadySeen__: the score penalty to prevent subscribers to be paired again in future round _(default=25)_
__score constraintSpecific__: the score penalty to balance table, specific to the type of encounters _(default=25)_

# Package Description

__src/main/resources/applicaion.conf__: configuration file for the parameters

__src/main/scala/Constraints.scala__: source file for describing constraints

__src/main/scala/Subscriber.scala__: source file for describing subscriber

__src/main/scala/ReduceAlgorithm.scala__: source file for the Reduce algorithm

__src/main/scala/GreedyAlgorithm.scala__: source file for the Greedy algorithm

__src/main/scala/ShuffleAlgorithm.scala__: source file for the Shuffle algorithm

__src/main/scala/Tools.scala__: source file for some tools: logging and providing a random input

__src/main/scala/FileOperation.scala__: source file for file operation: reading input and writting output

# TODO LIST

[x] Allow different number of subscriber per table 
[10%] Generic support for constraints
[ ] More file format support
[ ] Allow drop out of subscribers in between rounds
[ ] Heuristic algorithm
[ ] See if possible for Shuffle to make a mean over all the rounds
let taskSequences = {
  1: [['s', 'i', 's', 1, 1], ['r', 'c', 's', 1, 2], ['s', 'i', 's', 1, 3], ['r', 'c', 's', 1, 4]],
  2: [['r', 'c', 's', 2, 1], ['r', 'c', 's', 2, 2], ['s', 'i', 's', 2, 3], ['s', 'i', 's', 2, 4]],
  3: [['r', 'i', 's', 3, 1], ['s', 'c', 's', 3, 2], ['r', 'i', 's', 3, 3], ['s', 'c', 's', 3, 4]],
  4: [['s', 'c', 's', 4, 1], ['s', 'c', 's', 4, 2], ['r', 'i', 's', 4, 3], ['r', 'i', 's', 4, 4]]
}

let blockTypeFillers = {
  'A': ['r', 'i', 'f', NaN, NaN],
  'B': ['s', 'i', 'f', NaN, NaN],
  'C': ['r', 'c', 'f', NaN, NaN],
  'D': ['s', 'c', 'f', NaN, NaN]
}

let flipBlockType = {
  'A': 'D',
  'B': 'C',
  'C': 'B',
  'D': 'A'
}

function buildTaskSequence(){
  // get sequence order
  let sequenceOrder = shuffle(new Array(nSequenceReps).fill(_.range(1, nSequenceTypes + 1)).flat());

  // for each block, add to large array of all trials
  let taskArray = []
  blockOrder.forEach(blockLetter => {
    taskArray = taskArray.concat(buildBlockSequence(sequenceOrder, blockLetter))
  })

  return taskArray;
}

function buildBlockSequence(sequenceOrder, blockType){
  // get filler order
  let nFillerBlocks = nSequenceTypes * nSequenceReps + 1;
  let fillers = new Array(nFillerBlocks).fill(minFillerLength)
  let remainingFillers = nFillers - (fillers.length * minFillerLength)
  while (remainingFillers > 0) {
    let randIdx = Math.floor(Math.random() * fillers.length)
    if (fillers[randIdx] == maxFillerLength) {
      continue
    } else {
      fillers[randIdx] = fillers[randIdx] + 1
      remainingFillers--;
    }
  }

  // get sequence flips
  let sequenceFlips = shuffle(new Array(nSequenceFlips).fill('f').concat(new Array(nSequenceNotFlips).fill('n')));

  //build final task array of sequence and fillers
  let blockArray = []
  for (var i = 0; i < nFillerBlocks; i++) {
    // fillers
    for (let j = 0; j < fillers[i]; j++) {
      if (i == 0 && j == 0) {
        blockArray = blockArray.concat([['n', blockTypeFillers[blockType][1], blockTypeFillers[blockType][2], NaN, NaN]])
      } else {
        blockArray = blockArray.concat([blockTypeFillers[blockType]])
      }
    }

    if (i == nFillerBlocks - 1) {
      break;
    }

    // next add the sequence (except last iteration)
    blockArray = blockArray.concat(taskSequences[sequenceOrder[i]]);

    // finally add the random 50/50 sequence flipper
    if (sequenceFlips[i] == 'n') {
      blockArray = blockArray.concat([blockTypeFillers[blockType]]);
    } else {
      blockArray = blockArray.concat([blockTypeFillers[flipBlockType[blockType]]]);
    }
  }

  return blockArray;
}

function getTaskSet(switchCongruencyArr, task = null){
  if (task != null) {
    return new Array(switchCongruencyArr.length).fill(task);
  }

  let taskA = "m", taskB = "p";

  // task array build
  let taskSet = [], task1, task2, taskToAdd = task1;
  for (var i = 0; i < switchCongruencyArr.length; i++) {
    if (switchCongruencyArr[i][0] == 'n') {
      //if first trial of block, refigure out first and second
      task1 = (Math.random() > 0.5) ? taskA : taskB;
      task2 = (task1 == taskA) ? taskB : taskA;
      taskToAdd = task1;
      taskSet.push(taskToAdd)
    } else if (switchCongruencyArr[i][0] == 's') {
      taskToAdd = (taskToAdd == task1) ? task2 : task1;
      taskSet.push(taskToAdd)
    } else {
      taskSet.push(taskToAdd)
    }
  }
  return taskSet;
}

function getStimSet(switchCongruencyArr){
  let targetsArr = [];
  for (let i = 0; i < switchCongruencyArr.length; i++) {
    if (switchCongruencyArr[i][1] == "c") {
      if (taskMapping == 1 || taskMapping == 4) {
        if (i != 0) {
          targetsArr.push(_.sample([2,4,7,9].filter(n => n != targetsArr[i - 1])));
        } else {
          targetsArr.push(_.sample([2,4,7,9]));
        }
      } else {
        if (i != 0) {
          targetsArr.push(_.sample([1,3,6,8].filter(n => n != targetsArr[i - 1])));
        } else {
          targetsArr.push(_.sample([1,3,6,8]));
        }
      }
    } else {
      if (taskMapping == 1 || taskMapping == 4) {
        if (i != 0) {
          targetsArr.push(_.sample([1,3,6,8].filter(n => n != targetsArr[i - 1])));
        } else {
          targetsArr.push(_.sample([1,3,6,8]));
        }
      } else {
        if (i != 0) {
          targetsArr.push(_.sample([2,4,7,9].filter(n => n != targetsArr[i - 1])));
        } else {
          targetsArr.push(_.sample([2,4,7,9]));
        }
      }
    }
  }
  return targetsArr;
}

function createActionArray(){
  let responseMappings = {
    1: {
      odd : [90,122],
      even : [77,109],
      larger : [90,122],
      smaller : [77,109]
    },
    2: {
      odd : [90,122],
      even : [77,109],
      larger : [77,109],
      smaller : [90,122]
    },
    3: {
      odd : [77,109],
      even : [90,122],
      larger : [90,122],
      smaller : [77,109]
    },
    4: {
      odd : [77,109],
      even : [90,122],
      larger : [77,109],
      smaller : [90,122]
    }
  };

  // for each stimulus and associated task, identify required action for correct response
  let actionArr = [];
  taskStimuliSet.forEach(function(taskStim, index){
    let task = cuedTaskSet[index];
    actionArr.push(responseMappings[taskMapping][getCategory(taskStim, task)]);
  })
  return actionArr;
}

function getCategory(number, task){
  if (task == "m") {
    if (number < 5) {
      return "smaller";
    } else {
      return "larger";
    }
  } else {
    if (number%2 == 0) {
      return "even";
    } else {
      return "odd";
    }
  }
}

function buildPracticeTaskSequence(nTrials, task = null){
  let practiceSequences;
  if (task == null) {
    practiceSequences = {
      1: ['r', 'c', NaN, NaN, NaN],
      2: ['r', 'i', NaN, NaN, NaN],
      3: ['s', 'c', NaN, NaN, NaN],
      4: ['s', 'i', NaN, NaN, NaN]
    }
  } else {
    practiceSequences = {
      1: ['r', 'c', NaN, NaN, NaN],
      2: ['r', 'i', NaN, NaN, NaN]
    }
  }

  let nSequences = Math.ceil(nTrials / Object.keys(practiceSequences).length);
  let sequenceOrder = shuffle(new Array(nSequences).fill(Object.keys(practiceSequences)).flat());

  let taskArr = []
  for (let i = 0; i < sequenceOrder.length; i++) {
    if (i == 0) {
      taskArr.push(['n', (Math.random() > 0.5) ? 'c' : 'i', NaN, NaN, NaN])
    } else {
      taskArr.push(practiceSequences[sequenceOrder[i]])
    }
  }

  return taskArr.splice(0, nTrials);
}

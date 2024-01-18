"use strict"

// some tasks.js specific variables
let trialFunc;
let screenSizePromptCount = 0, numScreenSizeWarnings = 2;

// main task function
function runTasks(){
  //clear any instructions and show canvas
  hideInstructions();
  canvas.style.display = "inline-block";
  $(".canvasas").show();
  document.body.style.cursor = 'none';

  // change background color
  window.document.body.style.backgroundColor = "lightgrey";

  //reset accuracy and block trial count
  accCount = 0; blockTrialCount = 0;

  // --- PRACTICE 1 --- //
  if (expStage.indexOf("prac1") != -1){

    runPractice(numPracticeTrials, getFirstPracticeTask());

  // --- PRACTICE 2 --- //
  } else if (expStage.indexOf("prac2") != -1){

    runPractice(numPracticeTrials, getSecondPracticeTask());

  // --- PRACTICE 3 --- //
  } else if (expStage.indexOf("prac3") != -1) {

    runPractice(numPracticeTrials * 2);

  // --- MAIN TASK --- //
  } else if (expStage.indexOf("main") != -1) {

    // initialize block type
    blockIndexer = 0;
    blockType = blockOrder[blockIndexer];

    // initialize main experiment
    trialCount = 0; block = 1;

    // create task arrays
    createTaskArrays(trialsPerBlock);

    // start countdown into main task
    countDown(3);
  }
}

function runPractice(numPracticeTrials, task = null){
  trialCount = 0;
  if (repeatNecessary != true){
    block = 1;
  }

  // create task array for practice block
  createPracticeTaskArrays(numPracticeTrials, task);

  // start countdown into practice block
  countDown(3);
}

function createPracticeTaskArrays(nTrials, task){
  let taskSwitchCongruencyArr = buildPracticeTaskSequence(nTrials, task)
  taskStimuliSet = getStimSet(taskSwitchCongruencyArr);
  cuedTaskSet = getTaskSet(taskSwitchCongruencyArr, task);
  switchRepeatList = taskSwitchCongruencyArr.map(item => item[0])
  actionSet = createActionArray();
  sequenceTypeArr = taskSwitchCongruencyArr.map(item => item[2])
  sequenceKindArr = taskSwitchCongruencyArr.map(item => item[3])
  sequencePositionArr = taskSwitchCongruencyArr.map(item => item[4])
}

function createTaskArrays(){
  let taskSwitchCongruencyArr = buildTaskSequence()
  taskStimuliSet = getStimSet(taskSwitchCongruencyArr);
  cuedTaskSet = getTaskSet(taskSwitchCongruencyArr);
  switchRepeatList = taskSwitchCongruencyArr.map(item => item[0])
  actionSet = createActionArray();
  sequenceTypeArr = taskSwitchCongruencyArr.map(item => item[2])
  sequenceKindArr = taskSwitchCongruencyArr.map(item => item[3])
  sequencePositionArr = taskSwitchCongruencyArr.map(item => item[4])
}

function countDown(seconds){
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = "black";
  ctx.font = "bold 80px Arial";
  if (seconds > 0){
    ctx.fillText(seconds,canvas.width/2,canvas.height/2)
    setTimeout(function(){countDown(seconds - 1)},1000);
  } else {
    trialFunc = (expStage.indexOf("main") != -1) ? runTrial : runPracticeTrial;
    trialFunc();
  }
}

function runPracticeTrial(){
  sectionType = "pracTask";
  if (trialCount < taskStimuliSet.length){
    if (expType == 3){ //check if key is being held down
      expType = 4;
      promptLetGo();
    } else {
      // check if screen size is big enough
      if (screenSizeIsOk()){
        // start next trial cycle
        fixationScreen();
      } else {
        promptScreenSize();
      }
    }
  } else { //if practice block is over, go to feedback screen
    practiceAccuracyFeedback( Math.round( accCount / (blockTrialCount) * 100 ) );
  }
}

function runTrial(){
  sectionType = "mainTask";
  if (trialCount < numBlocks * trialsPerBlock) { //if exp isn't over yet

    if (trialCount % trialsPerBlock == 0 && !breakOn && trialCount != 0) {
      //if arrived at big block break, update block information
      breakOn = true;
      bigBlockScreen();

    } else if (trialCount % miniBlockLength == 0 && !breakOn && trialCount != 0) {

      //if arrived at miniblock break
      breakOn = true; miniBlockScreen();

    } else {

      if (expType == 3 || expType == 5){ //if key is being held down still
        expType = 4;
        promptLetGo();

      } else {

        // check if screen size is big enough
        if (screenSizeIsOk()){
          // start next trial cycle
          breakOn = false;
          fixationScreen();

        } else {

          promptScreenSize();

        }
      }
    }

  } else {
    endOfExperiment();
  }
}

function fixationScreen(){
  // prepare canvas
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  ctx.font = "bold 85px Arial";
  ctx.fillStyle = "black";

  // display fixation
  ctx.fillText("+",canvas.width/2,canvas.height/2);

  // go to next after appropriate time
  setTimeout(stimScreen, fixInterval);
}

function stimScreen(){
  if (expType == 5) {

    expType = 6;
    promptLetGo();

  } else {

    stimOnset = new Date().getTime() - runStart;
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    // draw rectangle cue
    drawRect();

    //reset all response variables and await response (expType = 1)
    expType = 1; acc = NaN, respTime = NaN, partResp = NaN, respOnset = NaN;

    // display stimulus
    drawStimulus();

    // proceed to ITI screen after timeout
    stimTimeout = setTimeout(itiScreen,stimInterval);
  }
}

function drawStimulus(){
  let number = taskStimuliSet[trialCount];
  ctx.fillStyle = "black";
  ctx.font = "bold 120px Arial";

  // draw number on canvas
  ctx.fillText(number,canvas.width/2,canvas.height/2);
}

function drawRect(){
  // text margin
  let borderMargin = 40;

  // set size of rectangle
  let frameWidth = 160;
  let frameHeight = 160;

  // draw box
  ctx.beginPath();
  ctx.lineWidth = "10";
  ctx.strokeStyle = (cuedTaskSet[trialCount] == "m") ? magnitudeColor : parityColor;
  ctx.rect((canvas.width/2) - (frameWidth/2), (canvas.height/2) - (frameHeight/2) - 5, frameWidth, frameHeight);
  ctx.stroke();
}

function itiScreen(){
  if (expType == 1) { // participant didn't respond
    expType = 0;
  } else if (expType == 2) { //participant still holding down response key
    expType = 3;
  }

  // variable for readability below
  let stim = taskStimuliSet[trialCount];

  // log data
  data.push(["task", sectionType, block, blockType, trialCount + 1,
    blockTrialCount + 1, getAccuracy(acc), respTime, taskStimuliSet[trialCount],
    getCongruency(taskStimuliSet[trialCount]), cuedTaskSet[trialCount], switchRepeatList[trialCount], partResp, stimOnset, respOnset, actionSet[trialCount][1], sequenceTypeArr[trialCount], sequenceKindArr[trialCount], sequencePositionArr[trialCount], NaN, NaN, NaN]);
  console.log(data);

  // prepare ITI canvas
  ctx.fillStyle = "black"//accFeedbackColor();
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  // display response feedback (correct/incorrect/too slow)
  ctx.font = "bold 80px Arial";
  ctx.fillText(accFeedback(),canvas.width/2,canvas.height/2);

  // trial finished. iterate trial counters
  trialCount++; blockTrialCount++;

  // proceed to next trial or to next section after delay
  setTimeout(trialFunc, ITIInterval());
}

function practiceAccuracyFeedback(accuracy){
  sectionStart = new Date().getTime() - runStart;
  sectionType = "pracFeedback";

  // prepare canvas
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = "black";
  ctx.font = "25px Arial";
  expType = 11;

  // display feedback
  if (accuracy < practiceAccCutoff) { //if accuracy is too low
    repeatNecessary = true;

    // display feedback text
    ctx.fillText("You got " + accuracy + "% correct in this practice block.",canvas.width/2,canvas.height/2 - 50);
    ctx.fillText("Remember, you need to get >" + practiceAccCutoff + "%.",canvas.width/2,canvas.height/2);
    ctx.fillText("Press any button to go back ",canvas.width/2,canvas.height/2 + 80);
    ctx.fillText("to the instructions and try again.",canvas.width/2,canvas.height/2 + 110);

  } else { //otherwise proceed to next section

    // display feedback text
    ctx.fillText("You got " + accuracy + "% correct in this practice block.",canvas.width/2,canvas.height/2 - 50);
    ctx.fillText("Press any button to go on to the next section.",canvas.width/2,canvas.height/2 + 100);

    // prep key press/instruction logic
    repeatNecessary = false;

  }
}

function miniBlockScreen(){
  sectionType = "miniblock";
  sectionStart = new Date().getTime() - runStart;
  expType = 7;

  // prep canvas
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = "black";
  ctx.font = "25px Arial";

  // display miniblock text
  ctx.fillText("You are "+ Math.round(((trialCount%trialsPerBlock)/trialsPerBlock)*100)+"% through this block.",canvas.width/2,canvas.height/2 - 50);
  ctx.fillText("Your overall accuracy so far is " + Math.round((accCount/trialCount)*100) + "%.",canvas.width/2,canvas.height/2);
  ctx.fillText("Press any button to continue.",canvas.width/2,canvas.height/2 + 100);
  ctx.font = "italic bold 22px Arial";
  ctx.fillText("Remember, you need >" + taskAccCutoff + "% accuracy.",canvas.width/2,canvas.height/2 + 50);
}

function bigBlockScreen(){
  let minutesBreak = 2;
  sectionType = "blockBreak";
  sectionStart = new Date().getTime() - runStart;
  expType = 0; //else expType stays = 1 till below runs
  setTimeout(function(){expType = 7},2000);

  // display break screen (With timer)
  drawBreakScreen("0" + minutesBreak,"00", minutesBreak);
  blockBreakFunction(minutesBreak,0,minutesBreak);

  function blockBreakFunction(minutes, seconds, max){
    let time = minutes*60 + seconds;
    ctx.fillStyle = "black";
    sectionTimer = setInterval(function(){
      if (time < 0) {return}
      ctx.fillStyle = (time <= 60) ? "red" : "black";
      let minutes = Math.floor(time / 60);
      if (minutes < 10) minutes = "0" + minutes;
      let seconds = Math.floor(time % 60);
      if (seconds < 10) seconds = "0" + seconds;
      drawBreakScreen(minutes, seconds, max);
      time--;
    }, 1000);
  }
}

function drawBreakScreen(minutes, seconds, max){
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  // draw timer (with color from previous function)
  ctx.font = "bold 45px Arial";
  ctx.fillText(minutes + ":" + seconds,canvas.width/2,canvas.height/2 - 100);

  // display miniblock text
  ctx.fillStyle = "black";
  ctx.font = "25px Arial";
  ctx.fillText("This is a short break. Please don't pause for more than " + max + " minutes.",canvas.width/2,canvas.height/2 - 150);
  if (numBlocks - block > 1) {
    ctx.fillText("You are finished with block " + block + ". You have " + (numBlocks - block) + " blocks left.",canvas.width/2,canvas.height/2);
  } else {
    ctx.fillText("You are finished with block " + block + ". You have " + (numBlocks - block) + " block left.",canvas.width/2,canvas.height/2);
  }
  ctx.fillText("Your overall accuracy so far is " + Math.round((accCount/trialCount)*100) + "%.",canvas.width/2,canvas.height/2+50);
  ctx.font = "bold 25px Arial";
  ctx.fillText("Press any button to continue.",canvas.width/2,canvas.height/2 + 200);
}

// functions for determining ITI feedback depending on accuracy
function accFeedback(){
  if (acc == 1){
    return "Correct";
  } else if (acc == 0) {
    return "Incorrect";
  } else {
    return "Too Slow";
  }
}

function accFeedbackColor(){
  if (acc == 1){
    return "green";
  } else if (acc == 0) {
    return "red";
  } else {
    return "black";
  }
}

function getCongruency(num){
  if ([2,4,7,9].indexOf(num) != -1) {
    if (taskMapping == 1 || taskMapping == 4) {
      return "c";
    } else {
      return "i";
    }
  } else {
    if (taskMapping == 1 || taskMapping == 4) {
      return "i";
    } else {
      return "c";
    }
  }
}

// see function navigateInstructionPath() in tasks.js for navigation code

// global instruction iterator information. Change as needed
let instructions = {
  // contains the interator for each instruction block
  iterator: {
    "prac1-1": 1, "prac1-2": 1, "prac2": 1, "prac3": 1, "main1": 1, "main1-2": 1
  },
  // contains the max value of each instruction iteration. iteration will STOP at max.
  max: {
    "prac1-1": 4, "prac1-2": 5, "prac2": 5, "prac3": 5, "main1": 3, "main1-2": 4
  },
  // what does instruction section end with?
  // #nextSectionButton, #startExpButton, keyPressNextSection, keyPressStartTask
  exitResponse: {
    "prac1-1": '#nextSectionButton',
    "prac1-2": 'keyPressStartTask',
    "prac2": 'keyPressStartTask',
    "prac3": 'keyPressStartTask',
    "main1": '#nextSectionButton',
    "main1-2": 'keyPressStartTask'
  }
}
let iterateAgain = false, task;

function navigateInstructionPath(repeat = false){
  if (repeat == true) {
    // if multi stage instructions, ensures it goes back to first not second
    // switch (expStage){
    //   case "prac1-1":
    //   case "prac1-2":
    //     expStage = "prac1-1";
    //     break;
    // }
    runInstructions();
  } else {
    switch (expStage){
      case "prac1-1":
        expStage = "prac1-2";
        break;
      case "prac1-2":
        expStage = "prac2";
        break;
      case "prac2":
        expStage = "prac3";
        break;
      case "prac3":
        expStage = "main1";
        break;
      case "main1":
        expStage = "main1-2";
        break;
    }
    runInstructions();
  }
}

function displayDefaults(stage){
  // default values of instruction blocks. add any special cases
  switch(stage){
    case "final":
      showFirst();
      $('.instruction-header').hide();
      break;
    case "intro1":
    case "main1":
    case "main2":
    case "main3":
    case "main4":
    case "main5":
       // showFirst();
    default:
      showFirst();
      $('.instruction-header').show();
      break;
  }
}

function getNextInstructions(slideNum, expStage){
/* use the following options when modifying text appearance
    -  iterateAgain = true;
    -  changeTextFormat('#instructions' + slideNum,'font-weight','bold');
    -  changeTextFormat('#instructions' + slideNum,'font-size','60px');
    -  changeTextFormat('#instructions' + slideNum,'color','red');
    -  changeTextFormat('#instructions' + slideNum,'margin-top', '20px');
    -  changeTextFormat('#instructions' + slideNum,'margin-bottom', '20px');
    - $("<img src='../pics/finalpics/M33.jpg' class='insertedImage'>").insertAfter( "#instructions" + slideNum);
*/
  switch (expStage){
    case "prac1-1":
      switch (slideNum){
        case 1:
          $( getImageText(instructionImages[2]) ).insertAfter( "#instructions" + slideNum);
          return "In this task, you will see a single number (1-9 excluding 5) appear in the middle of the screen.";
        case 2:
          return "You will either need to identify the number as " + first_task() + " or as " + second_task() + ".";
        case 3:
          return "On each trial, a colored rectangle surrounding the number will tell you how you should classify the number.";
        case 4:
          return "You will begin with a few practice sections to familiarize you with the task before beginning the main experiment. You will need to get at least " + practiceAccCutoff + "% correct on each practice section before you can move on to the next one. The entire experiment is expected to take about 40 minutes.";
      }
    case "prac1-2":
      switch (slideNum){
        case 1:
          $( getImageText(instructionImages[get_task_image(1)]) ).insertAfter( "#instructions" + slideNum);
          return "In the first practice task, you will indicate whether the number in the center of the screen is " + first_task() + ".";
        case 2:
          return "Press 'Z' with your left hand index finger if the number is " + task_instruction(1,1) + ".";
        case 3:
          return "Press 'M' with your right hand index finger if the number is " + task_instruction(1,2) + ".";
        case 4:
          $( getImageText(instructionImages[1]) ).insertAfter( "#instructions" + slideNum);
          iterateAgain = true;
          return "This block contains " + numPracticeTrials + " trials. Please place your hands on the 'Z' and 'M' keys as shown.";
        case 5:
          changeTextFormat('#instructions' + slideNum,'font-weight','bold');
          return "Press any button to begin.";
      }
    case "prac2":
      task = 2;
      switch (slideNum){
        case 1:
          $( getImageText(instructionImages[get_task_image(2)]) ).insertAfter( "#instructions" + slideNum);
          return "In the second practice task, you will indicate whether the number in the center of the screen is " + second_task() + ".";
        case 2:
          return "Press 'Z' with your left hand index finger if the number is " + task_instruction(2,1) + ".";
        case 3:
          return "Press 'M' with your right hand index finger if the number is " + task_instruction(2,2) + ".";
        case 4:
          $( getImageText(instructionImages[1]) ).insertAfter( "#instructions" + slideNum);
          iterateAgain = true;
          return "This block contains " + numPracticeTrials + " trials. Please place your hands on the 'Z' and 'M' keys as shown.";
        case 5:
          changeTextFormat('#instructions' + slideNum,'font-weight','bold');
          return "Press any button to begin.";
      }
    case "prac3":
      switch (slideNum){
        case 1:
          return "In this last practice task, you will either indicate if the number is " + first_task() + " or if the number is " + second_task() + ", based on the color of the rectangle surrounding the number.";
        case 2:
          $( getImageText(instructionImages[get_task_image(1)]) ).insertBefore( "#instructions" + slideNum);
          return "If the rectangle is " + colorFirstTask() + ", indicate if the number is " + first_task() + " using the 'Z' and 'M' keys, respectively." ;
        case 3:
          $( getImageText(instructionImages[get_task_image(2)]) ).insertBefore( "#instructions" + slideNum);
          return "If the rectangle is " + colorSecondTask() + ", indicate if the number is " + second_task() + " using the 'Z' and 'M' keys, respectively." ;
        case 4:
          iterateAgain = true;
          $( getImageText(instructionImages[1]) ).insertAfter( "#instructions" + slideNum);
          return "This block contains "+(numPracticeTrials * 2)+" trials. Please place your hands on the 'Z' and 'M' keys as shown.";
        case 5:
          changeTextFormat('#instructions' + slideNum,'font-weight','bold');
          return "Press any button to begin.";
      }
    case "main1":
      switch (slideNum){
        case 1:
          return "Great job! You are now ready to begin the main experiment.";
        case 2:
          return "This experiment consists of 4 sections, with each section lasting about 8 minutes. You will get short breaks in between each section.";
        case 3:
          changeTextFormat('#instructions' + slideNum,'font-weight','bold');
          return "Remember to respond as quickly and as accuractely as possible on each trial.";
      }
    case "main1-2":
      switch (slideNum){
        case 1:
          $( getImageText(instructionImages[get_task_image(1)]) ).insertBefore( "#instructions" + slideNum);
          return "If the rectangle is " + colorFirstTask() + ", indicate if the number is " + first_task() + " using the 'Z' and 'M' keys, respectively." ;
        case 2:
          $( getImageText(instructionImages[get_task_image(2)]) ).insertBefore( "#instructions" + slideNum);
          return "If the rectangle is " + colorSecondTask() + ", indicate if the number is " + second_task() + " using the 'Z' and 'M' keys, respectively." ;
        case 3:
          iterateAgain = true;
          $( getImageText(instructionImages[1])).insertAfter( "#instructions" + slideNum);
          return "Please place your hands on the 'Z' and 'M' keys as shown.";
        case 4:
          changeTextFormat('#instructions' + slideNum,'font-weight','bold');
          return "Press any button to begin.";
    }
  }
}

function runInstructions(){
  // show cursor
  document.body.style.cursor = 'auto';

  // set background color
  window.document.body.style.backgroundColor = "white"

  // main instruction function (come here at start of instruction block)
  sectionStart = new Date().getTime() - runStart;
  sectionType = "instructions";

  // hide/clear everything, just in case
  hideInstructions();

  // hide canvas and other tasks if visible
  $(".canvasas").hide();
  $("#oddOneOutTaskDiv").hide();
  $("#network-diagram").hide();
  // if (showNavButtons) {
  //   $("#navButtons").show();
  // }

  // if need to repeat instructions (e.g., participant failed to meet accuracy requirement), then reshow all instructions
  if (instructions["iterator"][expStage] >= instructions["max"][expStage]){

    // loop through instructions and show
    for (var i = 1; i <= instructions["max"][expStage]; i++) {
      $('#instructions' + i).text( getNextInstructions( i, expStage ));
    }

    // reset iterateAgain in case looping turned it on by accident
    iterateAgain = false;

    // display instructions and prepare exit response mapping
    $('#instructionsDiv').show();
    displayDefaults(expStage);
    exitResponse();

  } else {

    // remove any previous click listeners, if any
    $(document).off("click","#nextInstrButton");
    $(document).off("click","#startExpButton");
    $(document).off("click","#nextSectionButton");

    // clear all previous instructions, reset styles, and remove pictures
    for (let i = 1; i <= 8; i++) {
      $('#instructions' + i).text("");
      resetDefaultStyles('#instructions' + i);
      clearInsertedContent();
    }

    // display proper instruction components, in case they are hidden
    $('#instructionsDiv').show();
    $('#nextInstrButton').show();
    $('#nextSectionButton').hide();
    $('#startExpButton').hide();
    displayDefaults(expStage);
  }

  /* code for "Next" button click during instruction display
        if running from Atom, need to use $(document).on, if through Duke Public Home Directory, either works.
        https://stackoverflow.com/questions/19237235/jquery-button-click-event-not-firing
  */
  $(document).on("click", "#nextInstrButton", function(){
  // $("#nextInstrButton").on('click', function(){
    iterateInstruction();
  });

  // code for click startExperiment button
  $(document).on('click', '#startExpButton', function(){
    // update data logger on time spent in section
    sectionEnd = new Date().getTime() - runStart;
    logSectionData();

    $('#instructionsDiv').hide();
    $('#startExpButton').hide();
    clearInsertedContent();

    // clear all button press listeners
    $(document).off("click","#nextInstrButton");
    $(document).off("click","#startExpButton");
    $(document).off("click","#nextSectionButton");
    experimentFlow();
  });

  $(document).on('click', '#nextSectionButton', function(){
    // update data logger on time spent in section
    sectionEnd = new Date().getTime() - runStart;
    logSectionData();

    // clear all button press listeners
    $(document).off("click","#nextInstrButton");
    $(document).off("click","#startExpButton");
    $(document).off("click","#nextSectionButton");
    navigateInstructionPath();
  });
};

function iterateInstruction(){
  let instrNum = instructions["iterator"][expStage];
  $('#instructions' + instrNum).text( getNextInstructions( instrNum, expStage));

  // iterate as appropriate or allow next phase
  if (instrNum < instructions["max"][expStage]){
    instructions["iterator"][expStage]++;
  } else{
    exitResponse();
  }

  if (iterateAgain == true) {
    iterateAgain = false;
    iterateInstruction();
  }
}

function exitResponse(){
  $('#nextInstrButton').hide();
  if (instructions["exitResponse"][expStage] == "#startExpButton"){
    $('#startExpButton').show();
  } else if (instructions["exitResponse"][expStage] == "#nextSectionButton") {
    $('#nextSectionButton').show();
  } else if (instructions["exitResponse"][expStage] == "keyPressStartTask"){
    expType = 8;
  } //else if (instructions["exitResponse"][expStage] == "buttonPressNextSection"){
    //keyListener = 6;
  //}
}

function getImageText(imageURL){
  return "<img src='" + imageURL + "' class='insertedImage'>";
}

function showFirst() {
  iterateInstruction();
}

function changeTextFormat(elementName, property ,changeTo){
  $(elementName).css( property , changeTo );
}

function clearInsertedContent(){
  $('.insertedImage').remove();
  $('.insertedContent').remove();
}

function hideInstructions(){
  // remove any previous click listeners, if any
  $(document).off("click","#nextInstrButton");
  $(document).off("click","#startExpButton");
  $(document).off("click","#nextSectionButton");

  // hide instruction DOMs
  $('#instructionsDiv').hide();
  $('#startExpButton').hide();
  $('#nextSectionButton').hide();

  // clear text from instruction DOMs
  for (let i = 1; i <= 8; i++) {
    $('#instructions' + i).text("");
    resetDefaultStyles('#instructions' + i);
    clearInsertedContent();
  }
}

function resetDefaultStyles(domObject){
  $(domObject).css('font-weight','');
  $(domObject).css('font-size','');
  $(domObject).css('color','');
  $(domObject).css('margin-top','');
  $(domObject).css('margin-bottom','');
}

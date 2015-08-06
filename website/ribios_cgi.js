$(function() {
  var icons = {
    header: "ui-icon-circle-arrow-e",
    headerSelected: "ui-icon-circle-arrow-s"
  };
  $("#tabs").tabs({	
    // event: "mouseover"
  })
  $("#paraAccordion").accordion({
    icons: icons
  });
})

/*---------- row/column dendrograms ----------*/
function checkRow() {
  var rowvcheck = $("#rowvcheck").get(0);
  var showrowv = $("#showrowvcheck").get(0);
  if (rowvcheck.checked == false) {
    showrowv.checked=false;
    showrowv.disabled=true;
  } else {
    showrowv.disabled=false;
  }
}

function checkCol() {
  var colvcheck = $("#colvcheck").get(0);
  var showcolv = $("#showcolvcheck").get(0);
  if (colvcheck.checked == false) {
    showcolv.checked=false;
    showcolv.disabled=true;
  } else {
    showcolv.disabled=false;
  }
}

/*---------- input events  ----------*/
function clearUpDowns() {
  $("#uptags").val("");
  $("#downtags").val("");
  $("#chiptype").val("");
  inputOnchange();
}
function clearTextarea() {
  $("#inputfile").val("");
  $("#clearTextarea").hide();
  inputOnchange();
}

function removeUpFile() {
  $("#uptagsFile").replaceWith("<input type=\"file\" id=\"uptagsFile\" name=\"uptagsFile\" class=\"uploadCtrl\" onChange=\"upFileOnchange();\"/>");
  inputOnchange();
}

function removeDownFile() {
  $("#downtagsFile").replaceWith("<input type=\"file\" id=\"downtagsFile\" name=\"downtagsFile\" class=\"uploadCtrl\" onChange=\"downFileOnchange();\"/>")
  inputOnchange();
}

function upFileOnchange() {
  inputOnchange();
}
function downFileOnchange() {
  inputOnchange();

}
function chiptypeOnchange() {
  inputOnchange();
}
function datasetOnchange() {
  inputOnchange();
}

// inputOnchange: check whether following inputs are given
// (1) either neither of uptags/downtags is empty, or neither uptagsFile/downtagsFile are given
// (2) One valid annotation has been selected
// (3) At least one dataset has been selected

var help=0;
function submitButtonHelpClick() {
  help = (help==0) ? 1 : 0
  if(help==0) {
    $("#submitButtonLabel").hide()
    $("#submitButtonHelp").attr("title", "Turn on tips");
  } else {
    $("#submitButtonLabel").show()
    $("#submitButtonHelp").attr("title", "Turn off tips");
  }
}
function doSubmit() { 
 var f = function() { // use setTimeout trick to make the loading gif show
   var l=$('#waitIcon')
   l.show();
   l.attr('src', l.attr('src'));
  };
  setTimeout(f, 100); 
}

function warnInput(msg) {
  $("#submitButton").attr("title",msg)
  $("#submitButtonLabel").text(msg);
}
function inputOnchange() {
  $("#submitButton").attr("disabled", "disabled");
  $("#submitButtonHelp").show();
  
  var upByPaste=$("#uptags").val().length != 0;
  var upByFile=$("#uptagsFile").val().length != 0;
  var downByPaste=$("#downtags").val().length!=0;
  var downByFile=$("#downtagsFile").val().length!=0;

  if(upByPaste && upByFile) {
    warnInput("duplicated up-tags by pasting and file uploading: please remove one.")
    return
  }

  if(downByPaste && downByFile) {
    warnInput("duplicated down-tags by pasting and file uploading: please remove one.")
    return
  }  

  var hasUp=upByPaste || upByFile;
  var hasDown=downByPaste || downByFile;

  if(!(hasUp && hasDown)) {	
    warnInput("missing up or down tags: please check input")
    return
  }

  var hasChiptype = $("#chiptype").val()!=""
  if(!hasChiptype) {
    warnInput("select the annotation.")
    return
  }
  
  var datasets = $("input[name='dataset']").serializeArray(); 
  if (datasets.length == 0){
    warnInput("select at least one dataset for the repositioning")
    return
  }
  
  $("#submitButtonHelp").hide();
  $("#submitButtonLabel").text("");
  $("#submitButton").removeAttr("disabled");
}

function inputfileOnchange() {
  var input=$("#inputfile");
  if(input.length) {
    if(input.val().length == 0) {
      $("#clearTextarea").hide();
    } else {
      $("#clearTextarea").show();
    }
  }
  inputOnchange();
}

function uploadfOnchange() {
  var input=$("#uploadf");
  if(input.length) {
    if(input.val().length==0) {
       $("#clearInputf").hide() 
     } else {
       $("#clearInputf").show() 
     }
  }
    inputOnchange()
}

function uploadfOnblur() {
  uploadfOnchange();
}


$(document).ready(function () { 
  var input=$("#uptags");
  if(input.length != 0) {
    inputOnchange();
  }
  $("#waitIcon").hide()
  $("#tabs").css('height', 320)
  $("#tabs").css('background-image', "none")
  $("#tabs").css('background-color', "#F6FAFA")
  $("#submitButtonLabel").hide()
})

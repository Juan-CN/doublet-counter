// Macro to Batch Generate Image Coordinates in CSV File 

run("Clear Results"); // clear the results table of any previous measurements

// The next line prevents ImageJ from showing the processing steps during 
// processing of a large number of images, speeding up the macro

setBatchMode(true); 

// Show the user a dialog to select a directory of images

inputDirectory = getDirectory("Choose a Directory that contains only Images");

//inputDirectory = File.getDefaultDir;

// Get the list of files from that directory
// NOTE: if there are non-image files in this directory, it may cause the macro to crash

fileList = getFileList(inputDirectory);

for (i = 0; i < fileList.length; i++)
{
	processImage(fileList[i]);
}

updateResults();  // Update the results table so it shows the filenames

setBatchMode(false); // Now disable BatchMode since we are finished

// Show a dialog to allow user to save the results file

//outputFile = File.openDialog("Save results file");
outputFile = "Results.csv"

// Save the results data

saveAs("results",outputFile);

//cleanUp();

// Closes the "Results" and "Log" windows and all image windows
//function cleanUp() {
//    requires("1.30e");
//    if (isOpen("Results")) {
//         selectWindow("Results"); 
//         run("Close" );
//   {
//   if (isOpen("Log")) {
//         selectWindow("Log");
//         run("Close" );
//    }
//    while (nImages()>0) {
//          selectImage(nImages());  
//          run("Close");
//    }
//}

function processImage(imageFile)
{
	// store the number of results before executing the commands, so we can add the filename just to the new results

	prevNumResults = nResults;  
	
	open(imageFile);

	// Get the filename from the title of the image that's open for adding to the results table
	// We do this instead of using the imageFile parameter so that the directory path is not included on the table

	filename = getTitle();
	run("8-bit");
        	setAutoThreshold("Default dark no-reset");
        	setOption("BlackBackground", true);
        	run("Convert to Mask");
	//run("Set Measurements...", "area centroid limit redirect=None decimal=2");
	run("Set Measurements...", "area centroid redirect=None decimal=2");
	run("Analyze Particles...","size=0-Infinity circularity=0.00-1.00 display");

	// Now loop through each of the new results, and add the filename to the "Filename" column

	for (row = prevNumResults; row < nResults; row++)
	{
		setResult("Filename", row, filename);
	}
//	saveAs("Results", "./Results.csv");
	close("*");  // Closes all images
}

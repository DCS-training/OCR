# USING OCR in R #######
# Working with OCR

## Outline ========
#How to import scanned text and OCR it
#Reading Results
#Visualie Result 
#Clean images with Magik
#Compare result
#Different languages
#Import multiple pages together

# Load Required Packages
library(tidyverse) #Data Wrangling
library(magick)#Image Processing
library(tesseract)# OCR


## Import Scanned Data and OCR them =======
### Load our engine -----------
# Within Tesseract, create our engine object for English
Eng <- tesseract("eng") # specify the lang with a string as the input of Tesseract

### Reading basic texts --------
# Read a sample text (First Page of an illustrated version of Pride and Prejudice)
PrideAndPr <- ocr('Data/page10.jpg', engine = Eng)#it can be .pdf or .jpg
# Quick print the result
PrideAndPr


## Read The Results ===========
#not too bad let's have a look at the accuracy -----------
results1 <- tesseract::ocr_data('Data/page10.jpg', engine = Eng) #Look at the quality of all scanned bit
results1
#Let see the mean of the accuracy 
mean(results1$confidence)

#not too bad can we see where the main issues are?


## Visualise the Results =======
### Separate the bbox into a series of variables --------
resultsEdit <- results1%>% 
  separate(bbox, c('Minx', 'Miny', 'Maxx', 'Maxy'))%>%  #separate the bbox into 4 values
  mutate(Minx=as.integer(Minx),Miny=as.integer(Miny),Maxx=as.integer(Maxx),Maxy=as.integer(Maxy))%>% #save thema as full numbers
  mutate(MeanX=round((Maxx+Minx)/2), Meany=round((Maxy+Miny)/2))# get the average x and y (pixel)


### Now let's see where the issues are  --------

ggplot(resultsEdit, aes(MeanX, Meany, colour= confidence)) + #Colour code by confidence
  geom_point(shape=15, size=5)+#plot them as squared points
  scale_y_reverse()+ # because we want it to look like our page the origin of the axis need to be top left rather than bottom left
  scale_colour_continuous(low="red", high="green") + #Create a continous scale from green to red to colour-code the results
  theme_bw()+ # b/w background
  geom_text(label=resultsEdit$word, colour="black", size=7)#plot the words on top


#Something is easy to clean like the "" 
#It would be easier to do so directly on the file
### Export ----------
write.table(PrideAndPr, file = 'Data/PrideAndPrPage10.txt')

## Clean Images to improve OCR ==============
### Let's try with a different page ----------
Incipit <- ocr('Data/page1.jpg', engine = Eng)#it can be .pdf or .jpg
# Quick print the result
Incipit 

#oh no what a mess Let see the details of each word
results2 <- tesseract::ocr_data('Data/page1.jpg', engine = Eng)
results2

#Let see the mean of the accuracy 
mean(results$confidence)

#This is very low can we think why so?

## Visualise the Results =======
### Separate the bbox into a series of variables --------
resultsEdit2 <- results2%>% 
  separate(bbox, c('Minx', 'Miny', 'Maxx', 'Maxy'))%>% #separate the bbox into 4 values
  mutate(Minx=as.integer(Minx),Miny=as.integer(Miny),Maxx=as.integer(Maxx),Maxy=as.integer(Maxy))%>% #save thema as full numbers
  mutate(MeanX=round((Maxx+Minx)/2), Meany=round((Maxy+Miny)/2))# get the average x and y (pixel)


### Now let's see where the issues are --------
ggplot(resultsEdit2, aes(MeanX, Meany, colour= confidence)) + #Colour code by confidence
  geom_point(shape=15, size=5)+#plot them as squared points
  scale_y_reverse()+ # because we want it to look like our page the origin of the axis need to be top left rather than bottom left
  scale_colour_continuous(low="red", high="green") + #Create a continous scale from green to red to colour-code the results
  theme_bw()+ # b/w background
  geom_text(label=resultsEdit2$word, colour="black", size=5)#plot the words on top

#Do the errors concentrate somewhere?
# Can we improve it?
### Let's import the image ------------------
Image <- image_read('Data/page1.jpg')
Image

# try to use magik for more than one page
### Let's try to make it better ----------
ImageEdited<-Image %>%
  image_chop('1200x2000+150')%>% #cut the top and left bit
  image_crop('1800x2500')%>%#crop an image to 1800x2500 px
  image_contrast(sharpen = 5000)%>%#increase contrast
  image_convert(type = 'Grayscale')%>% #convert image to b/W
  image_modulate(brightness = 90, saturation = 160, hue = 120)#tweak image

# How it looks like now 
ImageEdited


### Let's see the new results ----------
ImageEditedOCR <- ocr(ImageEdited, engine = Eng)
# Quick print the result
ImageEditedOCR 


### Now recompute the results -------
resultsImageEdited <- tesseract::ocr_data(ImageEdited, engine = Eng)
resultsImageEdited
### Check again the Confidence ---------
mean(resultsImageEdited$confidence)

### How much did we improve? ------
Improvment<-mean(resultsImageEdited$confidence)-mean(results$confidence)
Improvment

### Separate the bbox into a series of variables ---------
resultsEdit3 <- resultsImageEdited%>% 
  separate(bbox, c('Minx', 'Miny', 'Maxx', 'Maxy'))%>% 
  mutate(Minx=as.integer(Minx),Miny=as.integer(Miny),Maxx=as.integer(Maxx),Maxy=as.integer(Maxy))%>% 
  mutate(MeanX=round((Maxx+Minx)/2), Meany=round((Maxy+Miny)/2))

### Plot the new result ------
ggplot(resultsEdit3, aes(MeanX, Meany, colour= confidence)) + #Colour code by confidence
  geom_point(shape=15, size=5)+#plot them as squared points
  scale_y_reverse()+ # because we want it to look like our page the origin of the axis need to be top left rather than bottom left
  scale_colour_continuous(low="red", high="green") + #Create a continous scale from green to red to colour-code the results
  theme_bw()+ # b/w background
  geom_text(label=resultsEdit3$word, colour="black", size=7)#plot the words on top

#Much better 
### Export ----------
write.table(ImageEditedOCR, file = 'Data/PrideAndPrPage1.txt')


## Other languages ================================

### Let's see what happens when we run this on non-English text  ------------
Liasons <- ocr("Data/LiasonsDangerouses.jpg", engine = Eng)
Liasons

### This is not great, and it gets worse the further away from the English alphabet you get (ie. Russian) ---------
MasterAndMarga <- ocr("Data/MasterAndMargarita.jpg", engine = Eng)
MasterAndMarga

### Let's do it again with the right vocabularies ---------------
tesseract_info()

### We do not have French and Russian Let's download them ---------------
tesseract_download("rus")#Download Russian 
tesseract_download("fra")# Download French

### Save the new engine ---------------------
French <- tesseract("fra")
### And run OCR -----------------
Liasons <- ocr("Data/LiasonsDangerouses.jpg", engine = French)
Liasons

# And again
### Save the new engine ---------------------
Russian <- tesseract("rus")
### And run OCR -----------------
MasterAndMarga <- ocr("Data/MasterAndMargarita.jpg", engine = Russian)
MasterAndMarga

# I do not know if you know Russian but I don't so let's see how we did in Google translate 

#First we need to make sure it will encode it correctly 
getOption("encoding")
Encoding(MasterAndMarga) <- "UTF-16" 

### Export --------------------
write.table(MasterAndMarga, file = 'Data/MasterAndMarga.txt')


## Working with Longer Files ================================

### If you have a Longer PDF you can simply pass it through the OCR function -------------
PrideAndPrejudice <- ocr("Data/ExtractP&PB-W.pdf", engine = Eng)
PrideAndPrejudice

### Export ------------------
write.table(PrideAndPrejudice, file = 'Data/PrideAndPrejudiceLong.txt')

## What if we have a folder with single pages ================================

# If you look at our folder, you see that we have a base pattern followed by 01-28, and .jpg
pattern <- 'PAndP_' # Create the base pattern
nums <- 1:15 # number of pages in the folder
filenames <- paste0('Data/PAndP/', pattern, nums, '.jpg')

### Now apply ocr to our files using lappy (to get a list) ----------------
PrideAndPrejudice <- lapply(filenames, ocr)

# Quick print the result
PrideAndPrejudice

### Export --------------------
write.table(PrideAndPrejudice, file = 'Data/PrideAndPrejudiceLong2.txt')

# Now it is your turn! Try to replicate any of the steps we covered today using your own data 

# THE END ###########

# ascii-drawing #

ASCII Drawing - test task

Simplified version of Paint. The basic CLI program that allow users to:

1. Create a new canvas
2. Draw on the canvas using text based commands
3. Quit the program

## Commands ##

 Example       | Description
 ------------- | ------------------------------------------------------------------------------------ 
 C w h         | Create a new canvas of width w and height h                                          
 L x1 y1 x2 y2 | Draw a new line from coordinates (x1, y1) to (x2, y2) horizontally or vertically    
 R x1 y1 x2 y2 | Draw a new rectangle, with upper left corner at (x1, y1) and lower right at (x2, y2)
 Q             | Quit the program

## Requirements ##

* SBT 1.2+
* Scala 2.12+
* Java 8

## Build ##

    sbt assembly

## Run ##

    java -jar ./target/scala-2.12/drawing.jar
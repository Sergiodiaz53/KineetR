# KineetR
Set of R scripts to process multiKinect skeleton files to improve them.

Description and order of the KineetR workflow:

* Calibrat.R: R script to rotate a skeleton points Kinect file (A) to the coordinate space of (B) using the Kabsch Algorithm.

* (Optional) Align.R: R script to align two Kinect skeleton points files and paint the distance between points 

* Summariz.R: R Script to trim, align and generate files for later combine them. It also counts inferred, replacable, nframes...

* Combin.R: R Script for combine two Kinects files in order to improve its quality replacing inferred points from one with not-inferred points from other.

* Walk.R: R Script for paint each frame of a Kinect file as a cloud point representation in a scatterplot 3D.

* (Extra) Mast.R: Set of R functions from proccess Kinect files 

Front file view: oclussed points in red
![img](http://i.imgur.com/1LllHyw.png)

Side file view: Not inferred points
![img](http://i.imgur.com/Ye7lPBJ.png)

Reconstructed file view:
![img](http://i.imgur.com/gxn80zg.png)